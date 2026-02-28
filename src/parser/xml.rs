//! Core XML 1.0 parser state machine.
//!
//! Implements a hand-rolled recursive descent parser for XML 1.0 (Fifth Edition).
//! See <https://www.w3.org/TR/xml/> for the specification.

use std::collections::HashMap;

use crate::error::{ErrorSeverity, ParseError};
use crate::tree::{Attribute, Document, NodeId, NodeKind};
use crate::validation::dtd::{parse_dtd, serialize_dtd, AttributeDecl, AttributeType, EntityKind};

use super::input::{
    find_invalid_xml_char, may_contain_invalid_xml_chars, parse_cdata_content,
    parse_comment_content, parse_pi_content, parse_xml_decl, split_name, split_owned_name,
    validate_pubid, ExternalEntityInfo, NamespaceResolver, ParserInput, XMLNS_NAMESPACE,
    XML_NAMESPACE,
};
use super::ParseOptions;

/// Default maximum amplification factor for entity/attribute expansion.
///
/// libxml2 uses a factor of 5 — if the expanded output exceeds 5x the input
/// size due to default attribute application or entity expansion, the document
/// is rejected as a potential denial-of-service attack.
const DEFAULT_MAX_AMPLIFICATION: usize = 5;

/// The core XML parser.
pub(crate) struct XmlParser<'a> {
    /// Shared low-level input state (position, peek, advance, name parsing, etc.).
    input: ParserInput<'a>,
    /// The document being built.
    doc: Document,
    /// Parser options.
    options: ParseOptions,
    /// Namespace resolver managing the scope stack.
    ns: NamespaceResolver,
    /// DTD attribute type declarations, keyed by `(element_name, attr_name)`.
    /// Used for attribute value normalization of namespace URIs.
    attr_types: HashMap<(String, String), AttributeType>,
    /// DTD attribute default value declarations, keyed by element name.
    /// Used for applying default/fixed attributes from ATTLIST.
    attr_defaults: HashMap<String, Vec<AttributeDecl>>,
    /// Total input size in bytes, used for amplification factor checking.
    input_size: usize,
    /// Running total of bytes added through default attribute expansion.
    expansion_size: usize,
}

impl<'a> XmlParser<'a> {
    pub fn new(input: &'a str, options: &ParseOptions) -> Self {
        let mut pi = ParserInput::new(input);
        pi.set_recover(options.recover);
        pi.set_max_depth(options.max_depth);
        pi.set_max_name_length(options.max_name_length);
        pi.set_max_entity_expansions(options.max_entity_expansions);
        pi.set_entity_resolver(options.entity_resolver.clone());

        Self {
            input: pi,
            doc: Document::new(),
            options: options.clone(),
            ns: NamespaceResolver::new(),
            attr_types: HashMap::new(),
            attr_defaults: HashMap::new(),
            input_size: input.len(),
            expansion_size: 0,
        }
    }

    /// Main parse entry point. Parses the entire document.
    pub fn parse(&mut self) -> Result<Document, ParseError> {
        // Parse optional XML declaration — must be at the very start of the
        // document with no leading whitespace (XML 1.0 §2.8).
        if self.input.looking_at(b"<?xml ")
            || self.input.looking_at(b"<?xml\t")
            || self.input.looking_at(b"<?xml\r")
        {
            self.parse_xml_declaration()?;
            // Skip whitespace immediately after the XML declaration — the
            // serializer always emits its own newline after the declaration.
            self.input.skip_whitespace();
        } else if !self.input.at_end() {
            // If there's no XML declaration, skip any leading whitespace.
            // Leading whitespace before a non-declaration is tolerated
            // (it will be handled as misc content).
            let had_leading_ws = self.input.skip_whitespace();
            // But if the whitespace was hiding an XML declaration, that's an error
            if had_leading_ws
                && (self.input.looking_at(b"<?xml ")
                    || self.input.looking_at(b"<?xml\t")
                    || self.input.looking_at(b"<?xml\r"))
            {
                return Err(self
                    .input
                    .fatal("XML declaration must be at the start of the document"));
            }
        }

        // Parse prolog content (comments, PIs, whitespace before root element)
        self.parse_misc(self.doc.root())?;

        // Parse optional DOCTYPE declaration
        if self.input.looking_at(b"<!DOCTYPE") || self.input.looking_at(b"<!doctype") {
            self.parse_doctype(self.doc.root())?;
            self.parse_misc(self.doc.root())?; // more misc after doctype
        }

        // Parse root element (required by XML 1.0 §2.1)
        if self.input.peek() == Some(b'<')
            && self
                .input
                .peek_at(1)
                .is_some_and(|b| b != b'!' && b != b'?')
        {
            self.parse_element(self.doc.root())?;
        } else if self.options.recover {
            self.input
                .push_diagnostic(ErrorSeverity::Error, "missing root element".to_string());
        } else {
            return Err(self.input.fatal("missing root element"));
        }

        // Parse trailing content (comments, PIs after root element)
        self.parse_misc(self.doc.root())?;

        self.input.skip_whitespace();
        if !self.input.at_end() && !self.options.recover {
            return Err(self.input.fatal("content after document element"));
        }

        // Sync diagnostics from input to document before returning.
        self.doc.diagnostics = std::mem::take(&mut self.input.diagnostics);

        Ok(std::mem::take(&mut self.doc))
    }

    // --- XML Declaration ---
    // See XML 1.0 §2.8: [23] XMLDecl

    fn parse_xml_declaration(&mut self) -> Result<(), ParseError> {
        let decl = parse_xml_decl(&mut self.input)?;
        self.doc.version = Some(decl.version);
        self.doc.encoding = decl.encoding;
        self.doc.standalone = decl.standalone;
        Ok(())
    }

    // --- Misc (comments, PIs, whitespace) ---

    fn parse_misc(&mut self, parent: NodeId) -> Result<(), ParseError> {
        loop {
            // Preserve document-level whitespace as text nodes (matches libxml2).
            // libxml2 normalizes prolog/epilog whitespace to a single `\n`
            // regardless of how many blank lines appear in the source.
            let ws = self.input.consume_whitespace();
            if !ws.is_empty() {
                let ws_node = self.doc.create_node(NodeKind::Text {
                    content: "\n".to_string(),
                });
                self.doc.append_child(parent, ws_node);
            }
            if self.input.at_end() {
                break;
            }
            if self.input.looking_at(b"<!--") {
                self.parse_comment(parent)?;
            } else if self.input.looking_at(b"<?") {
                self.parse_processing_instruction(parent)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    // --- DOCTYPE Declaration ---
    // See XML 1.0 §2.8: [28] doctypedecl

    #[allow(clippy::too_many_lines)]
    fn parse_doctype(&mut self, parent: NodeId) -> Result<(), ParseError> {
        // Consume <!DOCTYPE (case-insensitive match already checked by caller)
        self.input.expect_str(b"<!DOCTYPE")?;
        self.input.skip_whitespace_required()?;

        // Read the root element name
        let name = self.input.parse_name()?;

        self.input.skip_whitespace();

        // Check for external ID: SYSTEM or PUBLIC
        let mut system_id = None;
        let mut public_id = None;

        if self.input.looking_at(b"SYSTEM") {
            self.input.expect_str(b"SYSTEM")?;
            self.input.skip_whitespace_required()?;
            system_id = Some(self.input.parse_quoted_value()?);
            self.input.skip_whitespace();
        } else if self.input.looking_at(b"PUBLIC") {
            self.input.expect_str(b"PUBLIC")?;
            self.input.skip_whitespace_required()?;
            let pid = self.input.parse_quoted_value()?;
            // Validate public ID characters per XML 1.0 §2.3 [13].
            if let Some(msg) = validate_pubid(&pid) {
                if self.options.recover {
                    self.input.push_diagnostic(ErrorSeverity::Warning, msg);
                } else {
                    return Err(self.input.fatal(msg));
                }
            }
            public_id = Some(pid);
            self.input.skip_whitespace_required()?;
            system_id = Some(self.input.parse_quoted_value()?);
            self.input.skip_whitespace();
        }

        // Flag whether there's an external DTD subset. Per XML 1.0 §4.1 WFC:
        // Entity Declared, undeclared entities are not WF errors when the
        // document references an unread external DTD subset.
        if system_id.is_some() || public_id.is_some() {
            self.input.has_external_dtd = true;
        }

        // Parse optional internal subset: [ ... ]
        let mut internal_subset = None;
        if self.input.peek() == Some(b'[') {
            self.input.advance(1);
            let start = self.input.pos();

            // Scan to matching ']', tracking depth for bracket chars inside
            // entity values. Quoted strings and comments are skipped to avoid
            // misinterpreting brackets or apostrophes in comments.
            let mut depth: u32 = 1;
            while !self.input.at_end() && depth > 0 {
                if self.input.looking_at(b"<!--") {
                    // Skip XML comments (may contain apostrophes/quotes)
                    self.input.advance(4);
                    while !self.input.at_end() && !self.input.looking_at(b"-->") {
                        self.input.advance(1);
                    }
                    if !self.input.at_end() {
                        self.input.advance(3); // consume -->
                    }
                } else if let Some(b'"' | b'\'') = self.input.peek() {
                    let quote = self.input.peek().unwrap_or(b'"');
                    self.input.advance(1);
                    while !self.input.at_end() && self.input.peek() != Some(quote) {
                        self.input.advance(1);
                    }
                    if !self.input.at_end() {
                        self.input.advance(1); // closing quote
                    }
                } else if self.input.peek() == Some(b'[') {
                    depth += 1;
                    self.input.advance(1);
                } else if self.input.peek() == Some(b']') {
                    depth -= 1;
                    self.input.advance(1);
                } else {
                    self.input.advance(1);
                }
            }
            if depth > 0 {
                return Err(self
                    .input
                    .fatal("unexpected end of input in internal subset"));
            }

            // Extract the internal subset text (between '[' and ']').
            let end = self.input.pos() - 1; // exclude the closing ']'
            let subset_text = std::str::from_utf8(self.input.slice(start, end))
                .ok()
                .map(str::to_string);

            if let Some(subset_text) = subset_text {
                // Detect parameter entity references in the internal subset.
                // Per XML 1.0 §4.1 WFC: Entity Declared, their presence
                // means undeclared general entities are not WF errors.
                if subset_text.contains('%') {
                    self.input.has_pe_references = true;
                }

                match parse_dtd(&subset_text) {
                    Ok(dtd) => {
                        // Wire entity declarations into the parser input
                        // for entity reference resolution.
                        for (ent_name, ent_decl) in &dtd.entities {
                            match &ent_decl.kind {
                                EntityKind::Internal(value) => {
                                    self.input
                                        .entity_map
                                        .insert(ent_name.clone(), value.clone());
                                }
                                EntityKind::External {
                                    system_id,
                                    public_id,
                                } => {
                                    self.input.entity_external.insert(
                                        ent_name.clone(),
                                        ExternalEntityInfo {
                                            system_id: system_id.clone(),
                                            public_id: public_id.clone(),
                                        },
                                    );
                                }
                            }
                        }

                        // Wire attribute type declarations for namespace
                        // URI normalization (XML 1.0 §3.3.3).
                        for (element_name, attrs) in &dtd.attributes {
                            for attr_decl in attrs {
                                self.attr_types.insert(
                                    (element_name.clone(), attr_decl.attribute_name.clone()),
                                    attr_decl.attribute_type.clone(),
                                );
                            }
                        }

                        // Store attribute default values for later application.
                        for (element_name, attrs) in &dtd.attributes {
                            for attr_decl in attrs {
                                self.attr_defaults
                                    .entry(element_name.clone())
                                    .or_default()
                                    .push(attr_decl.clone());
                            }
                        }

                        // Re-serialize the DTD from parsed structures for
                        // consistent formatting (matches libxml2 behavior).
                        let serialized = serialize_dtd(&dtd);
                        if !serialized.is_empty() {
                            internal_subset = Some(serialized);
                        }
                    }
                    Err(e) => {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Warning,
                                format!("error parsing DTD internal subset: {}", e.message),
                            );
                        } else {
                            return Err(self
                                .input
                                .fatal(format!("error in DTD internal subset: {}", e.message)));
                        }
                    }
                }
            }

            self.input.skip_whitespace();
        }

        self.input.expect_byte(b'>')?;

        let doctype_id = self.doc.create_node(NodeKind::DocumentType {
            name,
            system_id,
            public_id,
            internal_subset,
        });
        self.doc.append_child(parent, doctype_id);
        Ok(())
    }

    // --- Elements ---
    // See XML 1.0 §3.1: [40] STag, [42] ETag, [44] EmptyElemTag

    #[allow(clippy::too_many_lines)]
    fn parse_element(&mut self, parent: NodeId) -> Result<NodeId, ParseError> {
        self.input.increment_depth()?;
        self.input.expect_byte(b'<')?;
        let name = self.input.parse_name()?;
        let mut attributes = Vec::new();

        // Parse attributes
        loop {
            let had_ws = self.input.skip_whitespace();
            if self.input.peek() == Some(b'>') || self.input.looking_at(b"/>") {
                break;
            }
            if !had_ws {
                return Err(self.input.fatal("whitespace required between attributes"));
            }
            let attr = self.parse_attribute()?;
            attributes.push(attr);
        }

        // Check for duplicate attributes (XML 1.0 §3.1 WFC: Unique Att Spec)
        // Skip for 0 or 1 attributes (no duplicates possible).
        if attributes.len() >= 2 {
            // O(n²) comparison avoids HashSet allocation for small attribute lists.
            let mut found_dup = false;
            'outer: for i in 1..attributes.len() {
                for j in 0..i {
                    if attributes[i].name == attributes[j].name
                        && attributes[i].prefix == attributes[j].prefix
                    {
                        let full_name = if let Some(ref pfx) = attributes[i].prefix {
                            format!("{pfx}:{}", attributes[i].name)
                        } else {
                            attributes[i].name.clone()
                        };
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                format!("duplicate attribute: '{full_name}'"),
                            );
                            found_dup = true;
                        } else {
                            return Err(self
                                .input
                                .fatal(format!("duplicate attribute: '{full_name}'")));
                        }
                        break 'outer;
                    }
                }
            }
            let _ = found_dup; // suppress unused warning
        }

        // --- Apply #FIXED default attributes from DTD ATTLIST declarations ---
        // Per XML 1.0 §3.3.2, if an attribute declared with #FIXED is not
        // present on the element, the parser must add it with the declared
        // default value. #DEFAULT attributes are tracked for amplification
        // factor checking but not inserted into the tree (matching libxml2).
        // Namespace declarations (xmlns, xmlns:prefix) are inserted before
        // other attributes to match libxml2's attribute ordering.
        if let Some(defaults) = if self.attr_defaults.is_empty() {
            None
        } else {
            self.attr_defaults.get(&name).cloned()
        } {
            let mut insert_pos = 0; // insertion point for namespace declarations
            for attr_decl in &defaults {
                let (default_value, is_fixed) = match &attr_decl.default {
                    crate::validation::dtd::AttributeDefault::Fixed(v) => (Some(v.clone()), true),
                    crate::validation::dtd::AttributeDefault::Default(v) => {
                        (Some(v.clone()), false)
                    }
                    _ => (None, false),
                };
                if let Some(value) = default_value {
                    // Check if the attribute is already present by comparing
                    // prefix:local components directly, avoiding format!/clone.
                    let attr_name = &attr_decl.attribute_name;
                    let (decl_pfx, decl_local) = split_name(attr_name);
                    let already_present = attributes
                        .iter()
                        .any(|a| a.name == decl_local && a.prefix.as_deref() == decl_pfx);
                    if !already_present {
                        // Track expansion for amplification factor check
                        // (both #FIXED and #DEFAULT contribute to expansion)
                        self.expansion_size += attr_name.len() + value.len();

                        // Only insert #FIXED attributes into the tree
                        if is_fixed {
                            let (decl_prefix, decl_local) = split_name(attr_name);
                            let attr = Attribute {
                                name: decl_local.to_string(),
                                value,
                                prefix: decl_prefix.map(String::from),
                                namespace: None,
                                raw_value: None,
                            };
                            let is_ns_decl =
                                attr_name == "xmlns" || attr_name.starts_with("xmlns:");
                            if is_ns_decl {
                                attributes.insert(insert_pos, attr);
                                insert_pos += 1;
                            } else {
                                attributes.push(attr);
                            }
                        }
                    }
                }
            }

            // Check amplification factor: reject if default attribute
            // expansion would exceed the input size by more than the
            // maximum factor (matching libxml2's xmlCtxtSetMaxAmplification).
            if self.expansion_size > self.input_size.saturating_mul(DEFAULT_MAX_AMPLIFICATION) {
                return Err(self
                    .input
                    .fatal("maximum entity amplification factor exceeded"));
            }
        }

        // --- Namespace processing (Namespaces in XML 1.0 section 3) ---

        // Check for namespace declarations to skip namespace scope push/pop
        // when not needed. Uses short-circuiting .any() for fast exit.
        let has_ns_decls = attributes.iter().any(|a| {
            a.prefix.as_deref() == Some("xmlns") || (a.prefix.is_none() && a.name == "xmlns")
        });
        if has_ns_decls {
            self.ns.push_scope();
        }

        // Split into prefix and local name for namespace processing.
        let (prefix, local_name) = split_name(&name);

        // Validate QName syntax: check for multiple colons (the local part
        // should not contain a colon after split_name).
        if prefix.is_some() && local_name.contains(':') {
            let msg = "QName contains multiple colons";
            if self.options.recover {
                self.input
                    .push_diagnostic(ErrorSeverity::Error, msg.to_string());
            } else {
                return Err(self.input.fatal(msg));
            }
        }

        // Reject element names with "xmlns" prefix (Namespaces in XML 1.0 §3).
        if prefix == Some("xmlns") {
            if self.options.recover {
                self.input.push_diagnostic(
                    ErrorSeverity::Error,
                    "elements must not have the prefix 'xmlns'".to_string(),
                );
            } else {
                return Err(self
                    .input
                    .fatal("elements must not have the prefix 'xmlns'"));
            }
        }

        // Scan attributes for namespace declarations and bind them,
        // with validation of namespace constraints.
        // In recovery mode, some invalid attributes may be stripped.
        let mut strip_attr_indices: Vec<usize> = Vec::new();
        if has_ns_decls {
            for (attr_idx, attr) in attributes.iter().enumerate() {
                if attr.prefix.as_deref() == Some("xmlns") {
                    // Prefixed namespace declaration: xmlns:prefix="uri"
                    let declared_prefix = &attr.name;

                    // Validate QName: the local part (declared_prefix) must be
                    // a non-empty NCName (no colon, not empty). An empty local
                    // part means the attribute was `xmlns:` with nothing after
                    // the colon, which is not a valid QName.
                    if declared_prefix.is_empty() {
                        let msg = "namespace prefix must not be empty (invalid QName 'xmlns:')";
                        if self.options.recover {
                            self.input
                                .push_diagnostic(ErrorSeverity::Error, msg.to_string());
                            continue;
                        }
                        return Err(self.input.fatal(msg));
                    }
                    if declared_prefix.contains(':') {
                        let msg = "QName contains multiple colons";
                        if self.options.recover {
                            self.input
                                .push_diagnostic(ErrorSeverity::Error, msg.to_string());
                        } else {
                            return Err(self.input.fatal(msg));
                        }
                    }

                    // Normalize namespace URI based on DTD-declared attribute
                    // type. For non-CDATA types (e.g., NMTOKEN), whitespace is
                    // collapsed per XML 1.0 §3.3.3. Construct the full attribute
                    // name only when attr_types is non-empty (DTD present).
                    let ns_value = if self.attr_types.is_empty() {
                        attr.value.clone()
                    } else {
                        let attr_qname = format!("xmlns:{declared_prefix}");
                        self.normalize_attr_value_by_type(&name, &attr_qname, &attr.value)
                    };

                    // XML 1.0 Namespaces: cannot unbind a prefix (xmlns:prefix="").
                    if ns_value.is_empty() {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                format!("namespace prefix '{declared_prefix}' cannot be undeclared in XML 1.0"),
                            );
                        } else {
                            return Err(self.input.fatal(format!(
                                "namespace prefix '{declared_prefix}' cannot be undeclared in XML 1.0"
                            )));
                        }
                    }

                    // Cannot declare the 'xmlns' prefix itself.
                    if declared_prefix == "xmlns" {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                "the 'xmlns' prefix must not be declared".to_string(),
                            );
                        } else {
                            return Err(self
                                .input
                                .fatal("the 'xmlns' prefix must not be declared"));
                        }
                    }

                    // 'xml' prefix must map to the XML namespace URI and vice versa.
                    if declared_prefix == "xml" && ns_value != XML_NAMESPACE {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                "the 'xml' prefix must be bound to the XML namespace".to_string(),
                            );
                            // In recovery mode, strip the invalid rebinding
                            // (matches libxml2: output is <tst/> not <tst xmlns:xml="..."/>).
                            strip_attr_indices.push(attr_idx);
                            continue;
                        }
                        return Err(self
                            .input
                            .fatal("the 'xml' prefix must be bound to the XML namespace"));
                    }

                    // No other prefix may be bound to the XML namespace URI.
                    if declared_prefix != "xml" && ns_value == XML_NAMESPACE {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                "only the 'xml' prefix may be bound to the XML namespace"
                                    .to_string(),
                            );
                        } else {
                            return Err(self
                                .input
                                .fatal("only the 'xml' prefix may be bound to the XML namespace"));
                        }
                    }

                    // No prefix may be bound to the xmlns namespace URI.
                    if ns_value == XMLNS_NAMESPACE {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                "the xmlns namespace must not be bound to any prefix".to_string(),
                            );
                        } else {
                            return Err(self
                                .input
                                .fatal("the xmlns namespace must not be bound to any prefix"));
                        }
                    }

                    self.ns.bind(Some(attr.name.clone()), ns_value);
                } else if attr.prefix.is_none() && attr.name == "xmlns" {
                    // Default namespace declaration: xmlns="uri"

                    // Normalize namespace URI based on DTD-declared attribute type.
                    let ns_value = if self.attr_types.is_empty() {
                        attr.value.clone()
                    } else {
                        self.normalize_attr_value_by_type(&name, "xmlns", &attr.value)
                    };

                    // Cannot bind default namespace to the XML or xmlns namespace URIs.
                    if ns_value == XML_NAMESPACE {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                "the xml namespace must not be declared as the default namespace"
                                    .to_string(),
                            );
                        } else {
                            return Err(self.input.fatal(
                                "the xml namespace must not be declared as the default namespace",
                            ));
                        }
                    }
                    if ns_value == XMLNS_NAMESPACE {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                "the xmlns namespace must not be declared as the default namespace"
                                    .to_string(),
                            );
                        } else {
                            return Err(self.input.fatal(
                                "the xmlns namespace must not be declared as the default namespace",
                            ));
                        }
                    }
                    self.ns.bind(None, ns_value);
                } else if attr.prefix.is_some() && attr.name.contains(':') {
                    // Validate QName syntax for prefixed non-namespace attributes.
                    let msg = "QName contains multiple colons";
                    if self.options.recover {
                        self.input
                            .push_diagnostic(ErrorSeverity::Error, msg.to_string());
                    } else {
                        return Err(self.input.fatal(msg));
                    }
                }
            }
        } else {
            // No namespace declarations — only validate QName syntax for
            // prefixed attributes (checking for multiple colons).
            for attr in &attributes {
                if attr.prefix.is_some() && attr.name.contains(':') {
                    let msg = "QName contains multiple colons";
                    if self.options.recover {
                        self.input
                            .push_diagnostic(ErrorSeverity::Error, msg.to_string());
                    } else {
                        return Err(self.input.fatal(msg));
                    }
                }
            }
        }

        // Resolve the element's namespace URI from its prefix.
        let elem_ns = self.ns.resolve(prefix).map(String::from);

        // Check for unbound element prefix.
        if let Some(pfx) = prefix {
            if pfx != "xml" && elem_ns.is_none() {
                if self.options.recover {
                    self.input.push_diagnostic(
                        ErrorSeverity::Error,
                        format!("unbound namespace prefix '{pfx}'"),
                    );
                } else {
                    return Err(self
                        .input
                        .fatal(format!("unbound namespace prefix '{pfx}'")));
                }
            }
        }

        // Resolve namespace URIs for non-xmlns prefixed attributes.
        // Unprefixed attributes do NOT inherit the default namespace (per spec).
        // Skip entirely when there are no prefixed non-xmlns attributes.
        let has_prefixed_attrs = attributes
            .iter()
            .any(|a| a.prefix.is_some() && a.prefix.as_deref() != Some("xmlns"));
        if has_prefixed_attrs {
            for attr in &mut attributes {
                if let Some(pfx) = &attr.prefix {
                    if pfx == "xmlns" {
                        continue; // namespace declaration, not a real attribute prefix
                    }
                    let resolved = self.ns.resolve(Some(pfx.as_str())).map(String::from);
                    if pfx != "xml" && resolved.is_none() {
                        if self.options.recover {
                            self.input.push_diagnostic(
                                ErrorSeverity::Error,
                                format!("unbound namespace prefix '{pfx}' on attribute"),
                            );
                        } else {
                            return Err(self
                                .input
                                .fatal(format!("unbound namespace prefix '{pfx}' on attribute")));
                        }
                    }
                    attr.namespace = resolved;
                }
            }
        }

        // Namespace-aware attribute uniqueness: two attributes with the same
        // namespace URI and local name are duplicates, even if they use different
        // prefixes (Namespaces in XML 1.0 §6.3).
        // Only meaningful when there are 2+ namespaced (non-xmlns) attributes.
        // Skip entirely when no prefixed attributes exist (no namespaces were
        // resolved, so ns_attr_count is guaranteed to be 0).
        if has_prefixed_attrs {
            let ns_attr_count = attributes.iter().filter(|a| a.namespace.is_some()).count();
            if ns_attr_count >= 2 {
                // O(n²) comparison avoids HashSet allocation
                'ns_outer: for i in 1..attributes.len() {
                    if attributes[i].namespace.is_none() {
                        continue;
                    }
                    for j in 0..i {
                        if attributes[j].namespace.is_none() {
                            continue;
                        }
                        if attributes[i].namespace == attributes[j].namespace
                            && attributes[i].name == attributes[j].name
                        {
                            let display = if let Some(ns) = &attributes[i].namespace {
                                format!("{{{}}}:{}", ns, attributes[i].name)
                            } else {
                                attributes[i].name.clone()
                            };
                            if self.options.recover {
                                self.input.push_diagnostic(
                                    ErrorSeverity::Error,
                                    format!("namespace-aware duplicate attribute: '{display}'"),
                                );
                            } else {
                                return Err(self.input.fatal(format!(
                                    "namespace-aware duplicate attribute: '{display}'"
                                )));
                            }
                            break 'ns_outer;
                        }
                    }
                }
            }
        }

        // Remove stripped attributes (e.g., invalid xmlns:xml rebindings).
        if !strip_attr_indices.is_empty() {
            // Remove in reverse order to preserve indices.
            for &idx in strip_attr_indices.iter().rev() {
                attributes.remove(idx);
            }
        }

        // Consume the original name String via split_owned_name, avoiding a
        // re-allocation for unprefixed names (the common case). For unprefixed
        // names, split_owned_name returns (None, name) — just a move, zero copy.
        let (elem_prefix_owned, elem_local_owned) = split_owned_name(name);
        let elem_id = self.doc.create_node(NodeKind::Element {
            name: elem_local_owned,
            prefix: elem_prefix_owned,
            namespace: elem_ns,
            attributes,
        });
        self.doc.append_child(parent, elem_id);

        // Empty element tag <foo/>
        if self.input.looking_at(b"/>") {
            self.input.advance(2);
            if has_ns_decls {
                self.ns.pop_scope();
            }
            self.input.decrement_depth();
            return Ok(elem_id);
        }

        // Start tag close >
        self.input.expect_byte(b'>')?;

        // Parse element content
        self.parse_content(elem_id)?;

        // Parse end tag — read back the stored name from the tree node
        // for matching, since the original name was consumed by split_owned_name.
        self.input.expect_str(b"</")?;
        let (match_prefix, match_local) = {
            let node = self.doc.node(elem_id);
            match &node.kind {
                NodeKind::Element { name, prefix, .. } => (prefix.as_deref(), name.as_str()),
                _ => unreachable!(),
            }
        };
        if let Some(end_name) = self.input.parse_name_eq_parts(match_prefix, match_local)? {
            let expected = match match_prefix {
                Some(pfx) => format!("{pfx}:{match_local}"),
                None => match_local.to_string(),
            };
            if self.options.recover {
                self.input.push_diagnostic(
                    ErrorSeverity::Error,
                    format!("mismatched end tag: expected </{expected}>, found </{end_name}>"),
                );
            } else {
                return Err(self.input.fatal(format!(
                    "mismatched end tag: expected </{expected}>, found </{end_name}>"
                )));
            }
        }
        self.input.skip_whitespace();
        self.input.expect_byte(b'>')?;

        // Pop the namespace scope when leaving this element (only if we pushed).
        if has_ns_decls {
            self.ns.pop_scope();
        }
        self.input.decrement_depth();

        Ok(elem_id)
    }

    /// Normalizes an attribute value based on its DTD-declared type.
    ///
    /// For non-CDATA types (e.g., NMTOKEN, ID, IDREF), collapses whitespace:
    /// trim leading/trailing whitespace, reduce internal whitespace sequences
    /// to single spaces (XML 1.0 §3.3.3).
    fn normalize_attr_value_by_type(
        &self,
        element_name: &str,
        attr_name: &str,
        value: &str,
    ) -> String {
        // Fast path: skip lookup when no DTD attribute types are declared
        // (the common case for documents without a DTD).
        if !self.attr_types.is_empty() {
            let key = (element_name.to_string(), attr_name.to_string());
            if let Some(attr_type) = self.attr_types.get(&key) {
                if !matches!(attr_type, AttributeType::CData) {
                    return value.split_whitespace().collect::<Vec<_>>().join(" ");
                }
            }
        }
        value.to_string()
    }

    // --- Content ---
    // See XML 1.0 §3.1: [43] content

    fn parse_content(&mut self, parent: NodeId) -> Result<(), ParseError> {
        loop {
            if self.input.at_end() {
                if self.options.recover {
                    break;
                }
                return Err(self
                    .input
                    .fatal("unexpected end of input in element content"));
            }

            // End tag starts
            if self.input.looking_at(b"</") {
                break;
            }

            if self.input.looking_at(b"<![CDATA[") {
                self.parse_cdata(parent)?;
            } else if self.input.looking_at(b"<!--") {
                self.parse_comment(parent)?;
            } else if self.input.looking_at(b"<?") {
                self.parse_processing_instruction(parent)?;
            } else if self.input.peek() == Some(b'<') {
                self.parse_element(parent)?;
            } else {
                self.parse_char_data(parent)?;
            }
        }
        Ok(())
    }

    // --- Character Data ---
    // See XML 1.0 §2.4: [14] CharData

    #[allow(clippy::too_many_lines)]
    fn parse_char_data(&mut self, parent: NodeId) -> Result<(), ParseError> {
        let mut text = String::new();

        while !self.input.at_end() {
            // Bulk scan: find the next `<`, `&`, or `]]>` boundary and
            // consume all safe bytes in one go.
            let safe_len = self.input.scan_char_data();
            if safe_len > 0 {
                let start = self.input.pos();
                let chunk = std::str::from_utf8(self.input.slice(start, start + safe_len))
                    .map_err(|_| self.input.fatal("invalid UTF-8 in character data"))?;
                // Fast byte-level pre-check for invalid XML chars (0x7F,
                // U+FFFE, U+FFFF). Skips the expensive char-by-char
                // validation for the 99.9% of chunks that are clean.
                let bad_char = if may_contain_invalid_xml_chars(chunk.as_bytes()) {
                    find_invalid_xml_char(chunk)
                } else {
                    None
                };
                // Append text with CR normalization if needed (XML 1.0 §2.11)
                if chunk.as_bytes().contains(&b'\r') {
                    let mut chars = chunk.chars().peekable();
                    while let Some(ch) = chars.next() {
                        if ch == '\r' {
                            if chars.peek() == Some(&'\n') {
                                chars.next();
                            }
                            text.push('\n');
                        } else {
                            text.push(ch);
                        }
                    }
                } else {
                    text.push_str(chunk);
                }
                // chunk borrow released — safe to mutably borrow self.input
                self.input.advance_counting_lines(safe_len);
                if let Some(bad) = bad_char {
                    if self.options.recover {
                        self.input.push_diagnostic(
                            ErrorSeverity::Error,
                            format!("invalid XML character: U+{:04X}", bad as u32),
                        );
                    } else {
                        return Err(self
                            .input
                            .fatal(format!("invalid XML character: U+{:04X}", bad as u32)));
                    }
                }
                continue;
            }

            if self.input.peek() == Some(b'<') {
                break;
            }

            // XML 1.0 §2.4: "]]>" is forbidden in character data
            if self.input.looking_at(b"]]>") {
                if self.options.recover {
                    self.input.push_diagnostic(
                        ErrorSeverity::Error,
                        "']]>' not allowed in character data".to_string(),
                    );
                    text.push_str("]]>");
                    self.input.advance(3);
                    continue;
                }
                return Err(self.input.fatal("']]>' not allowed in character data"));
            }

            if self.input.peek() == Some(b'&') {
                // Check if this is a named entity reference (not char ref, not builtin)
                // whose replacement text is plain text (no '<'). If so, preserve
                // it as an EntityRef node rather than expanding. Entities that
                // contain XML markup ('<') must still be expanded so that the
                // markup is validated (e.g., namespace prefix checking).
                // Skip the peek for builtins at byte level to avoid String allocation.
                if self.input.peek_at(1) != Some(b'#') && !self.is_looking_at_builtin_entity_ref() {
                    if let Some(entity_name) = self.peek_entity_ref_name() {
                        // entity_name is guaranteed non-builtin at this point.
                        // Preserve as EntityRef node if:
                        // 1. The entity is internally declared and text-only (no '<'), OR
                        // 2. The entity is undeclared but we're in tolerant mode
                        //    (external DTD or PE refs make undeclared entities non-fatal)
                        let is_text_only =
                            self.input.entity_map.get(&entity_name).is_some_and(|v| {
                                // Check the replacement text after expanding character
                                // references, not the raw value. An entity like
                                // "&#60;foo>" contains no literal '<' but expands to
                                // "<foo>" which is markup and must be re-parsed.
                                let replacement = crate::validation::dtd::expand_char_refs_only(v);
                                !replacement.contains('<')
                            });
                        let is_undeclared_tolerant =
                            !self.input.entity_map.contains_key(&entity_name)
                                && !self.input.entity_external.contains_key(&entity_name)
                                && (self.input.has_pe_references || self.input.has_external_dtd);
                        let should_preserve = is_text_only || is_undeclared_tolerant;

                        if should_preserve {
                            // Flush accumulated text before the entity ref
                            if !text.is_empty() {
                                let text_id = self.doc.create_node(NodeKind::Text {
                                    content: std::mem::take(&mut text),
                                });
                                self.doc.append_child(parent, text_id);
                            }
                            // Consume &name;
                            self.input.advance(1); // &
                            let name = self.input.parse_name()?;
                            self.input.expect_byte(b';')?;
                            // Count entity expansion for limit tracking
                            self.input.entity_expansions += 1;
                            let entity_value = self.input.entity_map.get(&name).cloned();
                            let ref_id = self.doc.create_node(NodeKind::EntityRef {
                                name,
                                value: entity_value,
                            });
                            self.doc.append_child(parent, ref_id);
                            continue;
                        }
                    }
                }
                self.input.parse_reference_into(&mut text)?;
            } else {
                let ch = self.input.next_char()?;
                text.push(ch);
            }
        }

        if !text.is_empty() {
            // Strip blank text nodes if configured
            if self.options.no_blanks && text.chars().all(char::is_whitespace) {
                return Ok(());
            }
            let text_id = self.doc.create_node(NodeKind::Text { content: text });
            self.doc.append_child(parent, text_id);
        }

        Ok(())
    }

    /// Checks if the input is positioned at a builtin entity reference
    /// (`&amp;`, `&lt;`, `&gt;`, `&apos;`, `&quot;`) using byte-level
    /// checks. This avoids the `String` allocation of `peek_entity_ref_name`.
    #[inline]
    fn is_looking_at_builtin_entity_ref(&self) -> bool {
        let remaining = self.input.remaining();
        if remaining.len() < 4 || remaining[0] != b'&' {
            return false;
        }
        let after_amp = &remaining[1..];
        after_amp.starts_with(b"lt;")
            || after_amp.starts_with(b"gt;")
            || after_amp.starts_with(b"amp;")
            || after_amp.starts_with(b"apos;")
            || after_amp.starts_with(b"quot;")
    }

    /// Peeks ahead to extract the entity name from `&name;` without consuming
    /// any input. Returns `None` if the next bytes don't form a valid entity
    /// reference pattern.
    fn peek_entity_ref_name(&self) -> Option<String> {
        // We're at `&` — look ahead past it to find the name and `;`
        let remaining = self.input.remaining();
        if remaining.len() < 2 || remaining[0] != b'&' {
            return None;
        }
        let mut i = 1;
        // Collect name bytes
        let name_start = i;
        while i < remaining.len()
            && (remaining[i].is_ascii_alphanumeric()
                || remaining[i] == b'_'
                || remaining[i] == b':'
                || remaining[i] == b'-'
                || remaining[i] == b'.')
        {
            i += 1;
        }
        if i == name_start || i >= remaining.len() || remaining[i] != b';' {
            return None;
        }
        std::str::from_utf8(&remaining[name_start..i])
            .ok()
            .map(String::from)
    }

    /// Checks if all entity references (`&name;`) in a raw attribute value
    /// text are declared in the entity map. Returns false if any undeclared
    /// entity ref is found (these should not be preserved via `raw_value`).
    fn all_entity_refs_declared(&self, raw: &str) -> bool {
        let bytes = raw.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'&' && i + 1 < bytes.len() && bytes[i + 1] != b'#' {
                // Named entity reference — extract the name
                let mut j = i + 1;
                while j < bytes.len()
                    && (bytes[j].is_ascii_alphanumeric()
                        || bytes[j] == b'_'
                        || bytes[j] == b':'
                        || bytes[j] == b'-'
                        || bytes[j] == b'.')
                {
                    j += 1;
                }
                if j < bytes.len() && bytes[j] == b';' {
                    let name = std::str::from_utf8(&bytes[i + 1..j]).unwrap_or("");
                    if !is_builtin_entity(name) && !self.input.entity_map.contains_key(name) {
                        return false;
                    }
                    i = j + 1;
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
        true
    }

    // --- Attributes ---
    // See XML 1.0 §3.1: [41] Attribute

    fn parse_attribute(&mut self) -> Result<Attribute, ParseError> {
        let name = self.input.parse_name()?;
        self.input.skip_whitespace();
        self.input.expect_byte(b'=')?;
        self.input.skip_whitespace();

        // Capture raw attribute value text (before entity expansion) so we
        // can preserve entity references during serialization.
        let raw_start = self.input.pos();
        let value = self.input.parse_attribute_value()?;
        let raw_end = self.input.pos();

        // Extract raw value (between quotes) — the raw slice includes the
        // outer quote chars, so trim them. Skip entirely when the raw bytes
        // contain no '&' (most attributes have no entity references).
        let raw_value = if raw_end > raw_start + 2 {
            let raw_bytes = self.input.slice(raw_start + 1, raw_end - 1);
            if raw_bytes.contains(&b'&') {
                let raw_str = std::str::from_utf8(raw_bytes).ok().map(str::to_string);
                // Only store raw_value if it differs from the expanded value
                // (i.e., it contained entity references that got expanded) AND
                // all entity references in the raw value are declared (not
                // undeclared entities that expanded to empty string).
                raw_str.filter(|raw| *raw != value && self.all_entity_refs_declared(raw))
            } else {
                None
            }
        } else {
            None
        };

        let (prefix, local_name) = split_owned_name(name);

        Ok(Attribute {
            name: local_name,
            value,
            prefix,
            namespace: None,
            raw_value,
        })
    }

    // --- Comments ---
    // See XML 1.0 §2.5: [15] Comment

    fn parse_comment(&mut self, parent: NodeId) -> Result<(), ParseError> {
        let content = parse_comment_content(&mut self.input)?;
        let comment_id = self.doc.create_node(NodeKind::Comment { content });
        self.doc.append_child(parent, comment_id);
        Ok(())
    }

    // --- CDATA Sections ---
    // See XML 1.0 §2.7: [18] CDSect

    fn parse_cdata(&mut self, parent: NodeId) -> Result<(), ParseError> {
        let content = parse_cdata_content(&mut self.input)?;
        let cdata_id = self.doc.create_node(NodeKind::CData { content });
        self.doc.append_child(parent, cdata_id);
        Ok(())
    }

    // --- Processing Instructions ---
    // See XML 1.0 §2.6: [16] PI

    fn parse_processing_instruction(&mut self, parent: NodeId) -> Result<(), ParseError> {
        let (target, data) = parse_pi_content(&mut self.input)?;
        let pi_id = self
            .doc
            .create_node(NodeKind::ProcessingInstruction { target, data });
        self.doc.append_child(parent, pi_id);
        Ok(())
    }
}

/// Returns true if the entity name is one of the five XML builtin entities.
fn is_builtin_entity(name: &str) -> bool {
    matches!(name, "amp" | "lt" | "gt" | "apos" | "quot")
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::tree::Document;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> Document {
        Document::parse_str(input).unwrap_or_else(|e| panic!("parse failed: {e}"))
    }

    #[test]
    fn test_parse_empty_element() {
        let doc = parse("<root/>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("root"));
        assert_eq!(doc.first_child(root), None);
    }

    #[test]
    fn test_parse_element_with_text() {
        let doc = parse("<greeting>Hello, world!</greeting>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("greeting"));
        assert_eq!(doc.text_content(root), "Hello, world!");
    }

    #[test]
    fn test_parse_nested_elements() {
        let doc = parse("<a><b><c/></b></a>");
        let a = doc.root_element().unwrap();
        assert_eq!(doc.node_name(a), Some("a"));

        let b = doc.first_child(a).unwrap();
        assert_eq!(doc.node_name(b), Some("b"));

        let c = doc.first_child(b).unwrap();
        assert_eq!(doc.node_name(c), Some("c"));
    }

    #[test]
    fn test_parse_attributes() {
        let doc = parse("<div id=\"main\" class=\"big\"/>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.attribute(root, "id"), Some("main"));
        assert_eq!(doc.attribute(root, "class"), Some("big"));
    }

    #[test]
    fn test_parse_single_quoted_attributes() {
        let doc = parse("<div id='main'/>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.attribute(root, "id"), Some("main"));
    }

    #[test]
    fn test_parse_xml_declaration() {
        let doc = parse("<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>");
        assert_eq!(doc.version.as_deref(), Some("1.0"));
        assert_eq!(doc.encoding.as_deref(), Some("UTF-8"));
    }

    #[test]
    fn test_parse_xml_declaration_standalone() {
        let doc = parse("<?xml version=\"1.0\" standalone=\"yes\"?><root/>");
        assert_eq!(doc.standalone, Some(true));
    }

    #[test]
    fn test_parse_comment() {
        let doc = parse("<root><!-- hello --></root>");
        let root = doc.root_element().unwrap();
        let child = doc.first_child(root).unwrap();
        assert_eq!(doc.node_text(child), Some(" hello "));
    }

    #[test]
    fn test_parse_cdata() {
        let doc = parse("<root><![CDATA[x < 1 && y > 2]]></root>");
        let root = doc.root_element().unwrap();
        let child = doc.first_child(root).unwrap();
        assert_eq!(doc.node_text(child), Some("x < 1 && y > 2"));
    }

    #[test]
    fn test_parse_processing_instruction() {
        let doc = parse("<?my-pi some data?><root/>");
        let pi = doc.first_child(doc.root()).unwrap();
        assert_eq!(doc.node_name(pi), Some("my-pi"));
        assert_eq!(doc.node_text(pi), Some("some data"));
    }

    #[test]
    fn test_parse_entity_references() {
        let doc = parse("<root>&amp; &lt; &gt; &apos; &quot;</root>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.text_content(root), "& < > ' \"");
    }

    #[test]
    fn test_parse_char_reference_decimal() {
        let doc = parse("<root>&#65;</root>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.text_content(root), "A");
    }

    #[test]
    fn test_parse_char_reference_hex() {
        let doc = parse("<root>&#x41;</root>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.text_content(root), "A");
    }

    #[test]
    fn test_parse_mixed_content() {
        let doc = parse("<p>Hello <b>world</b>!</p>");
        let p = doc.root_element().unwrap();
        let children: Vec<_> = doc.children(p).collect();
        assert_eq!(children.len(), 3); // "Hello ", <b>, "!"

        assert_eq!(doc.node_text(children[0]), Some("Hello "));
        assert_eq!(doc.node_name(children[1]), Some("b"));
        assert_eq!(doc.text_content(children[1]), "world");
        assert_eq!(doc.node_text(children[2]), Some("!"));
    }

    #[test]
    fn test_parse_prefixed_element() {
        let doc = parse("<svg:rect xmlns:svg=\"http://www.w3.org/2000/svg\"/>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("rect"));
        match &doc.node(root).kind {
            NodeKind::Element { prefix, .. } => {
                assert_eq!(prefix.as_deref(), Some("svg"));
            }
            _ => panic!("expected element"),
        }
    }

    #[test]
    fn test_parse_prefixed_attribute() {
        let doc = parse("<root xml:lang=\"en\"/>");
        let root = doc.root_element().unwrap();
        let attrs = doc.attributes(root);
        assert_eq!(attrs.len(), 1);
        assert_eq!(attrs[0].name, "lang");
        assert_eq!(attrs[0].prefix.as_deref(), Some("xml"));
        assert_eq!(attrs[0].value, "en");
    }

    #[test]
    fn test_parse_error_mismatched_tags() {
        let result = Document::parse_str("<a></b>");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_unexpected_eof() {
        let result = Document::parse_str("<a>");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_no_root() {
        let result = Document::parse_str("");
        // XML 1.0 §2.1 requires a root element
        assert!(result.is_err());
    }

    /// The XML declaration prefix that `serialize()` always emits.
    const DECL: &str = "<?xml version=\"1.0\"?>\n";

    #[test]
    fn test_roundtrip_simple() {
        let input = "<root><child>text</child></root>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(output, format!("{DECL}{input}\n"));
    }

    #[test]
    fn test_roundtrip_attributes() {
        let input = "<root attr=\"value\"><child id=\"1\"/></root>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(output, format!("{DECL}{input}\n"));
    }

    #[test]
    fn test_roundtrip_entities() {
        let input = "<root>&amp; &lt; &gt;</root>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        // After parsing, entities are resolved to characters.
        // Serialization re-escapes them.
        assert_eq!(output, format!("{DECL}<root>&amp; &lt; &gt;</root>\n"));
    }

    #[test]
    fn test_roundtrip_comment() {
        let input = "<root><!-- comment --></root>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(output, format!("{DECL}{input}\n"));
    }

    #[test]
    fn test_roundtrip_cdata() {
        let input = "<root><![CDATA[data & stuff]]></root>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(output, format!("{DECL}{input}\n"));
    }

    #[test]
    fn test_roundtrip_pi() {
        let input = "<?target data?><root/>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(output, format!("{DECL}{input}\n"));
    }

    #[test]
    fn test_roundtrip_xml_declaration() {
        let input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(
            output,
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<root/>\n"
        );
    }

    #[test]
    fn test_whitespace_in_attribute_value() {
        let doc = parse("<root attr=\"a\tb\nc\"/>");
        let root = doc.root_element().unwrap();
        // Tabs and newlines in attribute values are normalized to spaces
        assert_eq!(doc.attribute(root, "attr"), Some("a b c"));
    }

    #[test]
    fn test_name_chars() {
        use super::super::input::{is_name_char, is_name_start_char};

        assert!(is_name_start_char('A'));
        assert!(is_name_start_char('z'));
        assert!(is_name_start_char('_'));
        assert!(is_name_start_char(':'));
        assert!(!is_name_start_char('0'));
        assert!(!is_name_start_char('-'));

        assert!(is_name_char('A'));
        assert!(is_name_char('0'));
        assert!(is_name_char('-'));
        assert!(is_name_char('.'));
        assert!(!is_name_char(' '));
    }

    #[test]
    fn test_parse_doctype_simple() {
        let doc = parse("<!DOCTYPE html><html/>");
        let root = doc.root();
        let children: Vec<_> = doc.children(root).collect();
        assert_eq!(children.len(), 2);

        match &doc.node(children[0]).kind {
            NodeKind::DocumentType {
                name,
                system_id,
                public_id,
                ..
            } => {
                assert_eq!(name, "html");
                assert_eq!(*system_id, None);
                assert_eq!(*public_id, None);
            }
            other => panic!("expected DocumentType, got {other:?}"),
        }

        assert_eq!(doc.node_name(children[1]), Some("html"));
    }

    #[test]
    fn test_parse_doctype_system() {
        let doc = parse("<!DOCTYPE root SYSTEM \"root.dtd\"><root/>");
        let root = doc.root();
        let children: Vec<_> = doc.children(root).collect();
        assert_eq!(children.len(), 2);

        match &doc.node(children[0]).kind {
            NodeKind::DocumentType {
                name,
                system_id,
                public_id,
                ..
            } => {
                assert_eq!(name, "root");
                assert_eq!(system_id.as_deref(), Some("root.dtd"));
                assert_eq!(*public_id, None);
            }
            other => panic!("expected DocumentType, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_doctype_public() {
        let doc = parse(
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0//EN\" \
             \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html/>",
        );
        let root = doc.root();
        let children: Vec<_> = doc.children(root).collect();
        assert_eq!(children.len(), 2);

        match &doc.node(children[0]).kind {
            NodeKind::DocumentType {
                name,
                system_id,
                public_id,
                ..
            } => {
                assert_eq!(name, "html");
                assert_eq!(public_id.as_deref(), Some("-//W3C//DTD XHTML 1.0//EN"));
                assert_eq!(
                    system_id.as_deref(),
                    Some("http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
                );
            }
            other => panic!("expected DocumentType, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_doctype_internal_subset() {
        let doc = parse("<!DOCTYPE root [<!ELEMENT root (#PCDATA)>]><root/>");
        let root = doc.root();
        let children: Vec<_> = doc.children(root).collect();
        assert_eq!(children.len(), 2);

        match &doc.node(children[0]).kind {
            NodeKind::DocumentType {
                name,
                system_id,
                public_id,
                ..
            } => {
                assert_eq!(name, "root");
                assert_eq!(*system_id, None);
                assert_eq!(*public_id, None);
            }
            other => panic!("expected DocumentType, got {other:?}"),
        }

        assert_eq!(doc.node_name(children[1]), Some("root"));
    }

    #[test]
    fn test_parse_doctype_multiline_internal_subset() {
        let input =
            "<!DOCTYPE root [\n<!ELEMENT y (#PCDATA|x|x)*>\n<!ELEMENT root ANY>\n]>\n\n<root/>";
        let doc = parse(input);
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("root"));
    }

    #[test]
    fn test_parse_doctype_with_entity() {
        let input = "<!DOCTYPE doc [\n<!ELEMENT doc (#PCDATA)>\n<!ENTITY rsqb \"]\">\n]>\n<doc>&rsqb;</doc>";
        let doc = parse(input);
        let root = doc.root_element().unwrap();
        assert_eq!(doc.text_content(root), "]");
    }

    #[test]
    fn test_parse_doctype_content_model() {
        let input = "<!DOCTYPE violation [\n<!ELEMENT violation (a,a,a,b)>\n<!ELEMENT a EMPTY>\n<!ELEMENT b EMPTY>\n]>\n<violation>\n    <a/>\n    <a/>\n    <b/>\n</violation>";
        let doc = parse(input);
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("violation"));
    }

    #[test]
    fn test_parse_doctype_with_crlf() {
        // Test with CRLF line endings (like the OASIS conformance tests)
        let input =
            "<!DOCTYPE doc\r\n[\r\n<!ELEMENT doc ANY>\r\n<!ELEMENT a (doc?)>\r\n]>\r\n<doc/>";
        let doc = parse(input);
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("doc"));
    }

    #[test]
    fn test_parse_doctype_attlist() {
        let input = "<!DOCTYPE root [\n<!ELEMENT root EMPTY>\n<!ATTLIST root\n    token\tNMTOKEN\t\t#REQUIRED\n    >\n\n    <!-- comment -->\n]>\n<root token=\"dev@null\"/>";
        let doc = parse(input);
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("root"));
    }

    #[test]
    fn test_parse_doctype_comment_with_apostrophe() {
        // Apostrophes in DTD comments must not confuse the bracket scanner
        let input = "<!DOCTYPE root [\n<!ELEMENT root ANY>\n<!-- can't break -->\n]>\n<root/>";
        let doc = parse(input);
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("root"));
    }

    #[test]
    fn test_roundtrip_doctype() {
        // Simple DOCTYPE (no whitespace between DOCTYPE and root in input)
        let input = "<!DOCTYPE html><html/>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(output, format!("{DECL}<!DOCTYPE html><html/>\n"));

        // DOCTYPE with SYSTEM
        let input = "<!DOCTYPE root SYSTEM \"root.dtd\"><root/>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(
            output,
            format!("{DECL}<!DOCTYPE root SYSTEM \"root.dtd\"><root/>\n")
        );

        // DOCTYPE with PUBLIC
        let input = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0//EN\" \
                      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html/>";
        let doc = parse(input);
        let output = crate::serial::serialize(&doc);
        assert_eq!(
            output,
            format!(
                "{DECL}<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0//EN\" \
                 \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html/>\n"
            )
        );
    }

    // --- Namespace resolution tests ---

    #[test]
    fn test_parse_default_namespace() {
        let doc = parse("<root xmlns=\"http://example.com\"/>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_namespace(root), Some("http://example.com"));
    }

    #[test]
    fn test_parse_prefixed_namespace() {
        let doc = parse("<ns:root xmlns:ns=\"http://example.com\"/>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_name(root), Some("root"));
        assert_eq!(doc.node_namespace(root), Some("http://example.com"));
        match &doc.node(root).kind {
            NodeKind::Element { prefix, .. } => {
                assert_eq!(prefix.as_deref(), Some("ns"));
            }
            _ => panic!("expected element"),
        }
    }

    #[test]
    fn test_parse_nested_namespace() {
        // Child elements inherit the default namespace from the parent.
        let doc = parse("<root xmlns=\"http://example.com\"><child/></root>");
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_namespace(root), Some("http://example.com"));

        let child = doc.first_child(root).unwrap();
        assert_eq!(doc.node_name(child), Some("child"));
        assert_eq!(doc.node_namespace(child), Some("http://example.com"));
    }

    #[test]
    fn test_parse_namespace_override() {
        // A child element can override the parent's default namespace.
        let doc = parse(
            "<root xmlns=\"http://example.com\">\
             <child xmlns=\"http://other.com\"/>\
             </root>",
        );
        let root = doc.root_element().unwrap();
        assert_eq!(doc.node_namespace(root), Some("http://example.com"));

        let child = doc.first_child(root).unwrap();
        assert_eq!(doc.node_namespace(child), Some("http://other.com"));
    }

    #[test]
    fn test_parse_xml_namespace() {
        // The xml: prefix is always bound to the XML namespace URI.
        let doc = parse("<root xml:lang=\"en\"/>");
        let root = doc.root_element().unwrap();
        let attrs = doc.attributes(root);
        assert_eq!(attrs.len(), 1);
        assert_eq!(attrs[0].name, "lang");
        assert_eq!(attrs[0].prefix.as_deref(), Some("xml"));
        assert_eq!(
            attrs[0].namespace.as_deref(),
            Some("http://www.w3.org/XML/1998/namespace")
        );
    }

    #[test]
    fn test_parse_attribute_namespace() {
        // Prefixed attributes get their namespace resolved.
        let doc = parse("<root xmlns:app=\"http://example.com/app\" app:version=\"2.0\"/>");
        let root = doc.root_element().unwrap();
        let attrs = doc.attributes(root);

        // Find the app:version attribute
        let version_attr = attrs.iter().find(|a| a.name == "version").unwrap();
        assert_eq!(version_attr.prefix.as_deref(), Some("app"));
        assert_eq!(
            version_attr.namespace.as_deref(),
            Some("http://example.com/app")
        );

        // The xmlns:app attribute should not have a resolved namespace itself.
        let xmlns_attr = attrs.iter().find(|a| a.name == "app").unwrap();
        assert_eq!(xmlns_attr.prefix.as_deref(), Some("xmlns"));
        assert_eq!(xmlns_attr.namespace, None);
    }
}
