//! XML Schema (XSD 1.0) validation for XML documents.
//!
//! This module implements a subset of the W3C XML Schema Definition Language
//! (XSD) 1.0 specification (<https://www.w3.org/TR/xmlschema-1/>) for
//! validating XML documents against XSD schemas.
//!
//! # Supported Features
//!
//! - Global and local element declarations with type references or inline types
//! - Complex types with `sequence`, `choice`, `all`, and empty content models
//! - Simple types with restriction facets, list, and union varieties
//! - Built-in XSD datatypes (string, integer, boolean, date, etc.)
//! - Attribute declarations with required/optional, default, and fixed values
//! - Occurrence constraints (`minOccurs`, `maxOccurs`)
//! - Mixed content
//! - Attribute groups
//! - Simple content extensions
//!
//! # Architecture
//!
//! 1. **Data model** ([`XsdSchema`], [`XsdElement`], [`XsdType`], etc.) -- an
//!    algebraic representation of the schema structure.
//! 2. **Schema parser** ([`parse_xsd`]) -- reads an XSD XML document and
//!    produces an `XsdSchema`.
//! 3. **Validator** ([`validate_xsd`]) -- checks an XML document tree against
//!    a compiled schema.
//!
//! # Examples
//!
//! ```
//! use xmloxide::Document;
//! use xmloxide::validation::xsd::{parse_xsd, validate_xsd};
//!
//! let schema_xml = r#"
//!   <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
//!     <xs:element name="greeting" type="xs:string"/>
//!   </xs:schema>
//! "#;
//!
//! let schema = parse_xsd(schema_xml).unwrap();
//! let doc = Document::parse_str("<greeting>Hello!</greeting>").unwrap();
//! let result = validate_xsd(&doc, &schema);
//! assert!(result.is_valid);
//! ```

use std::collections::{HashMap, HashSet};

use crate::tree::{Document, NodeId, NodeKind};
use crate::validation::{ValidationError, ValidationResult};

/// The XML Schema namespace URI.
const XSD_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";

// ---------------------------------------------------------------------------
// Schema resolver
// ---------------------------------------------------------------------------

/// A trait for resolving external schema documents by URI.
///
/// Implementors provide schema content for `xsd:import` and `xsd:include`
/// directives. The resolver receives the `schemaLocation` URI and an optional
/// base URI for resolving relative paths.
///
/// A blanket implementation is provided for closures matching
/// `Fn(&str, Option<&str>) -> Option<String>`.
///
/// See XSD 1.0 section 4.2 for schema composition.
pub trait SchemaResolver {
    /// Resolves a schema location to its XML content.
    ///
    /// `location` is the `schemaLocation` attribute value, which may be
    /// an absolute URI or a relative path. `base` is the URI of the
    /// including/importing schema, if known, for resolving relative paths.
    ///
    /// Returns `Some(xml_content)` if the schema was found, or `None` if
    /// the schema cannot be resolved.
    fn resolve(&self, location: &str, base: Option<&str>) -> Option<String>;
}

impl<F> SchemaResolver for F
where
    F: Fn(&str, Option<&str>) -> Option<String>,
{
    fn resolve(&self, location: &str, base: Option<&str>) -> Option<String> {
        self(location, base)
    }
}

/// Options for parsing XSD schemas with multi-file schema composition.
///
/// See XSD 1.0 section 4.2 for `xsd:include` and `xsd:import`.
pub struct XsdParseOptions<'a> {
    /// Optional resolver for `xsd:include` and `xsd:import` directives.
    ///
    /// If `None`, include/import directives are silently ignored (matching
    /// the current behavior of [`parse_xsd`]).
    pub resolver: Option<&'a dyn SchemaResolver>,

    /// Optional base URI for resolving relative `schemaLocation` values.
    pub base_uri: Option<String>,
}

// ---------------------------------------------------------------------------
// Data model
// ---------------------------------------------------------------------------

/// A parsed XML Schema definition.
///
/// Contains all top-level declarations extracted from an `<xs:schema>` document:
/// global element declarations, named type definitions, and attribute groups.
#[derive(Debug, Clone)]
pub struct XsdSchema {
    /// The target namespace of the schema, if declared.
    pub target_namespace: Option<String>,
    /// Global element declarations, keyed by element name.
    pub elements: HashMap<String, XsdElement>,
    /// Named type definitions (both simple and complex), keyed by type name.
    pub types: HashMap<String, XsdType>,
    /// Named attribute groups, keyed by group name.
    pub attribute_groups: HashMap<String, Vec<XsdAttribute>>,
    /// Named model groups (`<xsd:group name="...">`).
    pub model_groups: HashMap<String, ComplexContent>,
    /// Imported schemas from other namespaces, keyed by namespace URI.
    pub imported_namespaces: HashMap<String, ImportedSchema>,
    /// Prefix-to-namespace-URI map from the root schema element.
    pub prefix_map: HashMap<String, String>,
    /// The `elementFormDefault` attribute from the schema root.
    pub element_form_default: FormDefault,
    /// Substitution group index: maps head element name to member element names.
    pub substitution_groups: HashMap<String, Vec<String>>,
}

/// Whether local elements/attributes must be namespace-qualified in instances.
///
/// See XSD 1.0 section 3.3.2.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormDefault {
    /// Local elements do not need to be namespace-qualified (default).
    Unqualified,
    /// Local elements must be namespace-qualified in instance documents.
    Qualified,
}

/// Declarations imported from another namespace via `xsd:import`.
///
/// See XSD 1.0 section 4.2.3.
#[derive(Debug, Clone)]
pub struct ImportedSchema {
    /// Global element declarations from the imported namespace.
    pub elements: HashMap<String, XsdElement>,
    /// Named type definitions from the imported namespace.
    pub types: HashMap<String, XsdType>,
    /// Named attribute groups from the imported namespace.
    pub attribute_groups: HashMap<String, Vec<XsdAttribute>>,
    /// Named model groups from the imported namespace.
    pub model_groups: HashMap<String, ComplexContent>,
    /// XML namespace prefix→URI mappings from this schema document.
    pub prefix_map: HashMap<String, String>,
}

/// An element declaration in the schema.
///
/// Elements can reference a named type via `type_ref`, define an inline type,
/// or default to `xs:anyType` if neither is specified.
///
/// See XSD 1.0 section 3.3: Element Declarations.
#[derive(Debug, Clone)]
pub struct XsdElement {
    /// The element name.
    pub name: String,
    /// Reference to a named type (e.g., `"xs:string"` or a user-defined name).
    pub type_ref: Option<String>,
    /// An inline anonymous type definition.
    pub inline_type: Option<XsdType>,
    /// Reference to a global element declaration (`ref` attribute `QName`).
    ///
    /// When present, the element's type is resolved from the referenced
    /// global element declaration rather than from `type_ref` or `inline_type`.
    pub element_ref: Option<String>,
    /// Minimum number of occurrences (default 1 for local elements).
    pub min_occurs: u32,
    /// Maximum number of occurrences (default 1 for local elements).
    pub max_occurs: MaxOccurs,
    /// The `substitutionGroup` attribute (`QName` of the head element).
    ///
    /// See XSD 1.0 section 3.3.6: when set, this element can appear anywhere
    /// the head element is expected in a content model.
    pub substitution_group: Option<String>,
    /// Whether this element is abstract (`abstract="true"`).
    ///
    /// Abstract elements cannot appear directly in instance documents;
    /// only their substitution group members can.
    pub is_abstract: bool,
}

/// Maximum occurrence constraint for particles.
///
/// Can be a concrete bound or unbounded (no upper limit).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MaxOccurs {
    /// A concrete upper bound.
    Bounded(u32),
    /// No upper limit (corresponds to `maxOccurs="unbounded"`).
    Unbounded,
}

impl std::fmt::Display for MaxOccurs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bounded(n) => write!(f, "{n}"),
            Self::Unbounded => write!(f, "unbounded"),
        }
    }
}

/// A type definition, either simple or complex.
///
/// See XSD 1.0 section 3.4 (Complex Type) and 3.14 (Simple Type).
#[derive(Debug, Clone)]
pub enum XsdType {
    /// A simple type for text-only content and attribute values.
    Simple(SimpleType),
    /// A complex type that can contain elements, attributes, and mixed content.
    Complex(ComplexType),
}

/// A simple type definition for text content and attribute values.
///
/// Simple types constrain the textual content of elements and attributes.
/// They are defined by restriction, list, or union derivation.
///
/// See XSD 1.0 section 3.14: Simple Type Definitions.
#[derive(Debug, Clone)]
pub struct SimpleType {
    /// The type name, if this is a named (non-anonymous) type.
    name: Option<String>,
    /// The variety of the simple type.
    variety: SimpleTypeVariety,
}

/// The variety (derivation method) of a simple type.
#[derive(Debug, Clone)]
pub enum SimpleTypeVariety {
    /// A restriction on a base type, optionally with constraining facets.
    Restriction {
        /// The base type name being restricted.
        base: String,
        /// Facets that further constrain the value space.
        facets: Vec<Facet>,
    },
    /// A list type whose items are whitespace-separated values of the item type.
    List {
        /// The name of the type for list items.
        item_type: String,
    },
    /// A union of multiple simple types.
    Union {
        /// The member type names.
        member_types: Vec<String>,
    },
    /// A reference to a built-in type by name.
    Builtin(String),
}

/// A constraining facet on a simple type restriction.
///
/// See XSD 1.0 section 4.3: Constraining Facets.
#[derive(Debug, Clone)]
pub enum Facet {
    /// Minimum number of characters / list items.
    MinLength(usize),
    /// Maximum number of characters / list items.
    MaxLength(usize),
    /// Exact number of characters / list items.
    Length(usize),
    /// A regular expression pattern the value must match.
    Pattern(String),
    /// An enumeration of allowed values.
    Enumeration(Vec<String>),
    /// Inclusive lower bound for ordered values.
    MinInclusive(String),
    /// Inclusive upper bound for ordered values.
    MaxInclusive(String),
    /// Exclusive lower bound for ordered values.
    MinExclusive(String),
    /// Exclusive upper bound for ordered values.
    MaxExclusive(String),
    /// Whitespace normalization rule.
    WhiteSpace(WhiteSpaceValue),
    /// Maximum total number of digits for decimal types.
    TotalDigits(usize),
    /// Maximum number of fractional digits for decimal types.
    FractionDigits(usize),
}

/// Whitespace normalization mode for simple type values.
///
/// See XSD 1.0 section 4.3.6.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhiteSpaceValue {
    /// Preserve all whitespace characters as-is.
    Preserve,
    /// Replace all occurrences of tab, line feed, and carriage return with space.
    Replace,
    /// After replacing, collapse contiguous sequences of spaces into one
    /// and strip leading/trailing spaces.
    Collapse,
}

/// A complex type definition for structured content.
///
/// Complex types describe elements that may contain child elements,
/// attributes, and optionally mixed text content.
///
/// See XSD 1.0 section 3.4: Complex Type Definitions.
#[derive(Debug, Clone)]
pub struct ComplexType {
    /// The type name, if this is a named (non-anonymous) type.
    pub name: Option<String>,
    /// The content model of the complex type.
    pub content: ComplexContent,
    /// Attribute declarations on elements of this type.
    pub attributes: Vec<XsdAttribute>,
    /// Whether the type allows mixed content (text interspersed with elements).
    pub mixed: bool,
    /// Base type name from `<xs:complexContent><xs:extension base="...">`.
    ///
    /// When set, the base type's content model particles must appear before
    /// this type's own particles during validation.
    pub extension_base: Option<String>,
}

/// The content model of a complex type.
#[derive(Debug, Clone)]
pub enum ComplexContent {
    /// No child elements or text content allowed.
    Empty,
    /// An ordered sequence of particles, all of which must appear in order.
    Sequence(Vec<XsdParticle>),
    /// A choice among particles, exactly one of which must appear.
    Choice(Vec<XsdParticle>),
    /// An unordered collection where each particle may appear at most once.
    All(Vec<XsdParticle>),
    /// Simple content (text only) derived from a base type.
    SimpleContent {
        /// The base type name.
        base: String,
    },
}

/// A particle in a content model -- either an element or a nested group.
#[derive(Debug, Clone)]
pub enum XsdParticle {
    /// An element declaration within the content model.
    Element(XsdElement),
    /// A nested compositor group (sequence, choice, or all).
    Group(ComplexContent),
    /// An element wildcard (`<xsd:any>`).
    Any(XsdAny),
}

/// Represents `<xsd:any>` element wildcard in a content model.
#[derive(Debug, Clone)]
pub struct XsdAny {
    /// Namespace constraint: `##any`, `##other`, or list of namespace URIs.
    pub namespace: XsdAnyNamespace,
    /// Processing mode for matched elements.
    pub process_contents: XsdProcessContents,
    /// Minimum occurrences (default 1).
    pub min_occurs: u32,
    /// Maximum occurrences.
    pub max_occurs: MaxOccurs,
}

/// Namespace constraint for `<xsd:any>`.
#[derive(Debug, Clone)]
pub enum XsdAnyNamespace {
    /// `##any` — any namespace.
    Any,
    /// `##other` — any namespace except the targetNamespace.
    Other,
    /// Explicit list of namespace URIs.
    List(Vec<String>),
}

/// Processing mode for `<xsd:any>` matched elements.
#[derive(Debug, Clone)]
pub enum XsdProcessContents {
    /// `strict` — validate against schema declaration (default).
    Strict,
    /// `lax` — validate if declaration found, accept otherwise.
    Lax,
    /// `skip` — no validation.
    Skip,
}

/// An attribute declaration.
///
/// See XSD 1.0 section 3.2: Attribute Declarations.
#[derive(Debug, Clone)]
pub struct XsdAttribute {
    /// The attribute name.
    name: String,
    /// Reference to the attribute's type (e.g., `"xs:string"`).
    type_ref: String,
    /// Whether the attribute is required (`use="required"`).
    required: bool,
    /// Fixed value that the attribute must have if present.
    fixed: Option<String>,
}

// ---------------------------------------------------------------------------
// Schema parser
// ---------------------------------------------------------------------------

/// Parses an XSD schema from its XML text representation.
///
/// The input should be a well-formed XML document with an `<xs:schema>` root
/// element using the XML Schema namespace
/// (`http://www.w3.org/2001/XMLSchema`).
///
/// This is a convenience wrapper around [`parse_xsd_with_options`] that does
/// not resolve `xsd:include` or `xsd:import` directives (they are silently
/// ignored).
///
/// # Errors
///
/// Returns a [`ValidationError`] if the input cannot be parsed as XML or
/// does not contain a valid XSD schema structure.
///
/// # Examples
///
/// ```
/// use xmloxide::validation::xsd::parse_xsd;
///
/// let schema = parse_xsd(r#"
///   <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
///     <xs:element name="root" type="xs:string"/>
///   </xs:schema>
/// "#).unwrap();
/// ```
pub fn parse_xsd(schema_xml: &str) -> Result<XsdSchema, ValidationError> {
    parse_xsd_with_options(
        schema_xml,
        &XsdParseOptions {
            resolver: None,
            base_uri: None,
        },
    )
}

/// Parses an XSD schema with support for `xsd:include` and `xsd:import`.
///
/// When a [`SchemaResolver`] is provided in the options, `xsd:include` and
/// `xsd:import` elements trigger loading and merging of referenced schemas.
///
/// See XSD 1.0 section 4.2 for schema composition rules.
///
/// # Errors
///
/// Returns a [`ValidationError`] if the input cannot be parsed as XML, does
/// not contain a valid XSD schema structure, or if an included/imported
/// schema cannot be resolved or has a namespace mismatch.
///
/// # Examples
///
/// ```
/// use xmloxide::validation::xsd::{parse_xsd_with_options, SchemaResolver, XsdParseOptions};
///
/// // A simple resolver that returns schema content by location
/// let resolver = |location: &str, _base: Option<&str>| -> Option<String> {
///     match location {
///         "types.xsd" => Some(r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
///             <xs:complexType name="NameType"><xs:sequence>
///                 <xs:element name="first" type="xs:string"/>
///             </xs:sequence></xs:complexType>
///         </xs:schema>"#.to_string()),
///         _ => None,
///     }
/// };
///
/// let opts = XsdParseOptions {
///     resolver: Some(&resolver),
///     base_uri: None,
/// };
///
/// let schema = parse_xsd_with_options(r#"
///   <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
///     <xs:include schemaLocation="types.xsd"/>
///     <xs:element name="name" type="NameType"/>
///   </xs:schema>
/// "#, &opts).unwrap();
/// ```
pub fn parse_xsd_with_options(
    schema_xml: &str,
    options: &XsdParseOptions<'_>,
) -> Result<XsdSchema, ValidationError> {
    // Parse the root schema document first to extract the prefix map
    let root_doc = Document::parse_str(schema_xml).map_err(|e| ValidationError {
        message: format!("failed to parse XSD schema XML: {e}"),
        line: None,
        column: None,
    })?;
    let root_elem = root_doc.root_element().ok_or_else(|| ValidationError {
        message: "XSD schema has no root element".to_string(),
        line: None,
        column: None,
    })?;
    let prefix_map = build_prefix_map(&root_doc, root_elem);
    let element_form_default = match root_doc.attribute(root_elem, "elementFormDefault") {
        Some("qualified") => FormDefault::Qualified,
        _ => FormDefault::Unqualified,
    };

    let mut schema = XsdSchema {
        target_namespace: None,
        elements: HashMap::new(),
        types: HashMap::new(),
        attribute_groups: HashMap::new(),
        model_groups: HashMap::new(),
        imported_namespaces: HashMap::new(),
        prefix_map,
        element_form_default,
        substitution_groups: HashMap::new(),
    };

    register_builtin_types(&mut schema);

    let mut loaded = HashSet::new();
    // Use a synthetic key for the top-level schema (it has no schemaLocation)
    loaded.insert("<root>".to_string());

    parse_xsd_internal(schema_xml, options, &mut loaded, &mut schema)?;

    // Build substitution group index from all element declarations.
    build_substitution_index(&mut schema);

    // Merge complexContent extension base content models.
    merge_extension_bases(&mut schema);

    // Inline attributeGroup references into complex type attributes.
    resolve_attribute_groups(&mut schema);

    Ok(schema)
}

/// Internal recursive schema parser with cycle detection.
fn parse_xsd_internal(
    schema_xml: &str,
    options: &XsdParseOptions<'_>,
    loaded: &mut HashSet<String>,
    schema: &mut XsdSchema,
) -> Result<(), ValidationError> {
    let doc = Document::parse_str(schema_xml).map_err(|e| ValidationError {
        message: format!("failed to parse XSD schema XML: {e}"),
        line: None,
        column: None,
    })?;

    let root = doc.root_element().ok_or_else(|| ValidationError {
        message: "XSD schema has no root element".to_string(),
        line: None,
        column: None,
    })?;

    let root_name = doc.node_name(root).unwrap_or("");
    if root_name != "schema" {
        return Err(ValidationError {
            message: format!("expected <xs:schema> root element, found <{root_name}>"),
            line: None,
            column: None,
        });
    }

    let this_ns = doc.attribute(root, "targetNamespace").map(String::from);

    // Set target_namespace from the first schema we parse (the root)
    if schema.target_namespace.is_none() && this_ns.is_some() {
        schema.target_namespace.clone_from(&this_ns);
    }

    parse_top_level_declarations(&doc, root, schema, options, loaded, this_ns.as_ref())?;

    Ok(())
}

/// Parses top-level declarations from the schema root element.
fn parse_top_level_declarations(
    doc: &Document,
    root: NodeId,
    schema: &mut XsdSchema,
    options: &XsdParseOptions<'_>,
    loaded: &mut HashSet<String>,
    this_ns: Option<&String>,
) -> Result<(), ValidationError> {
    // Pass 1: collect named model groups so type parsing can resolve <group ref="...">
    for child in doc.children(root) {
        if doc.node_name(child) != Some("group") {
            continue;
        }
        let Some(group_name) = doc.attribute(child, "name") else {
            continue;
        };
        if let Some(group_content) = parse_named_group(doc, child, &schema.model_groups) {
            schema
                .model_groups
                .insert(group_name.to_string(), group_content);
        }
    }

    let group_defs = schema.model_groups.clone();

    // Pass 2: parse all regular top-level declarations
    for child in doc.children(root) {
        let Some(name) = doc.node_name(child) else {
            continue;
        };
        match name {
            "element" => {
                if let Some(elem) = parse_element_decl(doc, child) {
                    schema.elements.insert(elem.name.clone(), elem);
                }
            }
            "complexType" => {
                let ct = parse_complex_type(doc, child, &group_defs);
                if let Some(ref type_name) = ct.name {
                    schema.types.insert(type_name.clone(), XsdType::Complex(ct));
                }
            }
            "simpleType" => {
                let st = parse_simple_type(doc, child);
                if let Some(ref type_name) = st.name {
                    schema.types.insert(type_name.clone(), XsdType::Simple(st));
                }
            }
            "attributeGroup" => {
                if let Some(group_name) = doc.attribute(child, "name") {
                    let attrs = parse_attributes(doc, child);
                    schema
                        .attribute_groups
                        .insert(group_name.to_string(), attrs);
                }
            }
            "include" => {
                handle_include(doc, child, schema, options, loaded, this_ns)?;
            }
            "import" => {
                handle_import(doc, child, schema, options, loaded)?;
            }
            _ => {}
        }
    }
    Ok(())
}

/// Handles an `<xsd:include>` element by resolving and merging the included
/// schema into the current schema.
///
/// See XSD 1.0 section 4.2.1.
fn handle_include(
    doc: &Document,
    node: NodeId,
    schema: &mut XsdSchema,
    options: &XsdParseOptions<'_>,
    loaded: &mut HashSet<String>,
    this_ns: Option<&String>,
) -> Result<(), ValidationError> {
    let Some(location) = doc.attribute(node, "schemaLocation") else {
        return Ok(());
    };

    // Cycle detection
    if loaded.contains(location) {
        return Ok(());
    }

    let Some(resolver) = options.resolver else {
        return Ok(());
    };

    let content = resolver
        .resolve(location, options.base_uri.as_deref())
        .ok_or_else(|| ValidationError {
            message: format!("cannot resolve included schema: {location}"),
            line: None,
            column: None,
        })?;

    // Check namespace compatibility before merging: parse just the root to
    // extract its targetNamespace.
    let included_doc = Document::parse_str(&content).map_err(|e| ValidationError {
        message: format!("failed to parse included schema '{location}': {e}"),
        line: None,
        column: None,
    })?;
    let included_root = included_doc.root_element().ok_or_else(|| ValidationError {
        message: format!("included schema '{location}' has no root element"),
        line: None,
        column: None,
    })?;
    let included_ns = included_doc
        .attribute(included_root, "targetNamespace")
        .map(String::from);

    // Per XSD 1.0 §4.2.1: included schema must have the same targetNamespace
    // or no targetNamespace (chameleon include).
    if let Some(ref inc_ns) = included_ns {
        if this_ns != Some(inc_ns) {
            return Err(ValidationError {
                message: format!(
                    "included schema '{location}' has targetNamespace '{inc_ns}' \
                     which does not match the including schema's namespace"
                ),
                line: None,
                column: None,
            });
        }
    }

    // Mark as loaded before recursing to prevent cycles
    loaded.insert(location.to_string());

    // Merge prefix declarations from the included schema so that
    // QName resolution (e.g., gmd:LI_Lineage) works for types and
    // elements declared in imported-but-not-included namespaces.
    let included_prefix_map = build_prefix_map(&included_doc, included_root);
    for (prefix, uri) in included_prefix_map {
        schema.prefix_map.entry(prefix).or_insert(uri);
    }

    // Parse and merge the included schema's declarations
    parse_xsd_internal(&content, options, loaded, schema)?;

    Ok(())
}

/// Handles an `<xsd:import>` element by resolving the imported schema and
/// storing its declarations under the imported namespace.
///
/// See XSD 1.0 section 4.2.3.
#[allow(clippy::too_many_lines)]
fn handle_import(
    doc: &Document,
    node: NodeId,
    schema: &mut XsdSchema,
    options: &XsdParseOptions<'_>,
    loaded: &mut HashSet<String>,
) -> Result<(), ValidationError> {
    let namespace = doc.attribute(node, "namespace").map(String::from);
    let location = doc.attribute(node, "schemaLocation");

    let Some(location) = location else {
        // Import without schemaLocation is valid — just declares the namespace
        return Ok(());
    };

    // Cycle detection
    if loaded.contains(location) {
        return Ok(());
    }

    let Some(resolver) = options.resolver else {
        return Ok(());
    };

    let content = resolver
        .resolve(location, options.base_uri.as_deref())
        .ok_or_else(|| ValidationError {
            message: format!("cannot resolve imported schema: {location}"),
            line: None,
            column: None,
        })?;

    // Parse the imported schema to extract its declarations
    let imported_doc = Document::parse_str(&content).map_err(|e| ValidationError {
        message: format!("failed to parse imported schema '{location}': {e}"),
        line: None,
        column: None,
    })?;
    let imported_root = imported_doc.root_element().ok_or_else(|| ValidationError {
        message: format!("imported schema '{location}' has no root element"),
        line: None,
        column: None,
    })?;

    let imported_root_name = imported_doc.node_name(imported_root).unwrap_or("");
    if imported_root_name != "schema" {
        return Err(ValidationError {
            message: format!(
                "imported schema '{location}' has root <{imported_root_name}>, expected <xs:schema>"
            ),
            line: None,
            column: None,
        });
    }

    let imported_ns = imported_doc
        .attribute(imported_root, "targetNamespace")
        .map(String::from);

    // Verify namespace matches if both are specified
    if let (Some(ref expected), Some(ref actual)) = (&namespace, &imported_ns) {
        if expected != actual {
            return Err(ValidationError {
                message: format!(
                    "imported schema '{location}' has targetNamespace '{actual}' \
                     but import declares namespace '{expected}'"
                ),
                line: None,
                column: None,
            });
        }
    }

    let ns_key = namespace.or(imported_ns).unwrap_or_default();

    // Mark as loaded before recursing
    loaded.insert(location.to_string());

    // Build an ImportedSchema by parsing the imported schema's declarations
    let mut imported = ImportedSchema {
        elements: HashMap::new(),
        types: HashMap::new(),
        attribute_groups: HashMap::new(),
        model_groups: HashMap::new(),
        prefix_map: HashMap::new(),
    };

    // We need a temporary XsdSchema to parse into, then extract declarations
    let imported_form_default = match imported_doc.attribute(imported_root, "elementFormDefault") {
        Some("qualified") => FormDefault::Qualified,
        _ => FormDefault::Unqualified,
    };
    let mut temp_schema = XsdSchema {
        target_namespace: Some(ns_key.clone()),
        elements: HashMap::new(),
        types: HashMap::new(),
        attribute_groups: HashMap::new(),
        model_groups: HashMap::new(),
        imported_namespaces: HashMap::new(),
        prefix_map: build_prefix_map(&imported_doc, imported_root),
        element_form_default: imported_form_default,
        substitution_groups: HashMap::new(),
    };
    register_builtin_types(&mut temp_schema);
    parse_top_level_declarations(
        &imported_doc,
        imported_root,
        &mut temp_schema,
        options,
        loaded,
        Some(&ns_key),
    )?;

    // Move non-builtin declarations to the ImportedSchema
    for (name, typ) in &temp_schema.types {
        // Skip built-in types — they are already registered on the main schema
        if matches!(typ, XsdType::Simple(st) if matches!(st.variety, SimpleTypeVariety::Builtin(_)))
        {
            continue;
        }
        imported.types.insert(name.clone(), typ.clone());
    }
    imported.elements = temp_schema.elements;
    imported.attribute_groups = temp_schema.attribute_groups;
    imported.model_groups = temp_schema.model_groups;
    imported.prefix_map = temp_schema.prefix_map;

    // Also merge any transitive imports
    for (k, v) in temp_schema.imported_namespaces {
        schema.imported_namespaces.entry(k).or_insert(v);
    }

    schema.imported_namespaces.entry(ns_key).or_insert(imported);

    Ok(())
}

/// Builds the substitution group index from all element declarations.
///
/// After all schemas (including includes/imports) are parsed, this scans
/// every `XsdElement` for a `substitution_group` attribute and populates
/// `schema.substitution_groups` as a map from head local name to member names.
fn build_substitution_index(schema: &mut XsdSchema) {
    // Collect substitution group memberships from local elements
    let mut sub_groups: Vec<(String, String)> = schema
        .elements
        .values()
        .filter_map(|e| {
            e.substitution_group.as_ref().map(|sg| {
                // Extract local name from QName like "adv:AU_Flaechenobjekt"
                let local = if let Some((_, l)) = sg.split_once(':') {
                    l.to_string()
                } else {
                    sg.clone()
                };
                (local, e.name.clone())
            })
        })
        .collect();

    // Also scan imported schemas for substitution group memberships.
    // Cross-namespace substitution groups (e.g., wfs:FeatureCollection
    // substituting for nas:FeatureCollection) are only discoverable here.
    for imported in schema.imported_namespaces.values() {
        for e in imported.elements.values() {
            if let Some(sg) = &e.substitution_group {
                let local = if let Some((_, l)) = sg.split_once(':') {
                    l.to_string()
                } else {
                    sg.clone()
                };
                sub_groups.push((local, e.name.clone()));
            }
        }
    }

    for (head, member) in sub_groups {
        schema
            .substitution_groups
            .entry(head)
            .or_default()
            .push(member);
    }
}

/// Resolves `<xs:attributeGroup ref="...">` references by inlining the
/// referenced group's attributes into each complex type's attribute list.
///
/// Handles transitive attributeGroup refs (e.g., `AssociationAttributeGroup`
/// → xlink:simpleAttrs) via iterative expansion.
fn resolve_attribute_groups(schema: &mut XsdSchema) {
    // Collect all attribute groups (main + imported) into owned data
    let mut all_groups: HashMap<String, Vec<XsdAttribute>> = HashMap::new();
    for (name, attrs) in &schema.attribute_groups {
        all_groups.insert(name.clone(), attrs.clone());
    }
    for imp in schema.imported_namespaces.values() {
        for (name, attrs) in &imp.attribute_groups {
            all_groups.insert(name.clone(), attrs.clone());
        }
    }

    // Iteratively expand attributeGroup placeholders within groups
    let mut changed = true;
    while changed {
        changed = false;
        let mut expanded_groups = HashMap::new();
        for (name, attrs) in &all_groups {
            let mut result = Vec::new();
            let mut any_expanded = false;
            for attr in attrs {
                if attr.type_ref == "__attr_group__" {
                    if let Some(group_attrs) = all_groups.get(&attr.name) {
                        result.extend(group_attrs.clone());
                        any_expanded = true;
                        continue;
                    }
                }
                result.push(attr.clone());
            }
            if any_expanded {
                changed = true;
            }
            expanded_groups.insert(name.clone(), result);
        }
        all_groups = expanded_groups;
    }

    // Expand attributeGroup placeholders in complex type attributes
    let expand_types = |types: &mut HashMap<String, XsdType>| {
        for typ in types.values_mut() {
            if let XsdType::Complex(ct) = typ {
                let mut expanded = Vec::new();
                let orig = std::mem::take(&mut ct.attributes);
                for attr in orig {
                    if attr.type_ref == "__attr_group__" {
                        if let Some(group_attrs) = all_groups.get(&attr.name) {
                            expanded.extend(group_attrs.clone());
                            continue;
                        }
                    }
                    expanded.push(attr);
                }
                ct.attributes = expanded;
            }
        }
    };

    expand_types(&mut schema.types);
    for imp in schema.imported_namespaces.values_mut() {
        expand_types(&mut imp.types);
    }
}

/// Merges base-type content models into derived types via `complexContent/extension`.
///
/// XSD 1.0 section 3.4.2: when a complex type is derived by extension,
/// the effective content model is the base type's particles followed by
/// the extension's own particles, forming a single sequence.
///
/// This must run after all schemas are loaded so base types from imported
/// namespaces are available.
fn merge_extension_bases(schema: &mut XsdSchema) {
    // Collect ALL extensions (main + imported) first, then merge.
    // This avoids borrow conflicts between mutable types and immutable schema.

    // Main schema extensions
    let main_extensions: Vec<(String, String)> = schema
        .types
        .iter()
        .filter_map(|(name, ty)| {
            if let XsdType::Complex(ct) = ty {
                ct.extension_base
                    .as_ref()
                    .map(|base| (name.clone(), base.clone()))
            } else {
                None
            }
        })
        .collect();

    for (type_name, base_name) in main_extensions {
        let base_particles = resolve_base_particles(&base_name, schema);
        let base_attrs = resolve_base_attributes(&base_name, schema);
        if base_particles.is_empty() && base_attrs.is_empty() {
            continue;
        }
        merge_type_extension(&mut schema.types, &type_name, base_particles, base_attrs);
    }

    // Imported namespace extensions
    let imported_extensions: Vec<(String, String, String)> = schema
        .imported_namespaces
        .iter()
        .flat_map(|(ns, imp)| {
            imp.types.iter().filter_map(|(name, ty)| {
                if let XsdType::Complex(ct) = ty {
                    ct.extension_base
                        .as_ref()
                        .map(|base| (ns.clone(), name.clone(), base.clone()))
                } else {
                    None
                }
            })
        })
        .collect();

    for (ns, type_name, base_name) in imported_extensions {
        let base_particles = resolve_base_particles(&base_name, schema);
        let base_attrs = resolve_base_attributes(&base_name, schema);
        if base_particles.is_empty() && base_attrs.is_empty() {
            continue;
        }
        if let Some(imp) = schema.imported_namespaces.get_mut(&ns) {
            merge_type_extension(&mut imp.types, &type_name, base_particles, base_attrs);
        }
    }
}

fn merge_type_extension(
    types: &mut HashMap<String, XsdType>,
    type_name: &str,
    base_particles: Vec<XsdParticle>,
    base_attrs: Vec<XsdAttribute>,
) {
    if let Some(XsdType::Complex(ct)) = types.get_mut(type_name) {
        // Merge content model particles
        match &mut ct.content {
            ComplexContent::Sequence(ext_particles) => {
                let mut merged = base_particles;
                merged.append(ext_particles);
                *ext_particles = merged;
            }
            ComplexContent::Empty => {
                ct.content = ComplexContent::Sequence(base_particles);
            }
            ComplexContent::Choice(_) | ComplexContent::All(_) => {
                let mut merged = base_particles;
                let existing = ct.content.clone();
                merged.push(XsdParticle::Group(existing));
                ct.content = ComplexContent::Sequence(merged);
            }
            ComplexContent::SimpleContent { .. } => {}
        }
        // Merge base attributes before extension attributes
        if !base_attrs.is_empty() {
            let mut merged_attrs = base_attrs;
            merged_attrs.append(&mut ct.attributes);
            ct.attributes = merged_attrs;
        }
        ct.extension_base = None;
    }
}

/// Resolves a type's attributes, chasing extension chains.
/// Returns all inherited attributes from the full type hierarchy.
fn resolve_base_attributes(type_name: &str, schema: &XsdSchema) -> Vec<XsdAttribute> {
    resolve_base_attributes_impl(type_name, schema, &mut HashSet::new())
}

fn resolve_base_attributes_impl(
    type_name: &str,
    schema: &XsdSchema,
    visited: &mut HashSet<String>,
) -> Vec<XsdAttribute> {
    let local_name = if let Some((_, l)) = type_name.split_once(':') {
        l
    } else {
        type_name
    };

    if !visited.insert(local_name.to_string()) {
        return Vec::new();
    }

    let Some(ct) = find_complex_type(local_name, schema) else {
        return Vec::new();
    };

    // Recursively get base attributes first
    let mut attrs = if let Some(ref base) = ct.extension_base {
        resolve_base_attributes_impl(base, schema, visited)
    } else {
        Vec::new()
    };

    // Then add this type's own attributes
    attrs.extend(ct.attributes.clone());
    attrs
}

/// Resolves a type's content model particles, chasing extension chains.
///
/// Returns the effective particles for a type including all inherited
/// base-type particles, in the correct XSD derivation order.
/// Looks up a complex type by local name, checking local types and
/// imported namespace types.
fn find_complex_type<'a>(local_name: &str, schema: &'a XsdSchema) -> Option<&'a ComplexType> {
    if let Some(XsdType::Complex(ct)) = schema.types.get(local_name) {
        return Some(ct);
    }
    // Check imported namespaces
    for imported in schema.imported_namespaces.values() {
        if let Some(XsdType::Complex(ct)) = imported.types.get(local_name) {
            return Some(ct);
        }
    }
    None
}

fn resolve_base_particles(type_name: &str, schema: &XsdSchema) -> Vec<XsdParticle> {
    resolve_base_particles_impl(type_name, schema, &mut HashSet::new())
}

fn resolve_base_particles_impl(
    type_name: &str,
    schema: &XsdSchema,
    visited: &mut HashSet<String>,
) -> Vec<XsdParticle> {
    // Resolve QName prefix (e.g., "adv:AA_ObjektType" → "AA_ObjektType")
    let local_name = if let Some((_, l)) = type_name.split_once(':') {
        l
    } else {
        type_name
    };

    if !visited.insert(local_name.to_string()) {
        return Vec::new(); // Cycle detected, stop
    }

    let Some(ct) = find_complex_type(local_name, schema) else {
        return Vec::new();
    };

    // Recursively resolve base type particles first
    let mut particles = if let Some(ref base) = ct.extension_base {
        resolve_base_particles_impl(base, schema, visited)
    } else {
        Vec::new()
    };

    // Then append this type's own particles
    match &ct.content {
        ComplexContent::Sequence(p) => particles.extend(p.iter().cloned()),
        ComplexContent::Empty | ComplexContent::SimpleContent { .. } => {}
        ComplexContent::Choice(p) => {
            particles.push(XsdParticle::Group(ComplexContent::Choice(p.clone())));
        }
        ComplexContent::All(p) => {
            particles.push(XsdParticle::Group(ComplexContent::All(p.clone())));
        }
    }

    particles
}

/// Registers all supported built-in XSD types in the schema.
fn register_builtin_types(schema: &mut XsdSchema) {
    let builtins = [
        "string",
        "normalizedString",
        "token",
        "integer",
        "int",
        "long",
        "short",
        "byte",
        "positiveInteger",
        "nonNegativeInteger",
        "negativeInteger",
        "nonPositiveInteger",
        "unsignedInt",
        "unsignedLong",
        "unsignedShort",
        "unsignedByte",
        "decimal",
        "float",
        "double",
        "boolean",
        "date",
        "dateTime",
        "time",
        "anyURI",
        "ID",
        "IDREF",
        "NMTOKEN",
        "anyType",
        "anySimpleType",
    ];
    for name in builtins {
        schema.types.insert(
            name.to_string(),
            XsdType::Simple(SimpleType {
                name: Some(name.to_string()),
                variety: SimpleTypeVariety::Builtin(name.to_string()),
            }),
        );
    }
}

/// Parses an `<xs:element>` declaration.
///
/// Handles both named declarations (`name="foo" type="xs:string"`) and
/// element references (`ref="cbc:ID"`). For references, the `ref` `QName`
/// Parses an `<xs:any>` element wildcard declaration.
fn parse_any_wildcard(doc: &Document, node: NodeId) -> XsdAny {
    let namespace_str = doc.attribute(node, "namespace").unwrap_or("##any");
    let namespace = match namespace_str {
        "##any" => XsdAnyNamespace::Any,
        "##other" => XsdAnyNamespace::Other,
        other => XsdAnyNamespace::List(other.split_whitespace().map(String::from).collect()),
    };

    let process_contents = match doc.attribute(node, "processContents").unwrap_or("") {
        "lax" => XsdProcessContents::Lax,
        "skip" => XsdProcessContents::Skip,
        _ => XsdProcessContents::Strict,
    };

    let min_occurs = doc
        .attribute(node, "minOccurs")
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1);
    let max_occurs = doc
        .attribute(node, "maxOccurs")
        .map_or(MaxOccurs::Bounded(1), |s| {
            if s == "unbounded" {
                MaxOccurs::Unbounded
            } else {
                MaxOccurs::Bounded(s.parse::<u32>().unwrap_or(1))
            }
        });

    XsdAny {
        namespace,
        process_contents,
        min_occurs,
        max_occurs,
    }
}

/// Parses an `<xs:element>` declaration within a content model. Element refs
fn parse_element_decl(doc: &Document, node: NodeId) -> Option<XsdElement> {
    let min_occurs = doc
        .attribute(node, "minOccurs")
        .and_then(|v| v.parse::<u32>().ok())
        .unwrap_or(1);
    let max_occurs = doc
        .attribute(node, "maxOccurs")
        .map_or(MaxOccurs::Bounded(1), |v| {
            if v == "unbounded" {
                MaxOccurs::Unbounded
            } else {
                MaxOccurs::Bounded(v.parse::<u32>().unwrap_or(1))
            }
        });

    // Handle ref="prefix:name" — reference to a global element declaration
    if let Some(ref_qname) = doc.attribute(node, "ref") {
        let local_name = if let Some((_prefix, local)) = ref_qname.split_once(':') {
            local.to_string()
        } else {
            ref_qname.to_string()
        };
        return Some(XsdElement {
            name: local_name,
            type_ref: None,
            inline_type: None,
            element_ref: Some(ref_qname.to_string()),
            min_occurs,
            max_occurs,
            substitution_group: None,
            is_abstract: false,
        });
    }

    let name = doc.attribute(node, "name")?.to_string();
    let type_ref = doc.attribute(node, "type").map(strip_xs_prefix);
    let inline_type = find_inline_type(doc, node);
    let substitution_group = doc.attribute(node, "substitutionGroup").map(String::from);
    let is_abstract = doc
        .attribute(node, "abstract")
        .is_some_and(|v| v == "true" || v == "1");
    Some(XsdElement {
        name,
        type_ref,
        inline_type,
        element_ref: None,
        min_occurs,
        max_occurs,
        substitution_group,
        is_abstract,
    })
}

/// Looks for an inline `<xs:complexType>` or `<xs:simpleType>` child.
fn find_inline_type(doc: &Document, node: NodeId) -> Option<XsdType> {
    for child in doc.children(node) {
        let Some(child_name) = doc.node_name(child) else {
            continue;
        };
        match child_name {
            "complexType" => {
                return Some(XsdType::Complex(parse_complex_type(
                    doc,
                    child,
                    &HashMap::new(),
                )))
            }
            "simpleType" => {
                return Some(XsdType::Simple(parse_simple_type(doc, child)));
            }
            _ => {}
        }
    }
    None
}

/// Parses an `<xs:complexType>` element.
fn parse_complex_type(
    doc: &Document,
    node: NodeId,
    group_defs: &HashMap<String, ComplexContent>,
) -> ComplexType {
    let name = doc.attribute(node, "name").map(String::from);
    let mixed = doc.attribute(node, "mixed") == Some("true");
    let mut content = ComplexContent::Empty;
    let mut attributes = Vec::new();
    let mut extension_base: Option<String> = None;

    for child in doc.children(node) {
        let Some(child_name) = doc.node_name(child) else {
            continue;
        };
        match child_name {
            "sequence" => {
                content = parse_compositor(doc, child, CompositorKind::Sequence, group_defs);
            }
            "choice" => {
                content = parse_compositor(doc, child, CompositorKind::Choice, group_defs);
            }
            "all" => {
                content = parse_compositor(doc, child, CompositorKind::All, group_defs);
            }
            "attribute" => {
                if let Some(attr) = parse_attribute_decl(doc, child) {
                    attributes.push(attr);
                }
            }
            "attributeGroup" => {
                if let Some(ref_name) = doc.attribute(child, "ref") {
                    let local = if let Some((_, l)) = ref_name.split_once(':') {
                        l.to_string()
                    } else {
                        ref_name.to_string()
                    };
                    attributes.push(XsdAttribute {
                        name: local,
                        type_ref: "__attr_group__".to_string(),
                        required: false,
                        fixed: None,
                    });
                }
            }
            "simpleContent" => {
                content = parse_simple_content(doc, child);
                collect_simple_content_attributes(doc, child, &mut attributes);
            }
            "complexContent" => {
                let (base, ct, ext_attrs) = parse_complex_content(doc, child, group_defs);
                extension_base = base;
                content = ct;
                attributes.extend(ext_attrs);
            }
            _ => {}
        }
    }
    ComplexType {
        name,
        content,
        attributes,
        mixed,
        extension_base,
    }
}

/// Parses `<xs:complexContent><xs:extension base="...">`.
///
/// Returns `(base_type_name, content_model, extra_attributes)`.
/// The content model contains only the extension's own particles;
/// base-type merging is done in [`merge_extension_bases`].
#[allow(clippy::too_many_lines)]
fn parse_complex_content(
    doc: &Document,
    cc_node: NodeId,
    group_defs: &HashMap<String, ComplexContent>,
) -> (Option<String>, ComplexContent, Vec<XsdAttribute>) {
    let mut base = None;
    let mut content = ComplexContent::Empty;
    let mut attributes = Vec::new();

    for cc_child in doc.children(cc_node) {
        let Some(cc_name) = doc.node_name(cc_child) else {
            continue;
        };
        match cc_name {
            "extension" => {
                base = doc.attribute(cc_child, "base").map(String::from);
                for ext_child in doc.children(cc_child) {
                    let Some(ext_name) = doc.node_name(ext_child) else {
                        continue;
                    };
                    match ext_name {
                        "sequence" => {
                            content = parse_compositor(
                                doc,
                                ext_child,
                                CompositorKind::Sequence,
                                group_defs,
                            );
                        }
                        "choice" => {
                            content = parse_compositor(
                                doc,
                                ext_child,
                                CompositorKind::Choice,
                                group_defs,
                            );
                        }
                        "all" => {
                            content =
                                parse_compositor(doc, ext_child, CompositorKind::All, group_defs);
                        }
                        "attribute" => {
                            if let Some(attr) = parse_attribute_decl(doc, ext_child) {
                                attributes.push(attr);
                            }
                        }
                        "attributeGroup" => {
                            if let Some(ref_name) = doc.attribute(ext_child, "ref") {
                                let local = if let Some((_, l)) = ref_name.split_once(':') {
                                    l.to_string()
                                } else {
                                    ref_name.to_string()
                                };
                                attributes.push(XsdAttribute {
                                    name: local,
                                    type_ref: "__attr_group__".to_string(),
                                    required: false,
                                    fixed: None,
                                });
                            }
                        }
                        _ => {}
                    }
                }
            }
            "restriction" => {
                // restriction replaces the base content model entirely
                let _base = doc.attribute(cc_child, "base").map(String::from);
                for restr_child in doc.children(cc_child) {
                    let Some(restr_name) = doc.node_name(restr_child) else {
                        continue;
                    };
                    match restr_name {
                        "sequence" => {
                            content = parse_compositor(
                                doc,
                                restr_child,
                                CompositorKind::Sequence,
                                group_defs,
                            );
                        }
                        "choice" => {
                            content = parse_compositor(
                                doc,
                                restr_child,
                                CompositorKind::Choice,
                                group_defs,
                            );
                        }
                        "all" => {
                            content =
                                parse_compositor(doc, restr_child, CompositorKind::All, group_defs);
                        }
                        "attribute" => {
                            if let Some(attr) = parse_attribute_decl(doc, restr_child) {
                                attributes.push(attr);
                            }
                        }
                        "attributeGroup" => {
                            if let Some(ref_name) = doc.attribute(restr_child, "ref") {
                                let local = if let Some((_, l)) = ref_name.split_once(':') {
                                    l.to_string()
                                } else {
                                    ref_name.to_string()
                                };
                                attributes.push(XsdAttribute {
                                    name: local,
                                    type_ref: "__attr_group__".to_string(),
                                    required: false,
                                    fixed: None,
                                });
                            }
                        }
                        _ => {}
                    }
                }
                // Restriction replaces the base content model, so no extension_base
                return (None, content, attributes);
            }
            _ => {}
        }
    }
    (base, content, attributes)
}

/// Collects attribute declarations from `<xs:simpleContent>` extension children.
fn collect_simple_content_attributes(
    doc: &Document,
    sc_node: NodeId,
    attributes: &mut Vec<XsdAttribute>,
) {
    for sc_child in doc.children(sc_node) {
        if !matches!(doc.node_name(sc_child), Some("extension" | "restriction")) {
            continue;
        }
        for ext_child in doc.children(sc_child) {
            match doc.node_name(ext_child) {
                Some("attribute") => {
                    if let Some(attr) = parse_attribute_decl(doc, ext_child) {
                        attributes.push(attr);
                    }
                }
                Some("attributeGroup") => {
                    if let Some(ref_name) = doc.attribute(ext_child, "ref") {
                        let local = if let Some((_, l)) = ref_name.split_once(':') {
                            l.to_string()
                        } else {
                            ref_name.to_string()
                        };
                        attributes.push(XsdAttribute {
                            name: local,
                            type_ref: "__attr_group__".to_string(),
                            required: false,
                            fixed: None,
                        });
                    }
                }
                _ => {}
            }
        }
    }
}

/// Compositor kind for parsing content model groups.
#[derive(Clone, Copy)]
enum CompositorKind {
    Sequence,
    Choice,
    All,
}

/// Parses top-level `<xsd:group name="...">` declarations.
fn parse_named_group(
    doc: &Document,
    node: NodeId,
    group_defs: &HashMap<String, ComplexContent>,
) -> Option<ComplexContent> {
    for child in doc.children(node) {
        let Some(name) = doc.node_name(child) else {
            continue;
        };
        match name {
            "sequence" => {
                return Some(parse_compositor(
                    doc,
                    child,
                    CompositorKind::Sequence,
                    group_defs,
                ));
            }
            "choice" => {
                return Some(parse_compositor(
                    doc,
                    child,
                    CompositorKind::Choice,
                    group_defs,
                ));
            }
            "all" => {
                return Some(parse_compositor(
                    doc,
                    child,
                    CompositorKind::All,
                    group_defs,
                ));
            }
            _ => {}
        }
    }
    None
}

/// Parses a compositor (`<xs:sequence>`, `<xs:choice>`, or `<xs:all>`).
fn parse_compositor(
    doc: &Document,
    node: NodeId,
    kind: CompositorKind,
    group_defs: &HashMap<String, ComplexContent>,
) -> ComplexContent {
    let mut particles = Vec::new();
    // Read compositor-level minOccurs/maxOccurs.
    // XSD 1.0: these apply to the group as a whole.
    // When minOccurs=0, all direct element children become effectively optional.
    let compositor_min = parse_min_occurs(doc, node);
    let compositor_max = doc
        .attribute(node, "maxOccurs")
        .map_or(MaxOccurs::Bounded(1), |v| {
            if v == "unbounded" {
                MaxOccurs::Unbounded
            } else {
                MaxOccurs::Bounded(v.parse::<u32>().unwrap_or(1))
            }
        });

    for child in doc.children(node) {
        let Some(child_name) = doc.node_name(child) else {
            continue;
        };
        match child_name {
            "element" => {
                if let Some(mut elem) = parse_element_decl(doc, child) {
                    // If the compositor itself is optional (minOccurs=0),
                    // propagate that to element children so the validator
                    // doesn't require them.
                    if compositor_min == 0 {
                        elem.min_occurs = 0;
                    }
                    particles.push(XsdParticle::Element(elem));
                }
            }
            "sequence" => {
                particles.push(XsdParticle::Group(parse_compositor(
                    doc,
                    child,
                    CompositorKind::Sequence,
                    group_defs,
                )));
            }
            "choice" => {
                particles.push(XsdParticle::Group(parse_compositor(
                    doc,
                    child,
                    CompositorKind::Choice,
                    group_defs,
                )));
            }
            "all" => {
                particles.push(XsdParticle::Group(parse_compositor(
                    doc,
                    child,
                    CompositorKind::All,
                    group_defs,
                )));
            }
            "group" => {
                if let Some(ref_qname) = doc.attribute(child, "ref") {
                    let local = if let Some((_, l)) = ref_qname.split_once(':') {
                        l
                    } else {
                        ref_qname
                    };
                    if let Some(group_content) = group_defs.get(local) {
                        particles.push(XsdParticle::Group(group_content.clone()));
                    }
                }
            }
            "any" => {
                let any = parse_any_wildcard(doc, child);
                particles.push(XsdParticle::Any(any));
            }
            _ => {}
        }
    }
    // For single-element compositor groups, group maxOccurs can be safely
    // propagated to the child element (A repeated N times == element A maxOccurs=N).
    if particles.len() == 1 {
        if let XsdParticle::Element(elem) = &mut particles[0] {
            match compositor_max {
                MaxOccurs::Bounded(n) if n > 1 => elem.max_occurs = MaxOccurs::Bounded(n),
                _ => {}
            }
        }
    }

    match kind {
        CompositorKind::Sequence => ComplexContent::Sequence(particles),
        CompositorKind::Choice => ComplexContent::Choice(particles),
        CompositorKind::All => ComplexContent::All(particles),
    }
}

/// Parses the `minOccurs` attribute from a particle node.
/// Returns 0 when not specified (XSD default for compositor-level is 1,
/// but individual element defaults are also 1 — we handle that in
/// `parse_element_decl`).
fn parse_min_occurs(doc: &Document, node: NodeId) -> u32 {
    doc.attribute(node, "minOccurs")
        .and_then(|v| v.parse().ok())
        .unwrap_or(1)
}

/// Parses `<xs:simpleContent>` within a complex type.
fn parse_simple_content(doc: &Document, node: NodeId) -> ComplexContent {
    for child in doc.children(node) {
        if matches!(doc.node_name(child), Some("extension" | "restriction")) {
            if let Some(base) = doc.attribute(child, "base") {
                return ComplexContent::SimpleContent {
                    base: strip_xs_prefix(base),
                };
            }
        }
    }
    ComplexContent::Empty
}

/// Parses an `<xs:simpleType>` element.
fn parse_simple_type(doc: &Document, node: NodeId) -> SimpleType {
    let name = doc.attribute(node, "name").map(String::from);
    for child in doc.children(node) {
        let Some(child_name) = doc.node_name(child) else {
            continue;
        };
        match child_name {
            "restriction" => {
                let base = doc
                    .attribute(child, "base")
                    .map_or_else(|| "string".to_string(), strip_xs_prefix);
                let facets = parse_facets(doc, child);
                return SimpleType {
                    name,
                    variety: SimpleTypeVariety::Restriction { base, facets },
                };
            }
            "list" => {
                let item_type = doc
                    .attribute(child, "itemType")
                    .map_or_else(|| "string".to_string(), strip_xs_prefix);
                return SimpleType {
                    name,
                    variety: SimpleTypeVariety::List { item_type },
                };
            }
            "union" => {
                let member_types = doc
                    .attribute(child, "memberTypes")
                    .map_or_else(Vec::new, |mt| {
                        mt.split_whitespace().map(strip_xs_prefix).collect()
                    });
                return SimpleType {
                    name,
                    variety: SimpleTypeVariety::Union { member_types },
                };
            }
            _ => {}
        }
    }
    SimpleType {
        name,
        variety: SimpleTypeVariety::Builtin("string".to_string()),
    }
}

/// Parses facet children from an `<xs:restriction>` element.
fn parse_facets(doc: &Document, restriction_node: NodeId) -> Vec<Facet> {
    let mut facets = Vec::new();
    let mut enumerations = Vec::new();
    for child in doc.children(restriction_node) {
        let Some(child_name) = doc.node_name(child) else {
            continue;
        };
        let Some(value) = doc.attribute(child, "value") else {
            continue;
        };
        match child_name {
            "minLength" => {
                if let Ok(n) = value.parse::<usize>() {
                    facets.push(Facet::MinLength(n));
                }
            }
            "maxLength" => {
                if let Ok(n) = value.parse::<usize>() {
                    facets.push(Facet::MaxLength(n));
                }
            }
            "length" => {
                if let Ok(n) = value.parse::<usize>() {
                    facets.push(Facet::Length(n));
                }
            }
            "pattern" => facets.push(Facet::Pattern(value.to_string())),
            "enumeration" => enumerations.push(value.to_string()),
            "minInclusive" => facets.push(Facet::MinInclusive(value.to_string())),
            "maxInclusive" => facets.push(Facet::MaxInclusive(value.to_string())),
            "minExclusive" => facets.push(Facet::MinExclusive(value.to_string())),
            "maxExclusive" => facets.push(Facet::MaxExclusive(value.to_string())),
            "whiteSpace" => {
                let ws = match value {
                    "replace" => WhiteSpaceValue::Replace,
                    "collapse" => WhiteSpaceValue::Collapse,
                    _ => WhiteSpaceValue::Preserve,
                };
                facets.push(Facet::WhiteSpace(ws));
            }
            "totalDigits" => {
                if let Ok(n) = value.parse::<usize>() {
                    facets.push(Facet::TotalDigits(n));
                }
            }
            "fractionDigits" => {
                if let Ok(n) = value.parse::<usize>() {
                    facets.push(Facet::FractionDigits(n));
                }
            }
            _ => {}
        }
    }
    if !enumerations.is_empty() {
        facets.push(Facet::Enumeration(enumerations));
    }
    facets
}

/// Parses an `<xs:attribute>` declaration.
fn parse_attribute_decl(doc: &Document, node: NodeId) -> Option<XsdAttribute> {
    // Handle both name="..." and ref="prefix:localName"
    let (name, type_ref) = if let Some(ref_qname) = doc.attribute(node, "ref") {
        let local = if let Some((_, l)) = ref_qname.split_once(':') {
            l.to_string()
        } else {
            ref_qname.to_string()
        };
        (local, "xs:anyURI".to_string())
    } else {
        let name = doc.attribute(node, "name")?.to_string();
        let type_ref = doc
            .attribute(node, "type")
            .map_or_else(|| "string".to_string(), strip_xs_prefix);
        (name, type_ref)
    };
    let required = doc.attribute(node, "use") == Some("required");
    let fixed = doc.attribute(node, "fixed").map(String::from);
    Some(XsdAttribute {
        name,
        type_ref,
        required,
        fixed,
    })
}

/// Parses all `<xs:attribute>` and `<xs:attributeGroup ref="...">` children
/// of a given node. `AttributeGroup` refs are stored as placeholders
/// (`type_ref`="__`attr_group`__") for later expansion.
fn parse_attributes(doc: &Document, node: NodeId) -> Vec<XsdAttribute> {
    let mut attrs = Vec::new();
    for child in doc.children(node) {
        let Some(name) = doc.node_name(child) else {
            continue;
        };
        if name == "attribute" {
            if let Some(attr) = parse_attribute_decl(doc, child) {
                attrs.push(attr);
            }
        } else if name == "attributeGroup" {
            if let Some(ref_name) = doc.attribute(child, "ref") {
                let local = if let Some((_, l)) = ref_name.split_once(':') {
                    l.to_string()
                } else {
                    ref_name.to_string()
                };
                attrs.push(XsdAttribute {
                    name: local,
                    type_ref: "__attr_group__".to_string(),
                    required: false,
                    fixed: None,
                });
            }
        }
    }
    attrs
}

/// Builds a prefix-to-namespace-URI map from `xmlns:*` attributes on a node.
///
/// Scans the attributes of the given node for namespace declarations
/// (`xmlns:prefix="uri"`) and returns a map from prefix to URI.
fn build_prefix_map(doc: &Document, node: NodeId) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for attr in doc.attributes(node) {
        if attr.prefix.as_deref() == Some("xmlns") {
            map.insert(attr.name.clone(), attr.value.clone());
        }
    }
    map
}

/// Resolves a `QName` type reference into a namespace URI and local name.
///
/// Given a type reference like `"xs:string"` or `"tns:AddressType"`, splits
/// on `:` and looks up the prefix in the provided prefix map to get the
/// namespace URI.
///
/// Returns `(None, local_name)` for unprefixed names and
/// `(Some(namespace_uri), local_name)` for prefixed names.
fn resolve_type_qname(
    qname: &str,
    prefix_map: &HashMap<String, String>,
) -> (Option<String>, String) {
    if let Some((prefix, local)) = qname.split_once(':') {
        let ns = prefix_map.get(prefix).cloned();
        (ns, local.to_string())
    } else {
        (None, qname.to_string())
    }
}

/// Strips an `xs:` or `xsd:` prefix from a type reference string.
fn strip_xs_prefix(name: &str) -> String {
    if let Some(local) = name.strip_prefix("xs:") {
        local.to_string()
    } else if let Some(local) = name.strip_prefix("xsd:") {
        local.to_string()
    } else {
        name.to_string()
    }
}

// ---------------------------------------------------------------------------
// Validator
// ---------------------------------------------------------------------------

/// Validates an XML document against an XSD schema.
///
/// Walks the document tree starting from the root element, matching elements
/// against their declarations in the schema, checking content models,
/// attribute constraints, and simple type facets.
///
/// # Examples
///
/// ```
/// use xmloxide::Document;
/// use xmloxide::validation::xsd::{parse_xsd, validate_xsd};
///
/// let schema = parse_xsd(r#"
///   <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
///     <xs:element name="note" type="xs:string"/>
///   </xs:schema>
/// "#).unwrap();
///
/// let doc = Document::parse_str("<note>Hello</note>").unwrap();
/// let result = validate_xsd(&doc, &schema);
/// assert!(result.is_valid);
/// ```
///
/// Looks up the type by name in the schema (including imported namespaces),
/// then extracts element names from the sequence in declared order.
/// Returns `None` if the type is not found or has no sequence content.
pub fn get_type_element_order(type_name: &str, schema: &XsdSchema) -> Option<Vec<String>> {
    let ct = find_complex_type(type_name, schema)?;
    extract_element_names(&ct.content)
}

fn extract_element_names(content: &ComplexContent) -> Option<Vec<String>> {
    match content {
        ComplexContent::Sequence(particles) => {
            let mut names = Vec::new();
            for p in particles {
                match p {
                    XsdParticle::Element(e) => names.push(e.name.clone()),
                    XsdParticle::Group(g) => {
                        if let Some(sub) = extract_element_names(g) {
                            names.extend(sub);
                        }
                    }
                    XsdParticle::Any(_) => {
                        // Wildcard — skip
                    }
                }
            }
            Some(names)
        }
        ComplexContent::Choice(particles) => {
            // For choice, collect all element names
            let mut names = Vec::new();
            for p in particles {
                match p {
                    XsdParticle::Element(e) => names.push(e.name.clone()),
                    XsdParticle::Group(g) => {
                        if let Some(sub) = extract_element_names(g) {
                            names.extend(sub);
                        }
                    }
                    XsdParticle::Any(_) => {}
                }
            }
            Some(names)
        }
        _ => None,
    }
}

/// `assert!(result.is_valid)`;
/// ```
pub fn validate_xsd(doc: &Document, schema: &XsdSchema) -> ValidationResult {
    let mut errors = Vec::new();
    let Some(root) = doc.root_element() else {
        errors.push(ValidationError {
            message: "document has no root element".to_string(),
            line: None,
            column: None,
        });
        return ValidationResult {
            is_valid: false,
            errors,
            warnings: vec![],
        };
    };
    let root_name = doc.node_name(root).unwrap_or("");
    if let Some(decl) = schema.elements.get(root_name) {
        validate_element(doc, root, decl, schema, &mut errors);
    } else if let Some(decl) = find_root_element_in_imports(root_name, schema) {
        validate_element(doc, root, decl, schema, &mut errors);
    } else {
        errors.push(ValidationError {
            message: format!(
                "element <{root_name}> not declared as a global element in the schema"
            ),
            line: None,
            column: None,
        });
    }
    ValidationResult {
        is_valid: errors.is_empty(),
        errors,
        warnings: vec![],
    }
}

/// Strict XSD validation — reports all deviations from the schema.
///
/// Like [`validate_xsd`] but additionally:
/// - Reports unknown/undeclared attributes as errors
/// - Treats `processContents="strict"` on `<xsd:any>` wildcards as actual
///   strict validation (attempts to resolve element declarations and reports
///   errors when elements cannot be validated)
/// - Reports elements whose type cannot be resolved (instead of silently
///   accepting them as `anyType`)
///
/// # Examples
///
/// ```
/// use xmloxide::Document;
/// use xmloxide::validation::xsd::{parse_xsd, validate_xsd_strict};
///
/// let schema = parse_xsd(r#"
///   <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
///     <xs:element name="note" type="xs:string"/>
///   </xs:schema>
/// "#).unwrap();
///
/// let doc = Document::parse_str(r#"<note extra="unknown">Hello</note>"#).unwrap();
/// let result = validate_xsd_strict(&doc, &schema);
/// assert!(!result.is_valid); // unknown attribute reported
/// ```
pub fn validate_xsd_strict(doc: &Document, schema: &XsdSchema) -> ValidationResult {
    let mut errors = Vec::new();
    let Some(root) = doc.root_element() else {
        errors.push(ValidationError {
            message: "document has no root element".to_string(),
            line: None,
            column: None,
        });
        return ValidationResult {
            is_valid: false,
            errors,
            warnings: vec![],
        };
    };
    let root_name = doc.node_name(root).unwrap_or("");
    if let Some(decl) = schema.elements.get(root_name) {
        validate_element_strict(doc, root, decl, schema, &mut errors);
    } else if let Some(decl) = find_root_element_in_imports(root_name, schema) {
        validate_element_strict(doc, root, decl, schema, &mut errors);
    } else {
        errors.push(ValidationError {
            message: format!(
                "element <{root_name}> not declared as a global element in the schema"
            ),
            line: None,
            column: None,
        });
    }
    ValidationResult {
        is_valid: errors.is_empty(),
        errors,
        warnings: vec![],
    }
}

/// Strict element validation: validates content and reports unknown attributes.
pub fn validate_element_strict(
    doc: &Document,
    node: NodeId,
    decl: &XsdElement,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    match resolve_element_type(decl, schema) {
        Some(XsdType::Complex(ct)) => {
            let mut declared_attrs = ct.attributes.clone();
            if let ComplexContent::SimpleContent { base } = &ct.content {
                let mut inherited = resolve_simple_content_base_attributes(base, schema);
                inherited.append(&mut declared_attrs);
                declared_attrs = inherited;
            }
            validate_attributes_strict(doc, node, &declared_attrs, schema, errors);
            validate_complex_element_strict(doc, node, ct, schema, errors);
        }
        Some(XsdType::Simple(st)) => {
            validate_simple_element(doc, node, st, schema, errors);
            validate_attributes_strict(doc, node, &[], schema, errors);
        }
        None => {
            // No declared type defaults to xs:anyType (valid in XSD).
            if decl.type_ref.is_none() && decl.inline_type.is_none() && decl.element_ref.is_none() {
                return;
            }
            // Otherwise this is a real unresolved-type error.
            let elem_name = doc.node_name(node).unwrap_or("<unknown>");
            errors.push(ValidationError {
                message: format!("element <{elem_name}> has no resolvable type declaration"),
                line: None,
                column: None,
            });
        }
    }
}

/// Deep validation fallback: when an element has no resolved type (e.g.
/// `wfs:member` from an external namespace), validate each of its children
/// by looking them up as global element declarations in the schema.
/// This enables validation of AAA feature elements nested inside WFS/GML
/// wrapper elements whose types are unknown to the AAA schema.
fn validate_children_by_schema_lookup(
    doc: &Document,
    node: NodeId,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let ce = collect_child_elements(doc, node);
    for child in ce {
        let child_name = doc.node_name(child).unwrap_or("");
        // Only attempt lookup for elements in the schema's target namespace
        // or elements with no namespace (unqualified).
        let child_ns = doc.node_namespace(child).unwrap_or("");
        let in_schema_ns = schema.target_namespace.as_deref() == Some(child_ns)
            || child_ns.is_empty();
        if !in_schema_ns {
            continue;
        }
        // Look up as global element declaration
        if let Some(child_decl) = schema.elements.get(child_name) {
            validate_element_strict(doc, child, child_decl, schema, errors);
        }
    }
}

fn resolve_simple_content_base_attributes(
    base_type: &str,
    schema: &XsdSchema,
) -> Vec<XsdAttribute> {
    resolve_simple_content_base_attributes_impl(base_type, schema, &mut HashSet::new())
}

fn resolve_simple_content_base_attributes_impl(
    base_type: &str,
    schema: &XsdSchema,
    visited: &mut HashSet<String>,
) -> Vec<XsdAttribute> {
    let local = base_type
        .split_once(':')
        .map_or(base_type, |(_, l)| l)
        .to_string();
    if !visited.insert(local.clone()) {
        return Vec::new();
    }

    let Some(ty) = resolve_type_name(base_type, schema).or_else(|| schema.types.get(&local)) else {
        return Vec::new();
    };

    match ty {
        XsdType::Complex(ct) => {
            let mut attrs = if let ComplexContent::SimpleContent { base } = &ct.content {
                resolve_simple_content_base_attributes_impl(base, schema, visited)
            } else {
                Vec::new()
            };
            attrs.extend(ct.attributes.clone());
            attrs
        }
        XsdType::Simple(_) => Vec::new(),
    }
}

/// Strict attribute validation: reports unknown attributes not declared in the schema.
fn validate_attributes_strict(
    doc: &Document,
    node: NodeId,
    declared_attrs: &[XsdAttribute],
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    // First run the normal attribute validation (required, fixed, type checks)
    validate_attributes(doc, node, declared_attrs, schema, errors);

    // Then check for unknown attributes
    let elem_name = doc.node_name(node).unwrap_or("<unknown>");
    let actual_attrs = doc.attributes(node);
    for attr in actual_attrs {
        // Skip xmlns namespace declarations
        // xmloxide stores xmlns:foo as prefix="xmlns", name="foo"
        // and the default namespace as name="xmlns"
        if attr.prefix.as_deref() == Some("xmlns") || attr.name == "xmlns" {
            continue;
        }
        // Skip xsi:* attributes (standard XSI, not user schema)
        if attr.prefix.as_deref() == Some("xsi") {
            continue;
        }
        let is_declared = declared_attrs.iter().any(|d| d.name == attr.name);
        if !is_declared {
            errors.push(ValidationError {
                message: format!(
                    "attribute \"{}\" on element <{elem_name}> is not declared in the schema",
                    attr.name
                ),
                line: None,
                column: None,
            });
        }
    }
}

/// Strict complex element validation: validates content with strict any-wildcard handling.
fn validate_complex_element_strict(
    doc: &Document,
    node: NodeId,
    ct: &ComplexType,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    match &ct.content {
        ComplexContent::Empty => {
            validate_empty_content(
                doc,
                node,
                doc.node_name(node).unwrap_or("<unknown>"),
                ct.mixed,
                errors,
            );
        }
        ComplexContent::Sequence(p) => {
            let ce = collect_child_elements(doc, node);
            let owner_ns = doc.node_namespace(node).unwrap_or("");
            let _ = validate_sequence_strict(
                doc,
                &ce,
                p,
                doc.node_name(node).unwrap_or("<unknown>"),
                owner_ns,
                schema,
                errors,
                true,
            );
        }
        ComplexContent::Choice(p) => {
            let ce = collect_child_elements(doc, node);
            validate_choice(
                doc,
                &ce,
                p,
                doc.node_name(node).unwrap_or("<unknown>"),
                schema,
                errors,
            );
        }
        ComplexContent::All(p) => {
            let ce = collect_child_elements(doc, node);
            validate_all(
                doc,
                &ce,
                p,
                doc.node_name(node).unwrap_or("<unknown>"),
                schema,
                errors,
            );
        }
        ComplexContent::SimpleContent { base } => {
            let text = doc.text_content(node);
            if let Some(XsdType::Simple(st)) = schema.types.get(base.as_str()) {
                validate_simple_value(
                    &text,
                    st,
                    doc.node_name(node).unwrap_or("<unknown>"),
                    schema,
                    errors,
                );
            }
        }
    }
}

/// Resolve effective declaration for substitution-group matches.
fn resolve_substitution_member_decl<'a>(
    doc: &Document,
    child: NodeId,
    decl: &'a XsdElement,
    schema: &'a XsdSchema,
) -> &'a XsdElement {
    let child_name = doc.node_name(child).unwrap_or("");
    if child_name == decl.name {
        return decl;
    }
    schema
        .elements
        .get(child_name)
        .or_else(|| find_root_element_in_imports(child_name, schema))
        .unwrap_or(decl)
}

/// Strict sequence validation: uses strict any-wildcard validation.
fn validate_sequence_strict(
    doc: &Document,
    children: &[NodeId],
    particles: &[XsdParticle],
    parent_name: &str,
    owner_ns: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
    report_unexpected: bool,
) -> usize {
    let mut idx = 0;
    let mut pidx = 0;

    while idx < children.len() && pidx < particles.len() {
        match &particles[pidx] {
            XsdParticle::Element(decl) => {
                if element_matches_decl(doc, children[idx], decl, schema) {
                    let effective =
                        resolve_substitution_member_decl(doc, children[idx], decl, schema);
                    validate_element_strict(doc, children[idx], effective, schema, errors);
                    // Deep validation: if the element has no declared type
                    // (e.g. wfs:member), validate its children against schema
                    // declarations that match by local name.
                    validate_children_by_schema_lookup(
                        doc, children[idx], schema, errors,
                    );
                    idx += 1;
                    handle_repeat_occurrences_strict(
                        doc, children, &mut idx, decl, schema, errors,
                    );
                    pidx += 1;
                } else {
                    let child = children[idx];
                    if let Some(later_offset) =
                        find_later_match(doc, child, &particles[pidx + 1..], schema)
                    {
                        let target_pidx = pidx + 1 + later_offset;
                        let mut can_skip = true;
                        for sp in pidx..target_pidx {
                            if let XsdParticle::Element(sd) = &particles[sp] {
                                if sd.min_occurs > 0 {
                                    can_skip = false;
                                    break;
                                }
                            }
                        }
                        if can_skip {
                            pidx = target_pidx;
                        } else {
                            let child_name = doc.node_name(child).unwrap_or("<unknown>");
                            errors.push(ValidationError {
                                message: format!(
                                    "cvc-complex-type.2.4.a: element <{child_name}> was found beginning at <{parent_name}>, \"{expected}\" is expected",
                                    expected = describe_expected_sequence_strict(
                                        particles, pidx, schema,
                                    ),
                                ),
                                line: None,
                                column: None,
                            });
                            idx += 1;
                        }
                    } else if report_unexpected {
                        if decl.min_occurs > 0 {
                            errors.push(ValidationError {
                                message: format!(
                                    "element <{}> requires at least {} occurrence(s) of <{}>, found 0",
                                    parent_name,
                                    decl.min_occurs,
                                    decl.element_ref.as_deref().unwrap_or(&decl.name)
                                ),
                                line: None,
                                column: None,
                            });
                        }
                        let child_name = doc.node_name(child).unwrap_or("<unknown>");
                        errors.push(ValidationError {
                            message: format!(
                                "unexpected element <{child_name}> in <{parent_name}>; not expected by the content model"
                            ),
                            line: None,
                            column: None,
                        });
                        idx += 1;
                    } else {
                        break;
                    }
                }
            }
            XsdParticle::Group(content) => {
                let consumed = validate_group_content_strict(
                    doc,
                    &children[idx..],
                    content,
                    parent_name,
                    owner_ns,
                    schema,
                    errors,
                );
                idx += consumed;
                pidx += 1;
            }
            XsdParticle::Any(any) => {
                let consumed = validate_any_wildcard_strict(
                    doc,
                    &children[idx..],
                    any,
                    parent_name,
                    owner_ns,
                    schema,
                    errors,
                );
                idx += consumed;
                pidx += 1;
            }
        }
    }

    if report_unexpected {
        while idx < children.len() {
            let unexpected = doc.node_name(children[idx]).unwrap_or("<unknown>");
            errors.push(ValidationError {
                message: format!("unexpected element <{unexpected}> in <{parent_name}>; not expected by the content model"),
                line: None,
                column: None,
            });
            idx += 1;
        }
    }
    idx
}

fn validate_group_content_strict(
    doc: &Document,
    children: &[NodeId],
    content: &ComplexContent,
    parent_name: &str,
    owner_ns: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) -> usize {
    match content {
        ComplexContent::Sequence(particles) => validate_sequence_strict(
            doc,
            children,
            particles,
            parent_name,
            owner_ns,
            schema,
            errors,
            false,
        ),
        ComplexContent::Choice(particles) => {
            validate_choice(doc, children, particles, parent_name, schema, errors);
            children.len()
        }
        ComplexContent::All(particles) => {
            validate_all(doc, children, particles, parent_name, schema, errors);
            children.len()
        }
        _ => 0,
    }
}

/// Strict `<xsd:any>` wildcard validation.
///
/// Unlike the lax version, this actually attempts to resolve element
/// declarations for `processContents="strict"` and reports errors when
/// elements cannot be validated.
fn validate_any_wildcard_strict(
    doc: &Document,
    children: &[NodeId],
    any: &XsdAny,
    parent_name: &str,
    owner_ns: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) -> usize {
    let target_ns = if owner_ns.is_empty() {
        schema.target_namespace.as_deref().unwrap_or("")
    } else {
        owner_ns
    };
    let mut count: usize = 0;

    for &child in children {
        let child_ns = doc.node_namespace(child).unwrap_or("");
        let matches_ns = match &any.namespace {
            XsdAnyNamespace::Any => true,
            XsdAnyNamespace::Other => child_ns != target_ns,
            XsdAnyNamespace::List(ns_list) => {
                ns_list.iter().any(|ns| child_ns == ns.as_str())
                    || (ns_list.iter().any(|ns| ns == "##targetNamespace") && child_ns == target_ns)
                    || (ns_list.iter().any(|ns| ns == "##local") && child_ns.is_empty())
            }
        };

        if !matches_ns {
            break;
        }

        if let MaxOccurs::Bounded(max) = any.max_occurs {
            if count >= max as usize {
                break;
            }
        }

        let child_name = doc.node_name(child).unwrap_or("");
        match any.process_contents {
            XsdProcessContents::Skip => {
                // Accept without validation
            }
            XsdProcessContents::Lax => {
                // Validate if declaration found, accept otherwise
                if let Some(decl) = schema.elements.get(child_name).cloned() {
                    validate_element_strict(doc, child, &decl, schema, errors);
                } else if let Some(decl) = find_root_element_in_imports(child_name, schema).cloned()
                {
                    validate_element_strict(doc, child, &decl, schema, errors);
                }
            }
            XsdProcessContents::Strict => {
                // Must validate — try to find the element declaration
                if let Some(decl) = schema.elements.get(child_name).cloned() {
                    validate_element_strict(doc, child, &decl, schema, errors);
                } else if let Some(decl) = find_root_element_in_imports(child_name, schema) {
                    validate_element_strict(doc, child, decl, schema, errors);
                } else {
                    errors.push(ValidationError {
                        message: format!(
                            "element <{child_name}> in <{parent_name}> matched xsd:any wildcard but has no declaration in the schema (processContents=strict)"
                        ),
                        line: None,
                        column: None,
                    });
                }
            }
        }

        count += 1;
    }

    if count < any.min_occurs as usize {
        errors.push(ValidationError {
            message: format!(
                "element <{parent_name}> requires at least {} wildcard element(s), found {count}",
                any.min_occurs
            ),
            line: None,
            column: None,
        });
    }

    count
}

/// Searches imported schemas for a global element declaration.
///
/// This handles cases where the root element is declared in an imported
/// schema (e.g., `AX_Bestandsdatenauszug` in `NAS-Operationen.xsd`
/// imported by `AAA-Basisschema.xsd`).
fn find_root_element_in_imports<'a>(
    root_name: &str,
    schema: &'a XsdSchema,
) -> Option<&'a XsdElement> {
    for imported in schema.imported_namespaces.values() {
        if let Some(decl) = imported.elements.get(root_name) {
            return Some(decl);
        }
    }
    None
}

/// Validates a single element against its declaration.
fn validate_element(
    doc: &Document,
    node: NodeId,
    decl: &XsdElement,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    match resolve_element_type(decl, schema) {
        Some(XsdType::Complex(ct)) => validate_complex_element(doc, node, ct, schema, errors),
        Some(XsdType::Simple(st)) => validate_simple_element(doc, node, st, schema, errors),
        None => {} // anyType
    }
}

/// Resolves the type for an element declaration, checking both local types
/// and imported namespaces for QName-prefixed type references.
///
/// For element references (`ref="cbc:ID"`), resolves the referenced global
/// element declaration and returns its type.
fn resolve_element_type<'a>(decl: &'a XsdElement, schema: &'a XsdSchema) -> Option<&'a XsdType> {
    // Handle element ref — look up the referenced global element's type
    if let Some(ref ref_qname) = decl.element_ref {
        if let Some(ref_decl) = resolve_element_ref(ref_qname, schema) {
            return resolve_element_type(ref_decl, schema);
        }
        return None;
    }
    if let Some(ref inline) = decl.inline_type {
        return Some(inline);
    }
    if let Some(ref type_name) = decl.type_ref {
        return resolve_type_name(type_name, schema);
    }
    None
}

/// Resolves a type by name, checking local types first, then imported namespaces.
fn resolve_type_name<'a>(type_name: &str, schema: &'a XsdSchema) -> Option<&'a XsdType> {
    // Try local types first (handles unprefixed names and xs:-stripped names)
    if let Some(t) = schema.types.get(type_name) {
        return Some(t);
    }
    // Try namespace-aware resolution for prefixed type references
    let (ns, local) = resolve_type_qname(type_name, &schema.prefix_map);
    if let Some(ref ns_uri) = ns {
        if ns_uri == XSD_NAMESPACE {
            // Built-in XSD type — look up by local name
            return schema.types.get(&local);
        }
        // If the namespace is our own targetNamespace, look up locally
        if schema.target_namespace.as_deref() == Some(ns_uri.as_str()) {
            return schema.types.get(&local);
        }
        // Check imported namespaces
        if let Some(imported) = schema.imported_namespaces.get(ns_uri) {
            return imported.types.get(&local);
        }
    }
    // Last resort: try local name without namespace
    if let Some(t) = schema.types.get(&local) {
        return Some(t);
    }
    // Fallback for prefixed names where the prefix is not present in root
    // prefix_map: scan imported namespaces by local type name.
    for imported in schema.imported_namespaces.values() {
        if let Some(t) = imported.types.get(&local) {
            return Some(t);
        }
    }
    None
}

/// Resolves an element reference `QName` to its global element declaration.
///
/// Checks local elements first, then imported namespaces for prefixed refs.
fn resolve_element_ref<'a>(ref_qname: &str, schema: &'a XsdSchema) -> Option<&'a XsdElement> {
    // Unprefixed ref — look up in local elements first, then imported schemas.
    if !ref_qname.contains(':') {
        if let Some(decl) = schema.elements.get(ref_qname) {
            return Some(decl);
        }
        for imported in schema.imported_namespaces.values() {
            if let Some(decl) = imported.elements.get(ref_qname) {
                return Some(decl);
            }
        }
        return None;
    }
    // Prefixed ref — resolve namespace and look up
    let (ns, local) = resolve_type_qname(ref_qname, &schema.prefix_map);
    if let Some(ref ns_uri) = ns {
        // If the namespace is our own targetNamespace, look up locally
        if schema.target_namespace.as_deref() == Some(ns_uri.as_str()) {
            return schema.elements.get(&local);
        }
        if let Some(imported) = schema.imported_namespaces.get(ns_uri) {
            return imported.elements.get(&local);
        }
    }
    None
}

/// Validates an element with a complex type.
fn validate_complex_element(
    doc: &Document,
    node: NodeId,
    ct: &ComplexType,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let elem_name = doc.node_name(node).unwrap_or("<unknown>");
    validate_attributes(doc, node, &ct.attributes, schema, errors);
    match &ct.content {
        ComplexContent::Empty => validate_empty_content(doc, node, elem_name, ct.mixed, errors),
        ComplexContent::Sequence(p) => {
            let ce = collect_child_elements(doc, node);
            validate_sequence(doc, &ce, p, elem_name, schema, errors);
        }
        ComplexContent::Choice(p) => {
            let ce = collect_child_elements(doc, node);
            validate_choice(doc, &ce, p, elem_name, schema, errors);
        }
        ComplexContent::All(p) => {
            let ce = collect_child_elements(doc, node);
            validate_all(doc, &ce, p, elem_name, schema, errors);
        }
        ComplexContent::SimpleContent { base } => {
            let text = doc.text_content(node);
            if let Some(XsdType::Simple(st)) = schema.types.get(base.as_str()) {
                validate_simple_value(&text, st, elem_name, schema, errors);
            }
        }
    }
}

/// Validates empty content model constraints.
fn validate_empty_content(
    doc: &Document,
    node: NodeId,
    elem_name: &str,
    mixed: bool,
    errors: &mut Vec<ValidationError>,
) {
    let has_children = doc
        .children(node)
        .any(|c| matches!(doc.node(c).kind, NodeKind::Element { .. }));
    if has_children {
        errors.push(ValidationError {
            message: format!(
                "element <{elem_name}> has empty content model but contains child elements"
            ),
            line: None,
            column: None,
        });
    }
    if !mixed && !doc.text_content(node).trim().is_empty() {
        errors.push(ValidationError {
            message: format!(
                "element <{elem_name}> has empty content model but contains text content"
            ),
            line: None,
            column: None,
        });
    }
}

/// Collects child element `NodeId`s.
fn collect_child_elements(doc: &Document, node: NodeId) -> Vec<NodeId> {
    doc.children(node)
        .filter(|&c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
        .collect()
}

/// Validates a sequence content model.
fn validate_sequence(
    doc: &Document,
    children: &[NodeId],
    particles: &[XsdParticle],
    parent_name: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let mut idx = 0;
    for (particle_idx, particle) in particles.iter().enumerate() {
        match particle {
            XsdParticle::Element(decl) => {
                let consumed = validate_sequence_element(
                    doc,
                    &children[idx..],
                    decl,
                    parent_name,
                    schema,
                    errors,
                );
                idx += consumed;

                // If nothing was consumed and children remain, check if the
                // child matches a later particle. If it does, this optional
                // particle is simply skipped. If it doesn't match anything,
                // it's out-of-order or unexpected.
                if consumed == 0 && idx < children.len() {
                    let child = children[idx];
                    let matches_later =
                        matches_later_particle(doc, child, &particles[particle_idx + 1..], schema);
                    if !matches_later {
                        let child_name = doc.node_name(child).unwrap_or("<unknown>");
                        errors.push(ValidationError {
                            message: format!(
                                "unexpected element <{child_name}> in <{parent_name}>; not expected by the content model at this position"
                            ),
                            line: None,
                            column: None,
                        });
                        idx += 1; // Skip and continue
                    }
                }
            }
            XsdParticle::Group(content) => {
                let consumed = validate_group_content(
                    doc,
                    &children[idx..],
                    content,
                    parent_name,
                    schema,
                    errors,
                );
                idx += consumed;
            }
            XsdParticle::Any(any) => {
                let consumed =
                    validate_any_wildcard(doc, &children[idx..], any, parent_name, schema, errors);
                idx += consumed;
            }
        }
    }
    if idx < children.len() {
        let unexpected = doc.node_name(children[idx]).unwrap_or("<unknown>");
        errors.push(ValidationError {
            message: format!("unexpected element <{unexpected}> in <{parent_name}>; not expected by the content model"),
            line: None, column: None,
        });
    }
}

/// Consumes additional occurrences of a sequence element when maxOccurs > 1.
fn handle_repeat_occurrences_strict(
    doc: &Document,
    children: &[NodeId],
    idx: &mut usize,
    decl: &XsdElement,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    if let MaxOccurs::Bounded(max) = decl.max_occurs {
        for _ in 1..max {
            if *idx >= children.len()
                || !element_matches_decl(doc, children[*idx], decl, schema)
            {
                break;
            }
            let effective =
                resolve_substitution_member_decl(doc, children[*idx], decl, schema);
            validate_element_strict(doc, children[*idx], effective, schema, errors);
            *idx += 1;
        }
    } else {
        while *idx < children.len()
            && element_matches_decl(doc, children[*idx], decl, schema)
        {
            let effective =
                resolve_substitution_member_decl(doc, children[*idx], decl, schema);
            validate_element_strict(doc, children[*idx], effective, schema, errors);
            *idx += 1;
        }
    }
}

/// Returns the index of the first particle in `later_particles` that matches
/// `child`, or `None` if no later particle matches.
fn find_later_match(
    doc: &Document,
    child: NodeId,
    later_particles: &[XsdParticle],
    schema: &XsdSchema,
) -> Option<usize> {
    for (i, particle) in later_particles.iter().enumerate() {
        match particle {
            XsdParticle::Element(decl) => {
                if element_matches_decl(doc, child, decl, schema) {
                    return Some(i);
                }
            }
            XsdParticle::Group(content) => {
                if matches_later_group(doc, child, content, schema) {
                    return Some(i);
                }
            }
            XsdParticle::Any(_) => {
                return Some(i);
            }
        }
    }
    None
}

/// Builds a human-readable description of expected elements at a given
/// sequence position, used in cvc-complex-type.2.4.a error messages.
fn describe_expected_sequence_strict(
    particles: &[XsdParticle],
    from_idx: usize,
    _schema: &XsdSchema,
) -> String {
    let mut names = Vec::new();
    for p in particles.iter().skip(from_idx).take(8) {
        match p {
            XsdParticle::Element(decl) => {
                let n = decl.element_ref.as_deref().unwrap_or(&decl.name);
                if names.len() >= 6 {
                    names.push("...".to_string());
                    break;
                }
                names.push(n.to_string());
            }
            XsdParticle::Group(_) => {
                if names.len() >= 6 {
                    names.push("...".to_string());
                    break;
                }
                names.push("(group)".to_string());
            }
            XsdParticle::Any(_) => {
                if names.len() >= 6 {
                    names.push("...".to_string());
                    break;
                }
                names.push("(any)".to_string());
            }
        }
    }
    names.join(", ")
}

/// Checks if a child element matches any particle in later positions of a sequence.
fn matches_later_particle(
    doc: &Document,
    child: NodeId,
    later_particles: &[XsdParticle],
    schema: &XsdSchema,
) -> bool {
    for particle in later_particles {
        match particle {
            XsdParticle::Element(decl) => {
                if element_matches_decl(doc, child, decl, schema) {
                    return true;
                }
            }
            XsdParticle::Group(content) => {
                if matches_later_group(doc, child, content, schema) {
                    return true;
                }
            }
            XsdParticle::Any(_) => {
                return true;
            }
        }
    }
    false
}

fn matches_later_group(
    doc: &Document,
    child: NodeId,
    content: &ComplexContent,
    schema: &XsdSchema,
) -> bool {
    match content {
        ComplexContent::Empty | ComplexContent::SimpleContent { .. } => false,
        ComplexContent::Sequence(particles) | ComplexContent::All(particles) => {
            matches_later_particle(doc, child, particles, schema)
        }
        ComplexContent::Choice(particles) => {
            for particle in particles {
                match particle {
                    XsdParticle::Element(decl) => {
                        if element_matches_decl(doc, child, decl, schema) {
                            return true;
                        }
                    }
                    XsdParticle::Group(c) => {
                        if matches_later_group(doc, child, c, schema) {
                            return true;
                        }
                    }
                    XsdParticle::Any(_) => {
                        return true;
                    }
                }
            }
            false
        }
    }
}

/// Validates a single element particle in a sequence, returning number consumed.
/// Checks if an instance element matches a schema element declaration,
/// accounting for `elementFormDefault` and element-level `form` attributes.
///
/// When qualified form is in effect, the element must have the schema's
/// target namespace. When unqualified, the element is matched by local
/// name only (no namespace required).
fn element_matches_decl(
    doc: &Document,
    node: NodeId,
    decl: &XsdElement,
    schema: &XsdSchema,
) -> bool {
    let child_name = doc.node_name(node).unwrap_or("");
    let child_ns = doc.node_namespace(node).unwrap_or("");

    if child_name != decl.name {
        // Check substitution groups: if the instance element is a member
        // of the substitution group headed by `decl`, it is a valid substitute.
        if is_substitution_member(child_name, decl, schema) {
            return true;
        }
        return false;
    }
    // Local names match. Verify namespace compatibility.
    //
    // If the declaration is an element ref (e.g., ref="wfs:FeatureCollection"),
    // resolve the referenced element and check its namespace. The child
    // element's namespace must match the referenced element's namespace,
    // not the main schema's targetNamespace.
    let expected_ns: Option<String> = if let Some(ref ref_qname) = decl.element_ref {
        // Resolve the ref to find which namespace the element lives in
        if let Some(_ref_elem) = resolve_element_ref(ref_qname, schema) {
            // The ref might point to an imported namespace — find it
            resolve_element_namespace(ref_qname, schema)
        } else {
            schema.target_namespace.clone()
        }
    } else {
        // For direct element declarations, use the main schema's targetNamespace
        // when elementFormDefault=qualified
        if schema.element_form_default == FormDefault::Qualified {
            schema.target_namespace.clone()
        } else {
            None // No namespace enforcement
        }
    };

    match expected_ns {
        Some(ref ns) => {
            // When an element_ref points to an imported namespace but the
            // child element has no namespace prefix (unqualified XML), accept it.
            // This handles XSD patterns where imported elements are used
            // without namespace qualification.
            if child_ns.is_empty() && decl.element_ref.is_some() {
                return true;
            }
            // For local element declarations (no ref), allow if the child
            // namespace matches any imported schema's namespace.
            // Local elements inherit their namespace from the type's schema.
            if decl.element_ref.is_none() {
                let imported_ns_match = schema
                    .imported_namespaces
                    .keys()
                    .any(|imp_ns| child_ns == imp_ns.as_str());
                let main_ns_match = child_ns == ns.as_str();
                return imported_ns_match || main_ns_match;
            }
            child_ns == ns.as_str()
        }
        None => true,
    }
}

/// Resolves the namespace URI for an element referenced by `QName`.
fn resolve_element_namespace(ref_qname: &str, schema: &XsdSchema) -> Option<String> {
    let Some((ns_prefix, local)) = ref_qname.split_once(':') else {
        return schema.target_namespace.clone();
    };
    // Look up prefix in the main schema's prefix map
    if let Some(ns_uri) = schema.prefix_map.get(ns_prefix) {
        return Some(ns_uri.clone());
    }
    // Fallback: check prefix maps of imported schemas (includes that
    // declare xmlns bindings not present in the root schema document).
    for imported in schema.imported_namespaces.values() {
        if let Some(ns_uri) = imported.prefix_map.get(ns_prefix) {
            return Some(ns_uri.clone());
        }
    }
    // Last resort: infer namespace from imported schemas that declare the element.
    for (ns_uri, imported) in &schema.imported_namespaces {
        if imported.elements.contains_key(local) {
            return Some(ns_uri.clone());
        }
    }
    schema.target_namespace.clone()
}

/// Checks whether `child_name` is a member of the substitution group
/// headed by `decl` (directly or transitively).
///
/// XSD 1.0 section 3.3.6: if element B declares `substitutionGroup="A"`,
/// then B can appear anywhere A is expected. This is transitive: if
/// C declares `substitutionGroup="B"`, C can also substitute for A.
fn is_substitution_member(child_name: &str, decl: &XsdElement, schema: &XsdSchema) -> bool {
    // Direct members of the declaration's substitution group
    if let Some(members) = schema.substitution_groups.get(&decl.name) {
        if members.iter().any(|m| m == child_name) {
            return true;
        }
        // Transitive: check if any member itself has substitution members.
        // Look up member declarations in both local and imported elements.
        for member in members {
            let member_decl = schema.elements.get(member).or_else(|| {
                schema
                    .imported_namespaces
                    .values()
                    .find_map(|imp| imp.elements.get(member))
            });
            if let Some(member_decl) = member_decl {
                if is_substitution_member(child_name, member_decl, schema) {
                    return true;
                }
            }
        }
    }
    false
}

fn validate_sequence_element(
    doc: &Document,
    children: &[NodeId],
    decl: &XsdElement,
    parent_name: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) -> usize {
    let mut count: u32 = 0;
    let mut consumed = 0;
    for &child in children {
        if !element_matches_decl(doc, child, decl, schema) {
            break;
        }
        if let MaxOccurs::Bounded(max) = decl.max_occurs {
            if count >= max {
                break;
            }
        }
        // Resolve the actual element declaration for validation.
        // When substitution groups are involved, the instance element may
        // differ from the schema declaration; we need the instance element's
        // own type for correct content validation.
        let child_name = doc.node_name(child).unwrap_or("");
        let effective_decl = if child_name == decl.name {
            decl
        } else {
            schema
                .elements
                .get(child_name)
                .map(|d| d as &XsdElement)
                .or_else(|| find_root_element_in_imports(child_name, schema))
                .unwrap_or(decl)
        };
        validate_element(doc, child, effective_decl, schema, errors);
        count += 1;
        consumed += 1;
    }
    if count < decl.min_occurs {
        errors.push(ValidationError {
            message: format!(
                "element <{parent_name}> requires at least {} occurrence(s) of <{}>, found {count}",
                decl.min_occurs, decl.name
            ),
            line: None,
            column: None,
        });
    }
    consumed
}

/// Validates a nested group content model, returning children consumed.
fn validate_group_content(
    doc: &Document,
    children: &[NodeId],
    content: &ComplexContent,
    parent_name: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) -> usize {
    match content {
        ComplexContent::Sequence(particles) => {
            let before = errors.len();
            validate_sequence(doc, children, particles, parent_name, schema, errors);
            if errors.len() == before {
                children.len()
            } else {
                0
            }
        }
        ComplexContent::Choice(particles) => {
            validate_choice(doc, children, particles, parent_name, schema, errors);
            usize::from(!children.is_empty())
        }
        _ => 0,
    }
}

/// Validates `<xsd:any>` wildcard: consumes child elements that match
/// the namespace constraint.
fn validate_any_wildcard(
    doc: &Document,
    children: &[NodeId],
    any: &XsdAny,
    parent_name: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) -> usize {
    let target_ns = schema.target_namespace.as_deref().unwrap_or("");
    let mut count: u32 = 0;
    let mut consumed = 0;

    for &child in children {
        let child_ns = doc.node_namespace(child).unwrap_or("");
        let matches_ns = match &any.namespace {
            XsdAnyNamespace::Any => true,
            XsdAnyNamespace::Other => child_ns != target_ns,
            XsdAnyNamespace::List(ns_list) => {
                ns_list.iter().any(|ns| child_ns == ns.as_str())
                    || (ns_list.iter().any(|ns| ns == "##targetNamespace") && child_ns == target_ns)
                    || (ns_list.iter().any(|ns| ns == "##local") && child_ns.is_empty())
            }
        };

        if !matches_ns {
            break;
        }

        if let MaxOccurs::Bounded(max) = any.max_occurs {
            if count >= max {
                break;
            }
        }

        // For lax/skip: just accept the element without validation
        // For strict: we would need to resolve the element's type,
        // but for now accept it (strict validation of xsd:any is
        // complex and requires cross-schema element resolution)
        match any.process_contents {
            XsdProcessContents::Skip | XsdProcessContents::Lax | XsdProcessContents::Strict => {
                // Accept without validation
            }
        }

        count += 1;
        consumed += 1;
    }

    if count < any.min_occurs {
        errors.push(ValidationError {
            message: format!(
                "element <{parent_name}> requires at least {} wildcard element(s), found {count}",
                any.min_occurs
            ),
            line: None,
            column: None,
        });
    }

    consumed
}

/// Validates a choice content model.
fn validate_choice(
    doc: &Document,
    children: &[NodeId],
    particles: &[XsdParticle],
    parent_name: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    if children.is_empty() {
        let any_optional = particles
            .iter()
            .any(|p| matches!(p, XsdParticle::Element(d) if d.min_occurs == 0));
        if !any_optional {
            errors.push(ValidationError {
                message: format!("element <{parent_name}> requires one of the choice alternatives but has no child elements"),
                line: None, column: None,
            });
        }
        return;
    }
    let first = children[0];
    let first_name = doc.node_name(first).unwrap_or("");
    let matched = particles.iter().any(|p| {
        match p {
            XsdParticle::Element(decl) => {
                if element_matches_decl(doc, first, decl, schema) {
                    validate_element(doc, first, decl, schema, errors);
                    return true;
                }
                false
            }
            XsdParticle::Any(_) => {
                // Wildcard matches any element — accept
                true
            }
            XsdParticle::Group(ct) => {
                // Try to match the first child against the group's
                // content model (handles sequences/choices nested in choice)
                match ct {
                    ComplexContent::Sequence(seq_particles) => {
                        if let Some(XsdParticle::Element(decl)) = seq_particles.first() {
                            if element_matches_decl(doc, first, decl, schema) {
                                // Validate the entire sequence against children
                                validate_sequence(
                                    doc,
                                    children,
                                    seq_particles,
                                    parent_name,
                                    schema,
                                    errors,
                                );
                                return true;
                            }
                        }
                    }
                    ComplexContent::Choice(choice_particles) => {
                        // Recurse: try to match child against choice alternatives
                        let mut sub_errors = Vec::new();
                        validate_choice(
                            doc,
                            children,
                            choice_particles,
                            parent_name,
                            schema,
                            &mut sub_errors,
                        );
                        if sub_errors.is_empty() {
                            return true;
                        }
                    }
                    _ => {}
                }
                false
            }
        }
    });
    if !matched {
        let choices: Vec<&str> = particles
            .iter()
            .filter_map(|p| {
                if let XsdParticle::Element(d) = p {
                    Some(d.name.as_str())
                } else {
                    None
                }
            })
            .collect();
        errors.push(ValidationError {
            message: format!("element <{first_name}> in <{parent_name}> does not match any choice alternative; expected one of: {}", choices.join(", ")),
            line: None, column: None,
        });
    }
}

/// Validates an `all` content model.
fn validate_all(
    doc: &Document,
    children: &[NodeId],
    particles: &[XsdParticle],
    parent_name: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let mut seen: HashMap<&str, u32> = HashMap::new();
    for &child in children {
        let child_name = doc.node_name(child).unwrap_or("");
        let matching = particles.iter().find(
            |p| matches!(p, XsdParticle::Element(d) if element_matches_decl(doc, child, d, schema)),
        );
        if let Some(XsdParticle::Element(decl)) = matching {
            let count = seen.entry(child_name).or_insert(0);
            *count += 1;
            if let MaxOccurs::Bounded(max) = decl.max_occurs {
                if *count > max {
                    errors.push(ValidationError {
                        message: format!("element <{child_name}> in <{parent_name}> appears more than {max} time(s) in all group"),
                        line: None, column: None,
                    });
                }
            }
            validate_element(doc, child, decl, schema, errors);
        } else {
            errors.push(ValidationError {
                message: format!("unexpected element <{child_name}> in <{parent_name}>; not declared in the all group"),
                line: None, column: None,
            });
        }
    }
    for particle in particles {
        if let XsdParticle::Element(decl) = particle {
            let count = seen.get(decl.name.as_str()).copied().unwrap_or(0);
            if count < decl.min_occurs {
                errors.push(ValidationError {
                    message: format!("element <{parent_name}> requires at least {} occurrence(s) of <{}> in the all group, found {count}", decl.min_occurs, decl.name),
                    line: None, column: None,
                });
            }
        }
    }
}

/// Validates an element with a simple type.
fn validate_simple_element(
    doc: &Document,
    node: NodeId,
    st: &SimpleType,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let elem_name = doc.node_name(node).unwrap_or("<unknown>");
    if doc
        .children(node)
        .any(|c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
    {
        errors.push(ValidationError {
            message: format!("element <{elem_name}> has simple type but contains child elements"),
            line: None,
            column: None,
        });
        return;
    }
    validate_simple_value(&doc.text_content(node), st, elem_name, schema, errors);
}

/// Validates a string value against a simple type definition.
fn validate_simple_value(
    value: &str,
    st: &SimpleType,
    context: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    match &st.variety {
        SimpleTypeVariety::Builtin(name) => validate_builtin_value(value, name, context, errors),
        SimpleTypeVariety::Restriction { base, facets } => {
            if let Some(XsdType::Simple(bt)) = schema.types.get(base.as_str()) {
                validate_simple_value(value, bt, context, schema, errors);
            } else {
                validate_builtin_value(value, base, context, errors);
            }
            validate_facets(value, facets, context, errors);
        }
        SimpleTypeVariety::List { item_type } => {
            for item in value.split_whitespace() {
                if let Some(XsdType::Simple(ist)) = schema.types.get(item_type.as_str()) {
                    validate_simple_value(item, ist, context, schema, errors);
                } else {
                    validate_builtin_value(item, item_type, context, errors);
                }
            }
        }
        SimpleTypeVariety::Union { member_types } => {
            validate_union_value(value, member_types, context, schema, errors);
        }
    }
}

/// Validates a value against a union type.
fn validate_union_value(
    value: &str,
    member_types: &[String],
    context: &str,
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let mut any_valid = false;
    for mt in member_types {
        let mut trial = Vec::new();
        if let Some(XsdType::Simple(mst)) = schema.types.get(mt.as_str()) {
            validate_simple_value(value, mst, context, schema, &mut trial);
        } else {
            validate_builtin_value(value, mt, context, &mut trial);
        }
        if trial.is_empty() {
            any_valid = true;
            break;
        }
    }
    if !any_valid && !member_types.is_empty() {
        errors.push(ValidationError {
            message: format!(
                "value \"{value}\" in <{context}> does not match any member type of the union"
            ),
            line: None,
            column: None,
        });
    }
}

/// Validates a value against a built-in XSD type.
#[allow(clippy::too_many_lines)]
fn validate_builtin_value(
    value: &str,
    type_name: &str,
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    match type_name {
        "integer" | "long" | "int" | "short" | "byte" => {
            validate_signed_integer(value, type_name, context, errors);
        }
        "positiveInteger" => {
            validate_constrained_integer(value, context, "positiveInteger", |n| n > 0, errors);
        }
        "nonNegativeInteger" => {
            validate_constrained_integer(value, context, "nonNegativeInteger", |n| n >= 0, errors);
        }
        "negativeInteger" => {
            validate_constrained_integer(value, context, "negativeInteger", |n| n < 0, errors);
        }
        "nonPositiveInteger" => {
            validate_constrained_integer(value, context, "nonPositiveInteger", |n| n <= 0, errors);
        }
        "unsignedInt" | "unsignedLong" | "unsignedShort" | "unsignedByte" => {
            validate_unsigned_integer(value, type_name, context, errors);
        }
        "decimal" if parse_decimal(value).is_none() => {
            errors.push(ValidationError {
                message: format!("value \"{value}\" in <{context}> is not a valid decimal"),
                line: None,
                column: None,
            });
        }
        "float" | "double"
            if !matches!(value, "INF" | "-INF" | "NaN") && value.parse::<f64>().is_err() =>
        {
            errors.push(ValidationError {
                message: format!("value \"{value}\" in <{context}> is not a valid {type_name}"),
                line: None,
                column: None,
            });
        }
        "boolean" if !matches!(value, "true" | "false" | "1" | "0") => {
            errors.push(ValidationError {
                message: format!(
                    "value \"{value}\" in <{context}> is not a valid boolean (expected true, false, 1, or 0)"
                ),
                line: None,
                column: None,
            });
        }
        "date" if !is_valid_date_pattern(value) => {
            errors.push(ValidationError {
                message: format!(
                    "value \"{value}\" in <{context}> is not a valid date (expected YYYY-MM-DD)"
                ),
                line: None,
                column: None,
            });
        }
        "dateTime" if !is_valid_datetime_pattern(value) => {
            errors.push(ValidationError {
                message: format!("value \"{value}\" in <{context}> is not a valid dateTime"),
                line: None,
                column: None,
            });
        }
        "time" if !is_valid_time_pattern(value) => {
            errors.push(ValidationError {
                message: format!(
                    "value \"{value}\" in <{context}> is not a valid time (expected hh:mm:ss)"
                ),
                line: None,
                column: None,
            });
        }
        _ => {}
    }
}

/// Validates and range-checks a signed integer value.
fn validate_signed_integer(
    value: &str,
    type_name: &str,
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    if value.parse::<i64>().is_err() {
        errors.push(ValidationError {
            message: format!("value \"{value}\" in <{context}> is not a valid {type_name}"),
            line: None,
            column: None,
        });
        return;
    }
    check_integer_range(value, type_name, context, errors);
}

/// Validates a constrained integer (positive, negative, etc.).
fn validate_constrained_integer(
    value: &str,
    context: &str,
    type_name: &str,
    predicate: fn(i64) -> bool,
    errors: &mut Vec<ValidationError>,
) {
    match value.parse::<i64>() {
        Ok(n) if predicate(n) => {}
        _ => {
            errors.push(ValidationError {
                message: format!("value \"{value}\" in <{context}> is not a valid {type_name}"),
                line: None,
                column: None,
            });
        }
    }
}

/// Validates and range-checks an unsigned integer value.
fn validate_unsigned_integer(
    value: &str,
    type_name: &str,
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    if value.parse::<u64>().is_err() {
        errors.push(ValidationError {
            message: format!("value \"{value}\" in <{context}> is not a valid {type_name}"),
            line: None,
            column: None,
        });
        return;
    }
    check_unsigned_range(value, type_name, context, errors);
}

/// Checks range constraints for signed integer types.
fn check_integer_range(
    value: &str,
    type_name: &str,
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    let Ok(n) = value.parse::<i64>() else { return };
    let (min, max) = match type_name {
        "byte" => (i64::from(i8::MIN), i64::from(i8::MAX)),
        "short" => (i64::from(i16::MIN), i64::from(i16::MAX)),
        "int" => (i64::from(i32::MIN), i64::from(i32::MAX)),
        "long" => (i64::MIN, i64::MAX),
        _ => return,
    };
    if n < min || n > max {
        errors.push(ValidationError {
            message: format!(
                "value \"{value}\" in <{context}> is out of range for {type_name} ({min}..{max})"
            ),
            line: None,
            column: None,
        });
    }
}

/// Checks range constraints for unsigned integer types.
fn check_unsigned_range(
    value: &str,
    type_name: &str,
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    let Ok(n) = value.parse::<u64>() else { return };
    let max = match type_name {
        "unsignedByte" => u64::from(u8::MAX),
        "unsignedShort" => u64::from(u16::MAX),
        "unsignedInt" => u64::from(u32::MAX),
        "unsignedLong" => u64::MAX,
        _ => return,
    };
    if n > max {
        errors.push(ValidationError {
            message: format!(
                "value \"{value}\" in <{context}> is out of range for {type_name} (0..{max})"
            ),
            line: None,
            column: None,
        });
    }
}

/// Parses a decimal value.
fn parse_decimal(value: &str) -> Option<f64> {
    let trimmed = value.trim();
    if trimmed.is_empty() || trimmed.contains('e') || trimmed.contains('E') {
        return None;
    }
    trimmed.parse::<f64>().ok()
}

/// Basic validation for `xs:date` pattern.
fn is_valid_date_pattern(value: &str) -> bool {
    let date_part = strip_timezone(value);
    if let Some(without_sign) = date_part.strip_prefix('-') {
        let parts: Vec<&str> = without_sign.split('-').collect();
        return parts.len() == 3
            && parts[0].len() >= 4
            && parts.iter().all(|p| p.chars().all(|c| c.is_ascii_digit()));
    }
    let parts: Vec<&str> = date_part.split('-').collect();
    parts.len() == 3
        && parts[0].len() >= 4
        && parts[0].chars().all(|c| c.is_ascii_digit())
        && parts[1].len() == 2
        && parts[1].chars().all(|c| c.is_ascii_digit())
        && parts[2].len() == 2
        && parts[2].chars().all(|c| c.is_ascii_digit())
}

/// Basic validation for `xs:dateTime` pattern.
fn is_valid_datetime_pattern(value: &str) -> bool {
    let dt = strip_timezone(value);
    let Some((date, time)) = dt.split_once('T') else {
        return false;
    };
    is_valid_date_pattern(date) && is_valid_time_pattern(time)
}

/// Basic validation for `xs:time` pattern.
fn is_valid_time_pattern(value: &str) -> bool {
    let time_part = strip_timezone(value);
    let parts: Vec<&str> = time_part.split(':').collect();
    if parts.len() != 3 {
        return false;
    }
    let sec = parts[2].split('.').next().unwrap_or("");
    parts[0].len() == 2
        && parts[0].chars().all(|c| c.is_ascii_digit())
        && parts[1].len() == 2
        && parts[1].chars().all(|c| c.is_ascii_digit())
        && !sec.is_empty()
        && sec.chars().all(|c| c.is_ascii_digit())
}

/// Strips timezone suffix.
fn strip_timezone(value: &str) -> &str {
    if let Some(s) = value.strip_suffix('Z') {
        return s;
    }
    if value.len() > 6 {
        let tail = &value[value.len() - 6..];
        if (tail.starts_with('+') || tail.starts_with('-')) && tail.as_bytes().get(3) == Some(&b':')
        {
            return &value[..value.len() - 6];
        }
    }
    value
}

/// Applies whitespace normalization to a value according to the XSD `whiteSpace` facet.
///
/// See XSD 1.0 section 4.3.6:
/// - `Preserve`: no normalization
/// - `Replace`: replace `\t`, `\n`, `\r` with space
/// - `Collapse`: replace + collapse contiguous spaces + strip leading/trailing
fn apply_whitespace_normalization(value: &str, ws: &WhiteSpaceValue) -> String {
    match ws {
        WhiteSpaceValue::Preserve => value.to_string(),
        WhiteSpaceValue::Replace => value
            .chars()
            .map(|c| {
                if matches!(c, '\t' | '\n' | '\r') {
                    ' '
                } else {
                    c
                }
            })
            .collect(),
        WhiteSpaceValue::Collapse => {
            let replaced: String = value
                .chars()
                .map(|c| {
                    if matches!(c, '\t' | '\n' | '\r') {
                        ' '
                    } else {
                        c
                    }
                })
                .collect();
            replaced.split_whitespace().collect::<Vec<_>>().join(" ")
        }
    }
}

/// Validates facet constraints on a string value.
fn validate_facets(
    value: &str,
    facets: &[Facet],
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    // Find any WhiteSpace facet and normalize the value before checking other facets.
    let normalized;
    let effective_value = if let Some(ws) = facets.iter().find_map(|f| {
        if let Facet::WhiteSpace(ws) = f {
            Some(ws)
        } else {
            None
        }
    }) {
        normalized = apply_whitespace_normalization(value, ws);
        &normalized
    } else {
        value
    };

    for facet in facets {
        validate_single_facet(effective_value, facet, context, errors);
    }
}

/// Validates a single facet constraint.
#[allow(clippy::too_many_lines)]
fn validate_single_facet(
    value: &str,
    facet: &Facet,
    context: &str,
    errors: &mut Vec<ValidationError>,
) {
    match facet {
        Facet::MinLength(min) => {
            if value.len() < *min {
                errors.push(ValidationError {
                    message: format!(
                        "value in <{context}> has length {} but minLength is {min}",
                        value.len()
                    ),
                    line: None,
                    column: None,
                });
            }
        }
        Facet::MaxLength(max) => {
            if value.len() > *max {
                errors.push(ValidationError {
                    message: format!(
                        "value in <{context}> has length {} but maxLength is {max}",
                        value.len()
                    ),
                    line: None,
                    column: None,
                });
            }
        }
        Facet::Length(len) => {
            if value.len() != *len {
                errors.push(ValidationError {
                    message: format!(
                        "value in <{context}> has length {} but required length is {len}",
                        value.len()
                    ),
                    line: None,
                    column: None,
                });
            }
        }
        Facet::Pattern(pattern) => {
            if !matches_xsd_pattern(value, pattern) {
                errors.push(ValidationError {
                    message: format!(
                        "value \"{value}\" in <{context}> does not match pattern \"{pattern}\""
                    ),
                    line: None,
                    column: None,
                });
            }
        }
        Facet::Enumeration(allowed) => {
            if !allowed.iter().any(|a| a == value) {
                errors.push(ValidationError {
                    message: format!(
                        "value \"{value}\" in <{context}> is not in the enumeration: {}",
                        allowed.join(", ")
                    ),
                    line: None,
                    column: None,
                });
            }
        }
        Facet::MinInclusive(min) => {
            if let (Some(v), Some(m)) = (parse_decimal(value), parse_decimal(min)) {
                if v < m {
                    errors.push(ValidationError {
                        message: format!(
                            "value \"{value}\" in <{context}> is less than minInclusive {min}"
                        ),
                        line: None,
                        column: None,
                    });
                }
            }
        }
        Facet::MaxInclusive(max) => {
            if let (Some(v), Some(m)) = (parse_decimal(value), parse_decimal(max)) {
                if v > m {
                    errors.push(ValidationError {
                        message: format!(
                            "value \"{value}\" in <{context}> is greater than maxInclusive {max}"
                        ),
                        line: None,
                        column: None,
                    });
                }
            }
        }
        Facet::MinExclusive(min) => {
            if let (Some(v), Some(m)) = (parse_decimal(value), parse_decimal(min)) {
                if v <= m {
                    errors.push(ValidationError {
                        message: format!("value \"{value}\" in <{context}> must be greater than minExclusive {min}"),
                        line: None, column: None,
                    });
                }
            }
        }
        Facet::MaxExclusive(max) => {
            if let (Some(v), Some(m)) = (parse_decimal(value), parse_decimal(max)) {
                if v >= m {
                    errors.push(ValidationError {
                        message: format!(
                            "value \"{value}\" in <{context}> must be less than maxExclusive {max}"
                        ),
                        line: None,
                        column: None,
                    });
                }
            }
        }
        Facet::TotalDigits(total) => {
            let digits = count_total_digits(value);
            if digits > *total {
                errors.push(ValidationError {
                    message: format!("value \"{value}\" in <{context}> has {digits} total digits but totalDigits is {total}"),
                    line: None, column: None,
                });
            }
        }
        Facet::FractionDigits(frac) => {
            let digits = count_fraction_digits(value);
            if digits > *frac {
                errors.push(ValidationError {
                    message: format!("value \"{value}\" in <{context}> has {digits} fraction digits but fractionDigits is {frac}"),
                    line: None, column: None,
                });
            }
        }
        Facet::WhiteSpace(_) => {}
    }
}

/// Validates element attributes against the declared attribute list.
fn validate_attributes(
    doc: &Document,
    node: NodeId,
    declared_attrs: &[XsdAttribute],
    schema: &XsdSchema,
    errors: &mut Vec<ValidationError>,
) {
    let elem_name = doc.node_name(node).unwrap_or("<unknown>");
    let actual_attrs = doc.attributes(node);
    for decl in declared_attrs {
        let actual = actual_attrs.iter().find(|a| a.name == decl.name);
        if decl.required && actual.is_none() {
            errors.push(ValidationError {
                message: format!(
                    "required attribute \"{}\" missing on element <{elem_name}>",
                    decl.name
                ),
                line: None,
                column: None,
            });
            continue;
        }
        if let Some(attr) = actual {
            if let Some(ref fixed) = decl.fixed {
                if attr.value != *fixed {
                    errors.push(ValidationError {
                        message: format!("attribute \"{}\" on <{elem_name}> must have fixed value \"{fixed}\", found \"{}\"", decl.name, attr.value),
                        line: None, column: None,
                    });
                }
            }
            let attr_context = format!("{elem_name}/@{}", decl.name);
            if let Some(XsdType::Simple(st)) = schema.types.get(&decl.type_ref) {
                validate_simple_value(&attr.value, st, &attr_context, schema, errors);
            } else {
                validate_builtin_value(&attr.value, &decl.type_ref, &attr_context, errors);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Pattern matching
// ---------------------------------------------------------------------------

/// Simple XSD pattern matching using basic character class support.
fn matches_xsd_pattern(value: &str, pattern: &str) -> bool {
    match_pattern_chars(value.as_bytes(), pattern.as_bytes(), 0, 0)
}

/// Recursive pattern matcher.
fn match_pattern_chars(value: &[u8], pattern: &[u8], vi: usize, pi: usize) -> bool {
    if pi >= pattern.len() {
        return vi >= value.len();
    }
    let (cc, next_pi) = parse_pattern_element(pattern, pi);
    let has_star = next_pi < pattern.len() && pattern[next_pi] == b'*';
    let has_plus = next_pi < pattern.len() && pattern[next_pi] == b'+';
    let has_question = next_pi < pattern.len() && pattern[next_pi] == b'?';
    let aq = if has_star || has_plus || has_question {
        next_pi + 1
    } else {
        next_pi
    };

    if has_star {
        match_quantified(value, pattern, vi, aq, &cc, 0)
    } else if has_plus {
        match_quantified(value, pattern, vi, aq, &cc, 1)
    } else if has_question {
        if match_pattern_chars(value, pattern, vi, aq) {
            return true;
        }
        vi < value.len()
            && matches_char_class(value[vi], &cc)
            && match_pattern_chars(value, pattern, vi + 1, aq)
    } else {
        vi < value.len()
            && matches_char_class(value[vi], &cc)
            && match_pattern_chars(value, pattern, vi + 1, next_pi)
    }
}

/// Matches `min_count` or more occurrences of a character class.
fn match_quantified(
    value: &[u8],
    pattern: &[u8],
    vi: usize,
    aq: usize,
    cc: &CharClass,
    min_count: usize,
) -> bool {
    let mut i = vi;
    let mut count = 0;
    // Try zero matches first (for star)
    if min_count == 0 && match_pattern_chars(value, pattern, i, aq) {
        return true;
    }
    while i < value.len() && matches_char_class(value[i], cc) {
        i += 1;
        count += 1;
        if count >= min_count && match_pattern_chars(value, pattern, i, aq) {
            return true;
        }
    }
    false
}

/// A character class element in a pattern.
enum CharClass {
    Literal(u8),
    Dot,
    Digit,
    Word,
    CharSet(Vec<u8>),
    NegCharSet(Vec<u8>),
}

/// Parses one pattern element.
fn parse_pattern_element(pattern: &[u8], pi: usize) -> (CharClass, usize) {
    if pi >= pattern.len() {
        return (CharClass::Literal(0), pi);
    }
    match pattern[pi] {
        b'.' => (CharClass::Dot, pi + 1),
        b'\\' if pi + 1 < pattern.len() => match pattern[pi + 1] {
            b'd' => (CharClass::Digit, pi + 2),
            b'w' => (CharClass::Word, pi + 2),
            ch => (CharClass::Literal(ch), pi + 2),
        },
        b'[' => parse_char_set(pattern, pi),
        ch => (CharClass::Literal(ch), pi + 1),
    }
}

/// Parses a character set `[...]` or `[^...]`.
fn parse_char_set(pattern: &[u8], pi: usize) -> (CharClass, usize) {
    let negated = pi + 1 < pattern.len() && pattern[pi + 1] == b'^';
    let start = if negated { pi + 2 } else { pi + 1 };
    let mut end = start;
    while end < pattern.len() && pattern[end] != b']' {
        end += 1;
    }
    let chars = expand_char_ranges(&pattern[start..end]);
    let class = if negated {
        CharClass::NegCharSet(chars)
    } else {
        CharClass::CharSet(chars)
    };
    (class, if end < pattern.len() { end + 1 } else { end })
}

/// Expands character ranges like `a-z`.
fn expand_char_ranges(set: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < set.len() {
        if i + 2 < set.len() && set[i + 1] == b'-' {
            for ch in set[i]..=set[i + 2] {
                result.push(ch);
            }
            i += 3;
        } else {
            result.push(set[i]);
            i += 1;
        }
    }
    result
}

/// Tests whether a byte matches a character class.
fn matches_char_class(byte: u8, class: &CharClass) -> bool {
    match class {
        CharClass::Literal(ch) => byte == *ch,
        CharClass::Dot => true,
        CharClass::Digit => byte.is_ascii_digit(),
        CharClass::Word => byte.is_ascii_alphanumeric() || byte == b'_',
        CharClass::CharSet(chars) => chars.contains(&byte),
        CharClass::NegCharSet(chars) => !chars.contains(&byte),
    }
}

/// Counts total significant digits.
fn count_total_digits(value: &str) -> usize {
    value
        .trim()
        .trim_start_matches('-')
        .chars()
        .filter(char::is_ascii_digit)
        .count()
}

/// Counts fractional digits after the decimal point.
fn count_fraction_digits(value: &str) -> usize {
    value.find('.').map_or(0, |pos| {
        value[pos + 1..]
            .chars()
            .filter(char::is_ascii_digit)
            .count()
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::items_after_statements)]
mod tests {
    use super::*;

    fn make_schema(xsd: &str) -> XsdSchema {
        parse_xsd(xsd).unwrap()
    }

    fn validate(xsd: &str, xml: &str) -> ValidationResult {
        let schema = make_schema(xsd);
        let doc = Document::parse_str(xml).unwrap();
        validate_xsd(&doc, &schema)
    }

    #[test]
    fn test_parse_simple_schema_with_one_element() {
        let schema = make_schema(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="greeting" type="xs:string"/>
        </xs:schema>"#,
        );
        assert!(schema.elements.contains_key("greeting"));
        assert_eq!(
            schema.elements["greeting"].type_ref.as_deref(),
            Some("string")
        );
    }

    #[test]
    fn test_parse_schema_with_complex_type_and_sequence() {
        let schema = make_schema(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="person"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="age" type="xs:integer"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
        );
        let elem = &schema.elements["person"];
        if let Some(XsdType::Complex(ct)) = &elem.inline_type {
            if let ComplexContent::Sequence(p) = &ct.content {
                assert_eq!(p.len(), 2);
            } else {
                panic!("expected sequence");
            }
        } else {
            panic!("expected complex type");
        }
    }

    #[test]
    fn test_parse_schema_with_simple_type_restriction_enumeration() {
        let schema = make_schema(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:simpleType name="colorType"><xs:restriction base="xs:string">
                <xs:enumeration value="red"/><xs:enumeration value="green"/><xs:enumeration value="blue"/>
            </xs:restriction></xs:simpleType>
        </xs:schema>"#,
        );
        if let Some(XsdType::Simple(st)) = schema.types.get("colorType") {
            if let SimpleTypeVariety::Restriction { facets, .. } = &st.variety {
                assert!(facets.iter().any(|f| matches!(f, Facet::Enumeration(_))));
            } else {
                panic!("expected restriction");
            }
        } else {
            panic!("expected simple type");
        }
    }

    #[test]
    fn test_parse_schema_with_attributes() {
        let schema = make_schema(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="item"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
            </xs:sequence>
            <xs:attribute name="id" type="xs:integer" use="required"/>
            <xs:attribute name="category" type="xs:string"/>
            </xs:complexType></xs:element>
        </xs:schema>"#,
        );
        if let Some(XsdType::Complex(ct)) = &schema.elements["item"].inline_type {
            assert_eq!(ct.attributes.len(), 2);
            assert!(ct.attributes[0].required);
            assert!(!ct.attributes[1].required);
        } else {
            panic!("expected complex type");
        }
    }

    #[test]
    fn test_parse_schema_with_target_namespace() {
        let schema = make_schema(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com/ns">
            <xs:element name="root" type="xs:string"/>
        </xs:schema>"#,
        );
        assert_eq!(
            schema.target_namespace.as_deref(),
            Some("http://example.com/ns")
        );
    }

    #[test]
    fn test_parse_schema_with_nested_complex_types() {
        let schema = make_schema(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="order"><xs:complexType><xs:sequence>
                <xs:element name="item"><xs:complexType><xs:sequence>
                    <xs:element name="name" type="xs:string"/>
                    <xs:element name="qty" type="xs:integer"/>
                </xs:sequence></xs:complexType></xs:element>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
        );
        if let Some(XsdType::Complex(ct)) = &schema.elements["order"].inline_type {
            if let ComplexContent::Sequence(p) = &ct.content {
                if let XsdParticle::Element(item) = &p[0] {
                    assert_eq!(item.name, "item");
                    assert!(item.inline_type.is_some());
                } else {
                    panic!("expected element");
                }
            } else {
                panic!("expected sequence");
            }
        } else {
            panic!("expected complex type");
        }
    }

    #[test]
    fn test_validate_valid_document() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="greeting" type="xs:string"/>
        </xs:schema>"#,
            "<greeting>Hello World</greeting>",
        );
        assert!(r.is_valid, "errors: {:?}", r.errors);
    }

    #[test]
    fn test_validate_invalid_missing_required_element() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="person"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="age" type="xs:integer"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
            "<person><name>Alice</name></person>",
        );
        assert!(!r.is_valid);
        assert!(r.errors.iter().any(|e| e.message.contains("age")));
    }

    #[test]
    fn test_validate_invalid_wrong_order_sequence() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="person"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="age" type="xs:integer"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
            "<person><age>30</age><name>Alice</name></person>",
        );
        assert!(!r.is_valid);
    }

    #[test]
    fn test_validate_invalid_too_many_occurrences() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="root"><xs:complexType><xs:sequence>
                <xs:element name="item" type="xs:string" maxOccurs="2"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
            "<root><item>a</item><item>b</item><item>c</item></root>",
        );
        assert!(!r.is_valid);
        assert!(
            r.errors.iter().any(|e| e.message.contains("item")),
            "errors: {:?}",
            r.errors
        );
    }

    #[test]
    fn test_validate_invalid_missing_required_attribute() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="item"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
            </xs:sequence>
            <xs:attribute name="id" type="xs:integer" use="required"/>
            </xs:complexType></xs:element>
        </xs:schema>"#,
            "<item><name>Test</name></item>",
        );
        assert!(!r.is_valid);
        assert!(r
            .errors
            .iter()
            .any(|e| e.message.contains("required attribute")));
    }

    #[test]
    fn test_validate_invalid_wrong_attribute_type() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="item"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
            </xs:sequence>
            <xs:attribute name="count" type="xs:integer"/>
            </xs:complexType></xs:element>
        </xs:schema>"#,
            r#"<item count="abc"><name>Test</name></item>"#,
        );
        assert!(!r.is_valid);
        assert!(r.errors.iter().any(|e| e.message.contains("integer")));
    }

    #[test]
    fn test_validate_builtin_type_integer() {
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="c" type="xs:integer"/></xs:schema>"#,
                "<c>42</c>"
            )
            .is_valid
        );
        assert!(
            !validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="c" type="xs:integer"/></xs:schema>"#,
                "<c>abc</c>"
            )
            .is_valid
        );
    }

    #[test]
    fn test_validate_builtin_type_boolean() {
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="f" type="xs:boolean"/></xs:schema>"#,
                "<f>true</f>"
            )
            .is_valid
        );
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="f" type="xs:boolean"/></xs:schema>"#,
                "<f>0</f>"
            )
            .is_valid
        );
        assert!(
            !validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="f" type="xs:boolean"/></xs:schema>"#,
                "<f>yes</f>"
            )
            .is_valid
        );
    }

    #[test]
    fn test_validate_builtin_type_decimal() {
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="p" type="xs:decimal"/></xs:schema>"#,
                "<p>19.99</p>"
            )
            .is_valid
        );
        assert!(
            !validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="p" type="xs:decimal"/></xs:schema>"#,
                "<p>abc</p>"
            )
            .is_valid
        );
    }

    #[test]
    fn test_validate_string_facets_min_max_length() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:simpleType name="nameType"><xs:restriction base="xs:string">
                <xs:minLength value="2"/><xs:maxLength value="10"/>
            </xs:restriction></xs:simpleType>
            <xs:element name="name" type="nameType"/>
        </xs:schema>"#;
        assert!(validate(xsd, "<name>Alice</name>").is_valid);
        let short = validate(xsd, "<name>A</name>");
        assert!(!short.is_valid);
        assert!(short.errors.iter().any(|e| e.message.contains("minLength")));
        let long = validate(xsd, "<name>Alexandrina Rose</name>");
        assert!(!long.is_valid);
        assert!(long.errors.iter().any(|e| e.message.contains("maxLength")));
    }

    #[test]
    fn test_validate_string_facets_pattern() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:simpleType name="zipType"><xs:restriction base="xs:string">
                <xs:pattern value="\d\d\d\d\d"/>
            </xs:restriction></xs:simpleType>
            <xs:element name="zip" type="zipType"/>
        </xs:schema>"#;
        assert!(validate(xsd, "<zip>12345</zip>").is_valid);
        assert!(!validate(xsd, "<zip>1234</zip>").is_valid);
        assert!(!validate(xsd, "<zip>abcde</zip>").is_valid);
    }

    #[test]
    fn test_validate_numeric_facets_min_max_inclusive() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:simpleType name="ageType"><xs:restriction base="xs:integer">
                <xs:minInclusive value="0"/><xs:maxInclusive value="150"/>
            </xs:restriction></xs:simpleType>
            <xs:element name="age" type="ageType"/>
        </xs:schema>"#;
        assert!(validate(xsd, "<age>25</age>").is_valid);
        assert!(validate(xsd, "<age>0</age>").is_valid);
        assert!(!validate(xsd, "<age>-1</age>").is_valid);
        assert!(!validate(xsd, "<age>200</age>").is_valid);
    }

    #[test]
    fn test_validate_enumeration() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:simpleType name="colorType"><xs:restriction base="xs:string">
                <xs:enumeration value="red"/><xs:enumeration value="green"/><xs:enumeration value="blue"/>
            </xs:restriction></xs:simpleType>
            <xs:element name="color" type="colorType"/>
        </xs:schema>"#;
        assert!(validate(xsd, "<color>red</color>").is_valid);
        let r = validate(xsd, "<color>yellow</color>");
        assert!(!r.is_valid);
        assert!(r.errors.iter().any(|e| e.message.contains("enumeration")));
    }

    #[test]
    fn test_validate_mixed_content() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="para"><xs:complexType mixed="true"><xs:sequence>
                <xs:element name="b" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
            "<para>Hello <b>world</b> end</para>",
        );
        assert!(r.is_valid, "errors: {:?}", r.errors);
    }

    #[test]
    fn test_validate_choice_content_model() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="pet"><xs:complexType><xs:choice>
                <xs:element name="cat" type="xs:string"/>
                <xs:element name="dog" type="xs:string"/>
            </xs:choice></xs:complexType></xs:element>
        </xs:schema>"#;
        assert!(validate(xsd, "<pet><cat>Whiskers</cat></pet>").is_valid);
        assert!(validate(xsd, "<pet><dog>Rex</dog></pet>").is_valid);
        assert!(!validate(xsd, "<pet><fish>Nemo</fish></pet>").is_valid);
    }

    #[test]
    fn test_validate_optional_element() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="person"><xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="email" type="xs:string" minOccurs="0"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#;
        assert!(validate(xsd, "<person><name>Alice</name><email>a@b</email></person>").is_valid);
        let r = validate(xsd, "<person><name>Alice</name></person>");
        assert!(r.is_valid, "errors: {:?}", r.errors);
    }

    #[test]
    fn test_validate_unbounded_element() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="list"><xs:complexType><xs:sequence>
                <xs:element name="item" type="xs:string" maxOccurs="unbounded"/>
            </xs:sequence></xs:complexType></xs:element>
        </xs:schema>"#,
            "<list><item>a</item><item>b</item><item>c</item><item>d</item></list>",
        );
        assert!(r.is_valid, "errors: {:?}", r.errors);
    }

    #[test]
    fn test_validate_undeclared_root_element() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="root" type="xs:string"/>
        </xs:schema>"#,
            "<unknown>text</unknown>",
        );
        assert!(!r.is_valid);
        assert!(r.errors.iter().any(|e| e.message.contains("not declared")));
    }

    #[test]
    fn test_validate_empty_content_model() {
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="br"><xs:complexType/></xs:element>
        </xs:schema>"#,
                "<br/>"
            )
            .is_valid
        );
        assert!(
            !validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="br"><xs:complexType/></xs:element>
        </xs:schema>"#,
                "<br>text</br>"
            )
            .is_valid
        );
    }

    #[test]
    fn test_validate_fixed_attribute_value() {
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="item"><xs:complexType>
                <xs:attribute name="version" type="xs:string" fixed="1.0"/>
            </xs:complexType></xs:element>
        </xs:schema>"#,
                r#"<item version="1.0"/>"#
            )
            .is_valid
        );
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="item"><xs:complexType>
                <xs:attribute name="version" type="xs:string" fixed="1.0"/>
            </xs:complexType></xs:element>
        </xs:schema>"#,
            r#"<item version="2.0"/>"#,
        );
        assert!(!r.is_valid);
        assert!(r.errors.iter().any(|e| e.message.contains("fixed")));
    }

    #[test]
    fn test_validate_simple_content_extension() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:complexType name="priceType"><xs:simpleContent>
                <xs:extension base="xs:decimal">
                    <xs:attribute name="currency" type="xs:string" use="required"/>
                </xs:extension>
            </xs:simpleContent></xs:complexType>
            <xs:element name="price" type="priceType"/>
        </xs:schema>"#,
            r#"<price currency="USD">19.99</price>"#,
        );
        assert!(r.is_valid, "errors: {:?}", r.errors);
    }

    #[test]
    fn test_validate_date_types() {
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="d" type="xs:date"/></xs:schema>"#,
                "<d>2024-01-15</d>"
            )
            .is_valid
        );
        assert!(
            !validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="d" type="xs:date"/></xs:schema>"#,
                "<d>not-a-date</d>"
            )
            .is_valid
        );
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="dt" type="xs:dateTime"/></xs:schema>"#,
                "<dt>2024-01-15T10:30:00</dt>"
            )
            .is_valid
        );
        assert!(
            validate(
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="t" type="xs:time"/></xs:schema>"#,
                "<t>10:30:00</t>"
            )
            .is_valid
        );
    }

    #[test]
    fn test_validate_all_content_model() {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="config"><xs:complexType><xs:all>
                <xs:element name="host" type="xs:string"/>
                <xs:element name="port" type="xs:integer"/>
            </xs:all></xs:complexType></xs:element>
        </xs:schema>"#;
        assert!(
            validate(
                xsd,
                "<config><host>localhost</host><port>8080</port></config>"
            )
            .is_valid
        );
        assert!(
            validate(
                xsd,
                "<config><port>8080</port><host>localhost</host></config>"
            )
            .is_valid
        );
    }

    #[test]
    fn test_parse_xsd_invalid_xml() {
        assert!(parse_xsd("<not valid xml<<<").is_err());
    }

    #[test]
    fn test_parse_xsd_wrong_root_element() {
        assert!(
            parse_xsd(r#"<xs:element xmlns:xs="http://www.w3.org/2001/XMLSchema" name="x"/>"#)
                .is_err()
        );
    }

    #[test]
    fn test_validate_named_complex_type() {
        let r = validate(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:complexType name="addressType"><xs:sequence>
                <xs:element name="street" type="xs:string"/>
                <xs:element name="city" type="xs:string"/>
            </xs:sequence></xs:complexType>
            <xs:element name="address" type="addressType"/>
        </xs:schema>"#,
            "<address><street>123 Main St</street><city>Springfield</city></address>",
        );
        assert!(r.is_valid, "errors: {:?}", r.errors);
    }

    #[test]
    fn test_whitespace_preserve() {
        use super::apply_whitespace_normalization;
        use super::WhiteSpaceValue;
        let result = apply_whitespace_normalization("  hello\tworld\n", &WhiteSpaceValue::Preserve);
        assert_eq!(result, "  hello\tworld\n");
    }

    #[test]
    fn test_whitespace_replace() {
        use super::apply_whitespace_normalization;
        use super::WhiteSpaceValue;
        let result = apply_whitespace_normalization("a\tb\nc\r", &WhiteSpaceValue::Replace);
        assert_eq!(result, "a b c ");
    }

    #[test]
    fn test_whitespace_collapse() {
        use super::apply_whitespace_normalization;
        use super::WhiteSpaceValue;
        let result =
            apply_whitespace_normalization("  hello \t world \n ", &WhiteSpaceValue::Collapse);
        assert_eq!(result, "hello world");
    }

    // -----------------------------------------------------------------------
    // Phase 0: Prefix map and QName resolution infrastructure
    // -----------------------------------------------------------------------

    #[test]
    fn test_build_prefix_map() {
        let doc = Document::parse_str(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:tns="http://example.com/types"
                        targetNamespace="http://example.com/types">
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();
        let root = doc.root_element().unwrap();
        let map = build_prefix_map(&doc, root);
        assert_eq!(
            map.get("xs"),
            Some(&"http://www.w3.org/2001/XMLSchema".to_string())
        );
        assert_eq!(
            map.get("tns"),
            Some(&"http://example.com/types".to_string())
        );
    }

    #[test]
    fn test_resolve_type_qname_builtin() {
        let mut map = HashMap::new();
        map.insert(
            "xs".to_string(),
            "http://www.w3.org/2001/XMLSchema".to_string(),
        );
        let (ns, local) = resolve_type_qname("xs:string", &map);
        assert_eq!(ns.as_deref(), Some("http://www.w3.org/2001/XMLSchema"));
        assert_eq!(local, "string");
    }

    #[test]
    fn test_resolve_type_qname_local() {
        let mut map = HashMap::new();
        map.insert("tns".to_string(), "http://example.com/types".to_string());
        let (ns, local) = resolve_type_qname("tns:MyType", &map);
        assert_eq!(ns.as_deref(), Some("http://example.com/types"));
        assert_eq!(local, "MyType");
    }

    #[test]
    fn test_resolve_type_qname_unprefixed() {
        let map = HashMap::new();
        let (ns, local) = resolve_type_qname("MyType", &map);
        assert_eq!(ns, None);
        assert_eq!(local, "MyType");
    }

    // -----------------------------------------------------------------------
    // Phase 1: xsd:include tests
    // -----------------------------------------------------------------------

    fn make_resolver(schemas: Vec<(&str, &str)>) -> impl SchemaResolver {
        let map: HashMap<String, String> = schemas
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        move |location: &str, _base: Option<&str>| map.get(location).cloned()
    }

    #[test]
    fn test_include_ignored_without_resolver() {
        // Without a resolver, include is silently skipped (backward compat)
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="types.xsd"/>
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();
        assert!(schema.elements.contains_key("root"));
    }

    #[test]
    fn test_include_merges_types() {
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="PersonType"><xs:sequence>
                    <xs:element name="name" type="xs:string"/>
                </xs:sequence></xs:complexType>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="types.xsd"/>
                <xs:element name="person" type="PersonType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema.types.contains_key("PersonType"));
        assert!(schema.elements.contains_key("person"));
    }

    #[test]
    fn test_include_merges_elements() {
        let resolver = make_resolver(vec![(
            "elements.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="greeting" type="xs:string"/>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="elements.xsd"/>
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema.elements.contains_key("greeting"));
        assert!(schema.elements.contains_key("root"));
    }

    #[test]
    fn test_include_chameleon() {
        // Included schema has no targetNamespace — adopts includer's namespace
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="AddrType"><xs:sequence>
                    <xs:element name="street" type="xs:string"/>
                </xs:sequence></xs:complexType>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com/main">
                <xs:include schemaLocation="types.xsd"/>
                <xs:element name="addr" type="AddrType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        // The type should be merged into the main schema
        assert!(schema.types.contains_key("AddrType"));
    }

    #[test]
    fn test_include_namespace_mismatch_error() {
        let resolver = make_resolver(vec![(
            "other.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://other.com">
                <xs:element name="x" type="xs:string"/>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let result = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com">
                <xs:include schemaLocation="other.xsd"/>
            </xs:schema>"#,
            &opts,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("namespace"));
    }

    #[test]
    fn test_include_cycle_detection() {
        // A includes B, B includes A — should not loop
        let resolver = make_resolver(vec![
            (
                "a.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    <xs:include schemaLocation="b.xsd"/>
                    <xs:element name="a" type="xs:string"/>
                </xs:schema>"#,
            ),
            (
                "b.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    <xs:include schemaLocation="a.xsd"/>
                    <xs:element name="b" type="xs:string"/>
                </xs:schema>"#,
            ),
        ]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        // Parse from a.xsd content — should include b.xsd but not re-include a.xsd
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="a.xsd"/>
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema.elements.contains_key("root"));
        assert!(schema.elements.contains_key("a"));
        assert!(schema.elements.contains_key("b"));
    }

    #[test]
    fn test_include_transitive() {
        // A includes B, B includes C — declarations from C available in A
        let resolver = make_resolver(vec![
            (
                "b.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    <xs:include schemaLocation="c.xsd"/>
                    <xs:element name="b" type="xs:string"/>
                </xs:schema>"#,
            ),
            (
                "c.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    <xs:complexType name="CType"><xs:sequence>
                        <xs:element name="val" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:schema>"#,
            ),
        ]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="b.xsd"/>
                <xs:element name="root" type="CType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema.elements.contains_key("root"));
        assert!(schema.elements.contains_key("b"));
        assert!(schema.types.contains_key("CType"));
    }

    #[test]
    fn test_include_resolver_returns_none() {
        let resolver = make_resolver(vec![]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let result = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="nonexistent.xsd"/>
            </xs:schema>"#,
            &opts,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("nonexistent.xsd"));
    }

    // -----------------------------------------------------------------------
    // Phase 2: xsd:import tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_import_cross_namespace_type() {
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com/types">
                <xs:complexType name="AddressType"><xs:sequence>
                    <xs:element name="street" type="xs:string"/>
                    <xs:element name="city" type="xs:string"/>
                </xs:sequence></xs:complexType>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:tns="http://example.com/types"
                        targetNamespace="http://example.com/main">
                <xs:import namespace="http://example.com/types" schemaLocation="types.xsd"/>
                <xs:element name="address" type="tns:AddressType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        // The imported type should be resolvable
        assert!(schema
            .imported_namespaces
            .contains_key("http://example.com/types"));
        let imported = &schema.imported_namespaces["http://example.com/types"];
        assert!(imported.types.contains_key("AddressType"));
    }

    #[test]
    fn test_import_namespace_mismatch_error() {
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://wrong.com">
                <xs:element name="x" type="xs:string"/>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let result = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:import namespace="http://expected.com" schemaLocation="types.xsd"/>
            </xs:schema>"#,
            &opts,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("namespace"));
    }

    #[test]
    fn test_import_without_schema_location() {
        // Import with just namespace attribute is valid (declares expected ns)
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:import namespace="http://example.com/types"/>
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();
        assert!(schema.elements.contains_key("root"));
    }

    #[test]
    fn test_import_cycle_detection() {
        let resolver = make_resolver(vec![
            (
                "a.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                            targetNamespace="http://example.com/a">
                    <xs:import namespace="http://example.com/b" schemaLocation="b.xsd"/>
                    <xs:element name="a" type="xs:string"/>
                </xs:schema>"#,
            ),
            (
                "b.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                            targetNamespace="http://example.com/b">
                    <xs:import namespace="http://example.com/a" schemaLocation="a.xsd"/>
                    <xs:element name="b" type="xs:string"/>
                </xs:schema>"#,
            ),
        ]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com/main">
                <xs:import namespace="http://example.com/a" schemaLocation="a.xsd"/>
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema.elements.contains_key("root"));
        assert!(schema
            .imported_namespaces
            .contains_key("http://example.com/a"));
    }

    #[test]
    fn test_import_multiple_namespaces() {
        let resolver = make_resolver(vec![
            (
                "types.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                            targetNamespace="http://example.com/types">
                    <xs:complexType name="NameType"><xs:sequence>
                        <xs:element name="first" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:schema>"#,
            ),
            (
                "addr.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                            targetNamespace="http://example.com/addr">
                    <xs:complexType name="AddrType"><xs:sequence>
                        <xs:element name="city" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:schema>"#,
            ),
        ]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:t="http://example.com/types"
                        xmlns:a="http://example.com/addr">
                <xs:import namespace="http://example.com/types" schemaLocation="types.xsd"/>
                <xs:import namespace="http://example.com/addr" schemaLocation="addr.xsd"/>
                <xs:element name="root" type="xs:string"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema
            .imported_namespaces
            .contains_key("http://example.com/types"));
        assert!(schema
            .imported_namespaces
            .contains_key("http://example.com/addr"));
        assert!(schema.imported_namespaces["http://example.com/types"]
            .types
            .contains_key("NameType"));
        assert!(schema.imported_namespaces["http://example.com/addr"]
            .types
            .contains_key("AddrType"));
    }

    #[test]
    fn test_import_and_include_combined() {
        let resolver = make_resolver(vec![
            (
                "local_types.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    <xs:complexType name="LocalType"><xs:sequence>
                        <xs:element name="value" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:schema>"#,
            ),
            (
                "foreign.xsd",
                r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                            targetNamespace="http://foreign.com">
                    <xs:complexType name="ForeignType"><xs:sequence>
                        <xs:element name="data" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:schema>"#,
            ),
        ]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:f="http://foreign.com">
                <xs:include schemaLocation="local_types.xsd"/>
                <xs:import namespace="http://foreign.com" schemaLocation="foreign.xsd"/>
                <xs:element name="root" type="LocalType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();
        assert!(schema.types.contains_key("LocalType"));
        assert!(schema
            .imported_namespaces
            .contains_key("http://foreign.com"));
        assert!(schema.imported_namespaces["http://foreign.com"]
            .types
            .contains_key("ForeignType"));
    }

    // -----------------------------------------------------------------------
    // Phase 3: Namespace-aware validation tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_validate_with_imported_types() {
        // End-to-end: parse multi-schema, validate document
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com/types">
                <xs:complexType name="AddressType"><xs:sequence>
                    <xs:element name="street" type="xs:string"/>
                    <xs:element name="city" type="xs:string"/>
                </xs:sequence></xs:complexType>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:tns="http://example.com/types">
                <xs:import namespace="http://example.com/types" schemaLocation="types.xsd"/>
                <xs:element name="address" type="tns:AddressType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();

        let doc = Document::parse_str(
            "<address><street>123 Main</street><city>Springfield</city></address>",
        )
        .unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_imported_content_model() {
        // Validate that child elements typed from imported schemas validate
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com/types">
                <xs:complexType name="NameType"><xs:sequence>
                    <xs:element name="first" type="xs:string"/>
                    <xs:element name="last" type="xs:string"/>
                </xs:sequence></xs:complexType>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:t="http://example.com/types">
                <xs:import namespace="http://example.com/types" schemaLocation="types.xsd"/>
                <xs:element name="person"><xs:complexType><xs:sequence>
                    <xs:element name="name" type="t:NameType"/>
                    <xs:element name="age" type="xs:integer"/>
                </xs:sequence></xs:complexType></xs:element>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();

        // Valid document
        let doc = Document::parse_str(
            "<person><name><first>John</first><last>Doe</last></name><age>30</age></person>",
        )
        .unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        // Invalid document: wrong child element in imported type
        let doc = Document::parse_str(
            "<person><name><wrong>X</wrong><last>Doe</last></name><age>30</age></person>",
        )
        .unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_validate_included_type_validation() {
        // Validate that included types work in validation too
        let resolver = make_resolver(vec![(
            "types.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="ItemType"><xs:sequence>
                    <xs:element name="name" type="xs:string"/>
                    <xs:element name="qty" type="xs:integer"/>
                </xs:sequence></xs:complexType>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:include schemaLocation="types.xsd"/>
                <xs:element name="item" type="ItemType"/>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();

        let doc = Document::parse_str("<item><name>Widget</name><qty>5</qty></item>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    // -----------------------------------------------------------------------
    // Element ref support tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_element_ref_local() {
        // ref to a global element in the same schema
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="name" type="xs:string"/>
                <xs:element name="person">
                    <xs:complexType><xs:sequence>
                        <xs:element ref="name"/>
                    </xs:sequence></xs:complexType>
                </xs:element>
            </xs:schema>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<person><name>Alice</name></person>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_element_ref_imported() {
        // ref to a global element in an imported namespace (UBL pattern)
        let resolver = make_resolver(vec![(
            "components.xsd",
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="http://example.com/components">
                <xs:element name="ID" type="xs:string"/>
                <xs:element name="Name" type="xs:string"/>
            </xs:schema>"#,
        )]);
        let opts = XsdParseOptions {
            resolver: Some(&resolver),
            base_uri: None,
        };
        let schema = parse_xsd_with_options(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        xmlns:cbc="http://example.com/components">
                <xs:import namespace="http://example.com/components"
                           schemaLocation="components.xsd"/>
                <xs:element name="Order">
                    <xs:complexType><xs:sequence>
                        <xs:element ref="cbc:ID"/>
                        <xs:element ref="cbc:Name" minOccurs="0"/>
                    </xs:sequence></xs:complexType>
                </xs:element>
            </xs:schema>"#,
            &opts,
        )
        .unwrap();

        let doc = Document::parse_str("<Order><ID>ORD-1</ID><Name>Test</Name></Order>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        // Valid without optional Name
        let doc2 = Document::parse_str("<Order><ID>ORD-2</ID></Order>").unwrap();
        let result2 = validate_xsd(&doc2, &schema);
        assert!(result2.is_valid, "errors: {:?}", result2.errors);

        // Invalid: wrong element
        let doc3 = Document::parse_str("<Order><Wrong>X</Wrong></Order>").unwrap();
        let result3 = validate_xsd(&doc3, &schema);
        assert!(!result3.is_valid);
    }

    #[test]
    fn test_element_ref_with_occurs() {
        // ref with minOccurs/maxOccurs overrides
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="item" type="xs:string"/>
                <xs:element name="list">
                    <xs:complexType><xs:sequence>
                        <xs:element ref="item" minOccurs="1" maxOccurs="unbounded"/>
                    </xs:sequence></xs:complexType>
                </xs:element>
            </xs:schema>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<list><item>a</item><item>b</item></list>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        // Invalid: empty list (minOccurs=1)
        let doc2 = Document::parse_str("<list/>").unwrap();
        let result2 = validate_xsd(&doc2, &schema);
        assert!(!result2.is_valid);
    }

    #[test]
    fn test_element_form_default_qualified() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="urn:example"
                        xmlns:tns="urn:example"
                        elementFormDefault="qualified">
                <xs:element name="order">
                    <xs:complexType><xs:sequence>
                        <xs:element name="item" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:element>
            </xs:schema>"#,
        )
        .unwrap();
        assert_eq!(schema.element_form_default, FormDefault::Qualified);

        // Valid: child element is namespace-qualified
        let doc = Document::parse_str(r#"<order xmlns="urn:example"><item>Widget</item></order>"#)
            .unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "qualified children should pass: {:?}",
            result.errors
        );

        // Invalid: child element is NOT namespace-qualified
        let doc_fail = Document::parse_str(
            r#"<tns:order xmlns:tns="urn:example"><item>Widget</item></tns:order>"#,
        )
        .unwrap();
        let result = validate_xsd(&doc_fail, &schema);
        assert!(
            !result.is_valid,
            "unqualified child should fail when elementFormDefault=qualified"
        );
    }

    #[test]
    fn test_element_form_default_unqualified() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                        targetNamespace="urn:example"
                        xmlns:tns="urn:example">
                <xs:element name="order">
                    <xs:complexType><xs:sequence>
                        <xs:element name="item" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:element>
            </xs:schema>"#,
        )
        .unwrap();
        assert_eq!(schema.element_form_default, FormDefault::Unqualified);

        // Valid: child element without namespace (unqualified is default)
        let doc = Document::parse_str(
            r#"<tns:order xmlns:tns="urn:example"><item>Widget</item></tns:order>"#,
        )
        .unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "unqualified children should pass: {:?}",
            result.errors
        );
    }

    // ── Substitution group tests ──────────────────────────────────────────

    /// Schema with a substitution group: `dog` and `cat` substitute for `pet`.
    #[test]
    fn test_substitution_group_direct_member() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="pets" type="PetsType"/>
                <xs:complexType name="PetsType">
                    <xs:sequence>
                        <xs:element ref="pet" maxOccurs="unbounded"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:element name="pet" type="xs:string" abstract="true"/>
                <xs:element name="dog" substitutionGroup="pet" type="xs:string"/>
                <xs:element name="cat" substitutionGroup="pet" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();

        // "dog" should be accepted where "pet" is expected
        let doc = Document::parse_str(r"<pets><dog>Rex</dog><cat>Mimi</cat></pets>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "substitution members should be valid: {:?}",
            result.errors
        );
    }

    /// Schema with transitive substitution: `poodle → dog → pet`.
    #[test]
    fn test_substitution_group_transitive() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="kennel" type="KennelType"/>
                <xs:complexType name="KennelType">
                    <xs:sequence>
                        <xs:element ref="pet" maxOccurs="unbounded"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:element name="pet" type="xs:string" abstract="true"/>
                <xs:element name="dog" substitutionGroup="pet" type="xs:string"/>
                <xs:element name="poodle" substitutionGroup="dog" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();

        // "poodle" is a transitive substitute for "pet" (via "dog")
        let doc = Document::parse_str(r"<kennel><poodle>Fifi</poodle></kennel>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "transitive substitution should be valid: {:?}",
            result.errors
        );
    }

    /// Verify substitution group index is built correctly.
    #[test]
    fn test_substitution_group_index_populated() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root" type="RootType"/>
                <xs:complexType name="RootType">
                    <xs:sequence>
                        <xs:element ref="base"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:element name="base" type="xs:string"/>
                <xs:element name="derived1" substitutionGroup="base" type="xs:string"/>
                <xs:element name="derived2" substitutionGroup="base" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();

        // "derived1" and "derived2" should both substitute for "base"
        let doc1 = Document::parse_str(r"<root><derived1>hello</derived1></root>").unwrap();
        let doc2 = Document::parse_str(r"<root><derived2>world</derived2></root>").unwrap();
        assert!(validate_xsd(&doc1, &schema).is_valid);
        assert!(validate_xsd(&doc2, &schema).is_valid);
    }

    /// Element not in the substitution group should still be rejected.
    #[test]
    fn test_non_member_rejected() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root" type="RootType"/>
                <xs:complexType name="RootType">
                    <xs:sequence>
                        <xs:element ref="base"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:element name="base" type="xs:string"/>
                <xs:element name="derived" substitutionGroup="base" type="xs:string"/>
            </xs:schema>"#,
        )
        .unwrap();

        // "unknown" is NOT a substitution group member
        let doc = Document::parse_str(r"<root><unknown>oops</unknown></root>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(!result.is_valid, "non-member should be rejected");
    }

    #[test]
    fn test_complex_content_extension_simple() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root" type="DerivedType"/>
                <xs:complexType name="BaseType">
                    <xs:sequence>
                        <xs:element name="a" type="xs:string"/>
                        <xs:element name="b" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:complexType name="DerivedType">
                    <xs:complexContent>
                        <xs:extension base="BaseType">
                            <xs:sequence>
                                <xs:element name="c" type="xs:string"/>
                            </xs:sequence>
                        </xs:extension>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>"#,
        )
        .unwrap();

        // Correct order: a, b (base), then c (extension)
        let doc = Document::parse_str("<root><a>1</a><b>2</b><c>3</c></root>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "correct order, errors: {:?}",
            result.errors
        );

        // Wrong order: c before b
        let doc = Document::parse_str("<root><a>1</a><c>3</c><b>2</b></root>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(!result.is_valid, "wrong order should be invalid");

        // Missing base element
        let doc = Document::parse_str("<root><c>3</c></root>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(!result.is_valid, "missing base element");
    }

    #[test]
    fn test_complex_content_extension_chain() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root" type="DerivedType"/>
                <xs:complexType name="GrandBaseType">
                    <xs:sequence>
                        <xs:element name="a" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:complexType name="BaseType">
                    <xs:complexContent>
                        <xs:extension base="GrandBaseType">
                            <xs:sequence>
                                <xs:element name="b" type="xs:string"/>
                            </xs:sequence>
                        </xs:extension>
                    </xs:complexContent>
                </xs:complexType>
                <xs:complexType name="DerivedType">
                    <xs:complexContent>
                        <xs:extension base="BaseType">
                            <xs:sequence>
                                <xs:element name="c" type="xs:string"/>
                            </xs:sequence>
                        </xs:extension>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<root><a>1</a><b>2</b><c>3</c></root>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "3-level chain, errors: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_complex_content_extension_empty_base() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root" type="DerivedType"/>
                <xs:complexType name="EmptyBaseType"/>
                <xs:complexType name="DerivedType">
                    <xs:complexContent>
                        <xs:extension base="EmptyBaseType">
                            <xs:sequence>
                                <xs:element name="x" type="xs:string"/>
                            </xs:sequence>
                        </xs:extension>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<root><x>hello</x></root>").unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "empty base extension, errors: {:?}",
            result.errors
        );
    }
}

#[cfg(test)]
#[test]
fn test_complex_content_extension_with_target_namespace() {
    let schema = parse_xsd(
        r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                     xmlns:adv="http://adv.example.com"
                     targetNamespace="http://adv.example.com"
                     elementFormDefault="qualified">
            <xs:element name="root" type="adv:DerivedType"/>
            <xs:complexType name="BaseType">
                <xs:sequence>
                    <xs:element name="a" type="xs:string"/>
                    <xs:element name="b" type="xs:string"/>
                </xs:sequence>
            </xs:complexType>
            <xs:complexType name="DerivedType">
                <xs:complexContent>
                    <xs:extension base="adv:BaseType">
                        <xs:sequence>
                            <xs:element name="c" type="xs:string"/>
                        </xs:sequence>
                    </xs:extension>
                </xs:complexContent>
            </xs:complexType>
        </xs:schema>"#,
    )
    .unwrap();

    // Correct order: a, b (base), c (extension)
    let doc = Document::parse_str(
        r#"<adv:root xmlns:adv="http://adv.example.com">
            <adv:a>1</adv:a><adv:b>2</adv:b><adv:c>3</adv:c>
        </adv:root>"#,
    )
    .unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(
        result.is_valid,
        "correct order, errors: {:?}",
        result.errors
    );

    // Wrong order: b before a
    let doc = Document::parse_str(
        r#"<adv:root xmlns:adv="http://adv.example.com">
            <adv:b>2</adv:b><adv:a>1</adv:a><adv:c>3</adv:c>
        </adv:root>"#,
    )
    .unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(!result.is_valid, "wrong order should be detected");
}

#[cfg(test)]
#[test]
fn test_sequence_optional_element_wrong_position() {
    // Sequence: required, optional, required
    // Instance has: optional, required, required (optional before its position)
    let schema = parse_xsd(
        r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                     xmlns:adv="http://adv.example.com"
                     targetNamespace="http://adv.example.com"
                     elementFormDefault="qualified">
            <xs:element name="root" type="adv:TestType"/>
            <xs:complexType name="TestType">
                <xs:sequence>
                    <xs:element name="required1" type="xs:string"/>
                    <xs:element name="optional" type="xs:string" minOccurs="0"/>
                    <xs:element name="required2" type="xs:string"/>
                </xs:sequence>
            </xs:complexType>
        </xs:schema>"#,
    )
    .unwrap();

    // Wrong: optional before required1
    let doc = Document::parse_str(
        r#"<adv:root xmlns:adv="http://adv.example.com">
            <adv:optional>x</adv:optional><adv:required1>a</adv:required1><adv:required2>b</adv:required2>
        </adv:root>"#,
    )
    .unwrap();
    let result = validate_xsd(&doc, &schema);
    eprintln!("Errors: {:?}", result.errors);
    assert!(
        !result.is_valid,
        "optional before required should be invalid"
    );
}

#[test]
fn test_sequence_order_violation() {
    // Schema: sequence with optional element between two required ones
    let schema = parse_xsd(
        r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root">
                    <xs:complexType><xs:sequence>
                        <xs:element name="a" type="xs:string"/>
                        <xs:element name="b" type="xs:string" minOccurs="0"/>
                        <xs:element name="c" type="xs:string"/>
                    </xs:sequence></xs:complexType>
                </xs:element>
            </xs:schema>"#,
    )
    .unwrap();

    // Valid: a, b, c in order
    let doc_ok = Document::parse_str("<root><a>1</a><b>2</b><c>3</c></root>").unwrap();
    let result_ok = validate_xsd(&doc_ok, &schema);
    assert!(
        result_ok.is_valid,
        "a,b,c should be valid: {:?}",
        result_ok.errors
    );

    // Valid: a, c (b optional, skipped)
    let doc_ok2 = Document::parse_str("<root><a>1</a><c>3</c></root>").unwrap();
    let result_ok2 = validate_xsd(&doc_ok2, &schema);
    assert!(
        result_ok2.is_valid,
        "a,c should be valid (b optional): {:?}",
        result_ok2.errors
    );

    // Invalid: c, a, b — c appears before a
    let doc_bad = Document::parse_str("<root><c>3</c><a>1</a><b>2</b></root>").unwrap();
    let result_bad = validate_xsd(&doc_bad, &schema);
    assert!(!result_bad.is_valid, "c before a should be invalid");
    assert!(
        result_bad
            .errors
            .iter()
            .any(|e| e.message.contains("unexpected")),
        "should report ordering error: {:?}",
        result_bad.errors
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn test_nas_substitution_group_resolution() {
    let schema_dir =
        std::path::Path::new("/Users/aw/Repository-CISS/konverter2.0/konverter/SCHEMA");
    if !schema_dir.exists() {
        eprintln!("Skipping NAS test - schema dir not found");
        return;
    }
    let entry = std::path::Path::new(
        "/Users/aw/Repository-CISS/konverter2.0/konverter/SCHEMA/NAS-Operationen.xsd",
    );
    let xml = std::fs::read_to_string(entry).unwrap();
    let _doc = Document::parse_str(&xml).unwrap();
    // Local resolver that maps import URLs to local SCHEMA/ directory files
    struct NasResolver {
        schema_dir: std::path::PathBuf,
    }
    impl crate::validation::xsd::SchemaResolver for NasResolver {
        fn resolve(&self, location: &str, _base: Option<&str>) -> Option<String> {
            let filename = location.rsplit('/').next().unwrap_or(location);
            let local_path = self.schema_dir.join(filename);
            std::fs::read_to_string(local_path).ok()
        }
    }
    let resolver = NasResolver {
        schema_dir: schema_dir.to_path_buf(),
    };

    let options = XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: schema_dir.to_str().map(String::from),
    };
    let schema = parse_xsd_with_options(&xml, &options).unwrap();

    // Debug: print FeatureCollectionType particles
    if let Some(XsdType::Complex(ct)) = schema.types.get("FeatureCollectionType") {
        eprintln!("\nFeatureCollectionType content:");
        match &ct.content {
            ComplexContent::Sequence(particles) => {
                for p in particles {
                    match p {
                        XsdParticle::Element(e) => {
                            eprintln!("  element: name={} ref={:?}", e.name, e.element_ref)
                        }
                        XsdParticle::Group(g) => eprintln!("  group: {g:?}"),
                        XsdParticle::Any(_) => eprintln!("  <any>"),
                    }
                }
            }
            other => eprintln!("  {other:?}"),
        }
    }
    // Also check imported types
    for (ns, imp) in &schema.imported_namespaces {
        if let Some(XsdType::Complex(ct)) = imp.types.get("FeatureCollectionType") {
            eprintln!("\nIMPORTED FeatureCollectionType [{ns}] content:");
            match &ct.content {
                ComplexContent::Sequence(particles) => {
                    for p in particles {
                        match p {
                            XsdParticle::Element(e) => {
                                eprintln!("  element: name={} ref={:?}", e.name, e.element_ref)
                            }
                            XsdParticle::Group(g) => eprintln!("  group: {g:?}"),
                            XsdParticle::Any(_) => eprintln!("  <any>"),
                        }
                    }
                }
                other => eprintln!("  {other:?}"),
            }
        }
    }

    // Debug: print substitution groups
    eprintln!(
        "Substitution groups (count={}):",
        schema.substitution_groups.len()
    );
    for (head, members) in &schema.substitution_groups {
        if head.contains("FeatureCollection") || head.contains("Abstract") {
            eprintln!("  {head} -> {members:?}");
        }
    }

    // Debug: FeatureCollection elements
    eprintln!("\nFeatureCollection elements:");
    for (name, elem) in &schema.elements {
        if name.contains("FeatureCollection") {
            eprintln!(
                "  LOCAL {name} -> sub_group={:?} abstract={}",
                elem.substitution_group, elem.is_abstract
            );
        }
    }
    for (ns, imp) in &schema.imported_namespaces {
        for (name, elem) in &imp.elements {
            if name.contains("FeatureCollection") {
                eprintln!(
                    "  IMPORTED[{ns}] {name} -> sub_group={:?} abstract={}",
                    elem.substitution_group, elem.is_abstract
                );
            }
        }
    }

    // Debug: AbstractCRS elements
    eprintln!("\nAbstractCRS elements:");
    for (name, elem) in &schema.elements {
        if name.contains("AbstractCRS") {
            eprintln!(
                "  LOCAL {name} -> sub_group={:?} abstract={}",
                elem.substitution_group, elem.is_abstract
            );
        }
    }
    eprintln!("\nAll imported namespaces:");
    for (ns, imp) in &schema.imported_namespaces {
        eprintln!("  {ns} ({} elements)", imp.elements.len());
        for name in imp.elements.keys() {
            if name.contains("Feature") || name.contains("CRS") || name.contains("Abstract") {
                eprintln!("    {name}");
            }
        }
    }

    // Now validate the actual NAS file
    let nas_file = "/Users/aw/Repository-CISS/konverter2.0/konverter/tests/assets/NAS/BE/auftragsposition_1_NAS_AMGR000000868064_1_.xml";
    if !std::path::Path::new(nas_file).exists() {
        eprintln!("Skipping NAS file validation - file not found");
        return;
    }
    let nas_xml = std::fs::read_to_string(nas_file).unwrap();
    let nas_doc = Document::parse_str(&nas_xml).unwrap();
    let result = validate_xsd(&nas_doc, &schema);
    eprintln!("  is_valid={}", result.is_valid);
    for err in &result.errors {
        eprintln!("  ERROR: {}", err.message);
    }
    // Known remaining limitations:
    // - AbstractCRS via xlink:href not recognized (XLink substitution for abstract elements)
    // - boundedBy in FeatureCollection (GML boundedBy support)
    // Serializer errors (antragsnummer, allgemeineAngaben, etc.) are expected
    // until the serializer is fixed.
    let non_serializer_errors: Vec<_> = result
        .errors
        .iter()
        .filter(|e| {
            !e.message.contains("<antragsnummer>")
                && !e.message.contains("<allgemeineAngaben>")
                && !e.message.contains("<erlaeuterung>")
        })
        .collect();
    eprintln!(
        "Non-serializer errors: {}/{}",
        non_serializer_errors.len(),
        result.errors.len()
    );
    // FeatureCollection substitution group should be resolved now
    assert!(
        !result.errors.iter().any(|e| e
            .message
            .contains("requires at least 1 occurrence(s) of <FeatureCollection>")
            || e.message.contains("unexpected element <FeatureCollection>")),
        "FeatureCollection substitution group should be resolved"
    );
}

/// Test that root elements declared in imported schemas are found.
///
/// Tests that `validate_xsd` finds `AX_Bestandsdatenauszug` from
/// `NAS-Operationen.xsd` (imported by `AAA-Basisschema.xsd`).
#[test]
fn test_root_element_from_imported_schema() {
    let schema_dir =
        std::path::Path::new("/Users/aw/Repository-CISS/konverter2.0/konverter/SCHEMA");
    let entry = schema_dir.join("AAA-Basisschema.xsd");
    if !entry.exists() {
        eprintln!("Skipping test - AAA-Basisschema.xsd not found");
        return;
    }
    let xsd_str = std::fs::read_to_string(&entry).unwrap();
    let resolver = |location: &str, _base: Option<&str>| -> Option<String> {
        let filename = location.rsplit('/').next().unwrap_or(location);
        std::fs::read_to_string(schema_dir.join(filename)).ok()
    };
    let options = XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: Some(format!("file:///{}", entry.display())),
    };
    let schema = parse_xsd_with_options(&xsd_str, &options).unwrap();

    // Minimal valid instance with correct element order
    let xml = br#"<?xml version="1.0" encoding="UTF-8"?>
<AX_Bestandsdatenauszug
  xmlns="http://www.adv-online.de/namespaces/adv/gid/7.1"
  xmlns:gml="http://www.opengis.net/gml/3.2"
  xmlns:wfs="http://www.opengis.net/wfs/2.0"
  xmlns:xlink="http://www.w3.org/1999/xlink">
  <erfolgreich>true</erfolgreich>
  <antragsnummer>123</antragsnummer>
  <allgemeineAngaben>
    <AX_K_Benutzungsergebnis>
      <erfolgreich>true</erfolgreich>
    </AX_K_Benutzungsergebnis>
  </allgemeineAngaben>
  <koordinatenangaben>
    <AA_Koordinatenreferenzsystemangaben>
      <crs xlink:href="urn:adv:crs:ETRS89_UTM33"/>
      <anzahlDerNachkommastellen>3</anzahlDerNachkommastellen>
      <standard>true</standard>
    </AA_Koordinatenreferenzsystemangaben>
  </koordinatenangaben>
  <enthaelt/>
</AX_Bestandsdatenauszug>"#;
    let doc = Document::parse_str(std::str::from_utf8(xml).unwrap()).unwrap();
    let result = validate_xsd(&doc, &schema);

    // Should NOT report "not declared as a global element"
    // If this fails, root element lookup in imported schemas is broken.
    assert!(
        !result
            .errors
            .iter()
            .any(|e| e.message.contains("not declared as a global element")),
        "AX_Bestandsdatenauszug should be found: {:?}",
        result.errors.iter().map(|e| &e.message).collect::<Vec<_>>()
    );

    // Should detect ordering: erlaeuterung (from base) is optional and absent here,
    // sequence is: erlaeuterung?, erfolgreich, antragsnummer, allgemeineAngaben, ...
    // With wrong order (allgemeineAngaben before antragsnummer):
    let xml_bad = br#"<?xml version="1.0" encoding="UTF-8"?>
<AX_Bestandsdatenauszug
  xmlns="http://www.adv-online.de/namespaces/adv/gid/7.1"
  xmlns:gml="http://www.opengis.net/gml/3.2"
  xmlns:xlink="http://www.w3.org/1999/xlink">
  <allgemeineAngaben>
    <AX_K_Benutzungsergebnis><erfolgreich>true</erfolgreich></AX_K_Benutzungsergebnis>
  </allgemeineAngaben>
  <antragsnummer>123</antragsnummer>
  <erfolgreich>true</erfolgreich>
  <koordinatenangaben>
    <AA_Koordinatenreferenzsystemangaben>
      <crs xlink:href="urn:adv:crs:ETRS89_UTM33"/>
      <anzahlDerNachkommastellen>3</anzahlDerNachkommastellen>
      <standard>true</standard>
    </AA_Koordinatenreferenzsystemangaben>
  </koordinatenangaben>
  <enthaelt/>
</AX_Bestandsdatenauszug>"#;
    let doc_bad = Document::parse_str(std::str::from_utf8(xml_bad).unwrap()).unwrap();
    let result_bad = validate_xsd(&doc_bad, &schema);
    assert!(
        !result_bad.is_valid,
        "wrong element order should be detected: {:?}",
        result_bad.errors
    );
}

/// Test that compositor-level minOccurs propagates to child elements.
///
/// When a `<sequence minOccurs="0">` contains an element with default
/// `minOccurs=1`, the validator should not require the element because
/// the entire sequence is optional.
#[test]
fn test_compositor_min_occurs_propagation() {
    let schema = parse_xsd(
        r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="root" type="RootType"/>
            <xs:complexType name="RootType">
                <xs:sequence>
                    <xs:element name="a" type="xs:string"/>
                    <xs:sequence minOccurs="0">
                        <xs:element name="b" type="xs:string"/>
                    </xs:sequence>
                </xs:sequence>
            </xs:complexType>
        </xs:schema>"#,
    )
    .unwrap();

    // "a" is required, "b" is inside an optional sequence
    let doc = Document::parse_str(r"<root><a>hello</a></root>").unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(
        result.is_valid,
        "optional sequence content should not be required: {:?}",
        result.errors
    );

    // But "a" IS required
    let doc_missing_a = Document::parse_str(r"<root><b>hello</b></root>").unwrap();
    let result_a = validate_xsd(&doc_missing_a, &schema);
    assert!(!result_a.is_valid, "'a' should be required");
}

/// Test that compositor-level minOccurs=0 works with GML-style property types.
///
/// Mirrors gml:CRSPropertyType where `<sequence minOccurs="0"><element ref="T"/>`.
#[test]
fn test_gml_style_optional_sequence_ref() {
    let schema = parse_xsd(
        r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                   xmlns:gml="http://example.com/gml"
                   targetNamespace="http://example.com/gml">
            <xs:element name="AbstractCRS" type="xs:string" abstract="true"/>
            <xs:element name="GeodeticCRS" substitutionGroup="gml:AbstractCRS" type="xs:string"/>
            <xs:complexType name="CRSPropertyType">
                <xs:sequence minOccurs="0">
                    <xs:element ref="gml:AbstractCRS"/>
                </xs:sequence>
                <xs:attribute name="href" type="xs:anyURI"/>
            </xs:complexType>
            <xs:element name="root" type="RootType"/>
            <xs:complexType name="RootType">
                <xs:sequence>
                    <xs:element name="crs" type="gml:CRSPropertyType"/>
                </xs:sequence>
            </xs:complexType>
        </xs:schema>"#,
    )
    .unwrap();

    // crs with only href, no AbstractCRS child (sequence minOccurs=0)
    let doc = Document::parse_str(
        r#"<root xmlns="http://example.com/gml" xmlns:gml="http://example.com/gml"><crs href="urn:ogc:crs:EPSG::4326"/></root>"#,
    )
    .unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(
        result.is_valid,
        "empty crs should be valid (optional sequence): {:?}",
        result.errors
    );

    // crs with substitution group member child
    let doc2 = Document::parse_str(
        r##"<root xmlns="http://example.com/gml" xmlns:gml="http://example.com/gml"><crs href="#crs1"><GeodeticCRS>EPSG:4326</GeodeticCRS></crs></root>"##,
    )
    .unwrap();
    let result2 = validate_xsd(&doc2, &schema);
    assert!(
        result2.is_valid,
        "substitution group member should be valid: {:?}",
        result2.errors
    );
}

#[cfg(test)]
mod test_envelope_lowercorner {
    use super::*;

    #[test]
    fn test_envelope_with_lower_upper_corner() {
        let schema = parse_xsd(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                   xmlns:gml="http://example.com/gml"
                   targetNamespace="http://example.com/gml">
                <xs:complexType name="EnvelopeType">
                    <xs:choice>
                        <xs:sequence>
                            <xs:element name="lowerCorner" type="xs:string"/>
                            <xs:element name="upperCorner" type="xs:string"/>
                        </xs:sequence>
                        <xs:element ref="gml:pos" minOccurs="2" maxOccurs="2"/>
                        <xs:element ref="gml:coordinates"/>
                    </xs:choice>
                </xs:complexType>
                <xs:element name="pos" type="xs:string"/>
                <xs:element name="coordinates" type="xs:string"/>
                <xs:element name="root" type="gml:EnvelopeType"/>
            </xs:schema>"#,
        )
        .unwrap();

        let doc = Document::parse_str(
            r#"<root xmlns="http://example.com/gml"><lowerCorner>1 2</lowerCorner><upperCorner>3 4</upperCorner></root>"#,
        )
        .unwrap();
        let result = validate_xsd(&doc, &schema);
        assert!(
            result.is_valid,
            "lowerCorner/upperCorner should be valid: {:?}",
            result.errors
        );
    }
}
