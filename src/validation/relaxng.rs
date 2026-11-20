//! `RelaxNG` schema validation for XML documents.
//!
//! This module implements the `RelaxNG` specification
//! (<https://relaxng.org/spec-20011203.html>) for validating XML documents
//! against `RelaxNG` schemas. `RelaxNG` schemas are themselves XML documents
//! that describe the structure and content of valid XML.
//!
//! # Architecture
//!
//! The implementation is split into three layers:
//!
//! 1. **Data model** ([`Pattern`], [`NameClass`], [`RelaxNgSchema`]) — an
//!    algebraic representation of the schema grammar.
//! 2. **Schema parser** ([`parse_relaxng`]) — reads a `RelaxNG` XML schema
//!    document and produces a `RelaxNgSchema`.
//! 3. **Validator** ([`validate`]) — checks an XML document tree against a
//!    compiled schema using James Clark's derivative algorithm, which
//!    runs in roughly linear time and under a work budget (see [`validate`]).
//!
//! # Examples
//!
//! ```
//! use xmloxide::Document;
//! use xmloxide::validation::relaxng::{parse_relaxng, validate};
//!
//! let schema_xml = r#"
//!   <element name="greeting" xmlns="http://relaxng.org/ns/structure/1.0">
//!     <text/>
//!   </element>
//! "#;
//!
//! let schema = parse_relaxng(schema_xml).unwrap();
//! let doc = Document::parse_str("<greeting>Hello!</greeting>").unwrap();
//! let result = validate(&doc, &schema);
//! assert!(result.is_valid);
//! ```

use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::tree::{Document, NodeId, NodeKind};
use crate::validation::{ValidationError, ValidationResult};

// ---------------------------------------------------------------------------
// Data model
// ---------------------------------------------------------------------------

/// A `RelaxNG` pattern — the core building block of a schema grammar.
///
/// Patterns form a tree that describes the allowed structure and content
/// of XML documents. The variants correspond to the grammar constructs
/// defined in the `RelaxNG` specification.
///
/// See <https://relaxng.org/spec-20011203.html#section:patterns>.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    /// Matches empty content (no elements, no text).
    Empty,
    /// Matches nothing — always fails. Used as an identity element for choice.
    NotAllowed,
    /// Matches arbitrary text content.
    Text,
    /// Matches an element whose name satisfies the [`NameClass`] and whose
    /// content matches the inner pattern.
    Element {
        /// Name constraint for the element.
        name: NameClass,
        /// Pattern that the element's content must match.
        pattern: Box<Pattern>,
    },
    /// Matches an attribute whose name satisfies the [`NameClass`] and whose
    /// value matches the inner pattern.
    Attribute {
        /// Name constraint for the attribute.
        name: NameClass,
        /// Pattern that the attribute value must match.
        pattern: Box<Pattern>,
    },
    /// Sequential composition — first pattern then second pattern.
    Group(Box<Pattern>, Box<Pattern>),
    /// Interleave — both patterns must match but in any order.
    Interleave(Box<Pattern>, Box<Pattern>),
    /// Choice — one of the two patterns must match.
    Choice(Box<Pattern>, Box<Pattern>),
    /// Optional — zero or one occurrence.
    Optional(Box<Pattern>),
    /// Zero or more occurrences.
    ZeroOrMore(Box<Pattern>),
    /// One or more occurrences.
    OneOrMore(Box<Pattern>),
    /// A named reference to a `<define>` block in the grammar.
    Ref(String),
    /// Matches a whitespace-separated list of tokens against the inner pattern.
    List(Box<Pattern>),
    /// Matches an exact string value.
    Value(String),
    /// Matches a value against a named datatype from a datatype library.
    Data {
        /// The datatype name (e.g., `"integer"`, `"string"`).
        datatype: String,
        /// The datatype library URI (e.g., the XML Schema datatypes namespace).
        library: String,
    },
    /// Matches a mixed content model (interleave of text and a pattern).
    Mixed(Box<Pattern>),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "empty"),
            Self::NotAllowed => write!(f, "notAllowed"),
            Self::Text => write!(f, "text"),
            Self::Element { name, .. } => write!(f, "element {name}"),
            Self::Attribute { name, .. } => write!(f, "attribute {name}"),
            Self::Group(a, b) => write!(f, "group({a}, {b})"),
            Self::Interleave(a, b) => write!(f, "interleave({a}, {b})"),
            Self::Choice(a, b) => write!(f, "choice({a}, {b})"),
            Self::Optional(p) => write!(f, "optional({p})"),
            Self::ZeroOrMore(p) => write!(f, "zeroOrMore({p})"),
            Self::OneOrMore(p) => write!(f, "oneOrMore({p})"),
            Self::Ref(name) => write!(f, "ref({name})"),
            Self::List(p) => write!(f, "list({p})"),
            Self::Value(v) => write!(f, "value(\"{v}\")"),
            Self::Data { datatype, .. } => write!(f, "data({datatype})"),
            Self::Mixed(p) => write!(f, "mixed({p})"),
        }
    }
}

/// A name class — constrains which element or attribute names are allowed.
///
/// Name classes can match specific names, any name in a namespace,
/// any name at all, or combinations via choice and exclusion.
///
/// See <https://relaxng.org/spec-20011203.html#section:name-classes>.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameClass {
    /// Matches a specific name (namespace URI + local name).
    Name {
        /// The namespace URI (empty string for no namespace).
        ns: String,
        /// The local name of the element or attribute.
        local: String,
    },
    /// Matches any name regardless of namespace.
    AnyName,
    /// Matches any name except those matching the excluded name class.
    AnyNameExcept(Box<NameClass>),
    /// Matches any name in the given namespace.
    NsName {
        /// The namespace URI to match.
        ns: String,
    },
    /// Matches any name in the given namespace except those matching the
    /// excluded name class.
    NsNameExcept {
        /// The namespace URI to match.
        ns: String,
        /// Names to exclude.
        except: Box<NameClass>,
    },
    /// Choice of two name classes — matches if either matches.
    Choice(Box<NameClass>, Box<NameClass>),
}

impl NameClass {
    /// Tests whether this name class matches the given namespace and local name.
    #[must_use]
    pub fn matches(&self, ns: &str, local: &str) -> bool {
        match self {
            Self::Name {
                ns: expected_ns,
                local: expected_local,
            } => expected_ns == ns && expected_local == local,
            Self::AnyName => true,
            Self::AnyNameExcept(except) => !except.matches(ns, local),
            Self::NsName { ns: expected_ns } => expected_ns == ns,
            Self::NsNameExcept {
                ns: expected_ns,
                except,
            } => expected_ns == ns && !except.matches(ns, local),
            Self::Choice(a, b) => a.matches(ns, local) || b.matches(ns, local),
        }
    }
}

impl fmt::Display for NameClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name { ns, local } => {
                if ns.is_empty() {
                    write!(f, "{local}")
                } else {
                    write!(f, "{{{ns}}}{local}")
                }
            }
            Self::AnyName => write!(f, "*"),
            Self::AnyNameExcept(except) => write!(f, "* - {except}"),
            Self::NsName { ns } => write!(f, "{{{ns}}}*"),
            Self::NsNameExcept { ns, except } => write!(f, "{{{ns}}}* - {except}"),
            Self::Choice(a, b) => write!(f, "{a} | {b}"),
        }
    }
}

/// A compiled `RelaxNG` schema ready for validation.
///
/// Contains the start pattern (the entry point for validation) and a map
/// of named definitions that can be referenced via `Ref` patterns.
#[derive(Debug, Clone)]
pub struct RelaxNgSchema {
    /// The start pattern — the root document must match this.
    pub start: Pattern,
    /// Named definitions (`<define name="...">` blocks).
    pub defines: HashMap<String, Pattern>,
}

// ---------------------------------------------------------------------------
// Schema parsing errors
// ---------------------------------------------------------------------------

/// Error type for schema parsing failures.
#[derive(Debug, Clone)]
pub struct SchemaParseError {
    /// Human-readable description of what went wrong.
    pub message: String,
}

impl fmt::Display for SchemaParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RelaxNG schema error: {}", self.message)
    }
}

impl std::error::Error for SchemaParseError {}

// ---------------------------------------------------------------------------
// Schema parser
// ---------------------------------------------------------------------------

/// Parses a `RelaxNG` XML schema string into a [`RelaxNgSchema`].
///
/// The input must be a valid XML document conforming to the `RelaxNG` XML
/// syntax (<https://relaxng.org/spec-20011203.html>). Both compact-element
/// form (e.g., `<element name="foo">`) and verbose form with child `<name>`
/// elements are supported.
///
/// # Errors
///
/// Returns [`SchemaParseError`] if the input is not well-formed XML or does
/// not conform to the expected `RelaxNG` structure.
///
/// # Examples
///
/// ```
/// use xmloxide::validation::relaxng::parse_relaxng;
///
/// let schema = parse_relaxng(r#"
///   <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
///     <empty/>
///   </element>
/// "#).unwrap();
/// ```
pub fn parse_relaxng(schema_xml: &str) -> Result<RelaxNgSchema, SchemaParseError> {
    let doc = Document::parse_str(schema_xml).map_err(|e| SchemaParseError {
        message: format!("failed to parse schema XML: {e}"),
    })?;

    let root_el = doc.root_element().ok_or_else(|| SchemaParseError {
        message: "schema has no root element".to_string(),
    })?;

    let root_name = doc.node_name(root_el).unwrap_or("");
    if !is_rng_element(&doc, root_el) {
        return Err(SchemaParseError {
            message: format!("root element <{root_name}> is not in the RELAX NG namespace"),
        });
    }

    // Determine default namespace for elements from the `ns` attribute
    // on the root schema element.
    let default_ns = element_ns_attr(&doc, root_el);

    if strip_rng_prefix(root_name) == "grammar" {
        parse_grammar(&doc, root_el, &default_ns)
    } else if strip_rng_prefix(root_name) == "element" {
        // Top-level element pattern (short form, no grammar wrapper).
        let pattern = parse_element_pattern(&doc, root_el, &default_ns)?;
        Ok(RelaxNgSchema {
            start: pattern,
            defines: HashMap::new(),
        })
    } else {
        Err(SchemaParseError {
            message: format!("expected <grammar> or <element> as root, found <{root_name}>"),
        })
    }
}

/// The `RelaxNG` structure namespace URI.
const RNG_NS: &str = "http://relaxng.org/ns/structure/1.0";

/// Returns `true` if `node` is a `RelaxNG` element, i.e. an element in the
/// `RelaxNG` namespace.
///
/// Elements in no namespace are also accepted for compatibility with
/// schemas that omit the namespace declaration. Every other element is a
/// *foreign element* (e.g. `<a:documentation>` compatibility annotations),
/// which `RelaxNG` §3 allows anywhere and §4.1 removes during simplification.
fn is_rng_element(doc: &Document, node: NodeId) -> bool {
    match &doc.node(node).kind {
        NodeKind::Element { namespace, .. } => match namespace.as_deref() {
            None => true,
            Some(ns) => ns == RNG_NS,
        },
        _ => false,
    }
}

/// Iterates over the `RelaxNG` element children of `el`, skipping text,
/// comments, and foreign elements (`RelaxNG` §4.1).
fn rng_children(doc: &Document, el: NodeId) -> impl Iterator<Item = NodeId> + '_ {
    doc.children(el).filter(move |&c| is_rng_element(doc, c))
}

/// Returns the value of an unqualified attribute on a schema element.
///
/// Attributes with a namespace prefix are either foreign attributes, which
/// `RelaxNG` §4.1 says are removed, or namespace declarations (whose local
/// part could otherwise collide with names like `ns` or `name`); both are
/// ignored.
fn rng_attr<'a>(doc: &'a Document, el: NodeId, name: &str) -> Option<&'a str> {
    doc.attributes(el)
        .iter()
        .find(|a| a.name == name && a.prefix.is_none() && a.namespace.is_none())
        .map(|a| a.value.as_str())
}

/// Strips a `rng:` prefix from a name, if present, returning the local part.
fn strip_rng_prefix(name: &str) -> &str {
    name.strip_prefix("rng:").unwrap_or(name)
}

/// Returns the value of the `ns` attribute on an element, defaulting to
/// the empty string.
fn element_ns_attr(doc: &Document, node: NodeId) -> String {
    rng_attr(doc, node, "ns").unwrap_or("").to_string()
}

/// Maximum length of a chain of `<ref>`s that are expanded without passing
/// through an `<element>` (e.g. `a` → `b` → `c` where each define's content
/// is just a reference to the next).
///
/// The validator follows such chains recursively, so this bounds its stack
/// usage on untrusted schemas. Real-world schemas stay far below it.
const MAX_REF_CHAIN_DEPTH: usize = 256;

/// Parses a `<grammar>` element containing `<start>` and `<define>` children.
fn parse_grammar(
    doc: &Document,
    grammar_el: NodeId,
    parent_ns: &str,
) -> Result<RelaxNgSchema, SchemaParseError> {
    let mut start = Combined::default();
    let mut components: HashMap<String, Combined> = HashMap::new();

    let ns = resolve_ns(doc, grammar_el, parent_ns);
    collect_grammar_content(doc, grammar_el, &ns, &mut start, &mut components)?;

    if start.patterns.is_empty() {
        return Err(SchemaParseError {
            message: "<grammar> has no <start> element".to_string(),
        });
    }
    let start = start.finish();

    let defines: HashMap<String, Pattern> = components
        .into_iter()
        .map(|(name, combined)| (name, combined.finish()))
        .collect();

    check_ref_recursion(&start, &defines)?;

    Ok(RelaxNgSchema { start, defines })
}

/// How same-named grammar components are merged (`combine` attribute).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CombineMethod {
    Choice,
    Interleave,
}

/// Accumulates every `<start>` or every same-named `<define>` of a grammar,
/// enforcing the `combine` rules of `RelaxNG` §4.17.
#[derive(Debug, Default)]
struct Combined {
    /// The agreed `combine` method, once any component has specified one.
    method: Option<CombineMethod>,
    /// Whether a component without a `combine` attribute has been seen.
    seen_without_combine: bool,
    /// Component contents, in document order.
    patterns: Vec<Pattern>,
}

impl Combined {
    /// Adds one component. `what` describes it for error messages.
    fn add(
        &mut self,
        combine: Option<&str>,
        pattern: Pattern,
        what: &str,
    ) -> Result<(), SchemaParseError> {
        match combine {
            None => {
                // §4.17: at most one component per name may omit `combine`.
                if self.seen_without_combine {
                    return Err(SchemaParseError {
                        message: format!(
                            "multiple {what} without a 'combine' attribute (RELAX NG §4.17)"
                        ),
                    });
                }
                self.seen_without_combine = true;
            }
            Some(value) => {
                let method = match value.trim() {
                    "choice" => CombineMethod::Choice,
                    "interleave" => CombineMethod::Interleave,
                    other => {
                        return Err(SchemaParseError {
                            message: format!("invalid 'combine' value \"{other}\" on {what}"),
                        });
                    }
                };
                // §4.17: all components of a name must agree on the method.
                if self.method.is_some_and(|m| m != method) {
                    return Err(SchemaParseError {
                        message: format!(
                            "conflicting 'combine' values (choice and interleave) \
                             on {what} (RELAX NG §4.17)"
                        ),
                    });
                }
                self.method = Some(method);
            }
        }
        self.patterns.push(pattern);
        Ok(())
    }

    /// Merges the components into one pattern: the `choice` or `interleave`
    /// (per the agreed method) of all of them. Must only be called after at
    /// least one component was added.
    fn finish(self) -> Pattern {
        match self.method {
            Some(CombineMethod::Interleave) => combine_interleave(self.patterns),
            Some(CombineMethod::Choice) | None => combine_choice(self.patterns),
        }
    }
}

/// Collects the `<start>` and `<define>` components of a grammar-content
/// container (`<grammar>` or `<div>`).
///
/// `<div>` is purely organizational: per `RelaxNG` §4.11 ("Each `div`
/// element is replaced by its children"), components nested in any number
/// of `<div>` levels belong to the enclosing grammar's single symbol table.
/// Recursion is bounded by the schema document's element depth, which the
/// XML parser limits.
fn collect_grammar_content(
    doc: &Document,
    container: NodeId,
    ns: &str,
    start: &mut Combined,
    defines: &mut HashMap<String, Combined>,
) -> Result<(), SchemaParseError> {
    for child in rng_children(doc, container) {
        let child_name = doc.node_name(child).unwrap_or("");
        let local = strip_rng_prefix(child_name);

        match local {
            "start" => {
                let inner = parse_pattern_children(doc, child, ns)?;
                start.add(rng_attr(doc, child, "combine"), inner, "<start> elements")?;
            }
            "define" => {
                let name = rng_attr(doc, child, "name").ok_or_else(|| SchemaParseError {
                    message: "<define> missing 'name' attribute".to_string(),
                })?;
                let inner = parse_pattern_children(doc, child, ns)?;
                defines.entry(name.to_string()).or_default().add(
                    rng_attr(doc, child, "combine"),
                    inner,
                    &format!("<define name=\"{name}\">"),
                )?;
            }
            "div" => {
                // `ns` on a `<div>` is inherited by its descendants (§4.9).
                let div_ns = resolve_ns(doc, child, ns);
                collect_grammar_content(doc, child, &div_ns, start, defines)?;
            }
            _ => {
                // Ignore unknown elements (`<include>` is not yet
                // implemented).
            }
        }
    }
    Ok(())
}

/// Collects the names of `<ref>`s in `pattern`. With `through_elements`
/// false, references inside `element` patterns are not included.
///
/// Uses an explicit stack, so deeply nested patterns cannot overflow the
/// call stack.
fn collect_refs<'p>(pattern: &'p Pattern, through_elements: bool, out: &mut Vec<&'p str>) {
    let mut stack = vec![pattern];
    while let Some(p) = stack.pop() {
        match p {
            Pattern::Ref(name) => out.push(name),
            Pattern::Element { pattern: inner, .. } => {
                if through_elements {
                    stack.push(inner);
                }
            }
            Pattern::Attribute { pattern: inner, .. }
            | Pattern::Optional(inner)
            | Pattern::ZeroOrMore(inner)
            | Pattern::OneOrMore(inner)
            | Pattern::List(inner)
            | Pattern::Mixed(inner) => stack.push(inner),
            Pattern::Group(a, b) | Pattern::Interleave(a, b) | Pattern::Choice(a, b) => {
                stack.push(a);
                stack.push(b);
            }
            Pattern::Empty
            | Pattern::NotAllowed
            | Pattern::Text
            | Pattern::Value(_)
            | Pattern::Data { .. } => {}
        }
    }
}

/// Rejects illegal recursion through `<ref>`s.
///
/// `RelaxNG` §4.19 expands every `ref` to a define whose content is not an
/// `element`, and requires that "this must not result in a loop": a define
/// may only refer back to itself through an `element` pattern. Recursion
/// outside an element (e.g. `<define name="a"><choice><ref name="a"/>…`,
/// possibly assembled from several `combine`d defines) would never consume
/// input and so would recurse forever during validation.
///
/// Only defines reachable from the start pattern are checked, matching the
/// spec's removal of unreachable defines. Chains of non-element refs longer
/// than [`MAX_REF_CHAIN_DEPTH`] are also rejected. The traversal is
/// iterative, so hostile schemas cannot overflow the stack here.
fn check_ref_recursion(
    start: &Pattern,
    defines: &HashMap<String, Pattern>,
) -> Result<(), SchemaParseError> {
    /// Traversal state of a define in the depth-first search.
    enum Visit {
        /// On the current DFS path.
        InProgress,
        /// Finished; holds the longest non-element ref chain starting here.
        Done(usize),
    }

    /// One define on the explicit DFS stack.
    struct Frame<'a> {
        name: &'a str,
        /// Defines referenced without passing through an element.
        succs: Vec<&'a str>,
        next: usize,
        max_depth: usize,
    }

    // Defines reachable from the start pattern through any reference.
    let mut reachable: Vec<&str> = Vec::new();
    let mut seen: HashSet<&str> = HashSet::new();
    let mut work = vec![start];
    let mut refs = Vec::new();
    while let Some(p) = work.pop() {
        refs.clear();
        collect_refs(p, true, &mut refs);
        for &name in &refs {
            if let Some((key, def)) = defines.get_key_value(name) {
                if seen.insert(key.as_str()) {
                    reachable.push(key.as_str());
                    work.push(def);
                }
            }
        }
    }
    // Deterministic error reporting.
    reachable.sort_unstable();

    let frame_for = |name| {
        let mut succs = Vec::new();
        if let Some(def) = defines.get(name) {
            collect_refs(def, false, &mut succs);
        }
        Frame {
            name,
            succs,
            next: 0,
            max_depth: 0,
        }
    };

    let mut state: HashMap<&str, Visit> = HashMap::new();
    for &root in &reachable {
        if state.contains_key(root) {
            continue;
        }
        state.insert(root, Visit::InProgress);
        let mut stack = vec![frame_for(root)];
        while let Some(frame) = stack.last_mut() {
            if let Some(&succ) = frame.succs.get(frame.next) {
                frame.next += 1;
                match state.get(succ) {
                    Some(Visit::InProgress) => {
                        return Err(SchemaParseError {
                            message: format!(
                                "illegal recursive reference to \"{succ}\": a define may \
                                 only refer to itself through an <element> (RELAX NG §4.19)"
                            ),
                        });
                    }
                    Some(Visit::Done(depth)) => frame.max_depth = frame.max_depth.max(*depth),
                    None => {
                        // Undefined references are reported during
                        // validation.
                        if defines.contains_key(succ) {
                            state.insert(succ, Visit::InProgress);
                            stack.push(frame_for(succ));
                        }
                    }
                }
            } else {
                let depth = frame.max_depth + 1;
                let name = frame.name;
                if depth > MAX_REF_CHAIN_DEPTH {
                    return Err(SchemaParseError {
                        message: format!(
                            "chain of references starting at \"{name}\" is nested more than \
                             {MAX_REF_CHAIN_DEPTH} levels deep without an <element>"
                        ),
                    });
                }
                stack.pop();
                state.insert(name, Visit::Done(depth));
                if let Some(parent) = stack.last_mut() {
                    parent.max_depth = parent.max_depth.max(depth);
                }
            }
        }
    }

    Ok(())
}

/// Resolves the effective namespace for pattern children. Uses the `ns`
/// attribute on the current element if present, otherwise inherits.
fn resolve_ns(doc: &Document, el: NodeId, parent_ns: &str) -> String {
    rng_attr(doc, el, "ns").map_or_else(|| parent_ns.to_string(), String::from)
}

/// Parses the child patterns of a container element (e.g., `<start>`,
/// `<group>`, `<choice>`). If there are multiple children, they are
/// implicitly grouped.
fn parse_pattern_children(
    doc: &Document,
    container: NodeId,
    ns: &str,
) -> Result<Pattern, SchemaParseError> {
    let patterns = collect_child_patterns(doc, container, ns)?;
    combine_patterns(patterns)
}

/// Collects all child pattern elements from a container node.
fn collect_child_patterns(
    doc: &Document,
    container: NodeId,
    ns: &str,
) -> Result<Vec<Pattern>, SchemaParseError> {
    let mut patterns = Vec::new();
    for child in rng_children(doc, container) {
        let p = parse_pattern(doc, child, ns)?;
        patterns.push(p);
    }
    Ok(patterns)
}

/// Combines a list of patterns using implicit `Group` (sequential composition).
///
/// `group` is associative, so the list is folded into a balanced tree (see
/// [`fold_balanced`]): a long sequence of siblings must not produce a
/// pattern nested deeply enough to overflow the stack in the recursive
/// validator.
#[allow(clippy::unnecessary_wraps)]
fn combine_patterns(patterns: Vec<Pattern>) -> Result<Pattern, SchemaParseError> {
    if patterns.is_empty() {
        return Ok(Pattern::Empty);
    }
    Ok(fold_balanced(patterns, &|a, b| {
        Pattern::Group(Box::new(a), Box::new(b))
    }))
}

/// Parses a single pattern element.
fn parse_pattern(doc: &Document, el: NodeId, parent_ns: &str) -> Result<Pattern, SchemaParseError> {
    let name = doc.node_name(el).unwrap_or("");
    let local = strip_rng_prefix(name);
    let ns = resolve_ns(doc, el, parent_ns);

    match local {
        "element" => parse_element_pattern(doc, el, &ns),
        "attribute" => parse_attribute_pattern(doc, el, &ns),
        "group" => {
            let children = collect_child_patterns(doc, el, &ns)?;
            combine_patterns(children)
        }
        "interleave" => {
            let children = collect_child_patterns(doc, el, &ns)?;
            Ok(combine_interleave(children))
        }
        "choice" => {
            let children = collect_child_patterns(doc, el, &ns)?;
            Ok(combine_choice(children))
        }
        "optional" => {
            let inner = parse_pattern_children(doc, el, &ns)?;
            Ok(Pattern::Optional(Box::new(inner)))
        }
        "zeroOrMore" => {
            let inner = parse_pattern_children(doc, el, &ns)?;
            Ok(Pattern::ZeroOrMore(Box::new(inner)))
        }
        "oneOrMore" => {
            let inner = parse_pattern_children(doc, el, &ns)?;
            Ok(Pattern::OneOrMore(Box::new(inner)))
        }
        "mixed" => {
            let inner = parse_pattern_children(doc, el, &ns)?;
            Ok(Pattern::Mixed(Box::new(inner)))
        }
        "ref" => {
            let ref_name = rng_attr(doc, el, "name")
                .ok_or_else(|| SchemaParseError {
                    message: "<ref> missing 'name' attribute".to_string(),
                })?
                .to_string();
            Ok(Pattern::Ref(ref_name))
        }
        "text" => Ok(Pattern::Text),
        "empty" => Ok(Pattern::Empty),
        "notAllowed" => Ok(Pattern::NotAllowed),
        "value" => {
            let text = doc.text_content(el);
            Ok(Pattern::Value(text))
        }
        "data" => {
            let datatype = rng_attr(doc, el, "type").unwrap_or("string").to_string();
            let library = rng_attr(doc, el, "datatypeLibrary")
                .unwrap_or("")
                .to_string();
            Ok(Pattern::Data { datatype, library })
        }
        "list" => {
            let inner = parse_pattern_children(doc, el, &ns)?;
            Ok(Pattern::List(Box::new(inner)))
        }
        other => Err(SchemaParseError {
            message: format!("unknown pattern element <{other}>"),
        }),
    }
}

/// Parses an `<element>` pattern, extracting the name class and content pattern.
fn parse_element_pattern(
    doc: &Document,
    el: NodeId,
    ns: &str,
) -> Result<Pattern, SchemaParseError> {
    let name_class = parse_name_class_from_element(doc, el, ns, false)?;
    let el_ns = resolve_ns(doc, el, ns);

    let mut content_patterns = Vec::new();
    for child in content_children(doc, el) {
        let p = parse_pattern(doc, child, &el_ns)?;
        content_patterns.push(p);
    }

    let content = combine_patterns(content_patterns)?;

    Ok(Pattern::Element {
        name: name_class,
        pattern: Box::new(content),
    })
}

/// Parses an `<attribute>` pattern, extracting the name class and value pattern.
fn parse_attribute_pattern(
    doc: &Document,
    el: NodeId,
    ns: &str,
) -> Result<Pattern, SchemaParseError> {
    let name_class = parse_name_class_from_element(doc, el, ns, true)?;
    let attr_ns = resolve_ns(doc, el, ns);

    let mut content_patterns = Vec::new();
    for child in content_children(doc, el) {
        let p = parse_pattern(doc, child, &attr_ns)?;
        content_patterns.push(p);
    }

    let content = if content_patterns.is_empty() {
        Pattern::Text // default: attribute value is text
    } else {
        combine_patterns(content_patterns)?
    };

    Ok(Pattern::Attribute {
        name: name_class,
        pattern: Box::new(content),
    })
}

/// Iterates over the content-pattern children of an `<element>` or
/// `<attribute>` pattern.
///
/// Without a `name` attribute, the first child is the name class (`RelaxNG`
/// §3: `<element> nameClass pattern+ </element>`) and every later child is
/// content — including a `<choice>`, which must not be mistaken for a
/// name-class choice just because it appears after one.
fn content_children(doc: &Document, el: NodeId) -> impl Iterator<Item = NodeId> + '_ {
    let skip = usize::from(rng_attr(doc, el, "name").is_none());
    rng_children(doc, el).skip(skip)
}

/// Extracts a [`NameClass`] from an element or attribute pattern element.
///
/// If the element has a `name` attribute, uses that directly. Otherwise,
/// looks for a child `<name>`, `<anyName>`, or `<nsName>` element.
///
/// For the `name` attribute shorthand, `RelaxNG` §4.8 says an `<attribute>`
/// without its own `ns` attribute names an attribute in no namespace (the
/// inherited `ns` applies only to elements), and §4.10 resolves a prefixed
/// value as a `QName`.
fn parse_name_class_from_element(
    doc: &Document,
    el: NodeId,
    ns: &str,
    is_attribute: bool,
) -> Result<NameClass, SchemaParseError> {
    // Check for `name` attribute shorthand.
    if let Some(name_attr) = rng_attr(doc, el, "name") {
        let default_ns = if is_attribute {
            rng_attr(doc, el, "ns").unwrap_or("").to_string()
        } else {
            resolve_ns(doc, el, ns)
        };
        return resolve_qname(doc, el, name_attr, default_ns);
    }

    // Otherwise the first child is the name class.
    if let Some(child) = rng_children(doc, el).next() {
        let child_name = doc.node_name(child).unwrap_or("");
        match strip_rng_prefix(child_name) {
            "name" => return parse_name_element(doc, child, ns),
            "anyName" => return parse_any_name_class(doc, child, ns),
            "nsName" => return parse_ns_name_class(doc, child, ns),
            "choice" => return parse_name_class_choice(doc, child, ns),
            _ => {}
        }
    }

    Err(SchemaParseError {
        message: "element/attribute pattern has no name or name class".to_string(),
    })
}

/// The namespace URI permanently bound to the `xml` prefix (Namespaces in
/// XML 1.0 §3).
const XML_NS: &str = "http://www.w3.org/XML/1998/namespace";

/// Looks up the namespace URI bound to `prefix` in the scope of `el` in the
/// schema document. The `xml` prefix is always bound, even without an
/// explicit declaration.
fn lookup_ns_prefix<'a>(doc: &'a Document, el: NodeId, prefix: &str) -> Option<&'a str> {
    if prefix == "xml" {
        return Some(XML_NS);
    }
    let mut cur = Some(el);
    while let Some(node) = cur {
        if let Some(decl) = doc
            .attributes(node)
            .iter()
            .find(|a| a.prefix.as_deref() == Some("xmlns") && a.name == prefix)
        {
            return Some(decl.value.as_str());
        }
        cur = doc.parent(node);
    }
    None
}

/// Resolves a `QName` used as an element or attribute name into a
/// [`NameClass::Name`].
///
/// Per `RelaxNG` §4.10, a prefix is replaced by the namespace URI it is
/// bound to in the context of `ctx` (the element carrying the name), and
/// the context must have a mapping for the prefix. An unprefixed name uses
/// `default_ns`.
fn resolve_qname(
    doc: &Document,
    ctx: NodeId,
    raw: &str,
    default_ns: String,
) -> Result<NameClass, SchemaParseError> {
    let qname = raw.trim();
    let Some((prefix, local)) = qname.split_once(':') else {
        if qname.is_empty() {
            return Err(SchemaParseError {
                message: "empty element/attribute name".to_string(),
            });
        }
        return Ok(NameClass::Name {
            ns: default_ns,
            local: qname.to_string(),
        });
    };
    if prefix.is_empty() || local.is_empty() || local.contains(':') {
        return Err(SchemaParseError {
            message: format!("invalid QName '{qname}'"),
        });
    }
    match lookup_ns_prefix(doc, ctx, prefix) {
        Some(uri) if !uri.is_empty() => Ok(NameClass::Name {
            ns: uri.to_string(),
            local: local.to_string(),
        }),
        _ => Err(SchemaParseError {
            message: format!("undeclared namespace prefix '{prefix}' in name '{qname}'"),
        }),
    }
}

/// Parses a `<name>` name-class element. Its text is a `QName` (§4.10);
/// an unprefixed name takes the inherited `ns` (§4.9).
fn parse_name_element(doc: &Document, el: NodeId, ns: &str) -> Result<NameClass, SchemaParseError> {
    let text = doc.text_content(el);
    resolve_qname(doc, el, &text, resolve_ns(doc, el, ns))
}

/// Parses an `<anyName>` name class, possibly with `<except>`.
fn parse_any_name_class(
    doc: &Document,
    el: NodeId,
    ns: &str,
) -> Result<NameClass, SchemaParseError> {
    for child in rng_children(doc, el) {
        let child_name = doc.node_name(child).unwrap_or("");
        if strip_rng_prefix(child_name) == "except" {
            let except = parse_name_class_children(doc, child, ns)?;
            return Ok(NameClass::AnyNameExcept(Box::new(except)));
        }
    }
    Ok(NameClass::AnyName)
}

/// Parses an `<nsName>` name class, possibly with `<except>`.
fn parse_ns_name_class(
    doc: &Document,
    el: NodeId,
    ns: &str,
) -> Result<NameClass, SchemaParseError> {
    let target_ns = resolve_ns(doc, el, ns);
    for child in rng_children(doc, el) {
        let child_name = doc.node_name(child).unwrap_or("");
        if strip_rng_prefix(child_name) == "except" {
            let except = parse_name_class_children(doc, child, ns)?;
            return Ok(NameClass::NsNameExcept {
                ns: target_ns,
                except: Box::new(except),
            });
        }
    }
    Ok(NameClass::NsName { ns: target_ns })
}

/// Parses a `<choice>` element used as a name class.
fn parse_name_class_choice(
    doc: &Document,
    el: NodeId,
    ns: &str,
) -> Result<NameClass, SchemaParseError> {
    let mut classes = Vec::new();
    for child in rng_children(doc, el) {
        let child_name = doc.node_name(child).unwrap_or("");
        let child_local = strip_rng_prefix(child_name);
        let nc = match child_local {
            "name" => parse_name_element(doc, child, ns)?,
            "anyName" => parse_any_name_class(doc, child, ns)?,
            "nsName" => parse_ns_name_class(doc, child, ns)?,
            "choice" => parse_name_class_choice(doc, child, ns)?,
            _ => {
                continue;
            }
        };
        classes.push(nc);
    }
    combine_name_classes(classes)
}

/// Parses name class children inside an `<except>` or similar container.
fn parse_name_class_children(
    doc: &Document,
    container: NodeId,
    ns: &str,
) -> Result<NameClass, SchemaParseError> {
    let mut classes = Vec::new();
    for child in rng_children(doc, container) {
        let child_name = doc.node_name(child).unwrap_or("");
        let child_local = strip_rng_prefix(child_name);
        let nc = match child_local {
            "name" => parse_name_element(doc, child, ns)?,
            "anyName" => parse_any_name_class(doc, child, ns)?,
            "nsName" => parse_ns_name_class(doc, child, ns)?,
            "choice" => parse_name_class_choice(doc, child, ns)?,
            _ => {
                continue;
            }
        };
        classes.push(nc);
    }
    combine_name_classes(classes)
}

/// Combines multiple name classes using `Choice`.
fn combine_name_classes(classes: Vec<NameClass>) -> Result<NameClass, SchemaParseError> {
    if classes.is_empty() {
        return Err(SchemaParseError {
            message: "empty name class".to_string(),
        });
    }
    let mut iter = classes.into_iter();
    let first = iter.next().ok_or_else(|| SchemaParseError {
        message: "internal error: empty name class list".to_string(),
    })?;
    Ok(iter.fold(first, |acc, nc| {
        NameClass::Choice(Box::new(acc), Box::new(nc))
    }))
}

/// Combines patterns using `Interleave`.
fn combine_interleave(patterns: Vec<Pattern>) -> Pattern {
    if patterns.is_empty() {
        return Pattern::Empty;
    }
    fold_balanced(patterns, &|a, b| {
        Pattern::Interleave(Box::new(a), Box::new(b))
    })
}

/// Combines patterns using `Choice`.
fn combine_choice(patterns: Vec<Pattern>) -> Pattern {
    if patterns.is_empty() {
        return Pattern::NotAllowed;
    }
    fold_balanced(patterns, &|a, b| Pattern::Choice(Box::new(a), Box::new(b)))
}

/// Folds a non-empty list of patterns with an associative binary
/// constructor into a balanced tree.
///
/// `choice` and `interleave` are associative, so the shape does not change
/// their meaning, but a balanced tree keeps the nesting depth logarithmic.
/// This matters for untrusted schemas: thousands of sibling alternatives or
/// `combine`d defines would otherwise produce a tree deep enough to overflow
/// the stack in the recursive validator (and in `Drop`). Lists of up to
/// three patterns produce the same left-leaning shape as a left fold.
fn fold_balanced(
    mut patterns: Vec<Pattern>,
    join: &dyn Fn(Pattern, Pattern) -> Pattern,
) -> Pattern {
    if patterns.len() <= 1 {
        return patterns.pop().unwrap_or(Pattern::Empty);
    }
    let right = patterns.split_off(patterns.len().div_ceil(2));
    let left = fold_balanced(patterns, join);
    let right = fold_balanced(right, join);
    join(left, right)
}

// ---------------------------------------------------------------------------
// Validation
// ---------------------------------------------------------------------------
//
// The validator implements James Clark's derivative algorithm
// (<https://relaxng.org/jclark/derivative.html>). Instead of searching for a
// way to split an element's children between the parts of a pattern (which
// backtracks, and is exponential for `interleave`), it walks the document
// once and computes, for each start tag, attribute, text node and end tag,
// the *derivative* of the current pattern: the pattern that the rest of the
// input must match. The input is valid when the final pattern is nullable
// (matches the empty sequence).
//
// Patterns are compiled into an arena of hash-consed nodes, so structurally
// equal patterns share one `PatId`, choices are normalized to sorted sets,
// and derivatives are memoized per `PatId`. This keeps the number of distinct
// patterns small and the work per input event roughly constant. Because
// `interleave` can still produce a number of states exponential in the
// schema size in contrived cases, every validation also runs under a work
// budget linear in the document and schema sizes (see
// `BASE_VALIDATION_STEPS`):
// once it is exhausted, validation stops with an error instead of running
// unbounded.

/// Work budget, in derivative steps, available to every [`validate`] call
/// regardless of document size.
///
/// A step is one memo-missing derivative computation or one newly interned
/// pattern, so the budget bounds both time and memory. Together with
/// [`VALIDATION_STEPS_PER_NODE`] it caps validation at work linear in the
/// document size, so a hostile document (or schema) cannot make validation
/// run unbounded; real-world schemas and documents use a small fraction of
/// it.
const BASE_VALIDATION_STEPS: u64 = 1_000_000;

/// Additional work budget, in derivative steps, granted per element,
/// attribute and text node of the validated document.
const VALIDATION_STEPS_PER_NODE: u64 = 1_000;

/// Additional work budget, in derivative steps, granted per pattern node in
/// the schema (its start pattern and all defines).
///
/// Compiling the schema interns one pattern per node, and a derivative over
/// a large content model touches each of its particles, so the budget must
/// grow with the schema as well as the document: otherwise a valid document
/// could exhaust it against a large (but legitimate) schema. This keeps the
/// bound linear in the combined input size.
const VALIDATION_STEPS_PER_SCHEMA_PATTERN: u64 = 16;

/// Maximum number of validation errors reported by one [`validate`] call.
const MAX_VALIDATION_ERRORS: usize = 100;

/// Validates an XML document against a compiled [`RelaxNgSchema`].
///
/// The validator checks that the document's root element matches the
/// schema's start pattern, recursively verifying element names, attributes,
/// text content, and structural constraints.
///
/// Validation uses the derivative algorithm, which runs in time roughly
/// linear in the document size. It is additionally bounded by a work budget
/// linear in the document and schema sizes; a schema/document pair that
/// exceeds it
/// is reported as invalid with a "work limit" error rather than validating
/// unbounded.
///
/// # Examples
///
/// ```
/// use xmloxide::Document;
/// use xmloxide::validation::relaxng::{parse_relaxng, validate};
///
/// let schema = parse_relaxng(r#"
///   <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
///     <empty/>
///   </element>
/// "#).unwrap();
///
/// let doc = Document::parse_str("<root/>").unwrap();
/// let result = validate(&doc, &schema);
/// assert!(result.is_valid);
/// ```
#[must_use]
pub fn validate(doc: &Document, schema: &RelaxNgSchema) -> ValidationResult {
    let nodes = doc.root_element().map_or(0, |root| {
        doc.descendants(root)
            .map(|id| match &doc.node(id).kind {
                NodeKind::Element { attributes, .. } => 1 + attributes.len(),
                _ => 1,
            })
            .sum::<usize>()
    });
    let per_node = u64::try_from(nodes)
        .unwrap_or(u64::MAX)
        .saturating_mul(VALIDATION_STEPS_PER_NODE);
    let patterns = std::iter::once(&schema.start)
        .chain(schema.defines.values())
        .map(pattern_size)
        .fold(0usize, usize::saturating_add);
    let per_pattern = u64::try_from(patterns)
        .unwrap_or(u64::MAX)
        .saturating_mul(VALIDATION_STEPS_PER_SCHEMA_PATTERN);
    let budget = BASE_VALIDATION_STEPS
        .saturating_add(per_node)
        .saturating_add(per_pattern);
    validate_with_budget(doc, schema, budget)
}

/// Counts the nodes of a pattern tree (without following `ref`s).
///
/// Iterative, so a deep pattern (e.g. a hand-built schema) cannot overflow
/// the stack.
fn pattern_size(pattern: &Pattern) -> usize {
    let mut count = 0usize;
    let mut stack = vec![pattern];
    while let Some(p) = stack.pop() {
        count = count.saturating_add(1);
        match p {
            Pattern::Element { pattern, .. }
            | Pattern::Attribute { pattern, .. }
            | Pattern::Optional(pattern)
            | Pattern::ZeroOrMore(pattern)
            | Pattern::OneOrMore(pattern)
            | Pattern::List(pattern)
            | Pattern::Mixed(pattern) => stack.push(pattern),
            Pattern::Group(a, b) | Pattern::Interleave(a, b) | Pattern::Choice(a, b) => {
                stack.push(a);
                stack.push(b);
            }
            Pattern::Empty
            | Pattern::NotAllowed
            | Pattern::Text
            | Pattern::Ref(_)
            | Pattern::Value(_)
            | Pattern::Data { .. } => {}
        }
    }
    count
}

/// Validates `doc` against `schema` with an explicit work budget.
fn validate_with_budget(doc: &Document, schema: &RelaxNgSchema, budget: u64) -> ValidationResult {
    let Some(root_el) = doc.root_element() else {
        return ValidationResult {
            is_valid: false,
            errors: vec![validation_error("document has no root element".to_string())],
            warnings: Vec::new(),
        };
    };

    let mut v = Validator::new(doc, &schema.defines, budget);
    let start = v.compile(&schema.start, 0);
    let rest = v.element_deriv(start, root_el);
    let ok = !v.exhausted && v.errors.is_empty() && v.nullable(rest);

    let mut errors = std::mem::take(&mut v.errors);
    if v.exhausted {
        errors.push(validation_error(format!(
            "RelaxNG validation aborted: exceeded the work limit of {budget} steps"
        )));
    } else if !ok && errors.is_empty() {
        errors.push(validation_error(
            "document does not match the schema's start pattern".to_string(),
        ));
    }
    if !ok {
        errors.extend(
            std::mem::take(&mut v.undefined_refs)
                .into_iter()
                .map(|name| validation_error(format!("undefined reference: {name}"))),
        );
    }

    ValidationResult {
        is_valid: ok,
        errors,
        warnings: Vec::new(),
    }
}

/// Builds a [`ValidationError`] without location information.
fn validation_error(message: String) -> ValidationError {
    ValidationError {
        message,
        line: None,
        column: None,
    }
}

/// Index of an interned pattern in the [`Validator`] arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct PatId(u32);

/// The interned `empty` pattern.
const EMPTY: PatId = PatId(0);
/// The interned `notAllowed` pattern.
const NOT_ALLOWED: PatId = PatId(1);
/// The interned `text` pattern.
const TEXT: PatId = PatId(2);

/// A hash-consed pattern node, in the normal form used by the derivative
/// algorithm. `optional`, `zeroOrMore`, `mixed` and `ref` are compiled
/// away; `After` is the algorithm's internal "match the first pattern, then
/// continue with the second after the end tag" pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PatNode {
    Empty,
    NotAllowed,
    Text,
    /// A sorted, deduplicated set of at least two alternatives, none of
    /// which is itself a choice or `notAllowed`.
    Choice(Box<[PatId]>),
    Group(PatId, PatId),
    Interleave(PatId, PatId),
    OneOrMore(PatId),
    List(PatId),
    /// An expected value (see `Validator::value_equals`).
    Value(Box<str>),
    Data(Box<str>),
    /// Attribute name class index and value pattern.
    Attribute(u32, PatId),
    /// Element name class index and content slot index; the content is
    /// compiled lazily, which is what lets element patterns be recursive.
    Element(u32, u32),
    After(PatId, PatId),
}

/// Properties of an interned pattern, computed bottom-up when it is interned.
#[derive(Debug, Clone, Copy)]
struct PatInfo {
    /// The pattern matches the empty sequence.
    nullable: bool,
    /// The pattern's text derivative depends on the text (it contains a
    /// `value`, `data` or `list` reachable without entering an element).
    text_sensitive: bool,
    /// The pattern contains an attribute pattern reachable without
    /// entering an element.
    has_attr: bool,
}

/// The pattern applied by `apply_after` to the continuation of every
/// `After` in a start-tag derivative (the `f` of Clark's `applyAfter`).
#[derive(Debug, Clone, Copy)]
enum AfterOp {
    /// `\p -> interleave(p, x)`.
    InterleaveWith(PatId),
    /// `\p -> interleave(x, p)`.
    InterleaveAfter(PatId),
    /// `\p -> group(p, x)`.
    GroupWith(PatId),
    /// `\p -> after(p, x)`.
    AfterWith(PatId),
}

/// A significant child of an element, with adjacent text coalesced.
enum ChildItem<'a> {
    Element(NodeId),
    Text(std::borrow::Cow<'a, str>),
}

/// Derivative-based `RelaxNG` validator state for one [`validate`] call.
struct Validator<'a> {
    doc: &'a Document,
    defines: &'a HashMap<String, Pattern>,

    nodes: Vec<PatNode>,
    info: Vec<PatInfo>,
    interned: HashMap<PatNode, PatId>,

    name_classes: Vec<&'a NameClass>,
    name_class_ids: HashMap<*const NameClass, u32>,
    /// Lazily compiled element contents: source pattern and compiled id.
    slots: Vec<(&'a Pattern, Option<PatId>)>,
    slot_ids: HashMap<*const Pattern, u32>,
    /// Compiled defines; `None` while a define is being compiled.
    compiled_defines: HashMap<&'a str, Option<PatId>>,
    undefined_refs: Vec<&'a str>,

    /// Interned `(namespace, local name)` pairs of document names.
    qnames: HashMap<(&'a str, &'a str), u32>,
    start_tag_memo: HashMap<(PatId, u32), PatId>,
    start_tag_close_memo: HashMap<(PatId, bool), PatId>,
    end_tag_memo: HashMap<(PatId, bool), PatId>,
    /// Text derivatives of patterns that are not `text_sensitive`.
    text_memo: HashMap<PatId, PatId>,

    /// Set while matching an attribute value (see `value_equals`).
    in_attribute_value: bool,

    steps_left: u64,
    exhausted: bool,
    errors: Vec<ValidationError>,
}

impl<'a> Validator<'a> {
    fn new(doc: &'a Document, defines: &'a HashMap<String, Pattern>, budget: u64) -> Self {
        let mut v = Self {
            doc,
            defines,
            nodes: Vec::new(),
            info: Vec::new(),
            interned: HashMap::new(),
            name_classes: Vec::new(),
            name_class_ids: HashMap::new(),
            slots: Vec::new(),
            slot_ids: HashMap::new(),
            compiled_defines: HashMap::new(),
            undefined_refs: Vec::new(),
            qnames: HashMap::new(),
            start_tag_memo: HashMap::new(),
            start_tag_close_memo: HashMap::new(),
            end_tag_memo: HashMap::new(),
            text_memo: HashMap::new(),
            in_attribute_value: false,
            steps_left: budget,
            exhausted: false,
            errors: Vec::new(),
        };
        // Pre-intern the constants in `PatId` order.
        for node in [PatNode::Empty, PatNode::NotAllowed, PatNode::Text] {
            v.steps_left += 1;
            v.intern(node);
        }
        v
    }

    // --- Budget and error bookkeeping ---

    /// Consumes one unit of the work budget. Returns `false` (and marks the
    /// validation as exhausted) when the budget is used up.
    fn step(&mut self) -> bool {
        if self.steps_left == 0 {
            self.exhausted = true;
            return false;
        }
        self.steps_left -= 1;
        true
    }

    fn error(&mut self, message: String) {
        if self.errors.len() < MAX_VALIDATION_ERRORS {
            self.errors.push(validation_error(message));
        }
    }

    // --- Interning and smart constructors ---

    fn node(&self, p: PatId) -> &PatNode {
        &self.nodes[p.0 as usize]
    }

    fn nullable(&self, p: PatId) -> bool {
        self.info[p.0 as usize].nullable
    }

    fn intern(&mut self, node: PatNode) -> PatId {
        if let Some(&id) = self.interned.get(&node) {
            return id;
        }
        if !self.step() {
            return NOT_ALLOWED;
        }
        let info_of = |v: &Self, p: PatId| v.info[p.0 as usize];
        let info = match &node {
            PatNode::Empty | PatNode::Text => PatInfo {
                nullable: true,
                text_sensitive: false,
                has_attr: false,
            },
            PatNode::NotAllowed | PatNode::Element(..) => PatInfo {
                nullable: false,
                text_sensitive: false,
                has_attr: false,
            },
            PatNode::Value(_) | PatNode::Data(_) | PatNode::List(_) => PatInfo {
                nullable: false,
                text_sensitive: true,
                has_attr: false,
            },
            PatNode::Attribute(..) => PatInfo {
                nullable: false,
                text_sensitive: false,
                has_attr: true,
            },
            PatNode::Choice(items) => items.iter().fold(info_of(self, NOT_ALLOWED), |acc, &p| {
                let i = info_of(self, p);
                PatInfo {
                    nullable: acc.nullable || i.nullable,
                    text_sensitive: acc.text_sensitive || i.text_sensitive,
                    has_attr: acc.has_attr || i.has_attr,
                }
            }),
            PatNode::Group(a, b) | PatNode::Interleave(a, b) => {
                let (a, b) = (info_of(self, *a), info_of(self, *b));
                PatInfo {
                    nullable: a.nullable && b.nullable,
                    text_sensitive: a.text_sensitive || b.text_sensitive,
                    has_attr: a.has_attr || b.has_attr,
                }
            }
            PatNode::OneOrMore(p) => info_of(self, *p),
            // Derivatives never look into the continuation of an `After`.
            PatNode::After(a, _) => PatInfo {
                nullable: false,
                ..info_of(self, *a)
            },
        };
        let id = PatId(u32::try_from(self.nodes.len()).unwrap_or(u32::MAX));
        self.nodes.push(node.clone());
        self.info.push(info);
        self.interned.insert(node, id);
        id
    }

    fn choice(&mut self, a: PatId, b: PatId) -> PatId {
        if a == NOT_ALLOWED || a == b {
            return b;
        }
        if b == NOT_ALLOWED {
            return a;
        }
        self.choice_of(vec![a, b])
    }

    /// Builds the choice of all `items`, flattening nested choices and
    /// dropping duplicates and `notAllowed`.
    fn choice_of(&mut self, items: Vec<PatId>) -> PatId {
        let mut flat = Vec::with_capacity(items.len());
        for p in items {
            match self.node(p) {
                PatNode::NotAllowed => {}
                PatNode::Choice(inner) => flat.extend_from_slice(inner),
                _ => flat.push(p),
            }
        }
        flat.sort_unstable();
        flat.dedup();
        match flat.len() {
            0 => NOT_ALLOWED,
            1 => flat[0],
            _ => self.intern(PatNode::Choice(flat.into_boxed_slice())),
        }
    }

    fn group(&mut self, a: PatId, b: PatId) -> PatId {
        if a == NOT_ALLOWED || b == NOT_ALLOWED {
            NOT_ALLOWED
        } else if a == EMPTY {
            b
        } else if b == EMPTY {
            a
        } else {
            self.intern(PatNode::Group(a, b))
        }
    }

    fn interleave(&mut self, a: PatId, b: PatId) -> PatId {
        if a == NOT_ALLOWED || b == NOT_ALLOWED {
            NOT_ALLOWED
        } else if a == EMPTY {
            b
        } else if b == EMPTY {
            a
        } else {
            self.intern(PatNode::Interleave(a, b))
        }
    }

    fn after(&mut self, a: PatId, b: PatId) -> PatId {
        if a == NOT_ALLOWED || b == NOT_ALLOWED {
            NOT_ALLOWED
        } else {
            self.intern(PatNode::After(a, b))
        }
    }

    fn one_or_more(&mut self, p: PatId) -> PatId {
        if p == NOT_ALLOWED || p == EMPTY {
            p
        } else {
            self.intern(PatNode::OneOrMore(p))
        }
    }

    // --- Compilation of schema patterns ---

    /// Compiles a schema pattern into the arena. `ref_depth` counts the
    /// `Ref`s being expanded; it guards hand-built schemas, which bypass the
    /// parser's §4.19 recursion checks.
    fn compile(&mut self, pattern: &'a Pattern, ref_depth: usize) -> PatId {
        match pattern {
            Pattern::Empty => EMPTY,
            Pattern::NotAllowed => NOT_ALLOWED,
            Pattern::Text => TEXT,
            Pattern::Element { name, pattern } => {
                let nc = self.name_class_id(name);
                let key: *const Pattern = &**pattern;
                let slot = if let Some(&slot) = self.slot_ids.get(&key) {
                    slot
                } else {
                    let slot = u32::try_from(self.slots.len()).unwrap_or(u32::MAX);
                    self.slots.push((&**pattern, None));
                    self.slot_ids.insert(key, slot);
                    slot
                };
                self.intern(PatNode::Element(nc, slot))
            }
            Pattern::Attribute { name, pattern } => {
                let nc = self.name_class_id(name);
                let value = self.compile(pattern, ref_depth);
                self.intern(PatNode::Attribute(nc, value))
            }
            Pattern::Group(a, b) => {
                let (a, b) = (self.compile(a, ref_depth), self.compile(b, ref_depth));
                self.group(a, b)
            }
            Pattern::Interleave(a, b) => {
                let (a, b) = (self.compile(a, ref_depth), self.compile(b, ref_depth));
                self.interleave(a, b)
            }
            Pattern::Choice(a, b) => {
                let (a, b) = (self.compile(a, ref_depth), self.compile(b, ref_depth));
                self.choice(a, b)
            }
            Pattern::Optional(p) => {
                let p = self.compile(p, ref_depth);
                self.choice(p, EMPTY)
            }
            Pattern::ZeroOrMore(p) => {
                let p = self.compile(p, ref_depth);
                let p = self.one_or_more(p);
                self.choice(p, EMPTY)
            }
            Pattern::OneOrMore(p) => {
                let p = self.compile(p, ref_depth);
                self.one_or_more(p)
            }
            Pattern::Mixed(p) => {
                let p = self.compile(p, ref_depth);
                self.interleave(TEXT, p)
            }
            Pattern::List(p) => {
                let p = self.compile(p, ref_depth);
                if p == NOT_ALLOWED {
                    NOT_ALLOWED
                } else {
                    self.intern(PatNode::List(p))
                }
            }
            Pattern::Value(v) => self.intern(PatNode::Value(v.as_str().into())),
            Pattern::Data { datatype, .. } => self.intern(PatNode::Data(datatype.as_str().into())),
            Pattern::Ref(name) => self.compile_ref(name, ref_depth),
        }
    }

    fn compile_ref(&mut self, name: &'a str, ref_depth: usize) -> PatId {
        match self.compiled_defines.get(name) {
            Some(Some(id)) => return *id,
            // A reference cycle that does not pass through an element.
            Some(None) => return NOT_ALLOWED,
            None => {}
        }
        let Some((key, def)) = self.defines.get_key_value(name) else {
            if !self.undefined_refs.contains(&name) {
                self.undefined_refs.push(name);
            }
            return NOT_ALLOWED;
        };
        if ref_depth >= MAX_REF_CHAIN_DEPTH {
            return NOT_ALLOWED;
        }
        self.compiled_defines.insert(key, None);
        let id = self.compile(def, ref_depth + 1);
        self.compiled_defines.insert(key, Some(id));
        id
    }

    fn name_class_id(&mut self, nc: &'a NameClass) -> u32 {
        let key: *const NameClass = nc;
        if let Some(&id) = self.name_class_ids.get(&key) {
            return id;
        }
        let id = u32::try_from(self.name_classes.len()).unwrap_or(u32::MAX);
        self.name_classes.push(nc);
        self.name_class_ids.insert(key, id);
        id
    }

    /// Returns the compiled content pattern of an element slot.
    fn element_content(&mut self, slot: u32) -> PatId {
        let (pattern, compiled) = self.slots[slot as usize];
        if let Some(id) = compiled {
            return id;
        }
        let id = self.compile(pattern, 0);
        self.slots[slot as usize].1 = Some(id);
        id
    }

    // --- Derivatives ---

    /// Clark's `startTagOpenDeriv`.
    fn start_tag_open_deriv(&mut self, p: PatId, qname: u32, ns: &str, local: &str) -> PatId {
        if let Some(&r) = self.start_tag_memo.get(&(p, qname)) {
            return r;
        }
        if !self.step() {
            return NOT_ALLOWED;
        }
        let r = match self.node(p).clone() {
            PatNode::Choice(items) => {
                let derivs = items
                    .iter()
                    .map(|&q| self.start_tag_open_deriv(q, qname, ns, local))
                    .collect();
                self.choice_of(derivs)
            }
            PatNode::Element(nc, slot) => {
                if self.name_classes[nc as usize].matches(ns, local) {
                    let content = self.element_content(slot);
                    self.after(content, EMPTY)
                } else {
                    NOT_ALLOWED
                }
            }
            PatNode::Interleave(a, b) => {
                let da = self.start_tag_open_deriv(a, qname, ns, local);
                let left = self.apply_after(AfterOp::InterleaveWith(b), da);
                let db = self.start_tag_open_deriv(b, qname, ns, local);
                let right = self.apply_after(AfterOp::InterleaveAfter(a), db);
                self.choice(left, right)
            }
            PatNode::OneOrMore(q) => {
                let dq = self.start_tag_open_deriv(q, qname, ns, local);
                let rest = self.choice(p, EMPTY);
                self.apply_after(AfterOp::GroupWith(rest), dq)
            }
            PatNode::Group(a, b) => {
                let da = self.start_tag_open_deriv(a, qname, ns, local);
                let left = self.apply_after(AfterOp::GroupWith(b), da);
                if self.nullable(a) {
                    let db = self.start_tag_open_deriv(b, qname, ns, local);
                    self.choice(left, db)
                } else {
                    left
                }
            }
            PatNode::After(a, b) => {
                let da = self.start_tag_open_deriv(a, qname, ns, local);
                self.apply_after(AfterOp::AfterWith(b), da)
            }
            _ => NOT_ALLOWED,
        };
        self.start_tag_memo.insert((p, qname), r);
        r
    }

    /// Clark's `applyAfter`: applies `op` to the continuation of every
    /// `After` in `p`, which is a choice of `After`s (or `notAllowed`).
    fn apply_after(&mut self, op: AfterOp, p: PatId) -> PatId {
        if !self.step() {
            return NOT_ALLOWED;
        }
        match self.node(p).clone() {
            PatNode::After(a, b) => {
                let b = match op {
                    AfterOp::InterleaveWith(x) => self.interleave(b, x),
                    AfterOp::InterleaveAfter(x) => self.interleave(x, b),
                    AfterOp::GroupWith(x) => self.group(b, x),
                    AfterOp::AfterWith(x) => self.after(b, x),
                };
                self.after(a, b)
            }
            PatNode::Choice(items) => {
                let mapped = items.iter().map(|&q| self.apply_after(op, q)).collect();
                self.choice_of(mapped)
            }
            _ => NOT_ALLOWED,
        }
    }

    /// Clark's `attDeriv` for one attribute. `memo` is per attribute.
    fn att_deriv(
        &mut self,
        p: PatId,
        ns: &str,
        local: &str,
        value: &str,
        memo: &mut HashMap<PatId, PatId>,
    ) -> PatId {
        if !self.info[p.0 as usize].has_attr {
            return NOT_ALLOWED;
        }
        if let Some(&r) = memo.get(&p) {
            return r;
        }
        if !self.step() {
            return NOT_ALLOWED;
        }
        let r = match self.node(p).clone() {
            PatNode::After(a, b) => {
                let da = self.att_deriv(a, ns, local, value, memo);
                self.after(da, b)
            }
            PatNode::Choice(items) => {
                let derivs = items
                    .iter()
                    .map(|&q| self.att_deriv(q, ns, local, value, memo))
                    .collect();
                self.choice_of(derivs)
            }
            PatNode::Group(a, b) => {
                let da = self.att_deriv(a, ns, local, value, memo);
                let left = self.group(da, b);
                let db = self.att_deriv(b, ns, local, value, memo);
                let right = self.group(a, db);
                self.choice(left, right)
            }
            PatNode::Interleave(a, b) => {
                let da = self.att_deriv(a, ns, local, value, memo);
                let left = self.interleave(da, b);
                let db = self.att_deriv(b, ns, local, value, memo);
                let right = self.interleave(a, db);
                self.choice(left, right)
            }
            PatNode::OneOrMore(q) => {
                let dq = self.att_deriv(q, ns, local, value, memo);
                let rest = self.choice(p, EMPTY);
                self.group(dq, rest)
            }
            PatNode::Attribute(nc, value_pattern) => {
                if self.name_classes[nc as usize].matches(ns, local)
                    && self.value_matches(value_pattern, value)
                {
                    EMPTY
                } else {
                    NOT_ALLOWED
                }
            }
            _ => NOT_ALLOWED,
        };
        memo.insert(p, r);
        r
    }

    /// Clark's `valueMatch`: whether an attribute value (or the text of a
    /// text-only element) matches pattern `p`.
    fn value_matches(&mut self, p: PatId, value: &str) -> bool {
        if self.nullable(p) && is_xml_whitespace(value) {
            return true;
        }
        let outer = std::mem::replace(&mut self.in_attribute_value, true);
        let d = self.text_deriv(p, value);
        self.in_attribute_value = outer;
        self.nullable(d)
    }

    /// Compares text against a `<value>`.
    ///
    /// The schema parser does not record a value's datatype, so this cannot
    /// apply the datatype's equality (whitespace-collapsing for the default
    /// `token`, exact for `string`). It keeps the long-standing behavior:
    /// attribute values must match exactly, element text up to leading and
    /// trailing whitespace.
    fn value_equals(&self, text: &str, expected: &str) -> bool {
        if self.in_attribute_value {
            text == expected
        } else {
            text.trim() == expected.trim()
        }
    }

    /// Clark's `startTagCloseDeriv`. With `recover`, attribute patterns that
    /// are still pending are dropped instead of making the pattern
    /// `notAllowed`, so validation can continue after a missing attribute.
    fn start_tag_close_deriv(&mut self, p: PatId, recover: bool) -> PatId {
        if !self.info[p.0 as usize].has_attr {
            return p;
        }
        if let Some(&r) = self.start_tag_close_memo.get(&(p, recover)) {
            return r;
        }
        if !self.step() {
            return NOT_ALLOWED;
        }
        let r = match self.node(p).clone() {
            PatNode::After(a, b) => {
                let da = self.start_tag_close_deriv(a, recover);
                self.after(da, b)
            }
            PatNode::Choice(items) => {
                let derivs = items
                    .iter()
                    .map(|&q| self.start_tag_close_deriv(q, recover))
                    .collect();
                self.choice_of(derivs)
            }
            PatNode::Group(a, b) => {
                let (a, b) = (
                    self.start_tag_close_deriv(a, recover),
                    self.start_tag_close_deriv(b, recover),
                );
                self.group(a, b)
            }
            PatNode::Interleave(a, b) => {
                let (a, b) = (
                    self.start_tag_close_deriv(a, recover),
                    self.start_tag_close_deriv(b, recover),
                );
                self.interleave(a, b)
            }
            PatNode::OneOrMore(q) => {
                let dq = self.start_tag_close_deriv(q, recover);
                self.one_or_more(dq)
            }
            PatNode::Attribute(..) if recover => EMPTY,
            PatNode::Attribute(..) => NOT_ALLOWED,
            _ => p,
        };
        self.start_tag_close_memo.insert((p, recover), r);
        r
    }

    /// Clark's `textDeriv`.
    fn text_deriv(&mut self, p: PatId, text: &str) -> PatId {
        let mut memo = HashMap::new();
        self.text_deriv_memo(p, text, &mut memo)
    }

    /// `text_deriv` with a memo for the text-dependent derivatives of
    /// this one text; text-independent ones are memoized globally.
    fn text_deriv_memo(&mut self, p: PatId, text: &str, memo: &mut HashMap<PatId, PatId>) -> PatId {
        let sensitive = self.info[p.0 as usize].text_sensitive;
        let cached = if sensitive {
            memo.get(&p)
        } else {
            self.text_memo.get(&p)
        };
        if let Some(&r) = cached {
            return r;
        }
        if !self.step() {
            return NOT_ALLOWED;
        }
        let r = match self.node(p).clone() {
            PatNode::Choice(items) => {
                let derivs = items
                    .iter()
                    .map(|&q| self.text_deriv_memo(q, text, memo))
                    .collect();
                self.choice_of(derivs)
            }
            PatNode::Interleave(a, b) => {
                let da = self.text_deriv_memo(a, text, memo);
                let left = self.interleave(da, b);
                let db = self.text_deriv_memo(b, text, memo);
                let right = self.interleave(a, db);
                self.choice(left, right)
            }
            PatNode::Group(a, b) => {
                let da = self.text_deriv_memo(a, text, memo);
                let left = self.group(da, b);
                if self.nullable(a) {
                    let db = self.text_deriv_memo(b, text, memo);
                    self.choice(left, db)
                } else {
                    left
                }
            }
            PatNode::After(a, b) => {
                let da = self.text_deriv_memo(a, text, memo);
                self.after(da, b)
            }
            PatNode::OneOrMore(q) => {
                let dq = self.text_deriv_memo(q, text, memo);
                let rest = self.choice(p, EMPTY);
                self.group(dq, rest)
            }
            PatNode::Text => TEXT,
            PatNode::Value(expected) => {
                if self.value_equals(text, &expected) {
                    EMPTY
                } else {
                    NOT_ALLOWED
                }
            }
            PatNode::Data(datatype) => {
                if validate_datatype(text, "<text>", &datatype, "", &mut Vec::new()) {
                    EMPTY
                } else {
                    NOT_ALLOWED
                }
            }
            PatNode::List(q) => {
                let mut d = q;
                for token in text.split_ascii_whitespace() {
                    d = self.text_deriv(d, token);
                    if d == NOT_ALLOWED {
                        break;
                    }
                }
                if self.nullable(d) {
                    EMPTY
                } else {
                    NOT_ALLOWED
                }
            }
            _ => NOT_ALLOWED,
        };
        if sensitive {
            memo.insert(p, r);
        } else {
            self.text_memo.insert(p, r);
        }
        r
    }

    /// Clark's `endTagDeriv`. With `recover`, the continuation is returned
    /// even if the element's content is incomplete.
    fn end_tag_deriv(&mut self, p: PatId, recover: bool) -> PatId {
        if let Some(&r) = self.end_tag_memo.get(&(p, recover)) {
            return r;
        }
        if !self.step() {
            return NOT_ALLOWED;
        }
        let r = match self.node(p).clone() {
            PatNode::Choice(items) => {
                let derivs = items
                    .iter()
                    .map(|&q| self.end_tag_deriv(q, recover))
                    .collect();
                self.choice_of(derivs)
            }
            PatNode::After(a, b) if recover || self.nullable(a) => b,
            _ => NOT_ALLOWED,
        };
        self.end_tag_memo.insert((p, recover), r);
        r
    }

    // --- Document traversal ---

    /// Computes the derivative of `p` with respect to a whole element
    /// (Clark's `childDeriv` for an element node), recording errors and
    /// recovering where possible so that later siblings are still checked.
    fn element_deriv(&mut self, p: PatId, el: NodeId) -> PatId {
        let doc = self.doc;
        let NodeKind::Element {
            name,
            namespace,
            attributes,
            ..
        } = &doc.node(el).kind
        else {
            return p;
        };
        let ns = namespace.as_deref().unwrap_or("");
        let qname = self.qname_id(ns, name);

        let open = self.start_tag_open_deriv(p, qname, ns, name);
        if self.exhausted {
            return NOT_ALLOWED;
        }
        if open == NOT_ALLOWED {
            let expected = self.describe_expected(p);
            self.error(format!(
                "element <{name}> is not allowed here; expected {expected}"
            ));
            // Skip the element and its content.
            return p;
        }

        let mut current = open;
        for attr in attributes.iter().filter(|a| !is_xmlns_attribute(a)) {
            let attr_ns = attr.namespace.as_deref().unwrap_or("");
            let next = self.att_deriv(
                current,
                attr_ns,
                &attr.name,
                &attr.value,
                &mut HashMap::new(),
            );
            if self.exhausted {
                return NOT_ALLOWED;
            }
            if next == NOT_ALLOWED {
                if self.attribute_name_allowed(current, attr_ns, &attr.name) {
                    self.error(format!(
                        "attribute '{}' on element '{name}' has invalid value \"{}\"",
                        attr.name, attr.value
                    ));
                } else {
                    self.error(format!(
                        "unexpected attribute '{}' on element '{name}'",
                        attr.name
                    ));
                }
            } else {
                current = next;
            }
        }

        let mut content = self.start_tag_close_deriv(current, false);
        if content == NOT_ALLOWED && !self.exhausted {
            self.error(format!("element '{name}' is missing a required attribute"));
            content = self.start_tag_close_deriv(current, true);
            if content == NOT_ALLOWED {
                return p;
            }
        }

        let content = self.children_deriv(content, el, name);
        if self.exhausted {
            return NOT_ALLOWED;
        }
        let rest = self.end_tag_deriv(content, false);
        if rest != NOT_ALLOWED || self.exhausted {
            return rest;
        }
        let expected = self.describe_expected(content);
        self.error(format!(
            "element <{name}> has incomplete content: missing {expected}"
        ));
        match self.end_tag_deriv(content, true) {
            NOT_ALLOWED => p,
            rest => rest,
        }
    }

    /// Clark's `childrenDeriv`: the derivative of `p` with respect to the
    /// content of element `el`.
    fn children_deriv(&mut self, p: PatId, el: NodeId, el_name: &str) -> PatId {
        let mut items = Vec::new();
        collect_child_items(self.doc, el, &mut items);

        match items.as_slice() {
            [] => {
                let d = self.text_deriv(p, "");
                self.choice(p, d)
            }
            [ChildItem::Text(text)] => {
                let d = self.text_deriv(p, text);
                if is_xml_whitespace(text) {
                    self.choice(p, d)
                } else if d == NOT_ALLOWED {
                    self.text_error(p, text, el_name);
                    p
                } else {
                    d
                }
            }
            _ => {
                let mut current = p;
                for item in &items {
                    if self.exhausted {
                        return NOT_ALLOWED;
                    }
                    match item {
                        ChildItem::Element(child) => current = self.element_deriv(current, *child),
                        ChildItem::Text(text) if is_xml_whitespace(text) => {}
                        ChildItem::Text(text) => {
                            let d = self.text_deriv(current, text);
                            if d == NOT_ALLOWED {
                                self.text_error(current, text, el_name);
                            } else {
                                current = d;
                            }
                        }
                    }
                }
                current
            }
        }
    }

    fn text_error(&mut self, p: PatId, text: &str, el_name: &str) {
        if self.exhausted {
            return;
        }
        let expected = self.describe_expected(p);
        let shown: String = text.trim().chars().take(30).collect();
        self.error(format!(
            "text \"{shown}\" is not allowed in element <{el_name}>; expected {expected}"
        ));
    }

    fn qname_id(&mut self, ns: &'a str, local: &'a str) -> u32 {
        let next = u32::try_from(self.qnames.len()).unwrap_or(u32::MAX);
        *self.qnames.entry((ns, local)).or_insert(next)
    }

    // --- Error diagnostics ---

    /// Whether some attribute pattern in `p` accepts the attribute name.
    fn attribute_name_allowed(&self, p: PatId, ns: &str, local: &str) -> bool {
        let mut stack = vec![p];
        let mut seen = HashSet::new();
        while let Some(q) = stack.pop() {
            if !self.info[q.0 as usize].has_attr || !seen.insert(q) {
                continue;
            }
            match self.node(q) {
                PatNode::Attribute(nc, _) => {
                    if self.name_classes[*nc as usize].matches(ns, local) {
                        return true;
                    }
                }
                PatNode::Choice(items) => stack.extend_from_slice(items),
                PatNode::Group(a, b) | PatNode::Interleave(a, b) => stack.extend([*a, *b]),
                PatNode::OneOrMore(a) | PatNode::After(a, _) => stack.push(*a),
                _ => {}
            }
        }
        false
    }

    /// Describes what `p` accepts next (element names, text), for error
    /// messages.
    fn describe_expected(&self, p: PatId) -> String {
        const MAX_SHOWN: usize = 8;
        let mut names: Vec<String> = Vec::new();
        let mut stack = vec![p];
        let mut seen = HashSet::new();
        let mut more = false;
        while let Some(q) = stack.pop() {
            if !seen.insert(q) {
                continue;
            }
            let item = match self.node(q) {
                PatNode::Element(nc, _) => format!("element {}", self.name_classes[*nc as usize]),
                PatNode::Text => "text".to_string(),
                PatNode::Value(v) => format!("value \"{v}\""),
                PatNode::Data(dt) => format!("data ({dt})"),
                PatNode::List(_) => "a list of values".to_string(),
                PatNode::Choice(items) => {
                    stack.extend(items.iter().rev());
                    continue;
                }
                PatNode::Group(a, b) => {
                    if self.nullable(*a) {
                        stack.push(*b);
                    }
                    stack.push(*a);
                    continue;
                }
                PatNode::Interleave(a, b) => {
                    stack.extend([*b, *a]);
                    continue;
                }
                PatNode::OneOrMore(a) | PatNode::After(a, _) => {
                    stack.push(*a);
                    continue;
                }
                PatNode::Empty | PatNode::NotAllowed | PatNode::Attribute(..) => continue,
            };
            if names.contains(&item) {
                continue;
            }
            if names.len() == MAX_SHOWN {
                more = true;
                break;
            }
            names.push(item);
        }
        if names.is_empty() {
            return "end of content".to_string();
        }
        let mut out = names.join(", ");
        if more {
            out.push_str(", ...");
        }
        out
    }
}

/// Collects the significant children of `el`: elements, and runs of
/// adjacent text and CDATA coalesced into one text item. Comments and
/// processing instructions are ignored; entity references are looked
/// through.
fn collect_child_items<'d>(doc: &'d Document, el: NodeId, items: &mut Vec<ChildItem<'d>>) {
    for child in doc.children(el) {
        match &doc.node(child).kind {
            NodeKind::Element { .. } => items.push(ChildItem::Element(child)),
            NodeKind::Text { content } | NodeKind::CData { content } => {
                if let Some(ChildItem::Text(text)) = items.last_mut() {
                    text.to_mut().push_str(content);
                } else {
                    items.push(ChildItem::Text(std::borrow::Cow::Borrowed(content)));
                }
            }
            NodeKind::EntityRef { .. } => collect_child_items(doc, child, items),
            _ => {}
        }
    }
}

/// Whether `s` consists only of XML whitespace (including being empty).
fn is_xml_whitespace(s: &str) -> bool {
    s.bytes().all(|b| matches!(b, b' ' | b'\t' | b'\n' | b'\r'))
}

/// Very basic datatype validation (token, string, integer).
fn validate_datatype(
    value: &str,
    attr_name: &str,
    datatype: &str,
    _library: &str,
    errors: &mut Vec<ValidationError>,
) -> bool {
    match datatype {
        "integer" | "int" | "long" | "short" | "byte" => {
            if value.trim().parse::<i64>().is_ok() {
                true
            } else {
                errors.push(ValidationError {
                    message: format!(
                        "attribute '{attr_name}' value \"{value}\" \
                         is not a valid {datatype}"
                    ),
                    line: None,
                    column: None,
                });
                false
            }
        }
        "positiveInteger" | "nonNegativeInteger" => match value.trim().parse::<i64>() {
            Ok(n) if n >= 0 => true,
            _ => {
                errors.push(ValidationError {
                    message: format!(
                        "attribute '{attr_name}' value \"{value}\" \
                         is not a valid {datatype}"
                    ),
                    line: None,
                    column: None,
                });
                false
            }
        },
        "boolean" => {
            let v = value.trim();
            if v == "true" || v == "false" || v == "1" || v == "0" {
                true
            } else {
                errors.push(ValidationError {
                    message: format!(
                        "attribute '{attr_name}' value \"{value}\" \
                         is not a valid boolean"
                    ),
                    line: None,
                    column: None,
                });
                false
            }
        }
        _ => true, // Unknown datatypes are accepted.
    }
}

/// Checks whether an attribute is an `xmlns` declaration (which are not
/// validated by `RelaxNG`).
fn is_xmlns_attribute(attr: &crate::tree::Attribute) -> bool {
    attr.name == "xmlns"
        || attr.prefix.as_deref() == Some("xmlns")
        || attr.namespace.as_deref() == Some("http://www.w3.org/2000/xmlns/")
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use std::fmt::Write as _;

    // --- Name class matching tests ---

    #[test]
    fn test_name_class_specific_name_matches() {
        let nc = NameClass::Name {
            ns: String::new(),
            local: "foo".to_string(),
        };
        assert!(nc.matches("", "foo"));
        assert!(!nc.matches("", "bar"));
    }

    #[test]
    fn test_name_class_specific_name_with_ns() {
        let nc = NameClass::Name {
            ns: "http://example.com".to_string(),
            local: "foo".to_string(),
        };
        assert!(nc.matches("http://example.com", "foo"));
        assert!(!nc.matches("", "foo"));
        assert!(!nc.matches("http://example.com", "bar"));
    }

    #[test]
    fn test_name_class_any_name() {
        let nc = NameClass::AnyName;
        assert!(nc.matches("", "anything"));
        assert!(nc.matches("http://example.com", "anything"));
    }

    #[test]
    fn test_name_class_any_name_except() {
        let nc = NameClass::AnyNameExcept(Box::new(NameClass::Name {
            ns: String::new(),
            local: "secret".to_string(),
        }));
        assert!(nc.matches("", "foo"));
        assert!(!nc.matches("", "secret"));
    }

    #[test]
    fn test_name_class_ns_name() {
        let nc = NameClass::NsName {
            ns: "http://example.com".to_string(),
        };
        assert!(nc.matches("http://example.com", "anything"));
        assert!(!nc.matches("http://other.com", "anything"));
    }

    #[test]
    fn test_name_class_ns_name_except() {
        let nc = NameClass::NsNameExcept {
            ns: "http://example.com".to_string(),
            except: Box::new(NameClass::Name {
                ns: "http://example.com".to_string(),
                local: "secret".to_string(),
            }),
        };
        assert!(nc.matches("http://example.com", "foo"));
        assert!(!nc.matches("http://example.com", "secret"));
        assert!(!nc.matches("http://other.com", "foo"));
    }

    #[test]
    fn test_name_class_choice() {
        let nc = NameClass::Choice(
            Box::new(NameClass::Name {
                ns: String::new(),
                local: "a".to_string(),
            }),
            Box::new(NameClass::Name {
                ns: String::new(),
                local: "b".to_string(),
            }),
        );
        assert!(nc.matches("", "a"));
        assert!(nc.matches("", "b"));
        assert!(!nc.matches("", "c"));
    }

    // --- Schema parsing tests ---

    #[test]
    fn test_parse_simple_element_schema() {
        let schema_xml = r#"
            <element name="greeting" xmlns="http://relaxng.org/ns/structure/1.0">
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        assert!(matches!(schema.start, Pattern::Element { .. }));
        assert!(schema.defines.is_empty());
    }

    #[test]
    fn test_parse_grammar_with_start_and_define() {
        let schema_xml = r#"
            <grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start>
                    <ref name="root"/>
                </start>
                <define name="root">
                    <element name="root">
                        <text/>
                    </element>
                </define>
            </grammar>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        assert!(matches!(schema.start, Pattern::Ref(ref name) if name == "root"));
        assert!(schema.defines.contains_key("root"));
    }

    #[test]
    fn test_parse_element_with_attributes() {
        let schema_xml = r#"
            <element name="person" xmlns="http://relaxng.org/ns/structure/1.0">
                <attribute name="id"/>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        // Should be Group(Attribute, Text)
        assert!(matches!(pattern.as_ref(), Pattern::Group(_, _)));
    }

    #[test]
    fn test_parse_choice_pattern() {
        let schema_xml = r#"
            <element name="value" xmlns="http://relaxng.org/ns/structure/1.0">
                <choice>
                    <element name="a"><text/></element>
                    <element name="b"><text/></element>
                </choice>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(matches!(pattern.as_ref(), Pattern::Choice(_, _)));
    }

    #[test]
    fn test_parse_zero_or_more() {
        let schema_xml = r#"
            <element name="list" xmlns="http://relaxng.org/ns/structure/1.0">
                <zeroOrMore>
                    <element name="item"><text/></element>
                </zeroOrMore>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(matches!(pattern.as_ref(), Pattern::ZeroOrMore(_)));
    }

    #[test]
    fn test_parse_one_or_more() {
        let schema_xml = r#"
            <element name="list" xmlns="http://relaxng.org/ns/structure/1.0">
                <oneOrMore>
                    <element name="item"><text/></element>
                </oneOrMore>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(matches!(pattern.as_ref(), Pattern::OneOrMore(_)));
    }

    #[test]
    fn test_parse_optional_pattern() {
        let schema_xml = r#"
            <element name="doc" xmlns="http://relaxng.org/ns/structure/1.0">
                <optional>
                    <attribute name="lang"/>
                </optional>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(matches!(pattern.as_ref(), Pattern::Group(_, _)));
    }

    #[test]
    fn test_parse_interleave_pattern() {
        let schema_xml = r#"
            <element name="doc" xmlns="http://relaxng.org/ns/structure/1.0">
                <interleave>
                    <element name="a"><text/></element>
                    <element name="b"><text/></element>
                </interleave>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(matches!(pattern.as_ref(), Pattern::Interleave(_, _)));
    }

    #[test]
    fn test_parse_value_pattern() {
        let schema_xml = r#"
            <element name="status" xmlns="http://relaxng.org/ns/structure/1.0">
                <value>active</value>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(matches!(pattern.as_ref(), Pattern::Value(v) if v == "active"));
    }

    #[test]
    fn test_parse_data_pattern() {
        let schema_xml = r#"
            <element name="count" xmlns="http://relaxng.org/ns/structure/1.0">
                <data type="integer"/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected Element pattern, got {:?}", schema.start);
        };
        assert!(
            matches!(pattern.as_ref(), Pattern::Data { datatype, .. } if datatype == "integer")
        );
    }

    // --- Validation tests ---

    #[test]
    fn test_validate_simple_element_with_text() {
        let schema_xml = r#"
            <element name="greeting" xmlns="http://relaxng.org/ns/structure/1.0">
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<greeting>Hello!</greeting>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_wrong_root_element() {
        let schema_xml = r#"
            <element name="greeting" xmlns="http://relaxng.org/ns/structure/1.0">
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<salutation>Hi</salutation>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_validate_missing_required_attribute() {
        let schema_xml = r#"
            <element name="person" xmlns="http://relaxng.org/ns/structure/1.0">
                <attribute name="id"/>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<person>John</person>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("attribute")),
            "expected attribute error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_validate_element_with_attribute() {
        let schema_xml = r#"
            <element name="person" xmlns="http://relaxng.org/ns/structure/1.0">
                <attribute name="id"/>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str(r#"<person id="42">John</person>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_unexpected_attribute() {
        let schema_xml = r#"
            <element name="item" xmlns="http://relaxng.org/ns/structure/1.0">
                <empty/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str(r#"<item extra="oops"/>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("unexpected attribute")),
            "expected unexpected attribute error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_validate_choice_first_alternative() {
        let schema_xml = r#"
            <element name="value" xmlns="http://relaxng.org/ns/structure/1.0">
                <choice>
                    <element name="a"><text/></element>
                    <element name="b"><text/></element>
                </choice>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<value><a>hello</a></value>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_choice_second_alternative() {
        let schema_xml = r#"
            <element name="value" xmlns="http://relaxng.org/ns/structure/1.0">
                <choice>
                    <element name="a"><text/></element>
                    <element name="b"><text/></element>
                </choice>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<value><b>hello</b></value>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_choice_invalid() {
        let schema_xml = r#"
            <element name="value" xmlns="http://relaxng.org/ns/structure/1.0">
                <choice>
                    <element name="a"><text/></element>
                    <element name="b"><text/></element>
                </choice>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<value><c>hello</c></value>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_validate_zero_or_more_empty() {
        let schema_xml = r#"
            <element name="list" xmlns="http://relaxng.org/ns/structure/1.0">
                <zeroOrMore>
                    <element name="item"><text/></element>
                </zeroOrMore>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<list/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_zero_or_more_multiple() {
        let schema_xml = r#"
            <element name="list" xmlns="http://relaxng.org/ns/structure/1.0">
                <zeroOrMore>
                    <element name="item"><text/></element>
                </zeroOrMore>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc =
            Document::parse_str("<list><item>a</item><item>b</item><item>c</item></list>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_one_or_more_empty_fails() {
        let schema_xml = r#"
            <element name="list" xmlns="http://relaxng.org/ns/structure/1.0">
                <oneOrMore>
                    <element name="item"><text/></element>
                </oneOrMore>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<list/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_validate_one_or_more_with_items() {
        let schema_xml = r#"
            <element name="list" xmlns="http://relaxng.org/ns/structure/1.0">
                <oneOrMore>
                    <element name="item"><text/></element>
                </oneOrMore>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<list><item>a</item><item>b</item></list>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_optional_present() {
        let schema_xml = r#"
            <element name="doc" xmlns="http://relaxng.org/ns/structure/1.0">
                <optional>
                    <attribute name="lang"/>
                </optional>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str(r#"<doc lang="en">Hello</doc>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_optional_absent() {
        let schema_xml = r#"
            <element name="doc" xmlns="http://relaxng.org/ns/structure/1.0">
                <optional>
                    <attribute name="lang"/>
                </optional>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<doc>Hello</doc>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_interleave_any_order() {
        let schema_xml = r#"
            <element name="doc" xmlns="http://relaxng.org/ns/structure/1.0">
                <interleave>
                    <element name="a"><text/></element>
                    <element name="b"><text/></element>
                </interleave>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();

        // Order 1: a then b
        let doc1 = Document::parse_str("<doc><a>1</a><b>2</b></doc>").unwrap();
        let r1 = validate(&doc1, &schema);
        assert!(r1.is_valid, "a,b order failed: {:?}", r1.errors);

        // Order 2: b then a
        let doc2 = Document::parse_str("<doc><b>2</b><a>1</a></doc>").unwrap();
        let r2 = validate(&doc2, &schema);
        assert!(r2.is_valid, "b,a order failed: {:?}", r2.errors);
    }

    #[test]
    fn test_validate_ref_define_resolution() {
        let schema_xml = r#"
            <grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start>
                    <ref name="root"/>
                </start>
                <define name="root">
                    <element name="root">
                        <ref name="content"/>
                    </element>
                </define>
                <define name="content">
                    <element name="child"><text/></element>
                </define>
            </grammar>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<root><child>hello</child></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_nested_elements() {
        let schema_xml = r#"
            <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                <element name="parent">
                    <element name="child">
                        <text/>
                    </element>
                </element>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<root><parent><child>text</child></parent></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_value_match() {
        let schema_xml = r#"
            <element name="status" xmlns="http://relaxng.org/ns/structure/1.0">
                <value>active</value>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();

        let doc1 = Document::parse_str("<status>active</status>").unwrap();
        let r1 = validate(&doc1, &schema);
        assert!(r1.is_valid, "errors: {:?}", r1.errors);

        let doc2 = Document::parse_str("<status>inactive</status>").unwrap();
        let r2 = validate(&doc2, &schema);
        assert!(!r2.is_valid);
    }

    #[test]
    fn test_validate_missing_element() {
        let schema_xml = r#"
            <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                <element name="required"><text/></element>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<root/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
        assert!(
            result.errors.iter().any(|e| e.message.contains("missing")),
            "expected missing element error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_validate_unexpected_element() {
        let schema_xml = r#"
            <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                <empty/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<root><surprise>oops</surprise></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_validate_empty_element() {
        let schema_xml = r#"
            <element name="br" xmlns="http://relaxng.org/ns/structure/1.0">
                <empty/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<br/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_attribute_value_mismatch() {
        let schema_xml = r#"
            <element name="item" xmlns="http://relaxng.org/ns/structure/1.0">
                <attribute name="type">
                    <value>book</value>
                </attribute>
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();

        // Correct value.
        let doc1 = Document::parse_str(r#"<item type="book">Title</item>"#).unwrap();
        let r1 = validate(&doc1, &schema);
        assert!(r1.is_valid, "errors: {:?}", r1.errors);

        // Wrong value.
        let doc2 = Document::parse_str(r#"<item type="dvd">Title</item>"#).unwrap();
        let r2 = validate(&doc2, &schema);
        assert!(!r2.is_valid);
    }

    #[test]
    fn test_validate_no_root_element() {
        let schema_xml = r#"
            <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                <text/>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();

        let doc = Document::new();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("no root element")),);
    }

    #[test]
    fn test_validate_sequence_of_elements() {
        let schema_xml = r#"
            <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                <element name="first"><text/></element>
                <element name="second"><text/></element>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();

        let doc = Document::parse_str("<root><first>a</first><second>b</second></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_sequence_wrong_order() {
        let schema_xml = r#"
            <element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                <element name="first"><text/></element>
                <element name="second"><text/></element>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();

        let doc = Document::parse_str("<root><second>b</second><first>a</first></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_pattern_display() {
        assert_eq!(Pattern::Empty.to_string(), "empty");
        assert_eq!(Pattern::Text.to_string(), "text");
        assert_eq!(Pattern::NotAllowed.to_string(), "notAllowed");
        assert_eq!(
            Pattern::Element {
                name: NameClass::Name {
                    ns: String::new(),
                    local: "div".to_string(),
                },
                pattern: Box::new(Pattern::Empty),
            }
            .to_string(),
            "element div"
        );
    }

    #[test]
    fn test_name_class_display() {
        assert_eq!(NameClass::AnyName.to_string(), "*");
        assert_eq!(
            NameClass::Name {
                ns: String::new(),
                local: "foo".to_string(),
            }
            .to_string(),
            "foo"
        );
        assert_eq!(
            NameClass::Name {
                ns: "http://example.com".to_string(),
                local: "foo".to_string(),
            }
            .to_string(),
            "{http://example.com}foo"
        );
    }

    #[test]
    fn test_schema_parse_error_display() {
        let err = SchemaParseError {
            message: "test error".to_string(),
        };
        assert_eq!(err.to_string(), "RelaxNG schema error: test error");
    }

    #[test]
    fn test_parse_complex_grammar_with_cross_refs() {
        let schema_xml = r#"
            <grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start>
                    <element name="addressBook">
                        <zeroOrMore>
                            <ref name="cardContent"/>
                        </zeroOrMore>
                    </element>
                </start>
                <define name="cardContent">
                    <element name="card">
                        <ref name="cardFields"/>
                    </element>
                </define>
                <define name="cardFields">
                    <element name="name"><text/></element>
                    <element name="email"><text/></element>
                </define>
            </grammar>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        assert!(schema.defines.contains_key("cardContent"));
        assert!(schema.defines.contains_key("cardFields"));

        let doc = Document::parse_str(
            "<addressBook>\
                <card><name>Alice</name><email>alice@example.com</email></card>\
                <card><name>Bob</name><email>bob@example.com</email></card>\
            </addressBook>",
        )
        .unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_validate_mixed_content() {
        let schema_xml = r#"
            <element name="p" xmlns="http://relaxng.org/ns/structure/1.0">
                <mixed>
                    <zeroOrMore>
                        <element name="b"><text/></element>
                    </zeroOrMore>
                </mixed>
            </element>
        "#;
        let schema = parse_relaxng(schema_xml).unwrap();
        let doc = Document::parse_str("<p>Hello <b>world</b> and more</p>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    // --- <div> grammar content (§4.11, issue #52) ---

    #[test]
    fn test_parse_grammar_define_inside_div() {
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start><ref name="root"/></start>
                <div>
                    <define name="root"><element name="root"><empty/></element></define>
                </div>
            </grammar>"#,
        )
        .unwrap();
        assert!(schema.defines.contains_key("root"));

        let doc = Document::parse_str("<root/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_parse_grammar_nested_divs() {
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <div>
                    <start><ref name="root"/></start>
                    <div>
                        <div>
                            <define name="root">
                                <element name="root"><ref name="child"/></element>
                            </define>
                        </div>
                        <define name="child"><element name="child"><text/></element></define>
                    </div>
                </div>
            </grammar>"#,
        )
        .unwrap();
        assert!(schema.defines.contains_key("root"));
        assert!(schema.defines.contains_key("child"));

        let doc = Document::parse_str("<root><child>x</child></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        let doc = Document::parse_str("<root><other/></root>").unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_grammar_div_inherits_ns() {
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start><ref name="root"/></start>
                <div ns="urn:example">
                    <define name="root"><element name="root"><empty/></element></define>
                </div>
            </grammar>"#,
        )
        .unwrap();

        let doc = Document::parse_str(r#"<root xmlns="urn:example"/>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        let doc = Document::parse_str("<root/>").unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_grammar_empty_div_without_start_is_error() {
        let err = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <div/>
            </grammar>"#,
        )
        .unwrap_err();
        assert!(err.message.contains("no <start>"), "{}", err.message);
    }

    // --- Foreign elements and attributes (§3, §4.1, issue #53) ---

    #[test]
    fn test_parse_relaxng_ignores_annotation_elements() {
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"
                        xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0">
                <a:documentation>Grammar-level docs</a:documentation>
                <start><ref name="root"/></start>
                <define name="root">
                    <a:documentation>some docs</a:documentation>
                    <element name="root">
                        <a:documentation>element docs</a:documentation>
                        <choice>
                            <a:documentation>choice docs</a:documentation>
                            <element name="a"><empty/></element>
                            <element name="b"><empty/></element>
                        </choice>
                    </element>
                </define>
            </grammar>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<root><b/></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        let doc = Document::parse_str("<root><c/></root>").unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_relaxng_ignores_foreign_element_content() {
        // A foreign element's subtree is ignored even if it contains
        // `RelaxNG`-namespaced elements.
        let schema = parse_relaxng(
            r#"<element name="root" xmlns="http://relaxng.org/ns/structure/1.0"
                        xmlns:x="urn:example:ext">
                <x:meta><element name="never"><empty/></element></x:meta>
                <empty/>
            </element>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<root/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        let doc = Document::parse_str("<root><never/></root>").unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_relaxng_ignores_annotations_in_name_classes() {
        let schema = parse_relaxng(
            r#"<element xmlns="http://relaxng.org/ns/structure/1.0"
                        xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0">
                <a:documentation>before the name class</a:documentation>
                <choice>
                    <a:documentation>name choice</a:documentation>
                    <name>x</name>
                    <name>y</name>
                </choice>
                <empty/>
            </element>"#,
        )
        .unwrap();

        let doc = Document::parse_str("<y/>").unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_parse_relaxng_ignores_foreign_attributes() {
        // `a:defaultValue` is a compatibility annotation; `x:name` and
        // `xmlns:ns` must not be mistaken for the `name`/`ns` attributes.
        let schema = parse_relaxng(
            r#"<element name="root" xmlns="http://relaxng.org/ns/structure/1.0"
                        xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                        xmlns:x="urn:example:ext"
                        xmlns:ns="urn:example:bogus">
                <optional>
                    <attribute name="mode" a:defaultValue="fast" x:name="bogus">
                        <text/>
                    </attribute>
                </optional>
            </element>"#,
        )
        .unwrap();

        let doc = Document::parse_str(r#"<root mode="slow"/>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_parse_relaxng_foreign_root_is_error() {
        let err = parse_relaxng(r#"<x:grammar xmlns:x="urn:example:not-rng"/>"#).unwrap_err();
        assert!(
            err.message.contains("not in the RELAX NG namespace"),
            "{}",
            err.message
        );
    }

    // --- QName resolution in names (§4.8, §4.10, issue #54) ---

    #[test]
    fn test_parse_name_class_from_element_xml_prefix_shorthand() {
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start>
                    <element name="root">
                        <attribute name="xml:lang"><text/></attribute>
                    </element>
                </start>
            </grammar>"#,
        )
        .unwrap();
        let Pattern::Element { pattern, .. } = &schema.start else {
            panic!("expected element pattern, got {:?}", schema.start);
        };
        let Pattern::Attribute { name, .. } = pattern.as_ref() else {
            panic!("expected attribute pattern, got {pattern:?}");
        };
        assert_eq!(
            name,
            &NameClass::Name {
                ns: XML_NS.to_string(),
                local: "lang".to_string(),
            }
        );

        let doc = Document::parse_str(r#"<root xml:lang="en"/>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        // An unqualified `lang` attribute is not `xml:lang`.
        let doc = Document::parse_str(r#"<root lang="en"/>"#).unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_name_class_from_element_declared_prefix() {
        // The schema and instance use different prefixes for the same URI.
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0" xmlns:x="urn:example">
                <start>
                    <element name="x:root">
                        <attribute name="x:id"><text/></attribute>
                    </element>
                </start>
            </grammar>"#,
        )
        .unwrap();

        let doc = Document::parse_str(r#"<e:root xmlns:e="urn:example" e:id="1"/>"#).unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);

        let doc = Document::parse_str(r#"<root id="1"/>"#).unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_name_element_qname_and_choice() {
        let schema = parse_relaxng(
            r#"<element xmlns="http://relaxng.org/ns/structure/1.0"
                        xmlns:x="urn:example">
                <name>x:root</name>
                <attribute>
                    <choice>
                        <name>xml:space</name>
                        <name>xml:base</name>
                    </choice>
                </attribute>
            </element>"#,
        )
        .unwrap();

        for attr in ["xml:space", "xml:base"] {
            let doc =
                Document::parse_str(&format!(r#"<root xmlns="urn:example" {attr}="v"/>"#)).unwrap();
            let result = validate(&doc, &schema);
            assert!(result.is_valid, "{attr}: errors: {:?}", result.errors);
        }

        let doc = Document::parse_str(r#"<root xmlns="urn:example" space="v"/>"#).unwrap();
        assert!(!validate(&doc, &schema).is_valid);
    }

    #[test]
    fn test_parse_name_class_from_element_attribute_does_not_inherit_ns() {
        // §4.8: the `name` shorthand on `<attribute>` without its own `ns`
        // names an unqualified attribute; elements do inherit `ns`.
        let schema = parse_relaxng(
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0" ns="urn:example">
                <start>
                    <element name="root">
                        <attribute name="id"><text/></attribute>
                        <attribute name="ref" ns="urn:other"><text/></attribute>
                    </element>
                </start>
            </grammar>"#,
        )
        .unwrap();

        let doc = Document::parse_str(
            r#"<root xmlns="urn:example" xmlns:o="urn:other" id="1" o:ref="2"/>"#,
        )
        .unwrap();
        let result = validate(&doc, &schema);
        assert!(result.is_valid, "errors: {:?}", result.errors);
    }

    #[test]
    fn test_resolve_qname_undeclared_prefix_is_error() {
        let err = parse_relaxng(
            r#"<element name="nope:root" xmlns="http://relaxng.org/ns/structure/1.0">
                <empty/>
            </element>"#,
        )
        .unwrap_err();
        assert!(
            err.message.contains("undeclared namespace prefix 'nope'"),
            "{}",
            err.message
        );
    }

    #[test]
    fn test_resolve_qname_malformed_is_error() {
        for name in [":root", "root:", "a:b:c", " "] {
            let schema = format!(
                r#"<element name="{name}" xmlns="http://relaxng.org/ns/structure/1.0"
                            xmlns:a="urn:a"><empty/></element>"#
            );
            assert!(
                parse_relaxng(&schema).is_err(),
                "name {name:?} should be rejected"
            );
        }
    }

    // --- combine attribute (§4.17) and ref recursion (§4.19), issue #55 ---

    fn grammar(body: &str) -> String {
        format!(r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">{body}</grammar>"#)
    }

    fn is_valid(schema: &RelaxNgSchema, xml: &str) -> bool {
        let doc = Document::parse_str(xml).unwrap();
        validate(&doc, schema).is_valid
    }

    #[test]
    fn test_parse_grammar_combine_choice() {
        let schema = parse_relaxng(&grammar(
            r#"<start><ref name="root"/></start>
               <define name="root"><element name="root"><ref name="item"/></element></define>
               <define name="item" combine="choice"><element name="a"><empty/></element></define>
               <define name="item" combine="choice"><element name="b"><empty/></element></define>
               <div>
                   <define name="item" combine="choice"><element name="c"><empty/></element></define>
               </div>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, "<root><a/></root>"));
        assert!(is_valid(&schema, "<root><b/></root>"));
        assert!(is_valid(&schema, "<root><c/></root>"));
        assert!(!is_valid(&schema, "<root><d/></root>"));
    }

    #[test]
    fn test_parse_grammar_combine_one_define_without_combine() {
        // §4.17 allows exactly one define of a name to omit `combine`.
        let schema = parse_relaxng(&grammar(
            r#"<start><element name="root"><ref name="item"/></element></start>
               <define name="item"><element name="a"><empty/></element></define>
               <define name="item" combine="choice"><element name="b"><empty/></element></define>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, "<root><a/></root>"));
        assert!(is_valid(&schema, "<root><b/></root>"));
    }

    #[test]
    fn test_parse_grammar_combine_interleave() {
        let schema = parse_relaxng(&grammar(
            r#"<start><element name="root"><ref name="content"/></element></start>
               <define name="content" combine="interleave"><element name="a"><empty/></element></define>
               <define name="content" combine="interleave"><element name="b"><empty/></element></define>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, "<root><a/><b/></root>"));
        assert!(is_valid(&schema, "<root><b/><a/></root>"));
        assert!(!is_valid(&schema, "<root><a/></root>"));
    }

    #[test]
    fn test_parse_grammar_combine_start() {
        let schema = parse_relaxng(&grammar(
            r#"<start combine="choice"><element name="a"><empty/></element></start>
               <start combine="choice"><element name="b"><empty/></element></start>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, "<a/>"));
        assert!(is_valid(&schema, "<b/>"));
        assert!(!is_valid(&schema, "<c/>"));
    }

    #[test]
    fn test_parse_grammar_combine_missing_is_error() {
        let err = parse_relaxng(&grammar(
            r#"<start><ref name="item"/></start>
               <define name="item"><element name="a"><empty/></element></define>
               <define name="item"><element name="b"><empty/></element></define>"#,
        ))
        .unwrap_err();
        assert!(
            err.message.contains("without a 'combine'"),
            "{}",
            err.message
        );

        let err = parse_relaxng(&grammar(
            r#"<start><element name="a"><empty/></element></start>
               <start><element name="b"><empty/></element></start>"#,
        ))
        .unwrap_err();
        assert!(err.message.contains("<start>"), "{}", err.message);
    }

    #[test]
    fn test_parse_grammar_combine_mismatch_is_error() {
        let err = parse_relaxng(&grammar(
            r#"<start><ref name="item"/></start>
               <define name="item" combine="choice"><element name="a"><empty/></element></define>
               <define name="item" combine="interleave"><element name="b"><empty/></element></define>"#,
        ))
        .unwrap_err();
        assert!(
            err.message.contains("conflicting 'combine'"),
            "{}",
            err.message
        );
    }

    #[test]
    fn test_parse_grammar_combine_invalid_value_is_error() {
        let err = parse_relaxng(&grammar(
            r#"<start><ref name="item"/></start>
               <define name="item" combine="group"><element name="a"><empty/></element></define>"#,
        ))
        .unwrap_err();
        assert!(err.message.contains("invalid 'combine'"), "{}", err.message);
    }

    #[test]
    fn test_check_ref_recursion_cycle_through_combined_defines_is_error() {
        // The second fragment makes `item` refer to itself outside any
        // element, which would recurse forever during validation.
        let err = parse_relaxng(&grammar(
            r#"<start><element name="root"><ref name="item"/></element></start>
               <define name="item" combine="choice"><element name="a"><empty/></element></define>
               <define name="item" combine="choice"><ref name="other"/></define>
               <define name="other"><optional><ref name="item"/></optional></define>"#,
        ))
        .unwrap_err();
        assert!(
            err.message.contains("illegal recursive reference"),
            "{}",
            err.message
        );

        let err = parse_relaxng(&grammar(
            r#"<start><ref name="self"/></start>
               <define name="self"><ref name="self"/></define>"#,
        ))
        .unwrap_err();
        assert!(
            err.message.contains("illegal recursive reference"),
            "{}",
            err.message
        );
    }

    #[test]
    fn test_check_ref_recursion_through_element_is_allowed() {
        let schema = parse_relaxng(&grammar(
            r#"<start><ref name="node"/></start>
               <define name="node">
                   <element name="node"><zeroOrMore><ref name="node"/></zeroOrMore></element>
               </define>"#,
        ))
        .unwrap();
        assert!(is_valid(
            &schema,
            "<node><node><node/></node><node/></node>"
        ));
        assert!(!is_valid(&schema, "<node><leaf/></node>"));
    }

    #[test]
    fn test_check_ref_recursion_ignores_unreachable_defines() {
        // §4.19 removes unreachable defines before checking for loops.
        let schema = parse_relaxng(&grammar(
            r#"<start><element name="root"><empty/></element></start>
               <define name="unused"><ref name="unused"/></define>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, "<root/>"));
    }

    #[test]
    fn test_check_ref_recursion_chain_depth_limit() {
        let chain = |len: usize| {
            let mut body =
                String::from(r#"<start><element name="root"><ref name="d0"/></element></start>"#);
            for i in 0..len {
                write!(
                    body,
                    r#"<define name="d{i}"><ref name="d{}"/></define>"#,
                    i + 1
                )
                .unwrap();
            }
            write!(body, r#"<define name="d{len}"><text/></define>"#).unwrap();
            grammar(&body)
        };

        let schema = parse_relaxng(&chain(MAX_REF_CHAIN_DEPTH - 1)).unwrap();
        assert!(is_valid(&schema, "<root>text</root>"));

        let err = parse_relaxng(&chain(MAX_REF_CHAIN_DEPTH + 10)).unwrap_err();
        assert!(err.message.contains("nested more than"), "{}", err.message);
    }

    #[test]
    fn test_parse_grammar_many_combined_defines() {
        // Thousands of combined fragments must not build a degenerate,
        // stack-overflowing pattern tree.
        let mut body =
            String::from(r#"<start><element name="root"><ref name="item"/></element></start>"#);
        for i in 0..5000 {
            write!(
                body,
                r#"<define name="item" combine="choice"><element name="e{i}"><empty/></element></define>"#
            )
            .unwrap();
        }
        let schema = parse_relaxng(&grammar(&body)).unwrap();
        assert!(is_valid(&schema, "<root><e0/></root>"));
        assert!(is_valid(&schema, "<root><e4999/></root>"));
        assert!(!is_valid(&schema, "<root><e5000/></root>"));
    }

    // --- Attributes reached through <ref> (issue #56) ---

    #[test]
    fn test_validate_attributes_via_ref_in_interleave() {
        let schema = parse_relaxng(&grammar(
            r#"<start>
                   <element name="root">
                       <interleave><ref name="a"/><ref name="b"/></interleave>
                   </element>
               </start>
               <define name="a"><optional><attribute name="id"><text/></attribute></optional></define>
               <define name="b"><optional><attribute name="class"><text/></attribute></optional></define>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, r#"<root id="x"/>"#));
        assert!(is_valid(&schema, r#"<root class="c"/>"#));
        assert!(is_valid(&schema, r#"<root class="c" id="x"/>"#));
        assert!(is_valid(&schema, "<root/>"));
        assert!(!is_valid(&schema, r#"<root style="s"/>"#));
    }

    #[test]
    fn test_validate_attributes_via_nested_refs() {
        // `interleave(interleave(ref a, ref b), ref c)`, with the attribute
        // group itself reached through another define, as in the HTML5
        // schema's `common.attrs`.
        let schema = parse_relaxng(&grammar(
            r#"<start>
                   <element name="root">
                       <ref name="common.attrs"/>
                       <ref name="body"/>
                   </element>
               </start>
               <define name="common.attrs">
                   <interleave>
                       <interleave><ref name="a"/><ref name="b"/></interleave>
                       <ref name="c"/>
                   </interleave>
               </define>
               <define name="a"><optional><attribute name="id"><text/></attribute></optional></define>
               <define name="b"><optional><attribute name="class"><text/></attribute></optional></define>
               <define name="c"><attribute name="lang"><text/></attribute></define>
               <define name="body"><zeroOrMore><element name="p"><text/></element></zeroOrMore></define>"#,
        ))
        .unwrap();
        assert!(is_valid(
            &schema,
            r#"<root lang="en" id="x"><p>hi</p></root>"#
        ));
        assert!(is_valid(&schema, r#"<root class="c" lang="en"/>"#));
        // `lang` is required.
        assert!(!is_valid(&schema, r#"<root id="x"/>"#));
        // Child content is still validated.
        assert!(!is_valid(&schema, r#"<root lang="en"><q/></root>"#));
    }

    #[test]
    fn test_validate_optional_ref_makes_attributes_optional() {
        let schema = parse_relaxng(&grammar(
            r#"<start>
                   <element name="root"><optional><ref name="pair"/></optional></element>
               </start>
               <define name="pair">
                   <attribute name="x"><text/></attribute>
                   <attribute name="y"><data type="integer"/></attribute>
               </define>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, "<root/>"));
        assert!(is_valid(&schema, r#"<root x="a" y="1"/>"#));
        // Value constraints still apply.
        assert!(!is_valid(&schema, r#"<root x="a" y="one"/>"#));
    }

    #[test]
    fn test_validate_ref_with_attributes_and_elements() {
        let schema = parse_relaxng(&grammar(
            r#"<start><element name="root"><ref name="content"/></element></start>
               <define name="content">
                   <attribute name="id"><text/></attribute>
                   <element name="child"><empty/></element>
               </define>"#,
        ))
        .unwrap();
        assert!(is_valid(&schema, r#"<root id="1"><child/></root>"#));
        assert!(!is_valid(&schema, r#"<root id="1"/>"#));
        assert!(!is_valid(&schema, "<root><child/></root>"));
    }

    #[test]
    fn test_validate_hand_built_cyclic_defines_terminates() {
        // Hand-built schemas bypass the parser's §4.19 check; a reference
        // cycle outside an element must not recurse forever.
        let mut defines = HashMap::new();
        defines.insert(
            "a".to_string(),
            Pattern::Group(
                Box::new(Pattern::Attribute {
                    name: NameClass::AnyName,
                    pattern: Box::new(Pattern::Text),
                }),
                Box::new(Pattern::Ref("a".to_string())),
            ),
        );
        let schema = RelaxNgSchema {
            start: Pattern::Element {
                name: NameClass::AnyName,
                pattern: Box::new(Pattern::Ref("a".to_string())),
            },
            defines,
        };
        assert!(!is_valid(&schema, r#"<root x="1"/>"#));
    }

    #[test]
    fn test_validate_hand_built_exponential_refs_bounded() {
        // d0 = group(ref d1, ref d1), d1 = group(ref d2, ref d2), ... would
        // expand to 2^40 attribute patterns if refs were inlined; compiled
        // defines are shared instead.
        let mut defines = HashMap::new();
        for i in 0..40 {
            let next = Pattern::Ref(format!("d{}", i + 1));
            defines.insert(
                format!("d{i}"),
                Pattern::Group(Box::new(next.clone()), Box::new(next)),
            );
        }
        defines.insert(
            "d40".to_string(),
            Pattern::Attribute {
                name: NameClass::AnyName,
                pattern: Box::new(Pattern::Text),
            },
        );
        let schema = RelaxNgSchema {
            start: Pattern::Element {
                name: NameClass::AnyName,
                pattern: Box::new(Pattern::Ref("d0".to_string())),
            },
            defines,
        };
        assert!(!is_valid(&schema, r#"<root a="1" b="2"/>"#));
    }

    // --- Derivative-based validation: performance and correctness ---

    /// An element whose content is an `interleave` of a repeated element
    /// and a required one. The previous matcher tried all 2^n ways of
    /// splitting n children between the two operands.
    fn interleave_schema() -> RelaxNgSchema {
        parse_relaxng(
            r#"<element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                 <zeroOrMore>
                   <element name="g">
                     <interleave>
                       <zeroOrMore><element name="a"><text/></element></zeroOrMore>
                       <element name="b"><empty/></element>
                     </interleave>
                   </element>
                 </zeroOrMore>
               </element>"#,
        )
        .unwrap()
    }

    #[test]
    fn test_validate_interleave_runtime_is_linear() {
        let schema = interleave_schema();
        // 200 groups of 19 children each: about 4 * 10^8 partition attempts
        // per group for the old subset-enumerating matcher.
        let group = format!("<g><b/>{}</g>", "<a>x</a>".repeat(18));
        let valid = format!("<root>{}</root>", group.repeat(200));
        // The same without `<b/>`, which must be rejected (the old matcher's
        // worst case: no partition succeeds).
        let invalid = valid.replacen("<b/>", "", 1);

        let start = std::time::Instant::now();
        assert!(is_valid(&schema, &valid));
        assert!(!is_valid(&schema, &invalid));
        let elapsed = start.elapsed();
        assert!(
            elapsed < std::time::Duration::from_secs(1),
            "validation took {elapsed:?}"
        );
    }

    fn binary_tree(depth: u32, out: &mut String) {
        out.push_str("<n>");
        if depth > 0 {
            binary_tree(depth - 1, out);
            binary_tree(depth - 1, out);
        }
        out.push_str("</n>");
    }

    #[test]
    fn test_validate_deep_nesting_runtime_is_linear() {
        let schema = parse_relaxng(&grammar(
            r#"<start><ref name="n"/></start>
               <define name="n">
                 <element name="n">
                   <zeroOrMore><ref name="n"/></zeroOrMore>
                   <optional><element name="leaf"><empty/></element></optional>
                 </element>
               </define>"#,
        ))
        .unwrap();
        // A complete binary tree of depth 12 (8191 elements).
        let mut xml = String::new();
        binary_tree(12, &mut xml);

        let start = std::time::Instant::now();
        assert!(is_valid(&schema, &xml));
        let elapsed = start.elapsed();
        assert!(
            elapsed < std::time::Duration::from_secs(1),
            "validation took {elapsed:?}"
        );
    }

    #[test]
    fn test_validate_interleave_more_than_twenty_children() {
        // The previous matcher fell back to a greedy split above 20
        // children, which cannot assign `<a/>` and `<b/>` to the group.
        let schema = parse_relaxng(
            r#"<element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                 <interleave>
                   <group>
                     <element name="a"><empty/></element>
                     <element name="b"><empty/></element>
                   </group>
                   <zeroOrMore><element name="c"><empty/></element></zeroOrMore>
                 </interleave>
               </element>"#,
        )
        .unwrap();
        let cs = "<c/>".repeat(25);
        assert!(is_valid(
            &schema,
            &format!("<root>{cs}<a/><c/><b/>{cs}</root>")
        ));
        assert!(!is_valid(
            &schema,
            &format!("<root>{cs}<b/><c/><a/>{cs}</root>")
        ));
    }

    #[test]
    fn test_validate_attributes_in_choice_of_groups() {
        // Miniature of libvirt.rng's `graphics` element: alternative
        // attribute sets in a `<choice>` of `<group>`s.
        let schema = parse_relaxng(
            r#"<element name="graphics" xmlns="http://relaxng.org/ns/structure/1.0">
                 <choice>
                   <group>
                     <attribute name="type"><value>sdl</value></attribute>
                     <optional><attribute name="display"/></optional>
                   </group>
                   <group>
                     <attribute name="type"><value>vnc</value></attribute>
                     <optional><attribute name="port"/></optional>
                   </group>
                 </choice>
               </element>"#,
        )
        .unwrap();
        assert!(is_valid(&schema, r#"<graphics type="vnc"/>"#));
        assert!(is_valid(&schema, r#"<graphics type="vnc" port="5900"/>"#));
        assert!(is_valid(&schema, r#"<graphics type="sdl" display=":0"/>"#));
        // Attributes from different alternatives cannot be mixed.
        assert!(!is_valid(&schema, r#"<graphics type="vnc" display=":0"/>"#));
        assert!(!is_valid(&schema, r#"<graphics type="rdp"/>"#));
        assert!(!is_valid(&schema, "<graphics/>"));
    }

    #[test]
    fn test_validate_text_split_by_comment_is_one_value() {
        let schema = parse_relaxng(
            r#"<element name="n" xmlns="http://relaxng.org/ns/structure/1.0">
                 <data type="integer"/>
               </element>"#,
        )
        .unwrap();
        assert!(is_valid(&schema, "<n>12<!-- c -->34</n>"));
        assert!(!is_valid(&schema, "<n>12<!-- c -->x</n>"));
    }

    #[test]
    fn test_validate_work_budget_exhaustion_is_an_error() {
        let schema = interleave_schema();
        let doc = Document::parse_str("<root><g><b/><a>x</a></g></root>").unwrap();
        assert!(validate_with_budget(&doc, &schema, 1_000).is_valid);

        let result = validate_with_budget(&doc, &schema, 5);
        assert!(!result.is_valid);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("work limit")),
            "{:?}",
            result.errors
        );
    }

    #[test]
    fn test_validate_reports_unexpected_element() {
        let schema = interleave_schema();
        let doc = Document::parse_str("<root><g><b/><z/></g></root>").unwrap();
        let result = validate(&doc, &schema);
        assert!(!result.is_valid);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("element <z> is not allowed here")),
            "{:?}",
            result.errors
        );
    }

    #[test]
    fn test_parse_element_name_class_child_then_content_choice() {
        // The first child is the name class; a later <choice> is content.
        let schema = parse_relaxng(
            r#"<element xmlns="http://relaxng.org/ns/structure/1.0">
                 <name>root</name>
                 <choice>
                   <element name="a"><empty/></element>
                   <element name="b"><empty/></element>
                 </choice>
               </element>"#,
        )
        .unwrap();
        assert!(is_valid(&schema, "<root><a/></root>"));
        assert!(is_valid(&schema, "<root><b/></root>"));
        assert!(!is_valid(&schema, "<root/>"));
    }

    #[test]
    fn test_parse_attribute_name_class_choice_then_content_choice() {
        let schema = parse_relaxng(
            r#"<element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
                 <attribute>
                   <choice><name>x</name><name>y</name></choice>
                   <choice><value>1</value><value>2</value></choice>
                 </attribute>
               </element>"#,
        )
        .unwrap();
        assert!(is_valid(&schema, r#"<root x="1"/>"#));
        assert!(is_valid(&schema, r#"<root y="2"/>"#));
        assert!(!is_valid(&schema, r#"<root x="3"/>"#));
        assert!(!is_valid(&schema, r#"<root z="1"/>"#));
    }

    #[test]
    fn test_parse_long_group_is_balanced() {
        // A long run of sibling patterns must not build a left-leaning chain
        // deep enough to overflow the stack in validation or `Drop`.
        let mut schema = String::from(
            r#"<element name="r" xmlns="http://relaxng.org/ns/structure/1.0"><group>"#,
        );
        for _ in 0..100_000 {
            schema.push_str(r#"<optional><element name="a"><empty/></element></optional>"#);
        }
        schema.push_str("</group></element>");
        let schema = parse_relaxng(&schema).unwrap();
        assert!(is_valid(&schema, "<r><a/></r>"));
    }
}
