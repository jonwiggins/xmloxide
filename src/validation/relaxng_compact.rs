//! `RelaxNG` Compact Syntax front end.
//!
//! This module parses schemas written in the RELAX NG Compact Syntax
//! (OASIS Committee Specification, 21 November 2002,
//! <https://relaxng.org/compact-20021121.html>; also ISO/IEC 19757-2
//! Annex C) — the non-XML notation usually stored in `.rnc` files:
//!
//! ```text
//! default namespace = "urn:example:addresses"
//!
//! start = addressBook
//! addressBook = element addressBook { card* }
//! card = element card { element name { text }, element email { text } }
//! ```
//!
//! # Design
//!
//! The compact specification defines the meaning of a compact schema by
//! its translation to the XML syntax. This module implements exactly that
//! translation: a hand-written tokenizer and recursive-descent parser turn
//! the compact text into an equivalent XML-syntax schema, which is then
//! handed to [`parse_relaxng`]. Every simplification step the XML-syntax
//! parser performs — `div` flattening, `combine` merging (`|=` / `&=`),
//! namespace inheritance, and the `ref`-recursion checks of `RelaxNG`
//! §4.19 — therefore applies to compact schemas unchanged, and both
//! syntaxes produce the same [`RelaxNgSchema`].
//!
//! Names are resolved while translating: every element, attribute, and
//! name-class name is emitted with an explicit `ns` attribute, so prefixes
//! declared with `namespace` never need to become XML namespace
//! declarations (which, unlike compact declarations, cannot bind a prefix
//! to the empty namespace).
//!
//! # Limitations
//!
//! - `include` and `external` are rejected with an error: the XML-syntax
//!   parser does not load external schemas (`<include>`, `<externalRef>`)
//!   either. Nested `grammar` patterns and `parent` references, which it
//!   also lacks, are likewise rejected.
//! - Annotations (`[ ... ]`, `>>` follow annotations, grammar-level
//!   annotation elements) and `##` documentation comments are parsed and
//!   discarded; they never affect validation.
//! - Datatype parameters and `-` exceptions on datatypes are translated
//!   faithfully, but the validator currently ignores them (as it does for
//!   the equivalent XML syntax).
//!
//! # Security
//!
//! Schemas may come from untrusted sources. Nesting (parentheses, element
//! bodies, name classes, datatype exceptions, `div`s) is limited to
//! [`MAX_NESTING_DEPTH`] levels, and malformed input of any kind yields a
//! [`SchemaParseError`] rather than a panic.

use std::collections::HashMap;

use super::relaxng::{parse_relaxng, RelaxNgSchema, SchemaParseError};
use crate::parser::input::{is_name_char, is_name_start_char, is_xml_char};

/// Maximum nesting depth accepted by the compact parser.
///
/// Each parenthesized group, element/attribute/list/mixed body, datatype
/// exception, name-class group, and `div` counts as a level. Deeper input
/// is rejected with an error instead of risking a stack overflow. The
/// limit is well below the XML parser's default element-depth limit, which
/// the translated schema must also respect.
pub const MAX_NESTING_DEPTH: usize = 128;

/// The `RelaxNG` structure namespace URI.
const RNG_NS: &str = "http://relaxng.org/ns/structure/1.0";

/// The namespace URI permanently bound to the `xml` prefix.
const XML_NS: &str = "http://www.w3.org/XML/1998/namespace";

/// The W3C XML Schema datatype library, predeclared as prefix `xsd`.
const XSD_DATATYPES: &str = "http://www.w3.org/2001/XMLSchema-datatypes";

/// Keywords of the compact syntax. Using one as an identifier (a `define`
/// or `ref` name) requires escaping it with a leading `\`.
const KEYWORDS: &[&str] = &[
    "attribute",
    "default",
    "datatypes",
    "div",
    "element",
    "empty",
    "external",
    "grammar",
    "include",
    "inherit",
    "list",
    "mixed",
    "namespace",
    "notAllowed",
    "parent",
    "start",
    "string",
    "text",
    "token",
];

fn is_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s)
}

/// Parses a RELAX NG Compact Syntax schema into a [`RelaxNgSchema`].
///
/// The schema is translated to the equivalent XML syntax (see
/// [`compact_to_xml`]) and compiled with
/// [`parse_relaxng`], so the result is
/// identical to parsing the XML form of the same schema and can be used
/// with [`validate`](super::relaxng::validate).
///
/// See <https://relaxng.org/compact-20021121.html>.
///
/// # Errors
///
/// Returns [`SchemaParseError`] if the input is not valid compact syntax
/// (with the line and column of the problem), uses an unsupported feature
/// (`include`, `external`, nested `grammar`, `parent`), nests deeper than
/// [`MAX_NESTING_DEPTH`], or fails the schema checks shared with the XML
/// syntax (e.g. a missing `start`, conflicting `combine` methods, or
/// illegal recursion).
///
/// # Examples
///
/// ```
/// use xmloxide::Document;
/// use xmloxide::validation::relaxng::validate;
/// use xmloxide::validation::relaxng_compact::parse_relaxng_compact;
///
/// let schema = parse_relaxng_compact(r#"
///     start = addressBook
///     addressBook = element addressBook { card* }
///     card = element card {
///         attribute id { xsd:integer }?,
///         element name { text },
///         element email { text }
///     }
/// "#).unwrap();
///
/// let doc = Document::parse_str(
///     "<addressBook><card id='1'><name>Jo</name><email>jo@example.com</email></card></addressBook>",
/// ).unwrap();
/// assert!(validate(&doc, &schema).is_valid);
///
/// let bad = Document::parse_str("<addressBook><card><name>Jo</name></card></addressBook>").unwrap();
/// assert!(!validate(&bad, &schema).is_valid);
/// ```
pub fn parse_relaxng_compact(input: &str) -> Result<RelaxNgSchema, SchemaParseError> {
    let xml = compact_to_xml(input)?;
    parse_relaxng(&xml)
}

/// Translates a RELAX NG Compact Syntax schema into the equivalent XML
/// syntax, as a string.
///
/// This is the translation [`parse_relaxng_compact`] performs before
/// compiling the schema; it is exposed for tooling and debugging (it plays
/// the role of `trang` for `.rnc` → `.rng`). Annotations and comments are
/// dropped, and every name carries an explicit `ns` attribute.
///
/// # Errors
///
/// Returns [`SchemaParseError`] if the input is not valid compact syntax or
/// uses an unsupported feature (see [`parse_relaxng_compact`]).
///
/// # Examples
///
/// ```
/// use xmloxide::validation::relaxng_compact::compact_to_xml;
///
/// let xml = compact_to_xml("element greeting { text }").unwrap();
/// assert!(xml.contains(r#"<element name="greeting" ns="">"#));
/// ```
pub fn compact_to_xml(input: &str) -> Result<String, SchemaParseError> {
    let chars = preprocess(input)?;
    let tokens = tokenize(&chars)?;
    let mut parser = Parser::new(tokens);
    parser.parse_top_level()
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

fn error_at(line: usize, col: usize, msg: impl std::fmt::Display) -> SchemaParseError {
    SchemaParseError {
        message: format!("compact syntax error at line {line}, column {col}: {msg}"),
    }
}

// ---------------------------------------------------------------------------
// Escape preprocessing (compact spec §"Lexical structure")
// ---------------------------------------------------------------------------

/// One input character after escape processing.
#[derive(Debug, Clone, Copy)]
struct Ch {
    c: char,
    /// Whether the character came from a `\x{...}` escape. Escaped
    /// characters never delimit string literals, so `\x{22}` and `\x{A}`
    /// can be used inside them.
    escaped: bool,
    line: usize,
    col: usize,
}

/// Replaces `\x{hex}` escapes with the characters they denote, normalizes
/// line ends to `\n`, and rejects characters that are not XML `Char`s.
fn preprocess(input: &str) -> Result<Vec<Ch>, SchemaParseError> {
    let input = input.strip_prefix('\u{FEFF}').unwrap_or(input);
    let chars: Vec<char> = input.chars().collect();
    let mut out = Vec::with_capacity(chars.len());
    let (mut line, mut col) = (1usize, 1usize);
    let mut i = 0;
    while let Some(&c) = chars.get(i) {
        if c == '\\' {
            if let Some((ch, len)) = parse_escape(&chars, i, line, col)? {
                out.push(Ch {
                    c: ch,
                    escaped: true,
                    line,
                    col,
                });
                col += len;
                i += len;
                continue;
            }
        }
        if c == '\r' || c == '\n' {
            out.push(Ch {
                c: '\n',
                escaped: false,
                line,
                col,
            });
            if c == '\r' && chars.get(i + 1) == Some(&'\n') {
                i += 1;
            }
            i += 1;
            line += 1;
            col = 1;
            continue;
        }
        if !is_xml_char(c) {
            return Err(error_at(
                line,
                col,
                format_args!("character U+{:04X} is not allowed", u32::from(c)),
            ));
        }
        out.push(Ch {
            c,
            escaped: false,
            line,
            col,
        });
        col += 1;
        i += 1;
    }
    Ok(out)
}

/// Parses an escape sequence `\x+{hex}` starting at `chars[start]` (a
/// backslash). Returns the character and the number of input characters
/// consumed, or `None` if the backslash does not start an escape (it may
/// be a keyword escape such as `\element`).
fn parse_escape(
    chars: &[char],
    start: usize,
    line: usize,
    col: usize,
) -> Result<Option<(char, usize)>, SchemaParseError> {
    let mut j = start + 1;
    while chars.get(j) == Some(&'x') {
        j += 1;
    }
    if j == start + 1 || chars.get(j) != Some(&'{') {
        return Ok(None);
    }
    let mut k = j + 1;
    let mut value: u32 = 0;
    let mut digits = 0usize;
    loop {
        match chars.get(k) {
            Some('}') => break,
            Some(d) => {
                let Some(v) = d.to_digit(16) else {
                    return Err(error_at(line, col, "malformed \\x{...} escape"));
                };
                value = value * 16 + v;
                digits += 1;
                if value > 0x0010_FFFF {
                    return Err(error_at(line, col, "\\x{...} escape is out of range"));
                }
            }
            None => return Err(error_at(line, col, "unterminated \\x{...} escape")),
        }
        k += 1;
    }
    if digits == 0 {
        return Err(error_at(line, col, "empty \\x{...} escape"));
    }
    match char::from_u32(value) {
        Some(ch) if is_xml_char(ch) => Ok(Some((ch, k + 1 - start))),
        _ => Err(error_at(
            line,
            col,
            format_args!("\\x{{{value:X}}} is not an XML character"),
        )),
    }
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    /// An unescaped `NCName`, possibly a keyword.
    Ident(String),
    /// A backslash-escaped `NCName` (never a keyword).
    EscIdent(String),
    /// A prefixed name `prefix:local`.
    CName(String, String),
    /// A namespace wildcard `prefix:*`.
    NsName(String),
    /// A string literal segment (before `~` concatenation).
    Literal(String),
    /// Punctuation or an operator.
    Punct(&'static str),
    Eof,
}

#[derive(Debug, Clone)]
struct Token {
    tok: Tok,
    line: usize,
    col: usize,
}

fn is_ncname_start(c: char) -> bool {
    c != ':' && is_name_start_char(c)
}

fn is_ncname_char(c: char) -> bool {
    c != ':' && is_name_char(c)
}

/// Splits the preprocessed input into tokens, dropping whitespace,
/// comments, and `##` documentation comments.
fn tokenize(chars: &[Ch]) -> Result<Vec<Token>, SchemaParseError> {
    let mut tokens = Vec::new();
    let mut i = 0;
    while let Some(&ch) = chars.get(i) {
        let (line, col) = (ch.line, ch.col);
        let push = |tokens: &mut Vec<Token>, tok| tokens.push(Token { tok, line, col });
        match ch.c {
            ' ' | '\t' | '\n' => i += 1,
            '#' => {
                while chars.get(i).is_some_and(|c| c.c != '\n') {
                    i += 1;
                }
            }
            '"' | '\'' if !ch.escaped => {
                let (s, next) = lex_literal(chars, i)?;
                push(&mut tokens, Tok::Literal(s));
                i = next;
            }
            '\\' => {
                let name_start = i + 1;
                if !chars.get(name_start).is_some_and(|c| is_ncname_start(c.c)) {
                    return Err(error_at(line, col, "'\\' must be followed by a name"));
                }
                let (name, next) = lex_ncname(chars, name_start);
                push(&mut tokens, Tok::EscIdent(name));
                i = next;
            }
            c if is_ncname_start(c) => {
                let (name, next) = lex_ncname(chars, i);
                i = next;
                if chars.get(i).is_some_and(|c| c.c == ':') {
                    match chars.get(i + 1) {
                        Some(c) if c.c == '*' => {
                            push(&mut tokens, Tok::NsName(name));
                            i += 2;
                        }
                        Some(c) if is_ncname_start(c.c) => {
                            let (local, next) = lex_ncname(chars, i + 1);
                            push(&mut tokens, Tok::CName(name, local));
                            i = next;
                        }
                        _ => {
                            return Err(error_at(
                                line,
                                col,
                                format_args!("expected a local name or '*' after \"{name}:\""),
                            ));
                        }
                    }
                } else {
                    push(&mut tokens, Tok::Ident(name));
                }
            }
            c => {
                let next = chars.get(i + 1).map(|n| n.c);
                let (punct, len): (&'static str, usize) = match (c, next) {
                    ('|', Some('=')) => ("|=", 2),
                    ('&', Some('=')) => ("&=", 2),
                    ('>', Some('>')) => (">>", 2),
                    ('{', _) => ("{", 1),
                    ('}', _) => ("}", 1),
                    ('(', _) => ("(", 1),
                    (')', _) => (")", 1),
                    ('[', _) => ("[", 1),
                    (']', _) => ("]", 1),
                    ('=', _) => ("=", 1),
                    (',', _) => (",", 1),
                    ('|', _) => ("|", 1),
                    ('&', _) => ("&", 1),
                    ('?', _) => ("?", 1),
                    ('*', _) => ("*", 1),
                    ('+', _) => ("+", 1),
                    ('-', _) => ("-", 1),
                    ('~', _) => ("~", 1),
                    _ => {
                        return Err(error_at(
                            line,
                            col,
                            format_args!("unexpected character {c:?}"),
                        ));
                    }
                };
                push(&mut tokens, Tok::Punct(punct));
                i += len;
            }
        }
    }
    let (line, col) = chars.last().map_or((1, 1), |c| (c.line, c.col + 1));
    tokens.push(Token {
        tok: Tok::Eof,
        line,
        col,
    });
    Ok(tokens)
}

/// Reads an `NCName` starting at `start` (which must be a name start
/// character). Returns the name and the index after it.
fn lex_ncname(chars: &[Ch], start: usize) -> (String, usize) {
    let mut name = String::new();
    let mut i = start;
    while let Some(c) = chars.get(i).filter(|c| is_ncname_char(c.c)) {
        name.push(c.c);
        i += 1;
    }
    (name, i)
}

/// Returns `true` if `chars[i]` is an unescaped `quote`.
fn is_quote(chars: &[Ch], i: usize, quote: char) -> bool {
    chars.get(i).is_some_and(|c| c.c == quote && !c.escaped)
}

/// Reads a string literal starting at `start` (an unescaped quote): either
/// `"..."` / `'...'` (no unescaped newlines) or the triple-quoted forms
/// `"""..."""` / `'''...'''` (which may span lines). Returns the contents
/// and the index after the closing delimiter.
fn lex_literal(chars: &[Ch], start: usize) -> Result<(String, usize), SchemaParseError> {
    let Some(&open) = chars.get(start) else {
        return Err(error_at(1, 1, "internal error: literal past end of input"));
    };
    let quote = open.c;
    let triple = is_quote(chars, start + 1, quote) && is_quote(chars, start + 2, quote);
    let mut s = String::new();
    let mut i = if triple { start + 3 } else { start + 1 };
    loop {
        let Some(&c) = chars.get(i) else {
            return Err(error_at(open.line, open.col, "unterminated string literal"));
        };
        if triple {
            if is_quote(chars, i, quote)
                && is_quote(chars, i + 1, quote)
                && is_quote(chars, i + 2, quote)
            {
                return Ok((s, i + 3));
            }
        } else if is_quote(chars, i, quote) {
            return Ok((s, i + 1));
        } else if c.c == '\n' && !c.escaped {
            return Err(error_at(
                c.line,
                c.col,
                "newline in string literal (use a triple-quoted literal or \\x{A})",
            ));
        }
        s.push(c.c);
        i += 1;
    }
}

// ---------------------------------------------------------------------------
// Name classes
// ---------------------------------------------------------------------------

/// A parsed name class, with all names already resolved to namespace URIs.
#[derive(Debug)]
enum NameClass {
    Name {
        ns: String,
        local: String,
    },
    NsName {
        ns: String,
        except: Option<Box<NameClass>>,
    },
    AnyName {
        except: Option<Box<NameClass>>,
    },
    Choice(Vec<NameClass>),
}

impl NameClass {
    /// Whether this name class contains `anyName` (or, with `ns_name`,
    /// also `nsName`) anywhere — used for the except restrictions of
    /// `RelaxNG` §7.1.6.
    fn contains_wildcard(&self, ns_name: bool) -> bool {
        match self {
            Self::Name { .. } => false,
            Self::AnyName { .. } => true,
            Self::NsName { except, .. } => {
                ns_name
                    || except
                        .as_ref()
                        .is_some_and(|e| e.contains_wildcard(ns_name))
            }
            Self::Choice(items) => items.iter().any(|nc| nc.contains_wildcard(ns_name)),
        }
    }

    fn write_xml(&self, out: &mut String) {
        match self {
            Self::Name { ns, local } => {
                out.push_str("<name ns=\"");
                push_escaped(out, ns);
                out.push_str("\">");
                push_escaped(out, local);
                out.push_str("</name>");
            }
            Self::NsName { ns, except } => {
                out.push_str("<nsName ns=\"");
                push_escaped(out, ns);
                out.push('"');
                write_except(out, "nsName", except.as_deref());
            }
            Self::AnyName { except } => {
                out.push_str("<anyName");
                write_except(out, "anyName", except.as_deref());
            }
            Self::Choice(items) => {
                out.push_str("<choice>");
                for nc in items {
                    nc.write_xml(out);
                }
                out.push_str("</choice>");
            }
        }
    }
}

/// Finishes an `<anyName` / `<nsName ...` start tag with an optional
/// `<except>` child.
fn write_except(out: &mut String, tag: &str, except: Option<&NameClass>) {
    match except {
        None => out.push_str("/>"),
        Some(e) => {
            out.push_str("><except>");
            e.write_xml(out);
            out.push_str("</except></");
            out.push_str(tag);
            out.push('>');
        }
    }
}

/// Escapes text for use in XML character data or attribute values.
fn push_escaped(out: &mut String, s: &str) {
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\t' => out.push_str("&#9;"),
            '\n' => out.push_str("&#10;"),
            '\r' => out.push_str("&#13;"),
            c => out.push(c),
        }
    }
}

/// Wraps `items` in `<tag>` unless there is exactly one.
fn wrap_all(tag: &str, mut items: Vec<String>) -> String {
    if items.len() == 1 {
        return items.pop().unwrap_or_default();
    }
    let mut out = format!("<{tag}>");
    for item in items {
        out.push_str(&item);
    }
    out.push_str("</");
    out.push_str(tag);
    out.push('>');
    out
}

fn wrap(tag: &str, inner: &str) -> String {
    format!("<{tag}>{inner}</{tag}>")
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

/// Recursive-descent parser over the token stream, producing XML syntax.
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    /// Fallback returned when peeking past the end (never reached in
    /// practice, since the stream ends with [`Tok::Eof`]).
    eof: Token,
    depth: usize,
    /// Namespace prefix bindings (`namespace p = "..."`).
    namespaces: HashMap<String, String>,
    /// Prefixes the schema itself declared (to detect duplicates).
    declared_namespaces: Vec<String>,
    /// The default namespace for unprefixed element names.
    default_ns: String,
    default_declared: bool,
    /// Datatype library prefix bindings (`datatypes p = "..."`).
    datatypes: HashMap<String, String>,
    declared_datatypes: Vec<String>,
}

/// How a `start` or `define` combines with others of the same name.
fn combine_attr(assign: &str) -> &'static str {
    match assign {
        "|=" => " combine=\"choice\"",
        "&=" => " combine=\"interleave\"",
        _ => "",
    }
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        let eof = tokens.last().cloned().unwrap_or(Token {
            tok: Tok::Eof,
            line: 1,
            col: 1,
        });
        let mut namespaces = HashMap::new();
        namespaces.insert("xml".to_string(), XML_NS.to_string());
        let mut datatypes = HashMap::new();
        datatypes.insert("xsd".to_string(), XSD_DATATYPES.to_string());
        Self {
            tokens,
            pos: 0,
            eof: Token {
                tok: Tok::Eof,
                ..eof
            },
            depth: 0,
            namespaces,
            declared_namespaces: Vec::new(),
            default_ns: String::new(),
            default_declared: false,
            datatypes,
            declared_datatypes: Vec::new(),
        }
    }

    // --- token helpers ---

    fn peek_at(&self, offset: usize) -> &Token {
        self.tokens.get(self.pos + offset).unwrap_or(&self.eof)
    }

    fn peek(&self) -> &Token {
        self.peek_at(0)
    }

    fn next_token(&mut self) -> Token {
        let t = self.peek().clone();
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        t
    }

    fn is_punct(&self, p: &str) -> bool {
        matches!(&self.peek().tok, Tok::Punct(q) if *q == p)
    }

    fn error(&self, msg: impl std::fmt::Display) -> SchemaParseError {
        let t = self.peek();
        error_at(t.line, t.col, msg)
    }

    fn expect_punct(&mut self, p: &str) -> Result<(), SchemaParseError> {
        if self.is_punct(p) {
            self.pos += 1;
            Ok(())
        } else {
            Err(self.error(format_args!(
                "expected '{p}', found {}",
                describe(self.peek())
            )))
        }
    }

    fn enter(&mut self) -> Result<(), SchemaParseError> {
        if self.depth >= MAX_NESTING_DEPTH {
            return Err(self.error(format_args!(
                "schema is nested more than {MAX_NESTING_DEPTH} levels deep"
            )));
        }
        self.depth += 1;
        Ok(())
    }

    fn exit(&mut self) {
        self.depth = self.depth.saturating_sub(1);
    }

    /// Parses `literal ("~" literal)*`, given the first segment.
    fn continue_literal(&mut self, mut s: String) -> Result<String, SchemaParseError> {
        while self.is_punct("~") {
            self.pos += 1;
            match self.peek().tok.clone() {
                Tok::Literal(more) => {
                    self.pos += 1;
                    s.push_str(&more);
                }
                _ => return Err(self.error("expected a string literal after '~'")),
            }
        }
        Ok(s)
    }

    fn parse_literal(&mut self) -> Result<String, SchemaParseError> {
        match self.peek().tok.clone() {
            Tok::Literal(s) => {
                self.pos += 1;
                self.continue_literal(s)
            }
            _ => Err(self.error(format_args!(
                "expected a string literal, found {}",
                describe(self.peek())
            ))),
        }
    }

    /// Parses an `identifierOrKeyword` (e.g. a prefix or parameter name).
    fn parse_identifier_or_keyword(&mut self) -> Result<String, SchemaParseError> {
        match self.peek().tok.clone() {
            Tok::Ident(s) | Tok::EscIdent(s) => {
                self.pos += 1;
                Ok(s)
            }
            _ => Err(self.error(format_args!(
                "expected a name, found {}",
                describe(self.peek())
            ))),
        }
    }

    // --- annotations (parsed and discarded) ---

    /// Skips a bracketed annotation block `[ ... ]` at the current token,
    /// if there is one. Nesting is tracked with a counter, not recursion.
    fn skip_annotation_block(&mut self) -> Result<(), SchemaParseError> {
        if !self.is_punct("[") {
            return Ok(());
        }
        let open = self.next_token();
        let mut level = 1usize;
        while level > 0 {
            match self.next_token().tok {
                Tok::Punct("[") => level += 1,
                Tok::Punct("]") => level -= 1,
                Tok::Eof => {
                    return Err(error_at(open.line, open.col, "unterminated annotation '['"));
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Skips follow annotations: `>> name [ ... ]`.
    fn skip_follow_annotations(&mut self) -> Result<(), SchemaParseError> {
        while self.is_punct(">>") {
            self.pos += 1;
            match self.peek().tok {
                Tok::Ident(_) | Tok::EscIdent(_) | Tok::CName(..) => self.pos += 1,
                _ => return Err(self.error("expected an annotation element name after '>>'")),
            }
            if !self.is_punct("[") {
                return Err(self.error("expected '[' after annotation element name"));
            }
            self.skip_annotation_block()?;
        }
        Ok(())
    }

    /// Returns the token index just past a leading annotation block that
    /// starts at `self.pos`, without consuming anything.
    fn index_after_annotation(&self) -> usize {
        let mut i = self.pos;
        if !matches!(self.tokens.get(i).map(|t| &t.tok), Some(Tok::Punct("["))) {
            return i;
        }
        let mut level = 0usize;
        while let Some(t) = self.tokens.get(i) {
            i += 1;
            match t.tok {
                Tok::Punct("[") => level += 1,
                Tok::Punct("]") => {
                    level -= 1;
                    if level == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
        i
    }

    // --- top level ---

    fn parse_top_level(&mut self) -> Result<String, SchemaParseError> {
        self.parse_decls()?;
        let mut out = format!("<grammar xmlns=\"{RNG_NS}\">");
        if self.at_grammar_content() {
            self.parse_grammar_content(&mut out, false)?;
        } else {
            let pattern = self.parse_pattern()?;
            if !matches!(self.peek().tok, Tok::Eof) {
                return Err(self.error(format_args!(
                    "unexpected {} after the schema pattern",
                    describe(self.peek())
                )));
            }
            out.push_str("<start>");
            out.push_str(&pattern);
            out.push_str("</start>");
        }
        out.push_str("</grammar>");
        Ok(out)
    }

    /// Whether the body of the schema is grammar content (`start`,
    /// definitions, `div`, ...) rather than a single pattern.
    fn at_grammar_content(&self) -> bool {
        let i = self.index_after_annotation();
        let tok = self.tokens.get(i).map_or(&Tok::Eof, |t| &t.tok);
        let next = self.tokens.get(i + 1).map(|t| &t.tok);
        let next_is_assign_or_annotation =
            matches!(next, Some(Tok::Punct("=" | "|=" | "&=" | "[")));
        match tok {
            Tok::Eof => true,
            Tok::Ident(k) if matches!(k.as_str(), "start" | "div" | "include") => true,
            Tok::Ident(k) if !is_keyword(k) => next_is_assign_or_annotation,
            Tok::EscIdent(_) | Tok::CName(..) => next_is_assign_or_annotation,
            _ => false,
        }
    }

    /// Parses the `namespace`, `default namespace`, and `datatypes`
    /// declarations at the top of the schema.
    fn parse_decls(&mut self) -> Result<(), SchemaParseError> {
        loop {
            let Tok::Ident(kw) = self.peek().tok.clone() else {
                return Ok(());
            };
            match kw.as_str() {
                "namespace" => {
                    self.pos += 1;
                    let prefix = self.parse_identifier_or_keyword()?;
                    self.expect_punct("=")?;
                    let uri = self.parse_namespace_uri()?;
                    self.bind_namespace(prefix, uri)?;
                }
                "default" => {
                    self.pos += 1;
                    match &self.peek().tok {
                        Tok::Ident(k) if k == "namespace" => self.pos += 1,
                        _ => return Err(self.error("expected 'namespace' after 'default'")),
                    }
                    let prefix = if self.is_punct("=") {
                        None
                    } else {
                        Some(self.parse_identifier_or_keyword()?)
                    };
                    self.expect_punct("=")?;
                    let uri = self.parse_namespace_uri()?;
                    if self.default_declared {
                        return Err(self.error("duplicate default namespace declaration"));
                    }
                    self.default_declared = true;
                    self.default_ns.clone_from(&uri);
                    if let Some(prefix) = prefix {
                        self.bind_namespace(prefix, uri)?;
                    }
                }
                "datatypes" => {
                    self.pos += 1;
                    let prefix = self.parse_identifier_or_keyword()?;
                    self.expect_punct("=")?;
                    let uri = self.parse_literal()?;
                    if prefix == "xml" || self.declared_datatypes.contains(&prefix) {
                        return Err(self.error(format_args!(
                            "datatypes prefix \"{prefix}\" cannot be (re)declared"
                        )));
                    }
                    self.declared_datatypes.push(prefix.clone());
                    self.datatypes.insert(prefix, uri);
                }
                _ => return Ok(()),
            }
        }
    }

    /// Parses `literal | "inherit"` in a namespace declaration. At the top
    /// level of a schema, `inherit` denotes the empty (absent) namespace.
    fn parse_namespace_uri(&mut self) -> Result<String, SchemaParseError> {
        if matches!(&self.peek().tok, Tok::Ident(k) if k == "inherit") {
            self.pos += 1;
            return Ok(String::new());
        }
        self.parse_literal()
    }

    fn bind_namespace(&mut self, prefix: String, uri: String) -> Result<(), SchemaParseError> {
        if prefix == "xmlns" {
            return Err(self.error("the prefix \"xmlns\" cannot be declared"));
        }
        if (prefix == "xml") != (uri == XML_NS) {
            return Err(self.error(format_args!(
                "the prefix \"xml\" can only be bound to \"{XML_NS}\", and that namespace \
                 only to \"xml\""
            )));
        }
        if self.declared_namespaces.contains(&prefix) {
            return Err(self.error(format_args!(
                "duplicate declaration of namespace prefix \"{prefix}\""
            )));
        }
        self.declared_namespaces.push(prefix.clone());
        self.namespaces.insert(prefix, uri);
        Ok(())
    }

    // --- grammar content ---

    /// Parses grammar content (`start`, definitions, `div`, annotation
    /// elements) until end of input (top level) or `}` (`nested`).
    fn parse_grammar_content(
        &mut self,
        out: &mut String,
        nested: bool,
    ) -> Result<(), SchemaParseError> {
        loop {
            self.skip_annotation_block()?;
            let tok = self.peek().tok.clone();
            let next_is_assign = matches!(self.peek_at(1).tok, Tok::Punct("=" | "|=" | "&="));
            let next_is_annotation = matches!(self.peek_at(1).tok, Tok::Punct("["));
            let define_name = match &tok {
                Tok::Ident(name) if !is_keyword(name) => Some(name.clone()),
                Tok::EscIdent(name) => Some(name.clone()),
                _ => None,
            };
            match tok {
                Tok::Eof if !nested => return Ok(()),
                Tok::Punct("}") if nested => return Ok(()),
                Tok::Ident(k) if k == "start" => {
                    self.pos += 1;
                    let assign = self.parse_assign()?;
                    let pattern = self.parse_pattern()?;
                    out.push_str("<start");
                    out.push_str(combine_attr(assign));
                    out.push('>');
                    out.push_str(&pattern);
                    out.push_str("</start>");
                }
                Tok::Ident(k) if k == "div" => {
                    self.pos += 1;
                    self.expect_punct("{")?;
                    self.enter()?;
                    out.push_str("<div>");
                    self.parse_grammar_content(out, true)?;
                    out.push_str("</div>");
                    self.exit();
                    self.expect_punct("}")?;
                }
                Tok::Ident(k) if k == "include" => {
                    return Err(self.error(
                        "'include' is not supported (external schema loading is not \
                         implemented for RELAX NG)",
                    ));
                }
                Tok::Ident(_) | Tok::EscIdent(_) if next_is_assign && define_name.is_some() => {
                    let name = define_name.unwrap_or_default();
                    self.pos += 1;
                    let assign = self.parse_assign()?;
                    let pattern = self.parse_pattern()?;
                    out.push_str("<define name=\"");
                    push_escaped(out, &name);
                    out.push('"');
                    out.push_str(combine_attr(assign));
                    out.push('>');
                    out.push_str(&pattern);
                    out.push_str("</define>");
                }
                Tok::Ident(_) | Tok::EscIdent(_) | Tok::CName(..) if next_is_annotation => {
                    // Grammar-level annotation element: discarded.
                    self.pos += 1;
                    self.skip_annotation_block()?;
                }
                _ => {
                    return Err(self.error(format_args!(
                        "expected 'start', a definition, 'div', or {}, found {}",
                        if nested { "'}'" } else { "end of input" },
                        describe(self.peek())
                    )));
                }
            }
        }
    }

    /// Parses an assignment operator: `=`, `|=`, or `&=`.
    fn parse_assign(&mut self) -> Result<&'static str, SchemaParseError> {
        match self.peek().tok {
            Tok::Punct(p @ ("=" | "|=" | "&=")) => {
                self.pos += 1;
                Ok(p)
            }
            _ => Err(self.error(format_args!(
                "expected '=', '|=', or '&=', found {}",
                describe(self.peek())
            ))),
        }
    }

    // --- patterns ---

    /// Parses a pattern: particles joined by one kind of binary operator
    /// (`,` for group, `|` for choice, `&` for interleave). Mixing
    /// operators requires parentheses.
    fn parse_pattern(&mut self) -> Result<String, SchemaParseError> {
        self.enter()?;
        let mut items = vec![self.parse_particle()?];
        let mut op: Option<&'static str> = None;
        while let Tok::Punct(this @ ("," | "|" | "&")) = self.peek().tok {
            if let Some(prev) = op {
                if prev != this {
                    return Err(self.error(format_args!(
                        "cannot mix '{prev}' and '{this}' without parentheses"
                    )));
                }
            }
            op = Some(this);
            self.pos += 1;
            items.push(self.parse_particle()?);
        }
        self.exit();
        let tag = match op {
            Some(",") => "group",
            Some("&") => "interleave",
            _ => "choice",
        };
        Ok(wrap_all(tag, items))
    }

    /// Parses a primary pattern with an optional `?`, `*`, or `+`.
    fn parse_particle(&mut self) -> Result<String, SchemaParseError> {
        let primary = self.parse_primary()?;
        self.skip_follow_annotations()?;
        let tag = match self.peek().tok {
            Tok::Punct("?") => "optional",
            Tok::Punct("*") => "zeroOrMore",
            Tok::Punct("+") => "oneOrMore",
            _ => return Ok(primary),
        };
        self.pos += 1;
        self.skip_follow_annotations()?;
        Ok(wrap(tag, &primary))
    }

    fn parse_primary(&mut self) -> Result<String, SchemaParseError> {
        self.skip_annotation_block()?;
        self.enter()?;
        let result = self.parse_primary_inner();
        self.exit();
        result
    }

    fn parse_primary_inner(&mut self) -> Result<String, SchemaParseError> {
        let token = self.next_token();
        match token.tok {
            Tok::Ident(k) => match k.as_str() {
                "element" | "attribute" => self.parse_element_or_attribute(k == "attribute"),
                "list" | "mixed" => {
                    let body = self.parse_braced_pattern()?;
                    Ok(wrap(&k, &body))
                }
                "empty" | "text" | "notAllowed" => Ok(format!("<{k}/>")),
                "string" | "token" => self.parse_datatype("", &k),
                "parent" => Err(error_at(
                    token.line,
                    token.col,
                    "'parent' is not supported (nested grammars are not implemented)",
                )),
                "grammar" => Err(error_at(
                    token.line,
                    token.col,
                    "nested 'grammar' patterns are not supported",
                )),
                "external" => Err(error_at(
                    token.line,
                    token.col,
                    "'external' is not supported (external schema loading is not \
                     implemented for RELAX NG)",
                )),
                _ if is_keyword(&k) => Err(error_at(
                    token.line,
                    token.col,
                    format_args!(
                        "keyword '{k}' cannot be used here (write '\\{k}' to refer to a \
                         definition with that name)"
                    ),
                )),
                _ => Ok(ref_xml(&k)),
            },
            Tok::EscIdent(name) => Ok(ref_xml(&name)),
            Tok::CName(prefix, local) => {
                let Some(library) = self.datatypes.get(&prefix).cloned() else {
                    return Err(error_at(
                        token.line,
                        token.col,
                        format_args!("undeclared datatypes prefix \"{prefix}\""),
                    ));
                };
                self.parse_datatype(&library, &local)
            }
            Tok::Literal(s) => {
                let value = self.continue_literal(s)?;
                let mut out = String::from("<value>");
                push_escaped(&mut out, &value);
                out.push_str("</value>");
                Ok(out)
            }
            Tok::Punct("(") => {
                let inner = self.parse_pattern()?;
                self.expect_punct(")")?;
                Ok(inner)
            }
            _ => Err(error_at(
                token.line,
                token.col,
                format_args!("expected a pattern, found {}", describe(&token)),
            )),
        }
    }

    /// Parses `{ pattern }`.
    fn parse_braced_pattern(&mut self) -> Result<String, SchemaParseError> {
        self.expect_punct("{")?;
        let body = self.parse_pattern()?;
        self.expect_punct("}")?;
        Ok(body)
    }

    /// Parses the rest of `element nameClass { pattern }` or
    /// `attribute nameClass { pattern }`.
    fn parse_element_or_attribute(
        &mut self,
        is_attribute: bool,
    ) -> Result<String, SchemaParseError> {
        let name_class = self.parse_name_class(is_attribute)?;
        let body = self.parse_braced_pattern()?;
        let tag = if is_attribute { "attribute" } else { "element" };
        let mut out = format!("<{tag}");
        if let NameClass::Name { ns, local } = &name_class {
            out.push_str(" name=\"");
            push_escaped(&mut out, local);
            out.push_str("\" ns=\"");
            push_escaped(&mut out, ns);
            out.push_str("\">");
        } else {
            out.push('>');
            name_class.write_xml(&mut out);
        }
        out.push_str(&body);
        out.push_str("</");
        out.push_str(tag);
        out.push('>');
        Ok(out)
    }

    /// Parses the rest of a datatype pattern after its name: either a
    /// value (`xsd:integer "5"`) or data with optional parameters and
    /// except (`xsd:string { maxLength = "5" } - "x"`).
    fn parse_datatype(&mut self, library: &str, name: &str) -> Result<String, SchemaParseError> {
        let mut attrs = String::from(" type=\"");
        push_escaped(&mut attrs, name);
        attrs.push_str("\" datatypeLibrary=\"");
        push_escaped(&mut attrs, library);
        attrs.push('"');

        if matches!(self.peek().tok, Tok::Literal(_)) {
            let value = self.parse_literal()?;
            let mut out = format!("<value{attrs}>");
            push_escaped(&mut out, &value);
            out.push_str("</value>");
            return Ok(out);
        }

        let mut out = format!("<data{attrs}>");
        if self.is_punct("{") {
            self.pos += 1;
            loop {
                self.skip_annotation_block()?;
                if self.is_punct("}") {
                    self.pos += 1;
                    break;
                }
                let param = self.parse_identifier_or_keyword()?;
                self.expect_punct("=")?;
                let value = self.parse_literal()?;
                out.push_str("<param name=\"");
                push_escaped(&mut out, &param);
                out.push_str("\">");
                push_escaped(&mut out, &value);
                out.push_str("</param>");
            }
        }
        if self.is_punct("-") {
            self.pos += 1;
            let except = self.parse_primary()?;
            out.push_str("<except>");
            out.push_str(&except);
            out.push_str("</except>");
        }
        out.push_str("</data>");
        Ok(out)
    }

    // --- name classes ---

    /// Parses a name class: a simple name class, a `|` choice of them, or
    /// `*` / `prefix:*` with a `-` exception.
    fn parse_name_class(&mut self, is_attribute: bool) -> Result<NameClass, SchemaParseError> {
        self.enter()?;
        let result = self.parse_name_class_inner(is_attribute);
        self.exit();
        result
    }

    fn parse_name_class_inner(
        &mut self,
        is_attribute: bool,
    ) -> Result<NameClass, SchemaParseError> {
        let (first, bare_wildcard) = self.parse_simple_name_class(is_attribute)?;
        if bare_wildcard && self.is_punct("-") {
            let (line, col) = (self.peek().line, self.peek().col);
            self.pos += 1;
            let (except, _) = self.parse_simple_name_class(is_attribute)?;
            return match first {
                NameClass::AnyName { .. } => {
                    if except.contains_wildcard(false) {
                        return Err(error_at(
                            line,
                            col,
                            "the exception of '*' must not contain '*'",
                        ));
                    }
                    Ok(NameClass::AnyName {
                        except: Some(Box::new(except)),
                    })
                }
                NameClass::NsName { ns, .. } => {
                    if except.contains_wildcard(true) {
                        return Err(error_at(
                            line,
                            col,
                            "the exception of 'prefix:*' must not contain '*' or 'prefix:*'",
                        ));
                    }
                    Ok(NameClass::NsName {
                        ns,
                        except: Some(Box::new(except)),
                    })
                }
                _ => Err(error_at(line, col, "unexpected '-'")),
            };
        }
        let mut items = vec![first];
        while self.is_punct("|") {
            self.pos += 1;
            items.push(self.parse_simple_name_class(is_attribute)?.0);
        }
        if items.len() == 1 {
            if let Some(only) = items.pop() {
                return Ok(only);
            }
        }
        Ok(NameClass::Choice(items))
    }

    /// Parses a simple name class: a name, `prefix:*`, `*`, or a
    /// parenthesized name class. The flag is `true` for an unparenthesized
    /// `*` or `prefix:*` (which may take a `-` exception).
    fn parse_simple_name_class(
        &mut self,
        is_attribute: bool,
    ) -> Result<(NameClass, bool), SchemaParseError> {
        self.skip_annotation_block()?;
        let token = self.next_token();
        let result = match token.tok {
            Tok::Ident(local) | Tok::EscIdent(local) => {
                if is_attribute && local == "xmlns" {
                    return Err(error_at(
                        token.line,
                        token.col,
                        "an attribute cannot be named \"xmlns\"",
                    ));
                }
                let ns = if is_attribute {
                    String::new()
                } else {
                    self.default_ns.clone()
                };
                (NameClass::Name { ns, local }, false)
            }
            Tok::CName(prefix, local) => {
                let ns = self.lookup_namespace(&prefix, token.line, token.col)?;
                (NameClass::Name { ns, local }, false)
            }
            Tok::NsName(prefix) => {
                let ns = self.lookup_namespace(&prefix, token.line, token.col)?;
                (NameClass::NsName { ns, except: None }, true)
            }
            Tok::Punct("*") => (NameClass::AnyName { except: None }, true),
            Tok::Punct("(") => {
                let nc = self.parse_name_class(is_attribute)?;
                self.expect_punct(")")?;
                (nc, false)
            }
            _ => {
                return Err(error_at(
                    token.line,
                    token.col,
                    format_args!("expected a name class, found {}", describe(&token)),
                ));
            }
        };
        self.skip_follow_annotations()?;
        Ok(result)
    }

    fn lookup_namespace(
        &self,
        prefix: &str,
        line: usize,
        col: usize,
    ) -> Result<String, SchemaParseError> {
        self.namespaces.get(prefix).cloned().ok_or_else(|| {
            error_at(
                line,
                col,
                format_args!("undeclared namespace prefix \"{prefix}\""),
            )
        })
    }
}

fn ref_xml(name: &str) -> String {
    let mut out = String::from("<ref name=\"");
    push_escaped(&mut out, name);
    out.push_str("\"/>");
    out
}

/// Describes a token for error messages.
fn describe(token: &Token) -> String {
    match &token.tok {
        Tok::Ident(s) => format!("'{s}'"),
        Tok::EscIdent(s) => format!("'\\{s}'"),
        Tok::CName(p, l) => format!("'{p}:{l}'"),
        Tok::NsName(p) => format!("'{p}:*'"),
        Tok::Literal(_) => "a string literal".to_string(),
        Tok::Punct(p) => format!("'{p}'"),
        Tok::Eof => "end of input".to_string(),
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::validation::relaxng::{validate, NameClass as Nc, Pattern};
    use crate::Document;

    fn schema(src: &str) -> RelaxNgSchema {
        match parse_relaxng_compact(src) {
            Ok(s) => s,
            Err(e) => panic!("schema failed to parse: {e}\n{src}"),
        }
    }

    fn is_valid(schema: &RelaxNgSchema, xml: &str) -> bool {
        let doc = Document::parse_str(xml).unwrap();
        validate(&doc, schema).is_valid
    }

    fn err(src: &str) -> String {
        match parse_relaxng_compact(src) {
            Ok(s) => panic!("expected an error, got {s:?}"),
            Err(e) => e.message,
        }
    }

    // --- basic patterns ---

    #[test]
    fn test_parse_relaxng_compact_top_level_element() {
        let s = schema("element greeting { text }");
        assert!(is_valid(&s, "<greeting>hi</greeting>"));
        assert!(!is_valid(&s, "<other/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_top_level_choice() {
        let s = schema("element a { empty } | element b { empty }");
        assert!(is_valid(&s, "<a/>"));
        assert!(is_valid(&s, "<b/>"));
        assert!(!is_valid(&s, "<c/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_empty_text_not_allowed() {
        let s = schema("element r { element e { empty }, element t { text }? }");
        assert!(is_valid(&s, "<r><e/><t>x</t></r>"));
        assert!(!is_valid(&s, "<r><e>x</e></r>"));
        let s = schema("element r { notAllowed }");
        assert!(!is_valid(&s, "<r/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_group_operators() {
        let s =
            schema("element r { element a { empty }, element b { empty }+, element c { empty }* }");
        assert!(is_valid(&s, "<r><a/><b/></r>"));
        assert!(is_valid(&s, "<r><a/><b/><b/><c/><c/></r>"));
        assert!(!is_valid(&s, "<r><a/></r>"));
        assert!(!is_valid(&s, "<r><b/><a/></r>"));
    }

    #[test]
    fn test_parse_relaxng_compact_interleave() {
        let s = schema("element r { element a { empty } & element b { empty } }");
        assert!(is_valid(&s, "<r><a/><b/></r>"));
        assert!(is_valid(&s, "<r><b/><a/></r>"));
        assert!(!is_valid(&s, "<r><a/></r>"));
    }

    #[test]
    fn test_parse_relaxng_compact_mixed_operators_rejected() {
        let msg = err("element r { element a { empty }, element b { empty } | empty }");
        assert!(msg.contains("without parentheses"), "{msg}");
        // Parentheses make it legal.
        schema("element r { element a { empty }, (element b { empty } | empty) }");
    }

    #[test]
    fn test_parse_relaxng_compact_list_and_mixed() {
        let s = schema(r#"element r { attribute v { list { ("a" | "b")+ } } }"#);
        assert!(is_valid(&s, r#"<r v="a b a"/>"#));
        assert!(!is_valid(&s, r#"<r v="a c"/>"#));
        let s = schema("element p { mixed { element b { text }* } }");
        assert!(is_valid(&s, "<p>x <b>y</b> z</p>"));
        assert!(!is_valid(&s, "<p>x <i>y</i></p>"));
    }

    #[test]
    fn test_parse_relaxng_compact_attribute_values() {
        let s = schema(r#"element r { attribute kind { "html" | "text" } }"#);
        assert!(is_valid(&s, r#"<r kind="html"/>"#));
        assert!(!is_valid(&s, r#"<r kind="pdf"/>"#));
        assert!(!is_valid(&s, "<r/>"));
    }

    // --- grammars ---

    #[test]
    fn test_parse_relaxng_compact_grammar_defines() {
        let s = schema(
            "start = doc
             doc = element doc { item* }
             item = element item { text }",
        );
        assert_eq!(s.start, Pattern::Ref("doc".to_string()));
        assert!(s.defines.contains_key("item"));
        assert!(is_valid(&s, "<doc><item>1</item><item>2</item></doc>"));
        assert!(!is_valid(&s, "<doc><x/></doc>"));
    }

    #[test]
    fn test_parse_relaxng_compact_combine_choice() {
        let s = schema(
            "start = element r { inline* }
             inline = element a { empty }
             inline |= element b { empty }",
        );
        assert!(is_valid(&s, "<r><a/><b/></r>"));
        assert!(!is_valid(&s, "<r><c/></r>"));
    }

    #[test]
    fn test_parse_relaxng_compact_combine_interleave() {
        let s = schema(
            "start = element r { attrs }
             attrs &= attribute x { text }
             attrs &= attribute y { text }",
        );
        assert!(is_valid(&s, r#"<r x="1" y="2"/>"#));
        assert!(!is_valid(&s, r#"<r x="1"/>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_conflicting_combine_rejected() {
        let msg = err("start = element r { a }\n a |= empty\n a &= text");
        assert!(msg.contains("conflicting"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_start_combine() {
        let s = schema("start |= element a { empty }\nstart |= element b { empty }");
        assert!(is_valid(&s, "<a/>"));
        assert!(is_valid(&s, "<b/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_div() {
        let s = schema(
            "start = element r { x }
             div { x = element x { empty } div { } }",
        );
        assert!(is_valid(&s, "<r><x/></r>"));
    }

    #[test]
    fn test_parse_relaxng_compact_missing_start() {
        let msg = err("a = element a { empty }");
        assert!(msg.contains("no <start>"), "{msg}");
        let msg = err("");
        assert!(msg.contains("no <start>"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_illegal_recursion() {
        let msg = err("start = element r { a }\na = a | empty");
        assert!(msg.contains("recursive"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_escaped_keyword_identifier() {
        let s = schema(
            r"start = element r { \element }
              \element = element element { empty }",
        );
        assert!(is_valid(&s, "<r><element/></r>"));
        assert!(parse_relaxng_compact("start = element r { element }").is_err());
        let msg = err("start = element r { empty, text, string, list }");
        assert!(msg.contains("expected '{'"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_keyword_as_ref_rejected() {
        let msg = err("start = element r { inherit }");
        assert!(msg.contains("keyword 'inherit'"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_keyword_names() {
        // Element and attribute names may be keywords without escaping.
        let s = schema("element text { attribute list { text }, element empty { empty } }");
        assert!(is_valid(&s, r#"<text list="x"><empty/></text>"#));
    }

    // --- namespaces and datatypes ---

    #[test]
    fn test_parse_relaxng_compact_default_namespace() {
        let s = schema(
            r#"default namespace = "urn:x"
               element r { attribute a { text }, element c { empty } }"#,
        );
        assert!(is_valid(&s, r#"<r xmlns="urn:x" a="1"><c/></r>"#));
        let Pattern::Element { name, .. } = &s.start else {
            panic!("expected element, got {:?}", s.start);
        };
        assert_eq!(
            name,
            &Nc::Name {
                ns: "urn:x".to_string(),
                local: "r".to_string()
            }
        );
        assert!(!is_valid(&s, r#"<r a="1"><c/></r>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_prefixed_names() {
        let s = schema(
            r#"namespace x = "urn:x"
               default namespace y = "urn:y"
               element x:r { attribute x:a { text }, element y:c { empty }, element d { empty } }"#,
        );
        assert!(is_valid(
            &s,
            r#"<x:r xmlns:x="urn:x" xmlns="urn:y" x:a="1"><c/><d/></x:r>"#
        ));
        assert!(!is_valid(
            &s,
            r#"<x:r xmlns:x="urn:x" xmlns="urn:y" a="1"><c/><d/></x:r>"#
        ));
    }

    #[test]
    fn test_parse_relaxng_compact_prefix_bound_to_empty_namespace() {
        let s = schema(
            r#"default namespace = "urn:x"
               namespace local = ""
               element r { element local:c { empty } }"#,
        );
        assert!(is_valid(&s, r#"<r xmlns="urn:x"><c xmlns=""/></r>"#));
        assert!(!is_valid(&s, r#"<r xmlns="urn:x"><c/></r>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_inherit() {
        let s = schema(
            "default namespace = inherit
             namespace i = inherit
             element r { element i:c { empty } }",
        );
        assert!(is_valid(&s, "<r><c/></r>"));
    }

    #[test]
    fn test_parse_relaxng_compact_xml_prefix_predeclared() {
        let s = schema("element r { attribute xml:lang { text } }");
        assert!(is_valid(&s, r#"<r xml:lang="en"/>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_namespace_errors() {
        assert!(err("element p:r { empty }").contains("undeclared namespace prefix \"p\""));
        assert!(err(r#"namespace xml = "urn:other" element r { empty }"#).contains("xml"));
        assert!(err(r#"namespace xmlns = "urn:x" element r { empty }"#).contains("xmlns"));
        assert!(
            err(r#"namespace a = "urn:1" namespace a = "urn:2" element r { empty }"#)
                .contains("duplicate")
        );
        assert!(
            err(r#"default namespace = "a" default namespace = "b" element r { empty }"#)
                .contains("duplicate")
        );
        assert!(err("element r { attribute xmlns { text } }").contains("xmlns"));
    }

    #[test]
    fn test_parse_relaxng_compact_datatypes() {
        let s = schema("element r { attribute n { xsd:integer }, attribute b { xsd:boolean } }");
        assert!(is_valid(&s, r#"<r n="42" b="true"/>"#));
        assert!(!is_valid(&s, r#"<r n="x" b="true"/>"#));
        assert!(!is_valid(&s, r#"<r n="1" b="maybe"/>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_datatypes_declaration() {
        let xml = compact_to_xml(
            r#"datatypes d = "urn:my-types"
               element r { d:thing, string, token "a" }"#,
        )
        .unwrap();
        assert!(xml.contains(r#"<data type="thing" datatypeLibrary="urn:my-types">"#));
        assert!(xml.contains(r#"<data type="string" datatypeLibrary="">"#));
        assert!(xml.contains(r#"<value type="token" datatypeLibrary="">a</value>"#));
        assert!(err("element r { foo:bar }").contains("undeclared datatypes prefix"));
    }

    #[test]
    fn test_parse_relaxng_compact_datatype_value() {
        let xml = compact_to_xml(r#"element r { attribute n { xsd:integer "5" } }"#).unwrap();
        assert!(xml.contains(&format!(
            r#"<value type="integer" datatypeLibrary="{XSD_DATATYPES}">5</value>"#
        )));
        let s = schema(r#"element r { attribute n { xsd:integer "5" } }"#);
        assert!(is_valid(&s, r#"<r n="5"/>"#));
        assert!(!is_valid(&s, r#"<r n="6"/>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_datatype_params_and_except() {
        let src = r#"element r { xsd:string { maxLength = "5" [ a:x = "1" ] pattern = "[a-z]+" }
                       - ("bad" | "worse") }"#;
        let xml = compact_to_xml(src).unwrap();
        assert!(
            xml.contains(r#"<param name="maxLength">5</param>"#),
            "{xml}"
        );
        assert!(
            xml.contains(r#"<param name="pattern">[a-z]+</param>"#),
            "{xml}"
        );
        assert!(xml.contains("<except><choice><value>bad</value>"), "{xml}");
        schema(src);
    }

    // --- name classes ---

    #[test]
    fn test_parse_relaxng_compact_any_name() {
        let s = schema("element * { text }");
        assert!(is_valid(&s, "<whatever>x</whatever>"));
        assert!(is_valid(&s, r#"<q:x xmlns:q="urn:q"/>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_any_name_except() {
        let s = schema("element * - (a | b) { empty }");
        assert!(is_valid(&s, "<c/>"));
        assert!(!is_valid(&s, "<a/>"));
        assert!(!is_valid(&s, "<b/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_ns_name_except() {
        let s = schema(
            r#"namespace x = "urn:x"
               element x:* - x:bad { empty }"#,
        );
        assert!(is_valid(&s, r#"<good xmlns="urn:x"/>"#));
        assert!(!is_valid(&s, r#"<bad xmlns="urn:x"/>"#));
        assert!(!is_valid(&s, "<good/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_name_class_choice() {
        let s = schema("element (a | b) { element c { empty } | element d { empty } }");
        assert!(is_valid(&s, "<a><c/></a>"));
        assert!(is_valid(&s, "<b><d/></b>"));
        assert!(!is_valid(&s, "<b/>"));
        assert!(!is_valid(&s, "<e><c/></e>"));
    }

    #[test]
    fn test_parse_relaxng_compact_attribute_wildcard() {
        let s = schema("element r { attribute * - id { text }, attribute id { text } }");
        assert!(is_valid(&s, r#"<r id="1" a="x"/>"#));
        assert!(!is_valid(&s, r#"<r a="x"/>"#));
        assert!(!is_valid(&s, r#"<r id="1"/>"#));
    }

    #[test]
    fn test_parse_relaxng_compact_name_class_except_restrictions() {
        assert!(err("element * - * { empty }").contains("must not contain"));
        assert!(err("element * - (a | *) { empty }").contains("must not contain"));
        assert!(
            err("namespace x = \"urn:x\" element x:* - x:* { empty }").contains("must not contain")
        );
        // `-` only applies to an unparenthesized wildcard.
        assert!(parse_relaxng_compact("element (*) - a { empty }").is_err());
        assert!(parse_relaxng_compact("element a - b { empty }").is_err());
    }

    // --- literals, escapes, comments, annotations ---

    #[test]
    fn test_parse_relaxng_compact_literal_forms() {
        let xml = compact_to_xml(
            r#"element r { 'single' | "double" | "con" ~ 'cat' ~ """enated""" | '''it's''' }"#,
        )
        .unwrap();
        assert!(xml.contains("<value>single</value>"), "{xml}");
        assert!(xml.contains("<value>double</value>"), "{xml}");
        assert!(xml.contains("<value>concatenated</value>"), "{xml}");
        assert!(xml.contains("<value>it's</value>"), "{xml}");
    }

    #[test]
    fn test_parse_relaxng_compact_triple_quoted_newline() {
        let xml = compact_to_xml("element r { \"\"\"a\nb\"\"\" }").unwrap();
        assert!(xml.contains("<value>a&#10;b</value>"), "{xml}");
        let msg = err("element r { \"a\nb\" }");
        assert!(msg.contains("newline in string literal"), "{msg}");
        assert!(msg.contains("line 1, column 15"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_escapes() {
        // `\x{...}` works anywhere, including in names and literals; escaped
        // quotes and newlines do not end a literal.
        let xml = compact_to_xml(r#"element \x{61}b { "\x{22}q\x{A}" | "x\xxx{3C}y" }"#).unwrap();
        assert!(xml.contains(r#"<element name="ab""#), "{xml}");
        assert!(xml.contains("<value>&quot;q&#10;</value>"), "{xml}");
        assert!(xml.contains("<value>x&lt;y</value>"), "{xml}");
        assert!(err(r#"element r { "\x{D800}" }"#).contains("not an XML character"));
        assert!(err(r#"element r { "\x{110000}" }"#).contains("out of range"));
        assert!(err(r#"element r { "\x{FFFFFFFFFFFF}" }"#).contains("out of range"));
        assert!(err(r#"element r { "\x{}" }"#).contains("empty"));
        assert!(err(r#"element r { "\x{4g}" }"#).contains("malformed"));
        assert!(err(r#"element r { "\x{41"#).contains("unterminated"));
        assert!(err(r#"element r { "\x{0}" }"#).contains("not an XML character"));
    }

    #[test]
    fn test_parse_relaxng_compact_comments_and_documentation() {
        let s = schema(
            "# a comment\n\
             ## documentation for start\n\
             start = element r { # trailing comment\n text } # end\n",
        );
        assert!(is_valid(&s, "<r>x</r>"));
    }

    #[test]
    fn test_parse_relaxng_compact_annotations_ignored() {
        let s = schema(
            r#"namespace a = "http://relaxng.org/ns/compatibility/annotations/1.0"
               namespace dc = "http://purl.org/dc/elements/1.1/"
               dc:title [ "Example" ]
               [ a:documentation [ "The root" ] ]
               start = [ a:defaultValue = "x" ] element r {
                   [ dc:note [ "nested" [ "deeper" ] x = "1" ] ]
                   attribute v { text } >> a:documentation [ "value" ],
                   element [ a:x = "1" ] c { empty }* >> dc:creator [ ]
               }"#,
        );
        assert!(is_valid(&s, r#"<r v="1"><c/></r>"#));
        assert!(
            err("start = [ a:x = \"1\" element r { empty }").contains("unterminated annotation")
        );
    }

    #[test]
    fn test_parse_relaxng_compact_crlf_and_bom() {
        let s = schema("\u{FEFF}start =\r\n  element r {\r text }\r\n");
        assert!(is_valid(&s, "<r/>"));
    }

    #[test]
    fn test_parse_relaxng_compact_xml_special_chars_escaped() {
        let s = schema(r#"element r { attribute a { "<&>\x{22}" } }"#);
        assert!(is_valid(&s, r#"<r a="&lt;&amp;&gt;&quot;"/>"#));
    }

    // --- unsupported features ---

    #[test]
    fn test_parse_relaxng_compact_unsupported_features() {
        assert!(err(r#"include "other.rnc""#).contains("'include' is not supported"));
        assert!(err(r#"start = external "other.rnc""#).contains("'external' is not supported"));
        assert!(err("start = grammar { start = empty }").contains("nested 'grammar'"));
        assert!(err("start = element r { parent x }").contains("'parent' is not supported"));
    }

    // --- error reporting and robustness ---

    #[test]
    fn test_parse_relaxng_compact_error_location() {
        let msg = err("start = element r {\n  text,\n  ;\n}");
        assert!(msg.contains("line 3, column 3"), "{msg}");
    }

    #[test]
    fn test_parse_relaxng_compact_syntax_errors() {
        for src in [
            "element r { text",
            "element r text }",
            "element { text }",
            "start = ",
            "start element r { text }",
            "element r { text } extra",
            "element r { text }}",
            "element r { (text }",
            "element r { text, }",
            "element r { text ?? }",
            "element r { \"unterminated }",
            "element r { 'a' ~ }",
            "element r { \\ }",
            "element p: { empty }",
            "namespace = \"x\" element r { empty }",
            "default = \"x\" element r { empty }",
            "datatypes xml = \"x\" element r { empty }",
            "element r { xsd:string { maxLength } }",
            "element r { empty } >> foo",
            "div { start = element r { empty }",
            "element = element r { empty }",
            "element r { \u{1} }",
        ] {
            assert!(parse_relaxng_compact(src).is_err(), "should fail: {src:?}");
        }
    }

    #[test]
    fn test_parse_relaxng_compact_deep_parens_rejected() {
        for n in [MAX_NESTING_DEPTH, 10_000, 200_000] {
            let src = format!("element r {{ {}empty{} }}", "(".repeat(n), ")".repeat(n));
            let msg = err(&src);
            assert!(msg.contains("nested more than"), "{msg}");
        }
    }

    #[test]
    fn test_parse_relaxng_compact_deep_constructs_rejected() {
        let n = 100_000;
        let cases = [
            format!("{}empty{}", "element a { ".repeat(n), " }".repeat(n)),
            format!(
                "element r {{ {}empty{} }}",
                "list { ".repeat(n),
                " }".repeat(n)
            ),
            format!("element r {{ xsd:string{} }}", " - xsd:string".repeat(n)),
            format!("element {}a{} {{ empty }}", "(".repeat(n), ")".repeat(n)),
            format!("{}start = empty{}", "div { ".repeat(n), " }".repeat(n)),
        ];
        for src in &cases {
            let msg = err(src);
            assert!(msg.contains("nested more than"), "{msg}");
        }
    }

    #[test]
    fn test_parse_relaxng_compact_moderate_nesting_accepted() {
        let n = 60;
        let src = format!("{}empty{}", "element a { ".repeat(n), " }".repeat(n));
        let s = schema(&src);
        let doc = format!("{}{}", "<a>".repeat(n), "</a>".repeat(n));
        assert!(is_valid(&s, &doc));
    }

    #[test]
    fn test_parse_relaxng_compact_long_sequences() {
        // Long flat operator chains are not nesting and must not overflow.
        let items = vec!["element a { empty }?"; 50_000];
        let src = format!("element r {{ {} }}", items.join(", "));
        let s = schema(&src);
        assert!(is_valid(&s, "<r><a/></r>"));
        let src = format!("element r {{ {} }}", vec!["\"v\""; 50_000].join(" | "));
        schema(&src);
    }

    /// Feeds truncated and garbled variants of a schema that exercises
    /// every construct; parsing must return (Ok or Err) without panicking.
    #[test]
    fn test_parse_relaxng_compact_fuzz_truncated_and_garbled() {
        let src = r#"# comment
namespace a = "http://relaxng.org/ns/compatibility/annotations/1.0"
namespace x = "urn:x"
default namespace d = "urn:d"
datatypes dt = "http://www.w3.org/2001/XMLSchema-datatypes"
## doc
[ a:documentation [ "top" ] ]
start |= element doc { (item | other)*, attribute * - (id | x:*) { text }* }
item = element item { attribute id { dt:integer { minInclusive = "0" } - "13" }?, mixed { inline* } }
inline &= element b { text } >> a:note [ "n" ]
inline &= element \element { list { ("a" | 'b' ~ """c""")+ } }
div { x:ann [ x = "1" [ "deep" ] ] other = element x:* - x:bad { empty | notAllowed | string "s\x{41}" } }
"#;
        assert!(parse_relaxng_compact(src).is_ok());
        let chars: Vec<char> = src.chars().collect();
        // Every truncation point.
        for end in 0..=chars.len() {
            let truncated: String = chars[..end].iter().collect();
            let _ = parse_relaxng_compact(&truncated);
        }
        // Deterministic pseudo-random character edits (xorshift).
        let alphabet: Vec<char> = "{}()[]=|&,?*+-~>:\\\"'#\n x\u{e9}\u{1F600}"
            .chars()
            .collect();
        let mut state: u64 = 0x2545_F491_4F6C_DD1D;
        let mut rand = move |n: usize| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            usize::try_from(state % u64::try_from(n).unwrap()).unwrap()
        };
        for _ in 0..3000 {
            let mut mutated = chars.clone();
            for _ in 0..=rand(3) {
                let pos = rand(mutated.len());
                match rand(3) {
                    0 => mutated[pos] = alphabet[rand(alphabet.len())],
                    1 => {
                        mutated.remove(pos);
                    }
                    _ => mutated.insert(pos, alphabet[rand(alphabet.len())]),
                }
            }
            let s: String = mutated.iter().collect();
            let _ = parse_relaxng_compact(&s);
        }
    }
}
