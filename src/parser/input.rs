//! Shared low-level input handling for XML and HTML parsers.
//!
//! [`ParserInput`] encapsulates the raw byte stream, position tracking
//! (line, column, byte offset), and common parsing primitives such as
//! peeking, advancing, name parsing, and entity reference resolution.
//!
//! # Security
//!
//! `ParserInput` tracks nesting depth and entity expansion count to guard
//! against denial-of-service attacks:
//!
//! - **Depth limit**: prevents stack overflow from deeply nested elements.
//! - **Entity expansion limit**: defense-in-depth counter for entity
//!   references. Currently only the five built-in XML entities are
//!   supported (amp, lt, gt, apos, quot), so recursive expansion is
//!   impossible, but the limit protects against future DTD entity support
//!   and documents with an unreasonable number of references.
//! - **Name length limit**: prevents memory exhaustion from huge names.
//!
//! No external entity loading is performed (immune to XXE).

use std::collections::HashMap;

use crate::error::{ErrorSeverity, ParseDiagnostic, ParseError, SourceLocation};

// -------------------------------------------------------------------------
// Security defaults
// -------------------------------------------------------------------------

/// Default maximum element nesting depth.
pub(crate) const DEFAULT_MAX_DEPTH: u32 = 256;

/// Default maximum number of attributes on a single element.
pub(crate) const DEFAULT_MAX_ATTRIBUTES: u32 = 256;

/// Default maximum length (in bytes) of an attribute value.
pub(crate) const DEFAULT_MAX_ATTRIBUTE_LENGTH: usize = 10 * 1024 * 1024; // 10 MB

/// Default maximum length (in bytes) of a text node.
pub(crate) const DEFAULT_MAX_TEXT_LENGTH: usize = 10 * 1024 * 1024; // 10 MB

/// Default maximum length (in bytes) of an element or attribute name.
pub(crate) const DEFAULT_MAX_NAME_LENGTH: usize = 50_000;

/// Default maximum number of entity expansions per document.
pub(crate) const DEFAULT_MAX_ENTITY_EXPANSIONS: u32 = 10_000;

// -------------------------------------------------------------------------
// XML Name character classes (XML 1.0 §2.3)
// -------------------------------------------------------------------------

/// Returns `true` if `c` is a valid `Char` per XML 1.0 §2.2 `[2]`.
///
/// The XML 1.0 (Fifth Edition) `Char` production allows:
/// `#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]`
pub(crate) fn is_xml_char(c: char) -> bool {
    matches!(c as u32,
        0x09 | 0x0A | 0x0D | 0x20..=0xD7FF | 0xE000..=0xFFFD | 0x0001_0000..=0x0010_FFFF
    )
}

/// Returns `true` if `c` is a valid `NameStartChar` per XML 1.0 §2.3 `[4]`.
pub(crate) fn is_name_start_char(c: char) -> bool {
    matches!(c,
        ':' | 'A'..='Z' | '_' | 'a'..='z' |
        '\u{C0}'..='\u{D6}' | '\u{D8}'..='\u{F6}' | '\u{F8}'..='\u{2FF}' |
        '\u{370}'..='\u{37D}' | '\u{37F}'..='\u{1FFF}' |
        '\u{200C}'..='\u{200D}' | '\u{2070}'..='\u{218F}' |
        '\u{2C00}'..='\u{2FEF}' | '\u{3001}'..='\u{D7FF}' |
        '\u{F900}'..='\u{FDCF}' | '\u{FDF0}'..='\u{FFFD}' |
        '\u{10000}'..='\u{EFFFF}'
    )
}

/// Returns `true` if `c` is a valid `NameChar` per XML 1.0 §2.3 [4a].
pub(crate) fn is_name_char(c: char) -> bool {
    is_name_start_char(c)
        || matches!(c,
            '-' | '.' | '0'..='9' | '\u{B7}' |
            '\u{300}'..='\u{36F}' | '\u{203F}'..='\u{2040}'
        )
}

/// Splits a qualified name into optional prefix and local part.
///
/// `"foo:bar"` → `(Some("foo"), "bar")`
/// `"bar"` → `(None, "bar")`
pub(crate) fn split_name(name: &str) -> (Option<&str>, &str) {
    match name.find(':') {
        Some(pos) => (Some(&name[..pos]), &name[pos + 1..]),
        None => (None, name),
    }
}

/// Validates that a name is a legal `QName` per Namespaces in XML 1.0 §4.
///
/// A `QName` has at most one colon, and neither prefix nor local part may be
/// empty. Returns an error message if invalid, or `None` if valid.
pub(crate) fn validate_qname(name: &str) -> Option<&'static str> {
    let colon_count = name.chars().filter(|&c| c == ':').count();
    if colon_count > 1 {
        return Some("QName contains multiple colons");
    }
    if colon_count == 1 && (name.starts_with(':') || name.ends_with(':')) {
        return Some("QName has empty prefix or local part");
    }
    None
}

/// The well-known xmlns namespace URI.
pub(crate) const XMLNS_NAMESPACE: &str = "http://www.w3.org/2000/xmlns/";

/// Returns `true` if `c` is a valid `PubidChar` per XML 1.0 §2.3 `[13]`.
///
/// `PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]`
pub(crate) fn is_pubid_char(c: char) -> bool {
    matches!(c,
        ' ' | '\r' | '\n' |
        'a'..='z' | 'A'..='Z' | '0'..='9' |
        '-' | '\'' | '(' | ')' | '+' | ',' | '.' | '/' | ':' |
        '=' | '?' | ';' | '!' | '*' | '#' | '@' | '$' | '_' | '%'
    )
}

/// Validates that a string contains only valid `PubidChar`s.
///
/// Returns `None` if valid, or a descriptive error message if not.
pub(crate) fn validate_pubid(s: &str) -> Option<String> {
    for c in s.chars() {
        if !is_pubid_char(c) {
            return Some(format!(
                "invalid character '{}' (U+{:04X}) in public ID",
                c.escape_default(),
                c as u32
            ));
        }
    }
    None
}

// -------------------------------------------------------------------------
// Position checkpointing (for backtracking)
// -------------------------------------------------------------------------

/// A snapshot of the input position (byte offset, line, column).
///
/// Obtained via [`ParserInput::save_position`] and restored via
/// [`ParserInput::restore_position`]. Used by error-tolerant parsers
/// (e.g., the HTML parser) that need to backtrack when a speculative
/// parse fails.
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub(crate) struct SavedPosition {
    pos: usize,
    line: u32,
    column: u32,
}

// -------------------------------------------------------------------------
// ParserInput
// -------------------------------------------------------------------------

/// Shared low-level input state for all parsers.
///
/// Tracks the byte stream, position (line/column/offset), nesting depth,
/// entity expansion count, and accumulated diagnostics. All parsers
/// (tree-building, SAX, reader, HTML) compose this struct rather than
/// reimplementing input handling.
pub(crate) struct ParserInput<'a> {
    /// The input bytes (must be valid UTF-8).
    input: &'a [u8],

    /// Current byte offset in `input`.
    pos: usize,

    /// Current line number (1-based).
    line: u32,

    /// Current column number (1-based).
    column: u32,

    /// Current element nesting depth.
    depth: u32,

    /// Maximum allowed nesting depth.
    max_depth: u32,

    /// Maximum allowed name length in bytes.
    max_name_length: usize,

    /// Number of entity references expanded so far.
    entity_expansions: u32,

    /// Maximum allowed entity expansions.
    max_entity_expansions: u32,

    /// Whether the parser is in error-recovery mode.
    recover: bool,

    /// Accumulated diagnostics (warnings and recoverable errors).
    pub(crate) diagnostics: Vec<ParseDiagnostic>,

    /// Entity replacement values from the DTD internal subset.
    /// Populated after parsing `<!DOCTYPE root [ ... ]>`.
    pub(crate) entity_map: HashMap<String, String>,

    /// Set of entity names that are external (SYSTEM/PUBLIC without being
    /// internal). Used to enforce WFC: No External Entity References in
    /// attribute values.
    pub(crate) entity_external: std::collections::HashSet<String>,

    /// Whether the DTD internal subset contained parameter entity references.
    /// Per XML 1.0 §4.1 WFC: Entity Declared, undeclared entity references
    /// are only well-formedness errors if the document has no parameter entity
    /// references in the internal subset.
    pub(crate) has_pe_references: bool,

    /// Entities whose content production has already been validated.
    /// Prevents redundant re-validation on repeated references.
    validated_entities: std::collections::HashSet<String>,
}

impl<'a> ParserInput<'a> {
    /// Creates a new `ParserInput` from a UTF-8 string with default limits.
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
            line: 1,
            column: 1,
            depth: 0,
            max_depth: DEFAULT_MAX_DEPTH,
            max_name_length: DEFAULT_MAX_NAME_LENGTH,
            entity_expansions: 0,
            max_entity_expansions: DEFAULT_MAX_ENTITY_EXPANSIONS,
            recover: false,
            diagnostics: Vec::new(),
            entity_map: HashMap::new(),
            entity_external: std::collections::HashSet::new(),
            has_pe_references: false,
            validated_entities: std::collections::HashSet::new(),
        }
    }

    /// Sets the maximum nesting depth.
    pub fn set_max_depth(&mut self, max: u32) {
        self.max_depth = max;
    }

    /// Sets the maximum name length.
    pub fn set_max_name_length(&mut self, max: usize) {
        self.max_name_length = max;
    }

    /// Sets the maximum entity expansion count.
    pub fn set_max_entity_expansions(&mut self, max: u32) {
        self.max_entity_expansions = max;
    }

    /// Enables or disables error-recovery mode.
    pub fn set_recover(&mut self, recover: bool) {
        self.recover = recover;
    }

    /// Returns whether recovery mode is enabled.
    pub fn recover(&self) -> bool {
        self.recover
    }

    // -- Depth tracking --

    /// Increments the nesting depth. Returns an error if the limit is exceeded.
    pub fn increment_depth(&mut self) -> Result<(), ParseError> {
        self.depth += 1;
        if self.depth > self.max_depth {
            return Err(self.fatal(format!(
                "maximum nesting depth exceeded ({})",
                self.max_depth
            )));
        }
        Ok(())
    }

    /// Decrements the nesting depth (saturating at 0).
    pub fn decrement_depth(&mut self) {
        self.depth = self.depth.saturating_sub(1);
    }

    /// Returns the current nesting depth.
    #[allow(dead_code)]
    pub fn depth(&self) -> u32 {
        self.depth
    }

    // -- Position queries --

    /// Returns the current source location.
    pub fn location(&self) -> SourceLocation {
        SourceLocation {
            line: self.line,
            column: self.column,
            byte_offset: self.pos,
        }
    }

    /// Returns `true` if all input has been consumed.
    pub fn at_end(&self) -> bool {
        self.pos >= self.input.len()
    }

    /// Returns the current byte offset.
    #[allow(dead_code)]
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Returns a slice of the raw input bytes from `start` to `end`.
    #[allow(dead_code)]
    pub fn slice(&self, start: usize, end: usize) -> &[u8] {
        &self.input[start..end]
    }

    /// Returns the remaining input bytes from the current position.
    #[allow(dead_code)]
    pub fn remaining(&self) -> &[u8] {
        &self.input[self.pos..]
    }

    /// Saves the current position (byte offset, line, column) so it can be
    /// restored later with [`restore_position`]. This is useful for
    /// backtracking in error-tolerant parsers (e.g., the HTML parser).
    #[allow(dead_code)]
    pub fn save_position(&self) -> SavedPosition {
        SavedPosition {
            pos: self.pos,
            line: self.line,
            column: self.column,
        }
    }

    /// Restores a previously saved position. All progress since the
    /// [`save_position`] call is discarded.
    #[allow(dead_code)]
    pub fn restore_position(&mut self, saved: SavedPosition) {
        self.pos = saved.pos;
        self.line = saved.line;
        self.column = saved.column;
    }

    // -- Peek operations --

    /// Returns the byte at the current position without consuming it.
    pub fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    /// Returns the byte at `current_position + offset` without consuming.
    pub fn peek_at(&self, offset: usize) -> Option<u8> {
        self.input.get(self.pos + offset).copied()
    }

    /// Returns the character at the current position without consuming it.
    pub fn peek_char(&self) -> Option<char> {
        if self.at_end() {
            return None;
        }
        let remaining = &self.input[self.pos..];
        std::str::from_utf8(remaining)
            .ok()
            .and_then(|s| s.chars().next())
    }

    // -- Advance operations --

    /// Advances the position by `count` bytes, updating line/column.
    pub fn advance(&mut self, count: usize) {
        for _ in 0..count {
            if self.pos < self.input.len() {
                if self.input[self.pos] == b'\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                self.pos += 1;
            }
        }
    }

    /// Advances by one UTF-8 character, updating line/column.
    pub fn advance_char(&mut self, ch: char) {
        let len = ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.pos += len;
    }

    /// Consumes and returns the next byte, or returns an error at EOF.
    pub fn next_byte(&mut self) -> Result<u8, ParseError> {
        if self.at_end() {
            return Err(self.fatal("unexpected end of input"));
        }
        let b = self.input[self.pos];
        self.advance(1);
        Ok(b)
    }

    /// Consumes and returns the next character with `\r\n` normalization
    /// (XML 1.0 §2.11) and character validation (XML 1.0 §2.2).
    pub fn next_char(&mut self) -> Result<char, ParseError> {
        let ch = self
            .peek_char()
            .ok_or_else(|| self.fatal("unexpected end of input"))?;
        self.advance_char(ch);
        // Handle \r\n → \n normalization (XML 1.0 §2.11)
        if ch == '\r' {
            if self.peek() == Some(b'\n') {
                self.advance(1);
            }
            return Ok('\n');
        }
        // Validate against XML 1.0 §2.2 Char production
        if !is_xml_char(ch) {
            if self.recover {
                self.push_diagnostic(
                    ErrorSeverity::Error,
                    format!("invalid XML character: U+{:04X}", ch as u32),
                );
            } else {
                return Err(self.fatal(format!("invalid XML character: U+{:04X}", ch as u32)));
            }
        }
        Ok(ch)
    }

    // -- Expect operations --

    /// Consumes the next byte and asserts it matches `expected`.
    pub fn expect_byte(&mut self, expected: u8) -> Result<(), ParseError> {
        let b = self.next_byte()?;
        if b != expected {
            return Err(self.fatal(format!(
                "expected '{}', found '{}'",
                expected as char, b as char
            )));
        }
        Ok(())
    }

    /// Consumes bytes and asserts they match the `expected` sequence.
    pub fn expect_str(&mut self, expected: &[u8]) -> Result<(), ParseError> {
        for &b in expected {
            self.expect_byte(b)?;
        }
        Ok(())
    }

    // -- Lookahead --

    /// Returns `true` if the remaining input starts with `s`.
    pub fn looking_at(&self, s: &[u8]) -> bool {
        self.input[self.pos..].starts_with(s)
    }

    /// Case-insensitive lookahead check. Returns `true` if the remaining
    /// input starts with `expected` when compared case-insensitively (ASCII).
    #[allow(dead_code)]
    pub fn looking_at_ci(&self, expected: &[u8]) -> bool {
        if self.pos + expected.len() > self.input.len() {
            return false;
        }
        self.input[self.pos..self.pos + expected.len()].eq_ignore_ascii_case(expected)
    }

    // -- Whitespace --

    /// Skips whitespace characters. Returns `true` if any were consumed.
    pub fn skip_whitespace(&mut self) -> bool {
        let start = self.pos;
        while let Some(b) = self.peek() {
            if b == b' ' || b == b'\t' || b == b'\r' || b == b'\n' {
                self.advance(1);
            } else {
                break;
            }
        }
        self.pos > start
    }

    /// Skips whitespace, returning an error if none is found.
    pub fn skip_whitespace_required(&mut self) -> Result<(), ParseError> {
        if !self.skip_whitespace() {
            return Err(self.fatal("whitespace required"));
        }
        Ok(())
    }

    // -- Take while --

    /// Consumes bytes while `pred` returns `true` and returns the string.
    pub fn take_while(&mut self, pred: impl Fn(u8) -> bool) -> String {
        let start = self.pos;
        while let Some(b) = self.peek() {
            if pred(b) {
                self.advance(1);
            } else {
                break;
            }
        }
        String::from_utf8_lossy(&self.input[start..self.pos]).to_string()
    }

    // -- Name parsing (XML 1.0 §2.3) --

    /// Parses an XML `Name` per XML 1.0 §2.3 production `[5]`.
    ///
    /// A `Name` starts with a `NameStartChar` followed by zero or more
    /// `NameChar`s. Returns an error if the name is empty or starts with
    /// an invalid character.
    pub fn parse_name(&mut self) -> Result<String, ParseError> {
        let start = self.pos;
        if self.at_end() {
            return Err(self.fatal("expected name, found end of input"));
        }

        let first = self
            .peek_char()
            .ok_or_else(|| self.fatal("expected name"))?;
        if !is_name_start_char(first) {
            return Err(self.fatal(format!("invalid name start character: '{first}'")));
        }
        self.advance_char(first);

        while let Some(ch) = self.peek_char() {
            if is_name_char(ch) {
                self.advance_char(ch);
            } else {
                break;
            }
        }

        let len = self.pos - start;
        if len > self.max_name_length {
            return Err(self.fatal(format!(
                "name length ({len}) exceeds maximum ({})",
                self.max_name_length
            )));
        }

        let name = std::str::from_utf8(&self.input[start..self.pos])
            .map_err(|_| self.fatal("invalid UTF-8 in name"))?;
        Ok(name.to_string())
    }

    // -- Reference parsing (XML 1.0 §4.1) --

    /// Parses an entity or character reference (`&...;`).
    ///
    /// Handles the five built-in XML entities (`amp`, `lt`, `gt`, `apos`,
    /// `quot`) and decimal/hexadecimal character references.
    ///
    /// # Security
    ///
    /// Increments the entity expansion counter and returns an error if the
    /// limit is exceeded.
    pub fn parse_reference(&mut self) -> Result<String, ParseError> {
        self.entity_expansions += 1;
        if self.entity_expansions > self.max_entity_expansions {
            return Err(self.fatal(format!(
                "entity expansion limit exceeded ({})",
                self.max_entity_expansions
            )));
        }

        self.expect_byte(b'&')?;

        if self.peek() == Some(b'#') {
            // Character reference
            self.advance(1);
            let value = if self.peek() == Some(b'x') {
                // Hexadecimal
                self.advance(1);
                let hex = self.take_while(|b| b.is_ascii_hexdigit());
                if hex.is_empty() {
                    return Err(self.fatal("empty hex character reference"));
                }
                u32::from_str_radix(&hex, 16)
                    .map_err(|_| self.fatal("invalid hex character reference"))?
            } else {
                // Decimal
                let dec = self.take_while(|b| b.is_ascii_digit());
                if dec.is_empty() {
                    return Err(self.fatal("empty decimal character reference"));
                }
                dec.parse::<u32>()
                    .map_err(|_| self.fatal("invalid decimal character reference"))?
            };
            self.expect_byte(b';')?;

            let ch = char::from_u32(value)
                .ok_or_else(|| self.fatal(format!("invalid character reference: U+{value:04X}")))?;

            // Validate against XML 1.0 §2.2 Char production —
            // character references must also resolve to valid XML characters.
            if !is_xml_char(ch) {
                return Err(self.fatal(format!(
                    "character reference &#x{value:X}; does not refer to a valid XML character"
                )));
            }

            Ok(ch.to_string())
        } else {
            // Entity reference
            let name = self.parse_name()?;
            self.expect_byte(b';')?;

            match name.as_str() {
                "amp" => Ok("&".to_string()),
                "lt" => Ok("<".to_string()),
                "gt" => Ok(">".to_string()),
                "apos" => Ok("'".to_string()),
                "quot" => Ok("\"".to_string()),
                _ => {
                    // Check external entity reference
                    if self.entity_external.contains(&name) {
                        return Err(self.fatal(format!(
                            "reference to external entity '{name}' is not supported"
                        )));
                    }
                    // Look up in DTD-declared entities
                    if let Some(value) = self.entity_map.get(&name).cloned() {
                        // Validate entity replacement text matches the
                        // content production (XML 1.0 §4.3.2 WFC: Parsed
                        // Entity). Only checked when the entity is actually
                        // referenced, and only once per entity.
                        if !self.validated_entities.contains(&name) {
                            self.validated_entities.insert(name.clone());
                            self.validate_entity_content(&name, &value)?;
                        }
                        // Recursively expand entity references in the
                        // replacement text (XML 1.0 §4.4)
                        self.expand_entity_text(&value)
                    } else if self.recover || self.has_pe_references {
                        // Per XML 1.0 §4.1 WFC: Entity Declared, undeclared
                        // entities are not WF errors when the internal subset
                        // contains parameter entity references (the PE could
                        // have declared additional entities).
                        self.push_diagnostic(
                            ErrorSeverity::Warning,
                            format!("unknown entity reference: &{name};"),
                        );
                        Ok(String::new())
                    } else {
                        Err(self.fatal(format!("unknown entity reference: &{name};")))
                    }
                }
            }
        }
    }

    /// Expands entity and character references in entity replacement text.
    ///
    /// Per XML 1.0 §4.4, when an entity's replacement text is included,
    /// character references and entity references within it are resolved.
    /// This method performs that resolution recursively, using the entity
    /// map populated from the DTD.
    #[allow(clippy::too_many_lines)]
    fn expand_entity_text(&mut self, text: &str) -> Result<String, ParseError> {
        // Fast path — no references to expand
        if !text.contains('&') {
            return Ok(text.to_string());
        }

        let bytes = text.as_bytes();
        let mut result = String::with_capacity(text.len());
        let mut i = 0;
        let mut in_cdata = false;

        while i < bytes.len() {
            // Track CDATA sections — entity references inside CDATA are
            // literal text and should not be expanded.
            if !in_cdata && i + 8 < bytes.len() && &bytes[i..i + 9] == b"<![CDATA[" {
                in_cdata = true;
                result.push_str("<![CDATA[");
                i += 9;
                continue;
            }
            if in_cdata {
                if i + 2 < bytes.len() && &bytes[i..i + 3] == b"]]>" {
                    in_cdata = false;
                    result.push_str("]]>");
                    i += 3;
                } else {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                continue;
            }
            if bytes[i] == b'&' {
                i += 1;
                if i < bytes.len() && bytes[i] == b'#' {
                    // Character reference
                    i += 1;
                    let char_val = if i < bytes.len() && bytes[i] == b'x' {
                        i += 1;
                        let start = i;
                        while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
                            i += 1;
                        }
                        let hex = std::str::from_utf8(&bytes[start..i])
                            .map_err(|_| self.fatal("invalid UTF-8 in entity value"))?;
                        u32::from_str_radix(hex, 16)
                            .map_err(|_| self.fatal("invalid hex character reference"))?
                    } else {
                        let start = i;
                        while i < bytes.len() && bytes[i].is_ascii_digit() {
                            i += 1;
                        }
                        let dec = std::str::from_utf8(&bytes[start..i])
                            .map_err(|_| self.fatal("invalid UTF-8 in entity value"))?;
                        dec.parse::<u32>()
                            .map_err(|_| self.fatal("invalid decimal character reference"))?
                    };
                    if i >= bytes.len() || bytes[i] != b';' {
                        return Err(self.fatal("incomplete character reference in entity value"));
                    }
                    i += 1;
                    let ch = char::from_u32(char_val).ok_or_else(|| {
                        self.fatal(format!("invalid character reference: U+{char_val:04X}"))
                    })?;
                    result.push(ch);
                } else {
                    // Entity reference
                    let start = i;
                    while i < bytes.len() && bytes[i] != b';' {
                        i += 1;
                    }
                    if i >= bytes.len() {
                        return Err(self.fatal("incomplete entity reference in entity value"));
                    }
                    let name = std::str::from_utf8(&bytes[start..i])
                        .map_err(|_| self.fatal("invalid UTF-8 in entity name"))?;
                    i += 1; // skip ';'

                    self.entity_expansions += 1;
                    if self.entity_expansions > self.max_entity_expansions {
                        return Err(self.fatal(format!(
                            "entity expansion limit exceeded ({})",
                            self.max_entity_expansions
                        )));
                    }

                    let expanded = match name {
                        "amp" => "&".to_string(),
                        "lt" => "<".to_string(),
                        "gt" => ">".to_string(),
                        "apos" => "'".to_string(),
                        "quot" => "\"".to_string(),
                        _ => {
                            if self.entity_external.contains(name) {
                                return Err(self.fatal(format!(
                                    "reference to external entity '{name}' is not supported"
                                )));
                            }
                            if let Some(value) = self.entity_map.get(name).cloned() {
                                self.expand_entity_text(&value)?
                            } else if self.recover || self.has_pe_references {
                                self.push_diagnostic(
                                    ErrorSeverity::Warning,
                                    format!("unknown entity reference: &{name};"),
                                );
                                String::new()
                            } else {
                                return Err(
                                    self.fatal(format!("unknown entity reference: &{name};"))
                                );
                            }
                        }
                    };
                    result.push_str(&expanded);
                }
            } else {
                // Regular character — copy as-is, handling multi-byte UTF-8
                let start = i;
                i += 1;
                // Skip continuation bytes
                while i < bytes.len() && bytes[i] & 0xC0 == 0x80 {
                    i += 1;
                }
                if let Ok(s) = std::str::from_utf8(&bytes[start..i]) {
                    result.push_str(s);
                }
            }
        }

        Ok(result)
    }

    /// Validates that an entity's replacement text matches the XML content
    /// production (XML 1.0 §4.3.2 WFC: Parsed Entity).
    ///
    /// Expands character references in the raw entity value, replaces entity
    /// references with placeholders, then wraps in a synthetic root element
    /// and parses. If parsing fails, the entity is not well-formed.
    fn validate_entity_content(&self, name: &str, raw_value: &str) -> Result<(), ParseError> {
        let replacement = crate::validation::dtd::expand_char_refs_only(raw_value);

        // If no '<', the text is just character data — always valid content.
        if !replacement.contains('<') {
            return Ok(());
        }

        let sanitized = crate::validation::dtd::replace_entity_refs(&replacement);
        let wrapped = format!("<_r>{sanitized}</_r>");

        let options = super::ParseOptions::default();
        if super::parse_str_with_options(&wrapped, &options).is_err() {
            return Err(self.fatal(format!(
                "entity '{name}' replacement text is not \
                 well-formed XML content"
            )));
        }

        Ok(())
    }

    // -- Attribute value parsing (XML 1.0 §3.3.3) --

    /// Parses a quoted attribute value with entity resolution and
    /// whitespace normalization.
    pub fn parse_attribute_value(&mut self) -> Result<String, ParseError> {
        let quote = self.next_byte()?;
        if quote != b'"' && quote != b'\'' {
            return Err(self.fatal("attribute value must be quoted"));
        }

        let mut value = String::new();
        loop {
            if self.at_end() {
                return Err(self.fatal("unexpected end of input in attribute value"));
            }
            let b = self.peek().ok_or_else(|| self.fatal("unexpected end"))?;
            if b == quote {
                self.advance(1);
                break;
            }
            if b == b'&' {
                // Check if this is a DTD entity reference (not a built-in
                // or character reference) by peeking ahead.
                let is_custom_entity = self.input.get(self.pos + 1) != Some(&b'#')
                    && !self.input[self.pos + 1..].starts_with(b"lt;")
                    && !self.input[self.pos + 1..].starts_with(b"gt;")
                    && !self.input[self.pos + 1..].starts_with(b"amp;")
                    && !self.input[self.pos + 1..].starts_with(b"apos;")
                    && !self.input[self.pos + 1..].starts_with(b"quot;");
                let resolved = self.parse_reference()?;
                // WFC: No < in Attribute Values — entity replacement text
                // must not contain '<' (XML 1.0 §3.1). Built-in entity
                // &lt; is explicitly excluded from this constraint.
                if is_custom_entity && resolved.contains('<') {
                    return Err(
                        self.fatal("'<' not allowed in attribute values (from entity expansion)")
                    );
                }
                value.push_str(&resolved);
            } else if b == b'<' {
                return Err(self.fatal("'<' not allowed in attribute values"));
            } else {
                let ch = self.next_char()?;
                // Normalize whitespace in attribute values (XML 1.0 §3.3.3)
                if ch == '\r' || ch == '\n' || ch == '\t' {
                    value.push(' ');
                } else {
                    value.push(ch);
                }
            }
        }

        Ok(value)
    }

    /// Parses a simple quoted value (single or double quotes, no entity
    /// resolution).
    pub fn parse_quoted_value(&mut self) -> Result<String, ParseError> {
        let quote = self.next_byte()?;
        if quote != b'"' && quote != b'\'' {
            return Err(self.fatal("expected quoted value"));
        }
        let start = self.pos;
        while !self.at_end() && self.peek() != Some(quote) {
            self.advance(1);
        }
        let value = std::str::from_utf8(&self.input[start..self.pos])
            .map_err(|_| self.fatal("invalid UTF-8 in quoted value"))?
            .to_string();
        self.expect_byte(quote)?;
        Ok(value)
    }

    // -- Error helpers --

    /// Creates a fatal `ParseError` at the current location.
    pub fn fatal(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            location: self.location(),
            diagnostics: self.diagnostics.clone(),
        }
    }

    /// Appends a diagnostic (warning or recoverable error) to the list.
    pub fn push_diagnostic(&mut self, severity: ErrorSeverity, message: String) {
        self.diagnostics.push(ParseDiagnostic {
            severity,
            message,
            location: self.location(),
        });
    }
}

// -------------------------------------------------------------------------
// Namespace resolver
// -------------------------------------------------------------------------

/// Manages namespace scope for XML parsers.
///
/// Maintains a stack of namespace binding frames that mirrors the element
/// nesting. Each frame contains the `xmlns` declarations introduced on
/// that element. Namespace resolution walks the stack from top to bottom.
pub(crate) struct NamespaceResolver {
    /// Stack of namespace binding frames. Each frame is a `Vec` of
    /// `(prefix, uri)` pairs where a `None` prefix represents the
    /// default namespace.
    stack: Vec<Vec<(Option<String>, String)>>,
}

/// The well-known XML namespace URI, pre-bound to the `xml` prefix.
pub(crate) const XML_NAMESPACE: &str = "http://www.w3.org/XML/1998/namespace";

impl NamespaceResolver {
    /// Creates a new resolver with the `xml` prefix pre-bound.
    pub fn new() -> Self {
        let initial = vec![(Some("xml".to_string()), XML_NAMESPACE.to_string())];
        Self {
            stack: vec![initial],
        }
    }

    /// Pushes a new (empty) namespace scope for an element.
    pub fn push_scope(&mut self) {
        self.stack.push(Vec::new());
    }

    /// Pops the current namespace scope.
    pub fn pop_scope(&mut self) {
        self.stack.pop();
    }

    /// Binds a namespace prefix to a URI in the current scope.
    ///
    /// Use `prefix = None` for the default namespace (`xmlns="..."`).
    pub fn bind(&mut self, prefix: Option<String>, uri: String) {
        if let Some(frame) = self.stack.last_mut() {
            frame.push((prefix, uri));
        }
    }

    /// Resolves a namespace prefix to its URI.
    ///
    /// Walks the stack from top to bottom, returning the first match.
    /// Use `prefix = None` to resolve the default namespace.
    pub fn resolve(&self, prefix: Option<&str>) -> Option<&str> {
        for frame in self.stack.iter().rev() {
            for (p, uri) in frame.iter().rev() {
                let matches = match (prefix, p.as_deref()) {
                    (None, None) => true,
                    (Some(a), Some(b)) => a == b,
                    _ => false,
                };
                if matches {
                    if uri.is_empty() {
                        // xmlns="" undeclares the default namespace
                        return None;
                    }
                    return Some(uri.as_str());
                }
            }
        }
        None
    }
}

// -------------------------------------------------------------------------
// Common XML parsing helpers
// -------------------------------------------------------------------------

/// Parses an XML comment (`<!-- ... -->`), returning the content text.
///
/// The opening `<!--` must not have been consumed yet.
///
/// See XML 1.0 §2.5 production `[15]`.
pub(crate) fn parse_comment_content(input: &mut ParserInput<'_>) -> Result<String, ParseError> {
    input.expect_str(b"<!--")?;
    let mut content = String::new();

    loop {
        if input.at_end() {
            return Err(input.fatal("unexpected end of input in comment"));
        }
        if input.looking_at(b"-->") {
            input.advance(3);
            break;
        }
        // XML 1.0 forbids -- inside comments
        if input.looking_at(b"--") {
            if input.recover() {
                input.push_diagnostic(
                    ErrorSeverity::Error,
                    "'--' not allowed inside comments".to_string(),
                );
                content.push_str("--");
                input.advance(2);
            } else {
                return Err(input.fatal("'--' not allowed inside comments"));
            }
        } else {
            let ch = input.next_char()?;
            content.push(ch);
        }
    }

    Ok(content)
}

/// Parses a CDATA section (`<![CDATA[ ... ]]>`), returning the content text.
///
/// The opening `<![CDATA[` must not have been consumed yet.
///
/// See XML 1.0 §2.7 production `[18]`.
pub(crate) fn parse_cdata_content(input: &mut ParserInput<'_>) -> Result<String, ParseError> {
    input.expect_str(b"<![CDATA[")?;
    let mut content = String::new();

    loop {
        if input.at_end() {
            return Err(input.fatal("unexpected end of input in CDATA section"));
        }
        if input.looking_at(b"]]>") {
            input.advance(3);
            break;
        }
        let ch = input.next_char()?;
        content.push(ch);
    }

    Ok(content)
}

/// Parses a processing instruction (`<?target data?>`), returning
/// `(target, optional_data)`.
///
/// The opening `<?` must not have been consumed yet.
///
/// See XML 1.0 §2.6 production `[16]`.
pub(crate) fn parse_pi_content(
    input: &mut ParserInput<'_>,
) -> Result<(String, Option<String>), ParseError> {
    input.expect_str(b"<?")?;
    let target = input.parse_name()?;

    // "xml" (case-insensitive) is reserved for the XML declaration
    if target.eq_ignore_ascii_case("xml") {
        return Err(input.fatal("PI target 'xml' is reserved"));
    }

    // Namespaces in XML 1.0 §3: PI targets must be NCNames (no colons).
    if target.contains(':') {
        return Err(input.fatal("PI target must not contain a colon"));
    }

    let data = if input.skip_whitespace() {
        let mut data = String::new();
        loop {
            if input.at_end() {
                return Err(input.fatal("unexpected end of input in processing instruction"));
            }
            if input.looking_at(b"?>") {
                input.advance(2);
                break;
            }
            let ch = input.next_char()?;
            data.push(ch);
        }
        if data.is_empty() {
            None
        } else {
            Some(data)
        }
    } else {
        input.expect_str(b"?>")?;
        None
    };

    Ok((target, data))
}

/// Parsed XML declaration data.
#[derive(Debug, Clone)]
pub(crate) struct XmlDeclaration {
    /// XML version (e.g. `"1.0"`).
    pub version: String,
    /// Optional encoding declaration.
    pub encoding: Option<String>,
    /// Optional standalone declaration.
    pub standalone: Option<bool>,
}

/// Parses an XML declaration (`<?xml version="1.0" ...?>`), returning the
/// parsed version, encoding, and standalone values.
///
/// The opening `<?xml ` must not have been consumed yet (but should be
/// verified by the caller via `looking_at`).
///
/// See XML 1.0 §2.8 production `[23]`.
pub(crate) fn parse_xml_decl(input: &mut ParserInput<'_>) -> Result<XmlDeclaration, ParseError> {
    input.expect_str(b"<?xml")?;
    input.skip_whitespace_required()?;

    // version is required
    input.expect_str(b"version")?;
    input.skip_whitespace();
    input.expect_byte(b'=')?;
    input.skip_whitespace();
    let version = input.parse_quoted_value()?;

    // XML 1.0 §2.8: VersionNum ::= '1.' [0-9]+
    if !is_valid_version_num(&version) {
        return Err(input.fatal(format!("invalid version number: '{version}'")));
    }

    // encoding is optional
    let had_ws = input.skip_whitespace();
    let encoding = if input.looking_at(b"encoding") {
        if !had_ws {
            return Err(input.fatal("whitespace required before encoding"));
        }
        input.expect_str(b"encoding")?;
        input.skip_whitespace();
        input.expect_byte(b'=')?;
        input.skip_whitespace();
        let enc = input.parse_quoted_value()?;

        // XML 1.0 §4.3.3: EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
        if !is_valid_encoding_name(&enc) {
            return Err(input.fatal(format!("invalid encoding name: '{enc}'")));
        }

        Some(enc)
    } else {
        None
    };

    // standalone is optional
    // If encoding was present, we need fresh whitespace before standalone.
    // If encoding was absent, the whitespace consumed when looking for
    // encoding already separates version from standalone.
    let had_ws2 = input.skip_whitespace() || (encoding.is_none() && had_ws);
    let standalone = if input.looking_at(b"standalone") {
        if !had_ws2 {
            return Err(input.fatal("whitespace required before standalone"));
        }
        input.expect_str(b"standalone")?;
        input.skip_whitespace();
        input.expect_byte(b'=')?;
        input.skip_whitespace();
        let val = input.parse_quoted_value()?;
        match val.as_str() {
            "yes" => Some(true),
            "no" => Some(false),
            _ => return Err(input.fatal("standalone must be 'yes' or 'no'")),
        }
    } else {
        None
    };

    input.skip_whitespace();
    input.expect_str(b"?>")?;

    Ok(XmlDeclaration {
        version,
        encoding,
        standalone,
    })
}

/// Validates an XML version number per XML 1.0 §2.8.
///
/// `VersionNum ::= '1.' [0-9]+`
fn is_valid_version_num(s: &str) -> bool {
    if let Some(rest) = s.strip_prefix("1.") {
        !rest.is_empty() && rest.bytes().all(|b| b.is_ascii_digit())
    } else {
        false
    }
}

/// Validates an encoding name per XML 1.0 §4.3.3.
///
/// `EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*`
fn is_valid_encoding_name(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() {
        return false;
    }
    if !bytes[0].is_ascii_alphabetic() {
        return false;
    }
    bytes[1..]
        .iter()
        .all(|&b| b.is_ascii_alphanumeric() || b == b'.' || b == b'_' || b == b'-')
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_peek_and_advance() {
        let mut input = ParserInput::new("abc");
        assert_eq!(input.peek(), Some(b'a'));
        assert_eq!(input.peek_at(1), Some(b'b'));
        input.advance(1);
        assert_eq!(input.peek(), Some(b'b'));
        input.advance(2);
        assert!(input.at_end());
    }

    #[test]
    fn test_line_column_tracking() {
        let mut input = ParserInput::new("ab\ncd");
        assert_eq!(input.location().line, 1);
        assert_eq!(input.location().column, 1);
        input.advance(2); // past "ab"
        assert_eq!(input.location().column, 3);
        input.advance(1); // past "\n"
        assert_eq!(input.location().line, 2);
        assert_eq!(input.location().column, 1);
    }

    #[test]
    fn test_next_char_cr_normalization() {
        let mut input = ParserInput::new("a\r\nb");
        assert_eq!(input.next_char().unwrap(), 'a');
        assert_eq!(input.next_char().unwrap(), '\n'); // \r\n → \n
        assert_eq!(input.next_char().unwrap(), 'b');
    }

    #[test]
    fn test_parse_name() {
        let mut input = ParserInput::new("foo:bar ");
        let name = input.parse_name().unwrap();
        assert_eq!(name, "foo:bar");
    }

    #[test]
    fn test_parse_name_length_limit() {
        let long_name = "a".repeat(100);
        let mut input = ParserInput::new(&long_name);
        input.set_max_name_length(50);
        let result = input.parse_name();
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("name length"));
    }

    #[test]
    fn test_parse_reference_builtin() {
        let mut input = ParserInput::new("&amp;");
        assert_eq!(input.parse_reference().unwrap(), "&");

        let mut input = ParserInput::new("&lt;");
        assert_eq!(input.parse_reference().unwrap(), "<");

        let mut input = ParserInput::new("&gt;");
        assert_eq!(input.parse_reference().unwrap(), ">");

        let mut input = ParserInput::new("&apos;");
        assert_eq!(input.parse_reference().unwrap(), "'");

        let mut input = ParserInput::new("&quot;");
        assert_eq!(input.parse_reference().unwrap(), "\"");
    }

    #[test]
    fn test_parse_reference_char_decimal() {
        let mut input = ParserInput::new("&#65;");
        assert_eq!(input.parse_reference().unwrap(), "A");
    }

    #[test]
    fn test_parse_reference_char_hex() {
        let mut input = ParserInput::new("&#x41;");
        assert_eq!(input.parse_reference().unwrap(), "A");
    }

    #[test]
    fn test_parse_reference_unknown_error() {
        let mut input = ParserInput::new("&bogus;");
        assert!(input.parse_reference().is_err());
    }

    #[test]
    fn test_parse_reference_unknown_recovery() {
        let mut input = ParserInput::new("&bogus;");
        input.set_recover(true);
        let result = input.parse_reference().unwrap();
        assert_eq!(result, "");
        assert_eq!(input.diagnostics.len(), 1);
    }

    #[test]
    fn test_entity_expansion_limit() {
        let mut input = ParserInput::new("&amp;&amp;&amp;");
        input.set_max_entity_expansions(2);
        assert!(input.parse_reference().is_ok());
        assert!(input.parse_reference().is_ok());
        assert!(input.parse_reference().is_err());
    }

    #[test]
    fn test_depth_limit() {
        let mut input = ParserInput::new("");
        input.set_max_depth(2);
        assert!(input.increment_depth().is_ok()); // depth = 1
        assert!(input.increment_depth().is_ok()); // depth = 2
        assert!(input.increment_depth().is_err()); // depth = 3 > 2
    }

    #[test]
    fn test_parse_attribute_value() {
        let mut input = ParserInput::new("\"hello &amp; world\"");
        let value = input.parse_attribute_value().unwrap();
        assert_eq!(value, "hello & world");
    }

    #[test]
    fn test_parse_attribute_value_whitespace_normalization() {
        let mut input = ParserInput::new("\"a\tb\nc\"");
        let value = input.parse_attribute_value().unwrap();
        assert_eq!(value, "a b c");
    }

    #[test]
    fn test_parse_quoted_value() {
        let mut input = ParserInput::new("'hello'");
        let value = input.parse_quoted_value().unwrap();
        assert_eq!(value, "hello");
    }

    #[test]
    fn test_skip_whitespace() {
        let mut input = ParserInput::new("  \t\n  abc");
        assert!(input.skip_whitespace());
        assert_eq!(input.peek(), Some(b'a'));
    }

    #[test]
    fn test_looking_at() {
        let input = ParserInput::new("<!--comment-->");
        assert!(input.looking_at(b"<!--"));
        assert!(!input.looking_at(b"<![CDATA["));
    }

    #[test]
    fn test_take_while() {
        let mut input = ParserInput::new("12345abc");
        let digits = input.take_while(|b| b.is_ascii_digit());
        assert_eq!(digits, "12345");
        assert_eq!(input.peek(), Some(b'a'));
    }

    #[test]
    fn test_split_name() {
        assert_eq!(split_name("foo:bar"), (Some("foo"), "bar"));
        assert_eq!(split_name("bar"), (None, "bar"));
        assert_eq!(split_name(":bar"), (Some(""), "bar"));
    }

    #[test]
    fn test_namespace_resolver() {
        let mut ns = NamespaceResolver::new();

        // xml prefix is pre-bound
        assert_eq!(ns.resolve(Some("xml")), Some(XML_NAMESPACE));
        assert_eq!(ns.resolve(None), None); // no default namespace

        ns.push_scope();
        ns.bind(None, "http://default".to_string());
        ns.bind(Some("foo".to_string()), "http://foo".to_string());

        assert_eq!(ns.resolve(None), Some("http://default"));
        assert_eq!(ns.resolve(Some("foo")), Some("http://foo"));

        ns.pop_scope();
        assert_eq!(ns.resolve(None), None);
        assert_eq!(ns.resolve(Some("foo")), None);
    }

    #[test]
    fn test_namespace_undeclare_default() {
        let mut ns = NamespaceResolver::new();
        ns.push_scope();
        ns.bind(None, "http://default".to_string());
        assert_eq!(ns.resolve(None), Some("http://default"));

        ns.push_scope();
        ns.bind(None, String::new()); // xmlns=""
        assert_eq!(ns.resolve(None), None);

        ns.pop_scope();
        assert_eq!(ns.resolve(None), Some("http://default"));
    }

    #[test]
    fn test_parse_comment_content() {
        let mut input = ParserInput::new("<!-- hello -->");
        let content = parse_comment_content(&mut input).unwrap();
        assert_eq!(content, " hello ");
    }

    #[test]
    fn test_parse_cdata_content() {
        let mut input = ParserInput::new("<![CDATA[some <data>]]>");
        let content = parse_cdata_content(&mut input).unwrap();
        assert_eq!(content, "some <data>");
    }

    #[test]
    fn test_parse_pi_content() {
        let mut input = ParserInput::new("<?target data?>");
        let (target, data) = parse_pi_content(&mut input).unwrap();
        assert_eq!(target, "target");
        assert_eq!(data.as_deref(), Some("data"));
    }

    #[test]
    fn test_parse_pi_no_data() {
        let mut input = ParserInput::new("<?target?>");
        let (target, data) = parse_pi_content(&mut input).unwrap();
        assert_eq!(target, "target");
        assert_eq!(data, None);
    }

    #[test]
    fn test_parse_xml_decl() {
        let mut input = ParserInput::new("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        let decl = parse_xml_decl(&mut input).unwrap();
        assert_eq!(decl.version, "1.0");
        assert_eq!(decl.encoding.as_deref(), Some("UTF-8"));
        assert_eq!(decl.standalone, None);
    }

    #[test]
    fn test_parse_xml_decl_standalone() {
        let mut input =
            ParserInput::new("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
        let decl = parse_xml_decl(&mut input).unwrap();
        assert_eq!(decl.standalone, Some(true));
    }

    #[test]
    fn test_is_name_chars() {
        assert!(is_name_start_char('a'));
        assert!(is_name_start_char('Z'));
        assert!(is_name_start_char('_'));
        assert!(is_name_start_char(':'));
        assert!(!is_name_start_char('0'));
        assert!(!is_name_start_char('-'));

        assert!(is_name_char('a'));
        assert!(is_name_char('0'));
        assert!(is_name_char('-'));
        assert!(is_name_char('.'));
        assert!(!is_name_char(' '));
    }
}
