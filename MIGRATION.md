# Migrating from libxml2 to xmloxide

This guide covers how to migrate C/C++ code from libxml2 to xmloxide's C FFI.

## Quick start

1. Replace `#include <libxml/parser.h>` (etc.) with `#include "libxml2_compat.h"`
2. Remove `xmlInitParser()` / `xmlCleanupParser()` calls (they become no-ops)
3. Replace direct struct member access (`node->name`) with accessor functions
4. Build and link against `libxmloxide` instead of `libxml2`

## Key differences

| Concept | libxml2 | xmloxide |
|---------|---------|----------|
| Node access | Dereference pointer: `node->name` | Call function: `xmlNodeGetName(node)` |
| Navigation | Pointer chain: `node->parent->children` | Function calls: `xmlNodeGetParent(node)` |
| Node identity | Raw pointer (`xmlNodePtr`) | Handle struct with `(doc, id)` pair |
| Global state | Required: `xmlInitParser()` | None needed |
| Thread safety | Manual synchronization | Automatic (`Send + Sync`) |
| Error handling | Global/context error handler | Thread-local: `xmloxide_last_error()` |
| String deallocation | `xmlFree()` | `xmlFree()` (compat) or `xmloxide_free_string()` |

## Before and after

### Parsing and inspecting a document

**libxml2:**
```c
#include <libxml/parser.h>
#include <libxml/tree.h>

xmlInitParser();
xmlDocPtr doc = xmlReadMemory(buf, size, NULL, NULL, 0);
xmlNodePtr root = xmlDocGetRootElement(doc);
printf("Root: %s\n", root->name);

for (xmlNodePtr cur = root->children; cur; cur = cur->next) {
    if (cur->type == XML_ELEMENT_NODE) {
        char *content = (char *)xmlNodeGetContent(cur);
        printf("  %s: %s\n", cur->name, content);
        xmlFree(content);
    }
}
xmlFreeDoc(doc);
xmlCleanupParser();
```

**xmloxide (with compat header):**
```c
#include "libxml2_compat.h"

xmlDocPtr doc = xmlReadMemory(buf, size, NULL, NULL, 0);
xmlNodePtr root = xmlDocGetRootElement(doc);
char *root_name = xmlNodeGetName(root);
printf("Root: %s\n", root_name);
xmlFree(root_name);

xmlNodePtr cur = xmlNodeGetChildren(root);
while (cur) {
    if (xmlNodeGetType(cur) == XMLOXIDE_NODE_ELEMENT) {
        char *name = xmlNodeGetName(cur);
        char *content = xmlNodeGetContent(cur);
        printf("  %s: %s\n", name, content);
        xmlFree(name);
        xmlFree(content);
    }
    xmlNodePtr next = xmlNodeGetNext(cur);
    xmlFreeNode(cur);
    cur = next;
}
xmlFreeNode(root);
xmlFreeDoc(doc);
```

**Key changes:**
- No `xmlInitParser()` / `xmlCleanupParser()`
- `root->name` becomes `xmlNodeGetName(root)` (returns owned string)
- `root->children` becomes `xmlNodeGetChildren(root)` (returns handle)
- `cur->next` becomes `xmlNodeGetNext(cur)`
- Node handles must be freed with `xmlFreeNode()` when done
- Strings from accessor functions are always owned and must be freed

### XPath queries

**libxml2:**
```c
xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
xmlXPathObjectPtr result = xmlXPathEvalExpression("//book/title", ctx);
if (result && result->nodesetval) {
    for (int i = 0; i < result->nodesetval->nodeNr; i++) {
        xmlNodePtr node = result->nodesetval->nodeTab[i];
        char *text = (char *)xmlNodeGetContent(node);
        printf("%s\n", text);
        xmlFree(text);
    }
}
xmlXPathFreeObject(result);
xmlXPathFreeContext(ctx);
```

**xmloxide (with compat header):**
```c
xmlNodePtr root = xmlDocGetRootElement(doc);
xmlXPathObjectPtr result = xmlXPathEval("//book/title", root);
if (result) {
    int count = xmlXPathNodeSetGetLength(result);
    for (int i = 0; i < count; i++) {
        xmlNodePtr node = xmlXPathNodeSetItem(result, i, doc);
        char *text = xmlNodeGetContent(node);
        printf("%s\n", text);
        xmlFree(text);
        xmlFreeNode(node);
    }
}
xmlXPathFreeObject(result);
xmlFreeNode(root);
```

**Key changes:**
- No `xmlXPathContext` needed — pass the context node directly
- `xmlXPathNodeSetItem()` takes the document pointer as an extra argument
- Node handles from XPath results must be freed

### Error handling

**libxml2:**
```c
xmlSetGenericErrorFunc(NULL, my_error_handler);
xmlDocPtr doc = xmlReadMemory(buf, size, NULL, NULL, 0);
if (!doc) {
    xmlErrorPtr err = xmlGetLastError();
    fprintf(stderr, "Error at %d:%d: %s\n", err->line, err->int2, err->message);
}
```

**xmloxide:**
```c
xmlDocPtr doc = xmlReadMemory(buf, size, NULL, NULL, 0);
if (!doc) {
    fprintf(stderr, "Error at %u:%u: %s\n",
            xmloxide_last_error_line(),
            xmloxide_last_error_column(),
            xmloxide_last_error());
}
```

**Key changes:**
- No error handler registration (use `xmloxide_last_error*()` functions)
- Structured error info via `xmloxide_last_error_line()` / `_column()` / `_severity()`
- For document diagnostics (recovered errors), use `xmloxide_doc_diagnostic_*()` functions

### HTML5 parsing (new in xmloxide)

libxml2 ships an HTML 4.01 parser but has no WHATWG HTML5 parser. xmloxide
implements the full WHATWG HTML Living Standard tokenizer (section 13.2.5) and tree
builder (section 13.2.6), giving you spec-compliant HTML5 parsing out of the box.

**Rust:**
```rust
use xmloxide::html5::{parse_html5, parse_html5_with_options, Html5ParseOptions};
use xmloxide::html5::parse_html5_full;

// Full document parsing
let doc = parse_html5("<p>Hello <b>world</b>").unwrap();

// Fragment parsing (the innerHTML algorithm)
let opts = Html5ParseOptions {
    scripting: false,
    fragment_context: Some("body".to_string()),
};
let doc = parse_html5_with_options("<li>Item 1<li>Item 2", &opts).unwrap();

// Parse and collect all errors (always succeeds — no Result)
let result = parse_html5_full("<p>Hello</p>");
println!("{} parse errors", result.errors.len());
let doc = result.document;
```

**C FFI:**
```c
#include "xmloxide.h"

/* Full document parsing */
xmloxide_document *doc = xmloxide_parse_html5("<p>Hello <b>world</b>");

/* Fragment parsing (innerHTML algorithm) */
xmloxide_document *frag = xmloxide_parse_html5_fragment("<li>A<li>B", "ul");

/* HTML5 serialization */
char *html = xmloxide_serialize_html5(doc);
printf("%s\n", html);
xmloxide_free_string(html);

xmloxide_free_doc(frag);
xmloxide_free_doc(doc);
```

**C FFI (with compat header):**
```c
#include "libxml2_compat.h"

xmlDocPtr doc = xmloxide_parse_html5("<p>Hello <b>world</b>");
xmlDocPtr frag = xmloxide_parse_html5_fragment("<li>A<li>B", "ul");

char *html = xmloxide_serialize_html5(doc);
printf("%s\n", html);
xmloxide_free_string(html);

xmlFreeDoc(frag);
xmlFreeDoc(doc);
```

### HTML5 streaming (SAX-like, Rust only)

xmloxide provides a streaming SAX-like API for HTML5 that fires callbacks for
each token without building a DOM tree. This is useful for large documents
where you only need to extract specific data.

**Note:** The C FFI SAX interface is XML-only. HTML5 streaming is available
only from Rust.

**Rust:**
```rust
use xmloxide::html5::sax::{Html5SaxHandler, parse_html5_sax};

struct LinkExtractor {
    hrefs: Vec<String>,
}

impl Html5SaxHandler for LinkExtractor {
    fn start_element(
        &mut self,
        name: &str,
        attributes: &[(String, String)],
        _self_closing: bool,
    ) {
        if name == "a" {
            if let Some((_, href)) = attributes.iter().find(|(n, _)| n == "href") {
                self.hrefs.push(href.clone());
            }
        }
    }
}

let mut handler = LinkExtractor { hrefs: Vec::new() };
parse_html5_sax(
    r#"<a href="https://example.com">Link</a><a href="/about">About</a>"#,
    &mut handler,
);
assert_eq!(handler.hrefs.len(), 2);
```

Available callbacks on `Html5SaxHandler` (all have default no-op implementations):
- `start_element(name, attributes, self_closing)` — start tags
- `end_element(name)` — end tags
- `characters(content)` — text content (coalesced)
- `comment(content)` — HTML comments
- `doctype(name, public_id, system_id)` — DOCTYPE declarations
- `error(error)` — tokenizer errors

### Schematron validation (new in xmloxide)

libxml2 has limited Schematron support (only basic assert/report). xmloxide
implements ISO Schematron (ISO/IEC 19757-3) with phases, abstract patterns,
and `is-a` instantiation.

**Rust:**
```rust
use xmloxide::Document;
use xmloxide::validation::schematron::{
    parse_schematron, validate_schematron, validate_schematron_with_phase,
};

// Parse the Schematron schema
let schema = parse_schematron(r#"
  <schema xmlns="http://purl.oclc.org/dml/schematron">
    <phase id="quick">
      <active pattern="structure"/>
    </phase>
    <pattern id="structure">
      <rule context="/order">
        <assert test="@id">Order must have an id attribute</assert>
        <assert test="item">Order must contain at least one item</assert>
      </rule>
    </pattern>
    <pattern id="business-rules">
      <rule context="/order/item">
        <assert test="@price > 0">Price must be positive</assert>
      </rule>
    </pattern>
  </schema>
"#).unwrap();

let doc = Document::parse_str(r#"<order id="123"><item price="10"/></order>"#).unwrap();

// Validate against all patterns
let result = validate_schematron(&doc, &schema);
assert!(result.is_valid);

// Validate against a specific phase (only "structure" patterns)
let result = validate_schematron_with_phase(&doc, &schema, "quick");
assert!(result.is_valid);

// Inspect validation errors
if !result.is_valid {
    for error in &result.errors {
        eprintln!("{}:{} {}", error.line.unwrap_or(0), error.column.unwrap_or(0), error.message);
    }
}
```

**C FFI:**
```c
#include "xmloxide.h"

const char *schema_xml = "<schema xmlns='http://purl.oclc.org/dml/schematron'>"
    "<pattern id='p1'>"
    "  <rule context='/order'>"
    "    <assert test='@id'>Order must have an id</assert>"
    "  </rule>"
    "</pattern>"
    "</schema>";

/* Parse schema */
xmloxide_schematron_schema *schema = xmloxide_parse_schematron(schema_xml);

/* Parse document */
xmloxide_document *doc = xmloxide_parse("<order id='1'><item/></order>");

/* Validate (all patterns) */
xmloxide_validation_result *result = xmloxide_validate_schematron(doc, schema);
if (xmloxide_validation_is_valid(result)) {
    printf("Valid!\n");
} else {
    size_t n = xmloxide_validation_error_count(result);
    for (size_t i = 0; i < n; i++) {
        char *msg = xmloxide_validation_error_message(result, i);
        printf("Error: %s\n", msg);
        xmloxide_free_string(msg);
    }
}
xmloxide_free_validation_result(result);

/* Validate with a specific phase */
result = xmloxide_validate_schematron_with_phase(doc, schema, "quick");
xmloxide_free_validation_result(result);

xmloxide_free_schematron(schema);
xmloxide_free_doc(doc);
```

### CSS selectors (new in xmloxide, Rust only)

libxml2 has no CSS selector API — it uses XPath exclusively. xmloxide provides
a `css::select` function that supports a broad set of CSS selectors as an
alternative to XPath for element queries.

**Rust:**
```rust
use xmloxide::css::select;
use xmloxide::Document;

let doc = Document::parse_str(r#"
    <html>
      <body>
        <div class="content">
          <p id="intro">Hello</p>
          <p class="highlight">World</p>
          <ul>
            <li class="active">One</li>
            <li>Two</li>
          </ul>
        </div>
      </body>
    </html>
"#).unwrap();

let root = doc.root_element().unwrap();

// Tag selector
let paragraphs = select(&doc, root, "p").unwrap();
assert_eq!(paragraphs.len(), 2);

// Class selector
let highlighted = select(&doc, root, ".highlight").unwrap();
assert_eq!(highlighted.len(), 1);

// ID selector
let intro = select(&doc, root, "#intro").unwrap();
assert_eq!(intro.len(), 1);

// Combinators and pseudo-classes
let first_li = select(&doc, root, "ul > li:first-child").unwrap();
assert_eq!(first_li.len(), 1);

// Attribute selectors
let active = select(&doc, root, "[class~='active']").unwrap();
assert_eq!(active.len(), 1);
```

Supported selectors include: tag, `.class`, `#id`, `*`, `[attr]`,
`[attr="value"]`, `[attr^=]`, `[attr$=]`, `[attr*=]`, `[attr~=]`,
`[attr|=]`, descendant (space), child (`>`), adjacent (`+`), general
sibling (`~`), grouping (`,`), `:first-child`, `:last-child`, `:only-child`,
`:empty`, `:not()`, and `:nth-child()`.

## What's NOT compatible

These libxml2 patterns require code changes and cannot be papered over:

1. **Direct struct member access** — `node->name`, `node->type`, `node->children`, `node->ns`. Use accessor functions instead.
2. **Pointer-based iteration** — `for (cur = node->children; cur; cur = cur->next)`. Use `xmlNodeGetChildren()` + `xmlNodeGetNext()`.
3. **Custom error handlers** — `xmlSetGenericErrorFunc()`. Use `xmloxide_last_error*()`.
4. **Parser context manipulation** — `xmlNewParserCtxt()`, `xmlCtxtReadMemory()`. Use the simpler parse functions directly.
5. **Custom entity loaders** — `xmlSetExternalEntityLoader()`. Not exposed.
6. **XSLT** — Out of scope (separate library in both libxml2 and xmloxide).

## Thread safety

Unlike libxml2, xmloxide requires **no synchronization** for independent documents. Each `xmlDocPtr` is fully self-contained. The only shared state is the per-thread error message (`xmloxide_last_error()`), which uses thread-local storage.

```c
/* Safe: different threads, different documents */
/* Thread 1 */ xmlDocPtr doc1 = xmlParseDoc(xml1);
/* Thread 2 */ xmlDocPtr doc2 = xmlParseDoc(xml2);

/* NOT safe: same document from multiple threads without synchronization */
/* Use a mutex if you need to share a document across threads */
```
