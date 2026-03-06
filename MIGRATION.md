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

```c
#include "libxml2_compat.h"

/* Full document parsing */
xmlDocPtr doc = xmloxide_parse_html5("<p>Hello <b>world</b>");

/* Fragment parsing (innerHTML algorithm) */
xmlDocPtr frag = xmloxide_parse_html5_fragment("<li>A<li>B", "ul");

/* HTML5 serialization */
char *html = xmloxide_serialize_html5(doc);
printf("%s\n", html);
xmloxide_free_string(html);

xmlFreeDoc(frag);
xmlFreeDoc(doc);
```

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
