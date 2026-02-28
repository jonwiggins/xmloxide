/*
 * xmloxide.h — C API for xmloxide
 *
 * A memory-safe XML parsing library implemented in Rust.
 *
 * All returned strings are caller-owned and must be freed with
 * xmloxide_free_string(). Document and XPath result pointers must be
 * freed with their respective free functions.
 *
 * Error handling: functions that can fail return NULL (for pointers)
 * or 0 (for node ids). Call xmloxide_last_error() to retrieve the
 * error message for the most recent failure on the current thread.
 *
 * Thread safety: Unlike libxml2, xmloxide requires no global
 * initialization or cleanup. Each document is independent and may be
 * used from any thread. The last-error message is stored in thread-local
 * storage, so each thread has its own error state. A single document
 * must not be accessed concurrently from multiple threads without
 * external synchronization.
 */

#ifndef XMLOXIDE_H
#define XMLOXIDE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ---------- Opaque types ---------- */

/** Opaque XML document handle. */
typedef struct xmloxide_document xmloxide_document;

/** Opaque XPath result handle. */
typedef struct xmloxide_xpath_value xmloxide_xpath_value;

/** Opaque DTD handle. */
typedef struct xmloxide_dtd xmloxide_dtd;

/** Opaque RelaxNG schema handle. */
typedef struct xmloxide_relaxng_schema xmloxide_relaxng_schema;

/** Opaque XSD schema handle. */
typedef struct xmloxide_xsd_schema xmloxide_xsd_schema;

/** Opaque validation result handle. */
typedef struct xmloxide_validation_result xmloxide_validation_result;

/** Opaque XML Catalog handle. */
typedef struct xmloxide_catalog xmloxide_catalog;

/* ---------- Node type constants ---------- */

#define XMLOXIDE_NODE_ELEMENT       1
#define XMLOXIDE_NODE_TEXT          3
#define XMLOXIDE_NODE_CDATA        4
#define XMLOXIDE_NODE_ENTITY_REF   5
#define XMLOXIDE_NODE_PI           7
#define XMLOXIDE_NODE_COMMENT      8
#define XMLOXIDE_NODE_DOCUMENT     9
#define XMLOXIDE_NODE_DOCUMENT_TYPE 10

/* ---------- XPath result type constants ---------- */

#define XMLOXIDE_XPATH_NODESET  1
#define XMLOXIDE_XPATH_BOOLEAN  2
#define XMLOXIDE_XPATH_NUMBER   3
#define XMLOXIDE_XPATH_STRING   4

/* ---------- Error handling ---------- */

/**
 * Returns the last error message, or NULL if no error occurred.
 *
 * The returned string is owned by the library and must NOT be freed.
 * It is valid until the next xmloxide FFI call on the same thread.
 */
const char *xmloxide_last_error(void);

/* ---------- Document lifecycle ---------- */

/**
 * Parses a null-terminated UTF-8 XML string into a document.
 *
 * Returns a document pointer on success, or NULL on failure.
 * The returned document must be freed with xmloxide_free_doc().
 */
xmloxide_document *xmloxide_parse_str(const char *input);

/**
 * Parses raw bytes as XML, with automatic encoding detection.
 *
 * Returns a document pointer on success, or NULL on failure.
 * The returned document must be freed with xmloxide_free_doc().
 */
xmloxide_document *xmloxide_parse_bytes(const uint8_t *data, size_t len);

/**
 * Parses an HTML string into a document.
 *
 * Returns a document pointer on success, or NULL on failure.
 * The returned document must be freed with xmloxide_free_doc().
 */
xmloxide_document *xmloxide_parse_html(const char *input);

/**
 * Parses an XML file from a filesystem path.
 *
 * Returns a document pointer on success, or NULL on failure.
 * The returned document must be freed with xmloxide_free_doc().
 */
xmloxide_document *xmloxide_parse_file(const char *path);

/**
 * Frees a document previously returned by a parse function.
 * Passing NULL is safe and does nothing.
 */
void xmloxide_free_doc(xmloxide_document *doc);

/* ---------- Document properties ---------- */

/**
 * Returns the XML version string (e.g., "1.0"), or NULL if not declared.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_doc_version(const xmloxide_document *doc);

/**
 * Returns the encoding string (e.g., "UTF-8"), or NULL if not declared.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_doc_encoding(const xmloxide_document *doc);

/* ---------- Tree navigation ---------- */

/*
 * Node IDs are uint32_t values. A value of 0 means "no node"
 * (invalid/missing).
 */

/** Returns the document root node id. */
uint32_t xmloxide_doc_root(const xmloxide_document *doc);

/** Returns the root element of the document, or 0 if none. */
uint32_t xmloxide_doc_root_element(const xmloxide_document *doc);

/** Returns the parent of a node, or 0 if none. */
uint32_t xmloxide_node_parent(const xmloxide_document *doc, uint32_t node);

/** Returns the first child of a node, or 0 if none. */
uint32_t xmloxide_node_first_child(const xmloxide_document *doc, uint32_t node);

/** Returns the last child of a node, or 0 if none. */
uint32_t xmloxide_node_last_child(const xmloxide_document *doc, uint32_t node);

/** Returns the next sibling of a node, or 0 if none. */
uint32_t xmloxide_node_next_sibling(const xmloxide_document *doc, uint32_t node);

/** Returns the previous sibling of a node, or 0 if none. */
uint32_t xmloxide_node_prev_sibling(const xmloxide_document *doc, uint32_t node);

/* ---------- Node inspection ---------- */

/**
 * Returns the node type as an integer constant.
 * Returns -1 if the document or node is invalid.
 */
int32_t xmloxide_node_type(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the name of a node (element local name or PI target).
 * Returns NULL for node types that have no name.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_name(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the direct text content of a text, comment, CDATA, or PI node.
 * Returns NULL for element and document nodes.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_text(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the concatenated text content of a node and all descendants.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_text_content(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the namespace URI of an element node, or NULL if none.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_namespace(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the namespace prefix of an element node (e.g., "svg" for <svg:rect>).
 * Returns NULL if no prefix. The returned string must be freed with
 * xmloxide_free_string().
 */
char *xmloxide_node_prefix(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the value of an attribute by name on an element node.
 * Returns NULL if the attribute is not present.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_attribute(const xmloxide_document *doc, uint32_t node,
                              const char *name);

/**
 * Returns the number of attributes on an element node.
 * Returns 0 for non-element nodes.
 */
size_t xmloxide_node_attribute_count(const xmloxide_document *doc, uint32_t node);

/**
 * Returns the name of the attribute at the given index.
 * Returns NULL if the index is out of range.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_attribute_name_at(const xmloxide_document *doc,
                                      uint32_t node, size_t index);

/**
 * Returns the value of the attribute at the given index.
 * Returns NULL if the index is out of range.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_node_attribute_value_at(const xmloxide_document *doc,
                                       uint32_t node, size_t index);

/* ---------- Tree mutation ---------- */

/**
 * Creates a new element node and returns its id (0 on failure).
 * The node is detached — use xmloxide_append_child() to add it to the tree.
 */
uint32_t xmloxide_create_element(xmloxide_document *doc, const char *name);

/**
 * Creates a new text node and returns its id (0 on failure).
 */
uint32_t xmloxide_create_text(xmloxide_document *doc, const char *content);

/**
 * Creates a new comment node and returns its id (0 on failure).
 */
uint32_t xmloxide_create_comment(xmloxide_document *doc, const char *content);

/**
 * Appends a child node to a parent. Returns 1 on success, 0 on failure.
 */
int32_t xmloxide_append_child(xmloxide_document *doc, uint32_t parent,
                              uint32_t child);

/**
 * Removes a node from the tree. Returns 1 on success, 0 on failure.
 * The node remains in the arena but is detached from the tree.
 */
int32_t xmloxide_remove_node(xmloxide_document *doc, uint32_t node);

/**
 * Clones a node (and optionally its descendants). Returns the new node id.
 * Set deep=1 for a deep clone, deep=0 for a shallow clone.
 * Returns 0 on failure.
 */
uint32_t xmloxide_clone_node(xmloxide_document *doc, uint32_t node, int32_t deep);

/**
 * Sets an attribute on an element node. Returns 1 on success, 0 on failure.
 * If the attribute already exists, its value is updated.
 */
int32_t xmloxide_set_attribute(xmloxide_document *doc, uint32_t node,
                               const char *name, const char *value);

/**
 * Sets the text content of a node. Returns 1 on success, 0 on failure.
 * For text/CDATA/comment nodes, updates content directly.
 * For element nodes, removes all children and replaces with a text node.
 */
int32_t xmloxide_set_text_content(xmloxide_document *doc, uint32_t node,
                                  const char *content);

/**
 * Inserts a node before a reference sibling. Returns 1 on success, 0 on failure.
 */
int32_t xmloxide_insert_before(xmloxide_document *doc, uint32_t reference,
                               uint32_t new_child);

/**
 * Returns the element with the given ID attribute, or 0 if not found.
 * The document's id_map must be populated first (typically via DTD validation).
 */
uint32_t xmloxide_element_by_id(const xmloxide_document *doc, const char *id);

/* ---------- Serialization ---------- */

/**
 * Serializes a document to an XML string.
 * Returns a caller-owned C string that must be freed with
 * xmloxide_free_string(). Returns NULL on failure.
 */
char *xmloxide_serialize(const xmloxide_document *doc);

/**
 * Serializes a document to a pretty-printed XML string with two-space indent.
 * Returns a caller-owned C string that must be freed with
 * xmloxide_free_string(). Returns NULL on failure.
 */
char *xmloxide_serialize_pretty(const xmloxide_document *doc);

/**
 * Serializes a document to a pretty-printed XML string with a custom indent.
 * indent_str is the string used for each level (e.g., "\t" or "    ").
 * Returns a caller-owned C string that must be freed with
 * xmloxide_free_string(). Returns NULL on failure.
 */
char *xmloxide_serialize_pretty_custom(const xmloxide_document *doc,
                                       const char *indent_str);

/**
 * Serializes a document to an HTML string.
 * Returns a caller-owned C string that must be freed with
 * xmloxide_free_string(). Returns NULL on failure.
 */
char *xmloxide_serialize_html(const xmloxide_document *doc);

/* ---------- Validation ---------- */

/**
 * Parses a DTD from a null-terminated UTF-8 string.
 * Returns a DTD pointer on success, or NULL on failure.
 * The returned DTD must be freed with xmloxide_free_dtd().
 */
xmloxide_dtd *xmloxide_parse_dtd(const char *input);

/** Frees a DTD. Passing NULL is safe and does nothing. */
void xmloxide_free_dtd(xmloxide_dtd *dtd);

/**
 * Validates a document against a DTD.
 * Note: DTD validation may populate the document's id_map (requires mutable doc).
 * Returns a validation result that must be freed with
 * xmloxide_free_validation_result().
 */
xmloxide_validation_result *xmloxide_validate_dtd(xmloxide_document *doc,
                                                  const xmloxide_dtd *dtd);

/**
 * Parses a RelaxNG schema from a null-terminated UTF-8 XML string.
 * Returns a schema pointer on success, or NULL on failure.
 * The returned schema must be freed with xmloxide_free_relaxng().
 */
xmloxide_relaxng_schema *xmloxide_parse_relaxng(const char *input);

/** Frees a RelaxNG schema. Passing NULL is safe and does nothing. */
void xmloxide_free_relaxng(xmloxide_relaxng_schema *schema);

/**
 * Validates a document against a RelaxNG schema.
 * Returns a validation result that must be freed with
 * xmloxide_free_validation_result().
 */
xmloxide_validation_result *xmloxide_validate_relaxng(const xmloxide_document *doc,
                                                      const xmloxide_relaxng_schema *schema);

/**
 * Parses an XSD schema from a null-terminated UTF-8 XML string.
 * Returns a schema pointer on success, or NULL on failure.
 * The returned schema must be freed with xmloxide_free_xsd().
 */
xmloxide_xsd_schema *xmloxide_parse_xsd(const char *input);

/** Frees an XSD schema. Passing NULL is safe and does nothing. */
void xmloxide_free_xsd(xmloxide_xsd_schema *schema);

/**
 * Validates a document against an XSD schema.
 * Returns a validation result that must be freed with
 * xmloxide_free_validation_result().
 */
xmloxide_validation_result *xmloxide_validate_xsd(const xmloxide_document *doc,
                                                  const xmloxide_xsd_schema *schema);

/**
 * Returns whether the validation result indicates a valid document.
 * Returns 1 for valid, 0 for invalid or NULL.
 */
int32_t xmloxide_validation_is_valid(const xmloxide_validation_result *result);

/**
 * Returns the number of validation errors.
 * Returns 0 if the result is NULL.
 */
size_t xmloxide_validation_error_count(const xmloxide_validation_result *result);

/**
 * Returns the error message at the given index.
 * Returns NULL if the index is out of range.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_validation_error_message(const xmloxide_validation_result *result,
                                        size_t index);

/**
 * Returns the number of validation warnings.
 * Returns 0 if the result is NULL.
 */
size_t xmloxide_validation_warning_count(const xmloxide_validation_result *result);

/**
 * Returns the warning message at the given index.
 * Returns NULL if the index is out of range.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_validation_warning_message(const xmloxide_validation_result *result,
                                          size_t index);

/**
 * Frees a validation result. Passing NULL is safe and does nothing.
 */
void xmloxide_free_validation_result(xmloxide_validation_result *result);

/* ---------- XPath ---------- */

/**
 * Evaluates an XPath expression against a context node.
 *
 * Returns a pointer to the result on success, or NULL on failure.
 * Use context_node=0 to use the document root as context.
 * The returned result must be freed with xmloxide_xpath_free_result().
 */
xmloxide_xpath_value *xmloxide_xpath_eval(const xmloxide_document *doc,
                                          uint32_t context_node,
                                          const char *expr);

/**
 * Returns the type of an XPath result.
 * Returns one of the XMLOXIDE_XPATH_* constants, or -1 on error.
 */
int32_t xmloxide_xpath_result_type(const xmloxide_xpath_value *result);

/**
 * Returns the boolean value of an XPath result.
 * Converts non-boolean results using XPath type coercion rules.
 */
int32_t xmloxide_xpath_result_boolean(const xmloxide_xpath_value *result);

/**
 * Returns the numeric value of an XPath result.
 * Converts non-number results using XPath type coercion rules.
 */
double xmloxide_xpath_result_number(const xmloxide_xpath_value *result);

/**
 * Returns the string value of an XPath result.
 * Converts non-string results using XPath type coercion rules.
 * The returned string must be freed with xmloxide_free_string().
 */
char *xmloxide_xpath_result_string(const xmloxide_xpath_value *result);

/** Returns the number of nodes in an XPath nodeset result. */
size_t xmloxide_xpath_nodeset_count(const xmloxide_xpath_value *result);

/**
 * Returns the node id at the given index in an XPath nodeset result.
 * Returns 0 if the result is not a nodeset or the index is out of bounds.
 */
uint32_t xmloxide_xpath_nodeset_item(const xmloxide_xpath_value *result,
                                     size_t index);

/**
 * Frees an XPath result previously returned by xmloxide_xpath_eval().
 * Passing NULL is safe and does nothing.
 */
void xmloxide_xpath_free_result(xmloxide_xpath_value *result);

/* ---------- Canonical XML (C14N) ---------- */

/**
 * Canonicalizes a document using inclusive C14N with comments.
 * Returns a caller-owned C string that must be freed with
 * xmloxide_free_string(). Returns NULL on failure.
 */
char *xmloxide_canonicalize(const xmloxide_document *doc);

/**
 * Canonicalizes a document with options.
 * with_comments: 1 to include comments, 0 to strip.
 * exclusive: 1 for exclusive C14N, 0 for inclusive.
 */
char *xmloxide_canonicalize_opts(const xmloxide_document *doc,
                                 int32_t with_comments, int32_t exclusive);

/**
 * Canonicalizes a subtree rooted at the given node.
 */
char *xmloxide_canonicalize_subtree(const xmloxide_document *doc,
                                    uint32_t node, int32_t with_comments,
                                    int32_t exclusive);

/* ---------- XInclude ---------- */

/**
 * Processes XInclude elements in a document using file-based resolution.
 * Returns the number of successful inclusions, or -1 on failure.
 * Errors are stored in the thread-local error (retrievable via xmloxide_last_error).
 */
int32_t xmloxide_process_xincludes(xmloxide_document *doc);

/* ---------- XML Catalogs ---------- */

/**
 * Parses an XML Catalog from a null-terminated UTF-8 XML string.
 * Returns a catalog pointer on success, or NULL on failure.
 * The returned catalog must be freed with xmloxide_free_catalog().
 */
xmloxide_catalog *xmloxide_parse_catalog(const char *input);

/** Frees a catalog. Passing NULL is safe and does nothing. */
void xmloxide_free_catalog(xmloxide_catalog *catalog);

/**
 * Resolves a system identifier using the catalog.
 * Returns a caller-owned URI string, or NULL if not found.
 */
char *xmloxide_catalog_resolve_system(const xmloxide_catalog *catalog,
                                      const char *system_id);

/**
 * Resolves a public identifier using the catalog.
 * Returns a caller-owned URI string, or NULL if not found.
 */
char *xmloxide_catalog_resolve_public(const xmloxide_catalog *catalog,
                                      const char *public_id);

/**
 * Resolves a URI using the catalog.
 * Returns a caller-owned URI string, or NULL if not found.
 */
char *xmloxide_catalog_resolve_uri(const xmloxide_catalog *catalog,
                                   const char *uri);

/* ---------- String lifecycle ---------- */

/**
 * Frees a string previously returned by an xmloxide FFI function.
 * Passing NULL is safe and does nothing.
 */
void xmloxide_free_string(char *ptr);

#ifdef __cplusplus
}
#endif

#endif /* XMLOXIDE_H */
