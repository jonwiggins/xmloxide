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

/* ---------- Serialization ---------- */

/**
 * Serializes a document to an XML string.
 * Returns a caller-owned C string that must be freed with
 * xmloxide_free_string(). Returns NULL on failure.
 */
char *xmloxide_serialize(const xmloxide_document *doc);

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
