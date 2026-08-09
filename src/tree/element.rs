//! Convenience wrapper around [`Document`] + [`NodeId`] for ergonomic XML traversal.
//!
//! Provides an [`Element`] type with methods like [`child_by_name`](Element::child_by_name),
//! [`attribute`](Element::attribute), [`children`](Element::children), etc.

use crate::{Document, NodeId};

/// A borrowed reference to an XML element node within a [`Document`].
///
/// Lightweight handle — holds a [`NodeId`] and a reference to the parent
/// [`Document`]. All methods delegate to the underlying tree.
#[derive(Clone, Copy)]
pub struct Element<'a> {
    pub(crate) doc: &'a Document,
    pub(crate) id: NodeId,
}

impl<'a> Element<'a> {
    /// Create a new `Element` wrapper.
    ///
    /// Returns `None` if `id` is not an element node in the document.
    #[must_use]
    pub fn new(doc: &'a Document, id: NodeId) -> Option<Self> {
        if doc.is_element(id) {
            Some(Self { doc, id })
        } else {
            None
        }
    }

    /// Returns the underlying [`NodeId`].
    #[must_use]
    #[inline]
    pub fn id(&self) -> NodeId {
        self.id
    }

    /// Returns the tag name of this element.
    #[must_use]
    #[inline]
    pub fn tag_name(&self) -> TagName<'_> {
        TagName {
            local: self.doc.node_name(self.id).unwrap_or(""),
            prefix: self.doc.node_prefix(self.id),
            namespace: self.doc.node_namespace(self.id),
        }
    }

    /// Returns the local name of this element (without namespace prefix).
    #[must_use]
    #[inline]
    pub fn local_name(&self) -> &'a str {
        self.doc.node_name(self.id).unwrap_or("")
    }

    /// Returns the text content of this element (concatenated text nodes).
    #[must_use]
    #[inline]
    pub fn text(&self) -> Option<&'a str> {
        self.doc.node_text(self.id)
    }

    /// Returns the first direct child element whose local name matches `name`.
    #[must_use]
    pub fn child_by_name(&self, name: &str) -> Option<Element<'a>> {
        for child_id in self.doc.children(self.id) {
            if self.doc.is_element(child_id) {
                if self.doc.node_name(child_id).map_or(false, |n| n == name) {
                    return Some(Element {
                        doc: self.doc,
                        id: child_id,
                    });
                }
            }
        }
        None
    }

    /// Returns the value of an attribute by local name.
    #[must_use]
    #[inline]
    pub fn attribute(&self, name: &str) -> Option<Attribute<'a>> {
        self.doc.attribute(self.id, name).map(|value| Attribute { value })
    }

    /// Returns an iterator over direct child elements.
    pub fn children(&self) -> ChildElements<'a> {
        ChildElements {
            inner: self.doc.children(self.id),
            doc: self.doc,
        }
    }

    /// Returns the parent element, if any.
    #[must_use]
    pub fn parent(&self) -> Option<Element<'a>> {
        self.doc.parent(self.id).and_then(|pid| {
            if self.doc.is_element(pid) {
                Some(Element { doc: self.doc, id: pid })
            } else {
                None
            }
        })
    }
}

/// The tag name of an element, split into local name, prefix, and namespace.
#[derive(Clone, Copy)]
pub struct TagName<'a> {
    /// Local name (without prefix).
    pub local: &'a str,
    /// Namespace prefix (e.g. `"wfs"` in `wfs:Query`).
    pub prefix: Option<&'a str>,
    /// Namespace URI.
    pub namespace: Option<&'a str>,
}

impl<'a> TagName<'a> {
    /// Returns the local name.
    #[must_use]
    #[inline]
    pub fn local(&self) -> &'a str {
        self.local
    }
}

/// An attribute value wrapper.
#[derive(Clone, Copy)]
pub struct Attribute<'a> {
    /// The attribute value.
    pub value: &'a str,
}

impl<'a> Attribute<'a> {
    /// Returns the attribute value as a string.
    #[must_use]
    #[inline]
    pub fn text(&self) -> &'a str {
        self.value
    }
}

/// Iterator over child element nodes.
pub struct ChildElements<'a> {
    inner: crate::tree::Children<'a>,
    doc: &'a Document,
}

impl<'a> Iterator for ChildElements<'a> {
    type Item = Element<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        for id in &mut self.inner {
            if self.doc.is_element(id) {
                return Some(Element { doc: self.doc, id });
            }
        }
        None
    }
}

// Extend Document with convenience methods.
impl Document {
    /// Returns the root element as an [`Element`].
    #[must_use]
    pub fn root_element_ref(&self) -> Option<Element<'_>> {
        self.root_element().map(|id| Element { doc: self, id })
    }
}
