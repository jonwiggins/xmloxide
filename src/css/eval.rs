//! CSS selector evaluation against a [`Document`] tree.

use crate::tree::{Document, NodeId, NodeKind};

use super::types::{
    AttrOp, AttrSelector, Combinator, CompoundSelector, NthExpr, PseudoClass, Selector,
    SelectorGroup,
};

/// Evaluate a parsed selector group against the document, starting from `scope`.
///
/// Returns all descendant nodes of `scope` that match any selector in the group.
pub fn select(doc: &Document, scope: NodeId, group: &SelectorGroup) -> Vec<NodeId> {
    // Fast path: if every selector in the group is a simple `#id` selector,
    // use element_by_id for O(1) lookup instead of walking the tree.
    if let Some(results) = try_fast_id_select(doc, scope, group) {
        return results;
    }

    let mut results = Vec::new();
    collect_descendants(doc, scope, group, &mut results);
    results
}

/// Attempts to use the fast `id_map` for pure `#id` selectors.
/// Returns `None` if any selector is not a simple ID selector.
fn try_fast_id_select(doc: &Document, scope: NodeId, group: &SelectorGroup) -> Option<Vec<NodeId>> {
    let mut results = Vec::new();
    for sel in &group.selectors {
        // Must be a single compound with only an ID
        if sel.compounds.len() != 1 {
            return None;
        }
        let compound = &sel.compounds[0].compound;
        let id = compound.id.as_ref()?;
        if compound.tag.is_some()
            || !compound.classes.is_empty()
            || !compound.attrs.is_empty()
            || !compound.pseudos.is_empty()
        {
            return None;
        }

        // Look up via id_map
        if let Some(node) = doc.element_by_id(id) {
            // Verify the node is a descendant of scope
            if is_descendant_of(doc, node, scope) && !results.contains(&node) {
                results.push(node);
            }
        }
    }
    Some(results)
}

/// Returns true if `node` is a descendant of `ancestor`.
fn is_descendant_of(doc: &Document, node: NodeId, ancestor: NodeId) -> bool {
    let mut current = doc.parent(node);
    while let Some(id) = current {
        if id == ancestor {
            return true;
        }
        current = doc.parent(id);
    }
    false
}

/// Recursively collect matching descendants.
fn collect_descendants(
    doc: &Document,
    node: NodeId,
    group: &SelectorGroup,
    results: &mut Vec<NodeId>,
) {
    for child in doc.children(node) {
        if matches!(doc.node(child).kind, NodeKind::Element { .. }) {
            if group
                .selectors
                .iter()
                .any(|sel| matches_selector(doc, child, sel))
            {
                results.push(child);
            }
            collect_descendants(doc, child, group, results);
        }
    }
}

/// Check if a node matches a complete selector (chain of compounds with combinators).
fn matches_selector(doc: &Document, node: NodeId, selector: &Selector) -> bool {
    // Walk the compound chain backwards from the rightmost (subject) compound
    let compounds = &selector.compounds;
    if compounds.is_empty() {
        return false;
    }

    // The last compound must match the node itself
    let last = compounds.len() - 1;
    if !matches_compound(doc, node, &compounds[last].compound) {
        return false;
    }

    // Walk backwards through the chain
    let mut current = node;
    for i in (0..last).rev() {
        let entry = &compounds[i];
        let next_combinator = compounds[i + 1].combinator;
        match next_combinator {
            Combinator::None => {}
            Combinator::Descendant => {
                // Find an ancestor that matches
                let mut found = false;
                let mut ancestor = doc.parent(current);
                while let Some(anc) = ancestor {
                    if matches!(doc.node(anc).kind, NodeKind::Element { .. })
                        && matches_compound(doc, anc, &entry.compound)
                    {
                        current = anc;
                        found = true;
                        break;
                    }
                    ancestor = doc.parent(anc);
                }
                if !found {
                    return false;
                }
            }
            Combinator::Child => {
                // Parent must match
                if let Some(parent) = doc.parent(current) {
                    if matches!(doc.node(parent).kind, NodeKind::Element { .. })
                        && matches_compound(doc, parent, &entry.compound)
                    {
                        current = parent;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            Combinator::NextSibling => {
                // Previous sibling element must match
                if let Some(prev) = prev_element_sibling(doc, current) {
                    if matches_compound(doc, prev, &entry.compound) {
                        current = prev;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            Combinator::SubsequentSibling => {
                // Any preceding sibling element must match
                let mut found = false;
                let mut prev = prev_element_sibling(doc, current);
                while let Some(p) = prev {
                    if matches_compound(doc, p, &entry.compound) {
                        current = p;
                        found = true;
                        break;
                    }
                    prev = prev_element_sibling(doc, p);
                }
                if !found {
                    return false;
                }
            }
        }
    }

    true
}

/// Check if a node matches a compound selector (all simple selectors must match).
fn matches_compound(doc: &Document, node: NodeId, compound: &CompoundSelector) -> bool {
    // Tag name
    if let Some(ref tag) = compound.tag {
        let name = doc.node_name(node).unwrap_or("");
        if !name.eq_ignore_ascii_case(tag) {
            return false;
        }
    }

    // ID — use element_by_id for O(1) lookup when the id_map is populated,
    // falling back to attribute scan when it's not.
    if let Some(ref id) = compound.id {
        if let Some(target) = doc.element_by_id(id) {
            if target != node {
                return false;
            }
        } else {
            // id_map doesn't have this ID — either the element doesn't exist
            // or the id_map wasn't populated. Fall back to attribute scan.
            let node_id_attr = doc.attribute(node, "id").unwrap_or("");
            if node_id_attr != id {
                return false;
            }
        }
    }

    // Classes
    for class in &compound.classes {
        let class_attr = doc.attribute(node, "class").unwrap_or("");
        if !class_attr.split_ascii_whitespace().any(|c| c == class) {
            return false;
        }
    }

    // Attribute selectors
    for attr in &compound.attrs {
        if !matches_attr(doc, node, attr) {
            return false;
        }
    }

    // Pseudo-classes
    for pseudo in &compound.pseudos {
        if !matches_pseudo(doc, node, pseudo) {
            return false;
        }
    }

    true
}

/// Check if a node matches an attribute selector.
fn matches_attr(doc: &Document, node: NodeId, sel: &AttrSelector) -> bool {
    let Some(value) = doc.attribute(node, &sel.name) else {
        return false;
    };

    let Some(matcher) = &sel.matcher else {
        return true; // existence check only
    };

    let (val, expected) = if matcher.case_insensitive {
        (
            value.to_ascii_lowercase(),
            matcher.value.to_ascii_lowercase(),
        )
    } else {
        (value.to_string(), matcher.value.clone())
    };

    match matcher.op {
        AttrOp::Exact => val == expected,
        AttrOp::Word => val.split_ascii_whitespace().any(|w| w == expected),
        AttrOp::DashPrefix => val == expected || val.starts_with(&format!("{expected}-")),
        AttrOp::Prefix => val.starts_with(&expected),
        AttrOp::Suffix => val.ends_with(&expected),
        AttrOp::Substring => val.contains(&expected),
    }
}

/// Check if a node matches a pseudo-class.
fn matches_pseudo(doc: &Document, node: NodeId, pseudo: &PseudoClass) -> bool {
    match pseudo {
        PseudoClass::FirstChild => {
            // Node is the first element child of its parent
            doc.parent(node)
                .and_then(|p| first_element_child(doc, p))
                .is_some_and(|first| first == node)
        }
        PseudoClass::LastChild => doc
            .parent(node)
            .and_then(|p| last_element_child(doc, p))
            .is_some_and(|last| last == node),
        PseudoClass::OnlyChild => {
            if let Some(parent) = doc.parent(node) {
                let element_children: Vec<_> = doc
                    .children(parent)
                    .filter(|&c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
                    .collect();
                element_children.len() == 1 && element_children[0] == node
            } else {
                false
            }
        }
        PseudoClass::Empty => {
            // No child elements or text nodes
            !doc.children(node).any(|c| {
                matches!(
                    doc.node(c).kind,
                    NodeKind::Element { .. } | NodeKind::Text { .. } | NodeKind::CData { .. }
                )
            })
        }
        PseudoClass::Not(inner) => !matches_compound(doc, node, inner),
        PseudoClass::NthChild(expr) => nth_child_matches(doc, node, *expr, false),
        PseudoClass::NthLastChild(expr) => nth_child_matches(doc, node, *expr, true),
    }
}

/// Check if a node's position among sibling elements matches an `An+B` expression.
fn nth_child_matches(doc: &Document, node: NodeId, expr: NthExpr, from_end: bool) -> bool {
    let Some(parent) = doc.parent(node) else {
        return false;
    };

    let element_children: Vec<_> = doc
        .children(parent)
        .filter(|&c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
        .collect();

    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    let pos = if from_end {
        element_children
            .iter()
            .rev()
            .position(|&c| c == node)
            .map(|p| p as i32 + 1)
    } else {
        element_children
            .iter()
            .position(|&c| c == node)
            .map(|p| p as i32 + 1)
    };

    pos.is_some_and(|p| expr.matches(p))
}

/// Find the previous element sibling of a node.
fn prev_element_sibling(doc: &Document, node: NodeId) -> Option<NodeId> {
    let mut prev = doc.prev_sibling(node);
    while let Some(p) = prev {
        if matches!(doc.node(p).kind, NodeKind::Element { .. }) {
            return Some(p);
        }
        prev = doc.prev_sibling(p);
    }
    None
}

/// Find the first element child.
fn first_element_child(doc: &Document, parent: NodeId) -> Option<NodeId> {
    doc.children(parent)
        .find(|&c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
}

/// Find the last element child.
fn last_element_child(doc: &Document, parent: NodeId) -> Option<NodeId> {
    doc.children(parent)
        .filter(|&c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
        .last()
}
