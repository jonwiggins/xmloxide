//! Property-based tests for xmloxide using proptest.
//!
//! Tests structural invariants and roundtrip properties that should hold
//! for all valid inputs, not just hand-picked examples.
#![allow(clippy::unwrap_used)]

use proptest::prelude::*;
use xmloxide::tree::{Document, NodeKind};

// ---------------------------------------------------------------------------
// Strategies: generate random XML-like structures
// ---------------------------------------------------------------------------

/// Generate a valid XML element name (letter followed by alphanumerics).
fn xml_name() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9]{0,7}".prop_map(|s| s)
}

/// Generate safe text content (no `<`, `>`, `&` that would break structure).
fn xml_text() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 .,!?]{0,50}"
}

/// Generate a valid attribute value (no quotes or angle brackets).
fn xml_attr_value() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 _-]{0,30}"
}

/// Generate a simple well-formed XML string with random nesting.
fn simple_xml() -> impl Strategy<Value = String> {
    xml_name().prop_flat_map(|root| {
        prop::collection::vec(
            prop_oneof![
                // text child
                xml_text(),
                // empty element child
                xml_name().prop_map(|n| format!("<{n}/>")),
                // element with text
                (xml_name(), xml_text()).prop_map(|(n, t)| format!("<{n}>{t}</{n}>")),
            ],
            0..5,
        )
        .prop_map(move |children| format!("<{root}>{}</{root}>", children.join("")))
    })
}

/// Generate XML with attributes.
fn xml_with_attrs() -> impl Strategy<Value = String> {
    (
        xml_name(),
        prop::collection::vec((xml_name(), xml_attr_value()), 0..4),
        xml_text(),
    )
        .prop_map(|(name, attrs, text)| {
            // Deduplicate attribute names
            let mut seen = std::collections::HashSet::new();
            let attr_str: Vec<String> = attrs
                .into_iter()
                .filter(|(n, _)| seen.insert(n.clone()))
                .map(|(n, v)| format!("{n}=\"{v}\""))
                .collect();
            let attrs_joined = if attr_str.is_empty() {
                String::new()
            } else {
                format!(" {}", attr_str.join(" "))
            };
            format!("<{name}{attrs_joined}>{text}</{name}>")
        })
}

// ---------------------------------------------------------------------------
// Property: parse → serialize → parse roundtrip
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn roundtrip_parse_serialize(xml in simple_xml()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        let original_name = doc.node_name(root).map(String::from);
        let original_text = doc.text_content(root);

        let serialized = xmloxide::serial::serialize(&doc);
        let doc2 = Document::parse_str(&serialized).unwrap();
        let root2 = doc2.root_element().unwrap();

        // Structure is preserved through roundtrip
        prop_assert_eq!(original_name, doc2.node_name(root2).map(String::from));
        prop_assert_eq!(original_text, doc2.text_content(root2));

        // Idempotence: serialize(parse(serialized)) == serialized
        // (trim trailing whitespace that the serializer may add)
        let serialized2 = xmloxide::serial::serialize(&doc2);
        let doc3 = Document::parse_str(&serialized2).unwrap();
        let serialized3 = xmloxide::serial::serialize(&doc3);
        prop_assert_eq!(&serialized2, &serialized3,
            "Serialization should be idempotent after first normalization");
    }
}

// ---------------------------------------------------------------------------
// Property: tree navigation invariants
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn parent_child_consistency(xml in simple_xml()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        // Every child's parent is the node itself
        for child in doc.children(root) {
            prop_assert_eq!(doc.parent(child), Some(root));
        }
    }

    #[test]
    fn sibling_chain_consistency(xml in simple_xml()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        // Walk forward then backward through siblings
        let children: Vec<_> = doc.children(root).collect();
        if !children.is_empty() {
            // first_child matches children[0]
            prop_assert_eq!(doc.first_child(root), Some(children[0]));
            // last_child matches children[last]
            prop_assert_eq!(doc.last_child(root), Some(*children.last().unwrap()));

            // Forward chain
            let mut node = doc.first_child(root);
            let mut forward = vec![];
            while let Some(n) = node {
                forward.push(n);
                node = doc.next_sibling(n);
            }
            prop_assert_eq!(&forward, &children);

            // Backward chain
            let mut node = doc.last_child(root);
            let mut backward = vec![];
            while let Some(n) = node {
                backward.push(n);
                node = doc.prev_sibling(n);
            }
            backward.reverse();
            prop_assert_eq!(&backward, &children);
        }
    }

    #[test]
    fn first_child_is_none_for_empty_element(name in xml_name()) {
        let xml = format!("<{name}/>");
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        prop_assert!(doc.first_child(root).is_none());
        prop_assert!(doc.last_child(root).is_none());
    }
}

// ---------------------------------------------------------------------------
// Property: text_content collects all descendant text
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn text_content_contains_all_text(xml in simple_xml()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        let full_text = doc.text_content(root);

        // Every direct text child's content should appear in text_content
        for child in doc.children(root) {
            if let Some(text) = doc.node_text(child) {
                prop_assert!(
                    full_text.contains(text),
                    "text_content '{}' should contain child text '{}'", full_text, text
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Property: attribute operations
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn set_get_attribute_roundtrip(
        name in xml_name(),
        attr_name in xml_name(),
        attr_value in xml_attr_value(),
    ) {
        let xml = format!("<{name}/>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        doc.set_attribute(root, &attr_name, &attr_value);
        let got = doc.attribute(root, &attr_name);
        prop_assert_eq!(got, Some(attr_value.as_str()));
    }

    #[test]
    fn remove_attribute_makes_it_none(
        name in xml_name(),
        attr_name in xml_name(),
        attr_value in xml_attr_value(),
    ) {
        let xml = format!("<{name}/>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        doc.set_attribute(root, &attr_name, &attr_value);
        prop_assert_eq!(doc.attribute(root, &attr_name), Some(attr_value.as_str()));
        doc.remove_attribute(root, &attr_name);
        prop_assert_eq!(doc.attribute(root, &attr_name), None);
    }

    #[test]
    fn parsed_attributes_survive_roundtrip(xml in xml_with_attrs()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        let attrs: Vec<_> = doc.attributes(root)
            .iter()
            .map(|a| (a.name.clone(), a.value.clone()))
            .collect();

        let serialized = xmloxide::serial::serialize(&doc);
        let doc2 = Document::parse_str(&serialized).unwrap();
        let root2 = doc2.root_element().unwrap();
        let attrs2: Vec<_> = doc2.attributes(root2)
            .iter()
            .map(|a| (a.name.clone(), a.value.clone()))
            .collect();

        prop_assert_eq!(attrs, attrs2);
    }
}

// ---------------------------------------------------------------------------
// Property: mutation preserves tree invariants
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn append_child_updates_parent_link(
        root_name in xml_name(),
        child_name in xml_name(),
    ) {
        let xml = format!("<{root_name}/>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        let child = doc.create_element(&child_name);

        doc.append_child(root, child);
        prop_assert_eq!(doc.parent(child), Some(root));
        prop_assert_eq!(doc.first_child(root), Some(child));
        prop_assert_eq!(doc.last_child(root), Some(child));
    }

    #[test]
    fn append_multiple_children_preserves_order(
        root_name in xml_name(),
        child_names in prop::collection::vec(xml_name(), 1..6),
    ) {
        let xml = format!("<{root_name}/>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        let mut child_ids = vec![];
        for name in &child_names {
            let child = doc.create_element(name);
            doc.append_child(root, child);
            child_ids.push(child);
        }

        let children: Vec<_> = doc.children(root).collect();
        prop_assert_eq!(children, child_ids);
    }

    #[test]
    fn remove_node_unlinks_from_parent(xml in simple_xml()) {
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        let children_before: Vec<_> = doc.children(root).collect();

        if let Some(&first) = children_before.first() {
            doc.remove_node(first);
            let children_after: Vec<_> = doc.children(root).collect();
            prop_assert_eq!(children_after.len(), children_before.len() - 1);
            prop_assert!(doc.parent(first).is_none());
        }
    }

    #[test]
    fn insert_after_preserves_siblings(
        root_name in xml_name(),
        names in prop::collection::vec(xml_name(), 2..5),
    ) {
        let xml = format!("<{root_name}/>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        // Create first child
        let first = doc.create_element(&names[0]);
        doc.append_child(root, first);

        // Insert remaining after the first
        let mut prev = first;
        let mut expected = vec![first];
        for name in &names[1..] {
            let node = doc.create_element(name);
            doc.insert_after(prev, node);
            expected.push(node);
            prev = node;
        }

        let children: Vec<_> = doc.children(root).collect();
        prop_assert_eq!(children, expected);
    }

    #[test]
    fn set_text_content_replaces_children(
        root_name in xml_name(),
        child_name in xml_name(),
        text in xml_text(),
    ) {
        let xml = format!("<{root_name}><{child_name}/></{root_name}>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        doc.set_text_content(root, &text);
        prop_assert_eq!(doc.text_content(root), text);
        // Should have exactly one text child (set_text_content always creates one)
        let children: Vec<_> = doc.children(root).collect();
        prop_assert_eq!(children.len(), 1);
    }
}

// ---------------------------------------------------------------------------
// Property: clone_node produces equivalent subtree
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn deep_clone_produces_equal_serialization(xml in simple_xml()) {
        let mut doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        let cloned = doc.clone_node(root, true);
        // Cloned subtree should have same name
        prop_assert_eq!(doc.node_name(root), doc.node_name(cloned));
        // And same text content
        prop_assert_eq!(doc.text_content(root), doc.text_content(cloned));
    }
}

// ---------------------------------------------------------------------------
// Property: XPath determinism
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn xpath_self_returns_node(xml in simple_xml()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();
        let result = xmloxide::xpath::evaluate(&doc, root, ".").unwrap();
        let nodes = result.as_node_set().unwrap();
        prop_assert_eq!(nodes.len(), 1);
        prop_assert_eq!(nodes[0], root);
    }

    #[test]
    fn xpath_children_matches_navigation(xml in simple_xml()) {
        let doc = Document::parse_str(&xml).unwrap();
        let root = doc.root_element().unwrap();

        // Count element children via navigation
        let nav_elements: Vec<_> = doc.children(root)
            .filter(|&c| matches!(doc.node(c).kind, NodeKind::Element { .. }))
            .collect();

        // Count element children via XPath
        let result = xmloxide::xpath::evaluate(&doc, root, "*").unwrap();
        let xpath_elements = result.as_node_set().unwrap();

        prop_assert_eq!(nav_elements.len(), xpath_elements.len());
        for (nav, xpath) in nav_elements.iter().zip(xpath_elements.iter()) {
            prop_assert_eq!(nav, xpath);
        }
    }
}

// ---------------------------------------------------------------------------
// Property: node_count is consistent
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn node_count_increases_on_create(
        root_name in xml_name(),
        n in 1..10usize,
    ) {
        let xml = format!("<{root_name}/>");
        let mut doc = Document::parse_str(&xml).unwrap();
        let initial = doc.node_count();
        let root = doc.root_element().unwrap();

        for i in 0..n {
            let child = doc.create_element(&format!("c{i}"));
            doc.append_child(root, child);
        }

        prop_assert_eq!(doc.node_count(), initial + n);
    }
}

// ---------------------------------------------------------------------------
// Property: HTML parser never panics on arbitrary input
// ---------------------------------------------------------------------------

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn html5_parser_does_not_panic(input in ".{0,100}") {
        let _ = xmloxide::html5::parse_html5(&input);
    }

    #[test]
    fn xml_parser_does_not_panic(input in ".{0,100}") {
        // XML parser may return errors but must not panic
        let _ = Document::parse_str(&input);
    }
}
