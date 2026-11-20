//! Equivalence tests for the RELAX NG Compact Syntax front end (#57).
//!
//! Each test parses a schema written in the compact syntax and the same
//! schema written in the XML syntax (mostly the address-book examples of
//! the RELAX NG tutorial, <https://relaxng.org/tutorial-20011203.html>, and
//! the compact tutorial, <https://relaxng.org/compact-tutorial-20030326.html>),
//! then checks that both accept and reject exactly the same instances.

#![allow(clippy::unwrap_used)]

use xmloxide::validation::relaxng::{parse_relaxng, validate, RelaxNgSchema};
use xmloxide::validation::relaxng_compact::parse_relaxng_compact;
use xmloxide::Document;

fn is_valid(schema: &RelaxNgSchema, xml: &str) -> bool {
    let doc = Document::parse_str(xml).unwrap();
    validate(&doc, schema).is_valid
}

/// Parses both syntaxes and asserts that each instance gets the expected
/// verdict from both schemas. When `same_structure` is set, the compiled
/// patterns must also be identical.
fn assert_equivalent(compact: &str, xml: &str, same_structure: bool, cases: &[(&str, bool)]) {
    let from_compact = match parse_relaxng_compact(compact) {
        Ok(s) => s,
        Err(e) => panic!("compact schema failed: {e}"),
    };
    let from_xml = match parse_relaxng(xml) {
        Ok(s) => s,
        Err(e) => panic!("XML schema failed: {e}"),
    };
    if same_structure {
        assert_eq!(from_compact.start, from_xml.start, "start patterns differ");
        assert_eq!(from_compact.defines, from_xml.defines, "defines differ");
    }
    for &(instance, expected) in cases {
        assert_eq!(
            is_valid(&from_xml, instance),
            expected,
            "XML-syntax schema verdict for {instance}"
        );
        assert_eq!(
            is_valid(&from_compact, instance),
            expected,
            "compact schema verdict for {instance}"
        );
    }
}

const CARD_OK: &str =
    "<addressBook><card><name>John Smith</name><email>js@example.com</email></card>\
     <card><name>Fred Bloggs</name><email>fb@example.net</email></card></addressBook>";

#[test]
fn test_compact_equivalence_tutorial_address_book() {
    assert_equivalent(
        "element addressBook {
           element card {
             element name { text },
             element email { text }
           }*
         }",
        r#"<element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0">
             <zeroOrMore>
               <element name="card">
                 <element name="name"><text/></element>
                 <element name="email"><text/></element>
               </element>
             </zeroOrMore>
           </element>"#,
        true,
        &[
            (CARD_OK, true),
            ("<addressBook/>", true),
            (
                "<addressBook><card><email>x</email><name>y</name></card></addressBook>",
                false,
            ),
            (
                "<addressBook><card><name>y</name></card></addressBook>",
                false,
            ),
            ("<book/>", false),
        ],
    );
}

#[test]
fn test_compact_equivalence_tutorial_optional_and_choice() {
    assert_equivalent(
        "element addressBook {
           element card {
             (element name { text }
              | (element givenName { text },
                 element familyName { text })),
             element email { text },
             element note { text }?
           }*
         }",
        r#"<element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0">
             <zeroOrMore>
               <element name="card">
                 <choice>
                   <element name="name"><text/></element>
                   <group>
                     <element name="givenName"><text/></element>
                     <element name="familyName"><text/></element>
                   </group>
                 </choice>
                 <element name="email"><text/></element>
                 <optional><element name="note"><text/></element></optional>
               </element>
             </zeroOrMore>
           </element>"#,
        true,
        &[
            (CARD_OK, true),
            (
                "<addressBook><card><givenName>J</givenName><familyName>S</familyName>\
                 <email>e</email><note>n</note></card></addressBook>",
                true,
            ),
            (
                "<addressBook><card><givenName>J</givenName><email>e</email></card></addressBook>",
                false,
            ),
            (
                "<addressBook><card><name>J</name><email>e</email><note>a</note><note>b</note>\
                 </card></addressBook>",
                false,
            ),
        ],
    );
}

#[test]
fn test_compact_equivalence_tutorial_attributes() {
    assert_equivalent(
        r#"element addressBook {
             element card {
               attribute name { text },
               attribute email { text },
               attribute preferredFormat { "html" | "text" }?
             }*
           }"#,
        r#"<element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0">
             <zeroOrMore>
               <element name="card">
                 <attribute name="name"><text/></attribute>
                 <attribute name="email"><text/></attribute>
                 <optional>
                   <attribute name="preferredFormat">
                     <choice><value>html</value><value>text</value></choice>
                   </attribute>
                 </optional>
               </element>
             </zeroOrMore>
           </element>"#,
        true,
        &[
            (
                r#"<addressBook><card name="J" email="e"/></addressBook>"#,
                true,
            ),
            (
                r#"<addressBook><card email="e" name="J" preferredFormat="html"/></addressBook>"#,
                true,
            ),
            (
                r#"<addressBook><card name="J" email="e" preferredFormat="pdf"/></addressBook>"#,
                false,
            ),
            (r#"<addressBook><card name="J"/></addressBook>"#, false),
            (
                r#"<addressBook><card name="J" email="e" other="x"/></addressBook>"#,
                false,
            ),
        ],
    );
}

#[test]
fn test_compact_equivalence_tutorial_named_patterns() {
    assert_equivalent(
        "start = AddressBook
         AddressBook = element addressBook { Card* }
         Card = element card { Name, Email }
         Name = element name { text }
         Email = element email { text }",
        r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
             <start><ref name="AddressBook"/></start>
             <define name="AddressBook">
               <element name="addressBook"><zeroOrMore><ref name="Card"/></zeroOrMore></element>
             </define>
             <define name="Card">
               <element name="card"><ref name="Name"/><ref name="Email"/></element>
             </define>
             <define name="Name"><element name="name"><text/></element></define>
             <define name="Email"><element name="email"><text/></element></define>
           </grammar>"#,
        true,
        &[
            (CARD_OK, true),
            (
                "<addressBook><card><name>x</name></card></addressBook>",
                false,
            ),
        ],
    );
}

#[test]
fn test_compact_equivalence_tutorial_recursive_and_combine() {
    // Recursion through an element, `|=` / `&=` combination, and `div`.
    assert_equivalent(
        "start = element doc { inline }
         inline = (text | bold | italic)*
         bold = element bold { inline }
         italic = element italic { inline }
         div {
           doc.attlist &= attribute id { text }?
           doc.attlist &= attribute lang { text }?
         }
         start |= element doc2 { doc.attlist }",
        r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0">
             <start><element name="doc"><ref name="inline"/></element></start>
             <define name="inline">
               <zeroOrMore><choice><text/><ref name="bold"/><ref name="italic"/></choice></zeroOrMore>
             </define>
             <define name="bold"><element name="bold"><ref name="inline"/></element></define>
             <define name="italic"><element name="italic"><ref name="inline"/></element></define>
             <div>
               <define name="doc.attlist" combine="interleave">
                 <optional><attribute name="id"><text/></attribute></optional>
               </define>
               <define name="doc.attlist" combine="interleave">
                 <optional><attribute name="lang"><text/></attribute></optional>
               </define>
             </div>
             <start combine="choice"><element name="doc2"><ref name="doc.attlist"/></element></start>
           </grammar>"#,
        true,
        &[
            ("<doc>a <bold>b <italic>c</italic></bold> d</doc>", true),
            ("<doc/>", true),
            ("<doc><underline/></doc>", false),
            (r#"<doc2 lang="en" id="x"/>"#, true),
            (r#"<doc2 other="x"/>"#, false),
        ],
    );
}

#[test]
fn test_compact_equivalence_tutorial_namespaces() {
    assert_equivalent(
        r#"default namespace = "http://www.example.com"
           namespace eg = "http://www.example.com/extra"
           element addressBook {
             element card {
               attribute eg:id { text }?,
               element name { text },
               element eg:email { text }
             }*
           }"#,
        r#"<element name="addressBook" ns="http://www.example.com"
                    xmlns="http://relaxng.org/ns/structure/1.0"
                    xmlns:eg="http://www.example.com/extra">
             <zeroOrMore>
               <element name="card">
                 <optional><attribute name="eg:id"><text/></attribute></optional>
                 <element name="name"><text/></element>
                 <element name="eg:email"><text/></element>
               </element>
             </zeroOrMore>
           </element>"#,
        true,
        &[
            (
                r#"<addressBook xmlns="http://www.example.com" xmlns:x="http://www.example.com/extra">
                     <card x:id="1"><name>J</name><x:email>e</x:email></card>
                   </addressBook>"#,
                true,
            ),
            (
                r#"<addressBook xmlns="http://www.example.com">
                     <card><name>J</name><email>e</email></card>
                   </addressBook>"#,
                false,
            ),
            (
                "<addressBook><card><name>J</name></card></addressBook>",
                false,
            ),
        ],
    );
}

#[test]
fn test_compact_equivalence_tutorial_name_classes() {
    // The tutorial's "anything" pattern plus an nsName exception.
    assert_equivalent(
        r#"namespace local = ""
           namespace eg = "http://www.example.com"
           start = element eg:card { attribute * - (local:* | eg:*) { text }?, anything* }
           anything = element * - eg:* { (attribute * { text } | text | anything)* }"#,
        r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"
                    xmlns:eg="http://www.example.com">
             <start>
               <element name="eg:card">
                 <optional>
                   <attribute>
                     <anyName><except><nsName ns=""/><nsName ns="http://www.example.com"/></except></anyName>
                     <text/>
                   </attribute>
                 </optional>
                 <zeroOrMore><ref name="anything"/></zeroOrMore>
               </element>
             </start>
             <define name="anything">
               <element>
                 <anyName><except><nsName ns="http://www.example.com"/></except></anyName>
                 <zeroOrMore>
                   <choice>
                     <attribute><anyName/><text/></attribute>
                     <text/>
                     <ref name="anything"/>
                   </choice>
                 </zeroOrMore>
               </element>
             </define>
           </grammar>"#,
        false,
        &[
            (
                r#"<eg:card xmlns:eg="http://www.example.com" xmlns:o="urn:o" o:x="1">
                     <o:foo a="1">text<bar/></o:foo>
                   </eg:card>"#,
                true,
            ),
            (
                r#"<eg:card xmlns:eg="http://www.example.com" a="1"/>"#,
                false,
            ),
            (
                r#"<eg:card xmlns:eg="http://www.example.com"><eg:inner/></eg:card>"#,
                false,
            ),
        ],
    );
}

#[test]
fn test_compact_equivalence_datatypes_and_lists() {
    assert_equivalent(
        r"element addressBook {
             element card {
               attribute age { xsd:integer }?,
               attribute vip { xsd:boolean }?,
               element tags { list { token+ } }?,
               element name { text }
             }+
           }",
        r#"<element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0"
                    datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
             <oneOrMore>
               <element name="card">
                 <optional><attribute name="age"><data type="integer"/></attribute></optional>
                 <optional><attribute name="vip"><data type="boolean"/></attribute></optional>
                 <optional>
                   <element name="tags">
                     <list><oneOrMore><data type="token" datatypeLibrary=""/></oneOrMore></list>
                   </element>
                 </optional>
                 <element name="name"><text/></element>
               </element>
             </oneOrMore>
           </element>"#,
        // The XML-syntax parser does not yet inherit `datatypeLibrary`, so
        // the library strings differ; validation does not depend on them.
        false,
        &[
            (
                r#"<addressBook><card age="42" vip="true"><tags>a b</tags><name>J</name></card></addressBook>"#,
                true,
            ),
            (
                r#"<addressBook><card age="old"><name>J</name></card></addressBook>"#,
                false,
            ),
            (
                r#"<addressBook><card vip="maybe"><name>J</name></card></addressBook>"#,
                false,
            ),
            ("<addressBook/>", false),
        ],
    );
}

#[test]
fn test_compact_equivalence_mixed_and_interleave() {
    assert_equivalent(
        "element card {
           element name { text } & element email { text } & element note { mixed { element b { text }* } }?
         }",
        r#"<element name="card" xmlns="http://relaxng.org/ns/structure/1.0">
             <interleave>
               <element name="name"><text/></element>
               <element name="email"><text/></element>
               <optional>
                 <element name="note">
                   <mixed><zeroOrMore><element name="b"><text/></element></zeroOrMore></mixed>
                 </element>
               </optional>
             </interleave>
           </element>"#,
        true,
        &[
            ("<card><email>e</email><name>n</name></card>", true),
            ("<card><note>hi <b>there</b></note><name>n</name><email>e</email></card>", true),
            ("<card><name>n</name></card>", false),
            ("<card><name>n</name><email>e</email><note><i/></note></card>", false),
        ],
    );
}

#[test]
fn test_compact_equivalence_annotated_schema() {
    // Annotations and documentation comments do not change the schema.
    assert_equivalent(
        r#"namespace a = "http://relaxng.org/ns/compatibility/annotations/1.0"

           ## An address book.
           [ a:documentation [ "root" ] ]
           start = element addressBook { card* }

           # A card.
           card = [ a:defaultValue = "x" ]
             element card { element name { text } >> a:documentation [ "the name" ] }"#,
        r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"
                    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0">
             <a:documentation>An address book.</a:documentation>
             <start><element name="addressBook"><zeroOrMore><ref name="card"/></zeroOrMore></element></start>
             <define name="card">
               <element name="card"><element name="name"><text/></element></element>
             </define>
           </grammar>"#,
        true,
        &[
            (
                "<addressBook><card><name>x</name></card></addressBook>",
                true,
            ),
            ("<addressBook><card/></addressBook>", false),
        ],
    );
}
