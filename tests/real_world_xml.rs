//! Integration tests parsing real-world XML formats.
//!
//! These serve as smoke tests ensuring the parser handles common patterns
//! found in Atom feeds, SVG, XHTML, Maven POMs, and Android manifests.

#![allow(clippy::unwrap_used)]

use xmloxide::serial::serialize;
use xmloxide::Document;

fn parse_and_roundtrip(input: &str) -> Document {
    let doc = Document::parse_str(input).unwrap_or_else(|e| panic!("parse failed: {e}"));
    // Roundtrip: serialize and re-parse
    let output = serialize(&doc);
    let doc2 =
        Document::parse_str(&output).unwrap_or_else(|e| panic!("roundtrip parse failed: {e}"));
    // Verify root element survived
    assert_eq!(
        doc.root_element().is_some(),
        doc2.root_element().is_some(),
        "root element mismatch after roundtrip"
    );
    doc
}

// --- Atom / RSS ---

#[test]
fn test_atom_feed() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Example Feed</title>
  <link href="http://example.org/"/>
  <updated>2025-12-13T18:30:02Z</updated>
  <author>
    <name>John Doe</name>
  </author>
  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
  <entry>
    <title>Atom-Powered Robots Run Amok</title>
    <link href="http://example.org/2003/12/13/atom03"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2025-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
  </entry>
</feed>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("feed"));
    assert_eq!(
        doc.node_namespace(root),
        Some("http://www.w3.org/2005/Atom")
    );
}

#[test]
fn test_rss_feed() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>Example RSS</title>
    <link>http://example.org</link>
    <description>An example RSS feed</description>
    <item>
      <title>First Post</title>
      <link>http://example.org/first</link>
      <description>Hello &amp; welcome!</description>
    </item>
  </channel>
</rss>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("rss"));
    assert_eq!(doc.attribute(root, "version"), Some("2.0"));
}

// --- SVG ---

#[test]
fn test_svg_document() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     viewBox="0 0 100 100"
     width="100" height="100">
  <defs>
    <linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="0%">
      <stop offset="0%" style="stop-color:rgb(255,255,0);stop-opacity:1"/>
      <stop offset="100%" style="stop-color:rgb(255,0,0);stop-opacity:1"/>
    </linearGradient>
  </defs>
  <circle cx="50" cy="50" r="40" fill="url(#grad1)"/>
  <text x="50" y="55" text-anchor="middle" fill="white">SVG</text>
  <!-- A comment in SVG -->
  <rect x="10" y="10" width="80" height="80" fill="none" stroke="black"/>
</svg>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("svg"));
    assert_eq!(doc.node_namespace(root), Some("http://www.w3.org/2000/svg"));
    assert_eq!(doc.attribute(root, "width"), Some("100"));
}

// --- XHTML ---

#[test]
fn test_xhtml_document() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <title>Test Page</title>
  </head>
  <body>
    <h1>Hello, World!</h1>
    <p>This is a <em>test</em> page with &amp; entities.</p>
    <div id="content">
      <ul>
        <li>Item 1</li>
        <li>Item 2</li>
        <li>Item 3</li>
      </ul>
    </div>
  </body>
</html>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
    assert_eq!(
        doc.node_namespace(root),
        Some("http://www.w3.org/1999/xhtml")
    );
    assert_eq!(doc.attribute(root, "lang"), Some("en"));
}

// --- Maven POM ---

#[test]
fn test_maven_pom() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
           http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>my-app</artifactId>
  <version>1.0-SNAPSHOT</version>
  <packaging>jar</packaging>
  <name>My Application</name>
  <dependencies>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.13.2</version>
      <scope>test</scope>
    </dependency>
  </dependencies>
  <build>
    <plugins>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-compiler-plugin</artifactId>
        <version>3.8.1</version>
        <configuration>
          <source>11</source>
          <target>11</target>
        </configuration>
      </plugin>
    </plugins>
  </build>
</project>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("project"));
    assert_eq!(
        doc.node_namespace(root),
        Some("http://maven.apache.org/POM/4.0.0")
    );
}

// --- Android Manifest ---

#[test]
fn test_android_manifest() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
          package="com.example.app">
  <uses-permission android:name="android.permission.INTERNET"/>
  <application
      android:label="My App"
      android:icon="@mipmap/ic_launcher"
      android:theme="@style/AppTheme">
    <activity
        android:name=".MainActivity"
        android:exported="true">
      <intent-filter>
        <action android:name="android.intent.action.MAIN"/>
        <category android:name="android.intent.category.LAUNCHER"/>
      </intent-filter>
    </activity>
  </application>
</manifest>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("manifest"));
    assert_eq!(doc.attribute(root, "package"), Some("com.example.app"));
}

// --- SOAP Envelope (multiple namespaces) ---

#[test]
fn test_soap_envelope() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <soap:Header>
    <auth xmlns="http://example.com/auth">
      <token>abc123</token>
    </auth>
  </soap:Header>
  <soap:Body>
    <GetUserResponse xmlns="http://example.com/api">
      <user>
        <name>Jane Doe</name>
        <email>jane@example.com</email>
      </user>
    </GetUserResponse>
  </soap:Body>
</soap:Envelope>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("Envelope"));
    assert_eq!(
        doc.node_namespace(root),
        Some("http://schemas.xmlsoap.org/soap/envelope/")
    );
}

// --- Edge cases ---

#[test]
fn test_cdata_and_entities_mixed() {
    let xml = r#"<root>
  Text with &amp; entities &lt;here&gt;
  <![CDATA[Raw <data> & more]]>
  <!-- A comment here -->
  <?pi some processing instruction?>
  <child attr="value with &quot;quotes&quot;"/>
</root>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("root"));
}

#[test]
fn test_deeply_nested() {
    use std::fmt::Write;
    // 50 levels of nesting
    let mut xml = String::new();
    for i in 0..50 {
        let _ = write!(xml, "<level{i}>");
    }
    xml.push_str("leaf");
    for i in (0..50).rev() {
        let _ = write!(xml, "</level{i}>");
    }

    let doc = parse_and_roundtrip(&xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("level0"));
}

#[test]
fn test_many_attributes() {
    use std::fmt::Write;
    let mut xml = String::from("<root");
    for i in 0..100 {
        let _ = write!(xml, " attr{i}=\"value{i}\"");
    }
    xml.push_str("/>");

    let doc = parse_and_roundtrip(&xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.attributes(root).len(), 100);
    assert_eq!(doc.attribute(root, "attr0"), Some("value0"));
    assert_eq!(doc.attribute(root, "attr99"), Some("value99"));
}

#[test]
fn test_unicode_content() {
    let xml = r"<root>
  <japanese>日本語テスト</japanese>
  <chinese>中文测试</chinese>
  <emoji>Hello 🌍</emoji>
  <arabic>مرحبا</arabic>
  <math>∀x∈ℝ: x² ≥ 0</math>
</root>";

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    let children: Vec<_> = doc.children(root).collect();
    // Filter to element children (skip whitespace text nodes)
    let elements: Vec<_> = children
        .iter()
        .filter(|&&id| doc.node_name(id).is_some())
        .copied()
        .collect();
    assert_eq!(elements.len(), 5);
    assert_eq!(doc.text_content(elements[0]), "日本語テスト");
    assert_eq!(doc.text_content(elements[4]), "∀x∈ℝ: x² ≥ 0");
}

#[test]
fn test_xml_with_byte_order_mark() {
    // UTF-8 BOM + XML
    let mut bytes = vec![0xEF, 0xBB, 0xBF];
    bytes.extend_from_slice(b"<root>hello</root>");

    let doc = Document::parse_bytes(&bytes).unwrap_or_else(|e| panic!("parse_bytes failed: {e}"));
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("root"));
    assert_eq!(doc.text_content(root), "hello");
}

#[test]
fn test_empty_elements_and_self_closing() {
    let xml = r"<root>
  <br/>
  <hr/>
  <empty></empty>
  <space> </space>
</root>";

    let doc = Document::parse_str(xml).unwrap();
    let root = doc.root_element().unwrap();
    let children: Vec<_> = doc
        .children(root)
        .filter(|&id| doc.node_name(id).is_some())
        .collect();
    assert_eq!(children.len(), 4);
    assert_eq!(doc.node_name(children[0]), Some("br"));
    assert_eq!(doc.first_child(children[0]), None); // self-closing, no children
    assert_eq!(doc.node_name(children[2]), Some("empty"));
    assert_eq!(doc.first_child(children[2]), None); // empty element, no children
}

// --- Validation integration tests ---

#[test]
fn test_dtd_validation_valid_catalog() {
    use xmloxide::validation::dtd::{parse_dtd, validate};

    let dtd_text = "\
        <!ELEMENT catalog (book*)>\n\
        <!ELEMENT book (title, author, year)>\n\
        <!ELEMENT title (#PCDATA)>\n\
        <!ELEMENT author (#PCDATA)>\n\
        <!ELEMENT year (#PCDATA)>";
    let dtd = parse_dtd(dtd_text).unwrap();

    let xml = "<!DOCTYPE catalog>\
        <catalog>\
            <book><title>Rust in Action</title><author>Tim McNamara</author><year>2021</year></book>\
            <book><title>Programming Rust</title><author>Jim Blandy</author><year>2021</year></book>\
        </catalog>";
    let mut doc = Document::parse_str(xml).unwrap();
    let result = validate(&mut doc, &dtd);
    assert!(result.is_valid, "errors: {:?}", result.errors);
}

#[test]
fn test_dtd_validation_invalid_content_model() {
    use xmloxide::validation::dtd::{parse_dtd, validate};

    let dtd_text = "\
        <!ELEMENT root (a, b)>\n\
        <!ELEMENT a (#PCDATA)>\n\
        <!ELEMENT b (#PCDATA)>";
    let dtd = parse_dtd(dtd_text).unwrap();

    // Wrong order: b before a
    let xml = "<!DOCTYPE root><root><b>1</b><a>2</a></root>";
    let mut doc = Document::parse_str(xml).unwrap();
    let result = validate(&mut doc, &dtd);
    assert!(!result.is_valid);
    assert!(
        result.errors.iter().any(|e| e.message.contains("content")),
        "expected content model error, got: {:?}",
        result.errors
    );
}

#[test]
fn test_dtd_validation_id_idref() {
    use xmloxide::validation::dtd::{parse_dtd, validate};

    let dtd_text = "\
        <!ELEMENT root (item*, ref*)>\n\
        <!ELEMENT item (#PCDATA)>\n\
        <!ATTLIST item id ID #REQUIRED>\n\
        <!ELEMENT ref EMPTY>\n\
        <!ATTLIST ref target IDREF #REQUIRED>";
    let dtd = parse_dtd(dtd_text).unwrap();

    let xml = r#"<!DOCTYPE root><root><item id="a">A</item><item id="b">B</item><ref target="a"/><ref target="b"/></root>"#;
    let mut doc = Document::parse_str(xml).unwrap();
    let result = validate(&mut doc, &dtd);
    assert!(result.is_valid, "errors: {:?}", result.errors);

    // Verify id_map was populated
    assert!(doc.element_by_id("a").is_some());
    assert!(doc.element_by_id("b").is_some());
}

#[test]
fn test_relaxng_validation_valid() {
    use xmloxide::validation::relaxng::{parse_relaxng, validate};

    let schema_xml = r#"
        <element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0">
            <zeroOrMore>
                <element name="card">
                    <element name="name"><text/></element>
                    <element name="email"><text/></element>
                </element>
            </zeroOrMore>
        </element>
    "#;
    let schema = parse_relaxng(schema_xml).unwrap();

    let xml = "<addressBook>\
        <card><name>Alice</name><email>alice@example.com</email></card>\
        <card><name>Bob</name><email>bob@example.com</email></card>\
    </addressBook>";
    let doc = Document::parse_str(xml).unwrap();
    let result = validate(&doc, &schema);
    assert!(result.is_valid, "errors: {:?}", result.errors);
}

#[test]
fn test_relaxng_validation_invalid() {
    use xmloxide::validation::relaxng::{parse_relaxng, validate};

    let schema_xml = r#"
        <element name="person" xmlns="http://relaxng.org/ns/structure/1.0">
            <element name="name"><text/></element>
            <element name="email"><text/></element>
        </element>
    "#;
    let schema = parse_relaxng(schema_xml).unwrap();

    // Missing required <email> element
    let xml = "<person><name>Alice</name></person>";
    let doc = Document::parse_str(xml).unwrap();
    let result = validate(&doc, &schema);
    assert!(!result.is_valid);
    assert!(!result.errors.is_empty());
}

#[test]
fn test_xsd_validation_valid() {
    use xmloxide::validation::xsd::{parse_xsd, validate_xsd};

    let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="person">
            <xs:complexType><xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="age" type="xs:integer"/>
            </xs:sequence></xs:complexType>
        </xs:element>
    </xs:schema>"#;
    let schema = parse_xsd(xsd).unwrap();

    let xml = "<person><name>Alice</name><age>30</age></person>";
    let doc = Document::parse_str(xml).unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(result.is_valid, "errors: {:?}", result.errors);
}

#[test]
fn test_xsd_validation_invalid_type() {
    use xmloxide::validation::xsd::{parse_xsd, validate_xsd};

    let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="count" type="xs:integer"/>
    </xs:schema>"#;
    let schema = parse_xsd(xsd).unwrap();

    let xml = "<count>not-a-number</count>";
    let doc = Document::parse_str(xml).unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(!result.is_valid);
    assert!(
        result.errors.iter().any(|e| e.message.contains("integer")),
        "expected integer type error, got: {:?}",
        result.errors
    );
}

#[test]
fn test_docbook_like_document() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<article>
  <info>
    <title>xmloxide Architecture</title>
    <author><personname><firstname>Jon</firstname><surname>Wiggins</surname></personname></author>
  </info>
  <section>
    <title>Introduction</title>
    <para>xmloxide is a pure Rust reimplementation of libxml2.</para>
    <para>It uses arena allocation with typed indices.</para>
  </section>
  <section>
    <title>Design</title>
    <itemizedlist>
      <listitem><para>Arena-based tree</para></listitem>
      <listitem><para>Recursive descent parser</para></listitem>
      <listitem><para>Zero-copy where possible</para></listitem>
    </itemizedlist>
  </section>
</article>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("article"));
}

#[test]
fn test_configuration_xml() {
    let xml = r#"<?xml version="1.0"?>
<configuration>
  <appSettings>
    <add key="DatabaseHost" value="localhost"/>
    <add key="DatabasePort" value="5432"/>
    <add key="DatabaseName" value="myapp"/>
    <add key="LogLevel" value="info"/>
  </appSettings>
  <connectionStrings>
    <add name="default" connectionString="Host=localhost;Database=myapp" providerName="Npgsql"/>
  </connectionStrings>
  <system.web>
    <compilation debug="true"/>
    <httpRuntime targetFramework="4.8"/>
  </system.web>
</configuration>"#;

    let doc = parse_and_roundtrip(xml);
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("configuration"));

    // Verify nested elements survived roundtrip
    let output = serialize(&doc);
    assert!(output.contains("DatabaseHost"));
    assert!(output.contains("connectionString"));
    assert!(output.contains("system.web"));
}

// --- Multi-file XSD with imports (UBL-like pattern, issue #3) ---

#[test]
fn test_xsd_import_include_ubl_like_pattern() {
    use std::collections::HashMap;
    use xmloxide::validation::xsd::{parse_xsd_with_options, validate_xsd, XsdParseOptions};

    // Simulate UBL 2.4 structure:
    //   maindoc/UBL-Order-2.4.xsd  (targetNamespace = urn:oasis:names:...:Order-2)
    //     imports common/UBL-CommonBasicComponents-2.4.xsd (ns: urn:...:CommonBasicComponents-2)
    //       includes common/UBL-UnqualifiedDataTypes-2.4.xsd (same ns, chameleon)

    let unqualified_types_xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:simpleType name="QuantityType">
            <xs:restriction base="xs:decimal"/>
        </xs:simpleType>
        <xs:simpleType name="AmountType">
            <xs:restriction base="xs:decimal"/>
        </xs:simpleType>
    </xs:schema>"#;

    let common_basic_xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="urn:oasis:names:tc:ubl:CommonBasicComponents-2">
        <xs:include schemaLocation="UBL-UnqualifiedDataTypes-2.4.xsd"/>
        <xs:complexType name="NameType">
            <xs:sequence>
                <xs:element name="value" type="xs:string"/>
            </xs:sequence>
        </xs:complexType>
        <xs:complexType name="ItemType">
            <xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="quantity" type="xs:integer"/>
            </xs:sequence>
        </xs:complexType>
    </xs:schema>"#;

    let main_order_xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:cbc="urn:oasis:names:tc:ubl:CommonBasicComponents-2"
                targetNamespace="urn:oasis:names:tc:ubl:Order-2">
        <xs:import namespace="urn:oasis:names:tc:ubl:CommonBasicComponents-2"
                   schemaLocation="UBL-CommonBasicComponents-2.4.xsd"/>
        <xs:element name="Order">
            <xs:complexType>
                <xs:sequence>
                    <xs:element name="ID" type="xs:string"/>
                    <xs:element name="Item" type="cbc:ItemType"/>
                </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>"#;

    // Build a resolver that simulates filesystem access
    let schemas: HashMap<String, String> = [
        (
            "UBL-CommonBasicComponents-2.4.xsd".to_string(),
            common_basic_xsd.to_string(),
        ),
        (
            "UBL-UnqualifiedDataTypes-2.4.xsd".to_string(),
            unqualified_types_xsd.to_string(),
        ),
    ]
    .into_iter()
    .collect();

    let resolver = move |location: &str, _base: Option<&str>| schemas.get(location).cloned();

    let opts = XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: None,
    };

    // Parse the main schema (follows imports and includes)
    let schema = parse_xsd_with_options(main_order_xsd, &opts).unwrap();

    // Validate a valid document
    let xml =
        "<Order><ID>ORD-001</ID><Item><name>Widget</name><quantity>10</quantity></Item></Order>";
    let doc = Document::parse_str(xml).unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(
        result.is_valid,
        "expected valid, errors: {:?}",
        result.errors
    );

    // Validate an invalid document (wrong child elements)
    let bad_xml = "<Order><ID>ORD-002</ID><Item><wrong>X</wrong></Item></Order>";
    let bad_doc = Document::parse_str(bad_xml).unwrap();
    let bad_result = validate_xsd(&bad_doc, &schema);
    assert!(
        !bad_result.is_valid,
        "expected invalid for wrong child elements"
    );
}

#[test]
fn test_xsd_import_with_catalog_resolution() {
    use xmloxide::catalog::{Catalog, CatalogEntry};
    use xmloxide::validation::xsd::{parse_xsd_with_options, validate_xsd, XsdParseOptions};

    // Simulate: catalog maps namespace URIs to local schema files
    let mut catalog = Catalog::new();
    catalog.add_entry(CatalogEntry::Uri {
        name: "http://example.com/types".to_string(),
        uri: "schemas/types.xsd".to_string(),
    });

    // Schema files keyed by their resolved paths
    let types_xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="http://example.com/types">
        <xs:complexType name="PersonType">
            <xs:sequence>
                <xs:element name="name" type="xs:string"/>
                <xs:element name="email" type="xs:string"/>
            </xs:sequence>
        </xs:complexType>
    </xs:schema>"#;

    // Resolver that uses catalog to map namespace URIs, and has file content
    let resolver = move |location: &str, _base: Option<&str>| -> Option<String> {
        if location == "schemas/types.xsd" {
            return Some(types_xsd.to_string());
        }
        // Try catalog resolution (maps namespace URI -> local path)
        let resolved = catalog.resolve_uri(location)?;
        if resolved == "schemas/types.xsd" {
            Some(types_xsd.to_string())
        } else {
            None
        }
    };

    let main_xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:t="http://example.com/types">
        <xs:import namespace="http://example.com/types" schemaLocation="schemas/types.xsd"/>
        <xs:element name="directory">
            <xs:complexType>
                <xs:sequence>
                    <xs:element name="person" type="t:PersonType" maxOccurs="unbounded"/>
                </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>"#;

    let opts = XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: None,
    };

    let schema = parse_xsd_with_options(main_xsd, &opts).unwrap();

    let xml = "<directory>\
        <person><name>Alice</name><email>alice@example.com</email></person>\
        <person><name>Bob</name><email>bob@example.com</email></person>\
    </directory>";
    let doc = Document::parse_str(xml).unwrap();
    let result = validate_xsd(&doc, &schema);
    assert!(result.is_valid, "errors: {:?}", result.errors);
}
