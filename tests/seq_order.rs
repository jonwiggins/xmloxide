use xmloxide::Document;
use xmloxide::validation::xsd::{
    parse_xsd_with_options, validate_xsd_strict, validate_element_strict,
    XsdParseOptions, XsdType,
};

fn load_aaa_schema() -> (xmloxide::validation::xsd::XsdSchema, String) {
    let schema_dir = "/Users/aw/Repositories-CISS/konverter2.0/adv-cert/SCHEMA";
    let entry_xsd = format!("{schema_dir}/AAA-Basisschema.xsd");
    let xsd_str = std::fs::read_to_string(&entry_xsd).unwrap();

    let sd = schema_dir.to_string();
    let resolver = |location: &str, _base: Option<&str>| -> Option<String> {
        let filename = location.rsplit('/').next().unwrap_or(location);
        std::fs::read_to_string(format!("{sd}/{filename}")).ok()
    };

    let opts = XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: Some(format!("file:///{entry_xsd}")),
    };
    let schema = parse_xsd_with_options(&xsd_str, &opts).unwrap();
    (schema, sd)
}

fn validate_element_direct(schema: &xmloxide::validation::xsd::XsdSchema, xml: &str) -> Vec<String> {
    let doc = Document::parse_str(xml).unwrap();
    let mut errors = Vec::new();
    
    // Find the AX_Grenzpunkt element declaration
    let decl = schema.elements.get("AX_Grenzpunkt").unwrap();
    validate_element_strict(&doc, doc.root_element().unwrap(), decl, schema, &mut errors);
    
    errors.iter().map(|e| e.message.clone()).collect()
}

#[test]
fn test_sequence_order_strict_simple() {
    let xsd = r#"
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" targetNamespace="urn:test">
      <xs:complexType name="T"><xs:sequence>
        <xs:element name="a" type="xs:string"/>
        <xs:element name="b" type="xs:string"/>
        <xs:element name="c" type="xs:string"/>
      </xs:sequence></xs:complexType>
      <xs:element name="root" type="T"/>
    </xs:schema>"#;

    let opts = XsdParseOptions { resolver: None, base_uri: None };
    let schema = parse_xsd_with_options(xsd, &opts).unwrap();

    let doc = Document::parse_str(r#"<?xml version="1.0"?><root xmlns="urn:test"><a>1</a><b>2</b><c>3</c></root>"#).unwrap();
    let r = validate_xsd_strict(&doc, &schema);
    assert!(r.is_valid);

    let doc = Document::parse_str(r#"<?xml version="1.0"?><root xmlns="urn:test"><b>2</b><a>1</a><c>3</c></root>"#).unwrap();
    let r = validate_xsd_strict(&doc, &schema);
    assert!(!r.is_valid);
}

#[test]
fn test_aaa_grenzpunkt_wrong_order() {
    let (schema, _) = load_aaa_schema();

    // Correct order: punktkennung before abmarkung_Marke
    let correct = r#"<?xml version="1.0" encoding="UTF-8"?>
<AX_Grenzpunkt xmlns="http://www.adv-online.de/namespaces/adv/gid/7.1" xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="TEST001">
  <gml:identifier codeSpace="http://www.adv-online.de/">urn:adv:oid:TEST001</gml:identifier>
  <lebenszeitintervall><AA_Lebenszeitintervall><beginnt>2013-02-01T06:44:33Z</beginnt></AA_Lebenszeitintervall></lebenszeitintervall>
  <modellart><AA_Modellart><advStandardModell>DLKM</advStandardModell></AA_Modellart></modellart>
  <punktkennung>333555831200100</punktkennung>
  <abmarkung_Marke>1100</abmarkung_Marke>
</AX_Grenzpunkt>"#;

    // Wrong order: abmarkung_Marke before punktkennung
    let wrong = r#"<?xml version="1.0" encoding="UTF-8"?>
<AX_Grenzpunkt xmlns="http://www.adv-online.de/namespaces/adv/gid/7.1" xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="TEST001">
  <gml:identifier codeSpace="http://www.adv-online.de/">urn:adv:oid:TEST001</gml:identifier>
  <lebenszeitintervall><AA_Lebenszeitintervall><beginnt>2013-02-01T06:44:33Z</beginnt></AA_Lebenszeitintervall></lebenszeitintervall>
  <modellart><AA_Modellart><advStandardModell>DLKM</advStandardModell></AA_Modellart></modellart>
  <abmarkung_Marke>1100</abmarkung_Marke>
  <punktkennung>333555831200100</punktkennung>
</AX_Grenzpunkt>"#;

    let correct_errors = validate_element_direct(&schema, correct);
    println!("CORRECT errors ({}):", correct_errors.len());
    for e in &correct_errors { println!("  {}", e); }

    let wrong_errors = validate_element_direct(&schema, wrong);
    println!("WRONG errors ({}):", wrong_errors.len());
    for e in &wrong_errors { println!("  {}", e); }

    // Wrong order should have more errors than correct
    assert!(wrong_errors.len() > correct_errors.len(),
        "wrong order should have more errors: wrong={} correct={}", 
        wrong_errors.len(), correct_errors.len());
    
    // Should contain cvc-complex-type.2.4.a or similar ordering error
    let has_order_error = wrong_errors.iter().any(|e| 
        e.contains("cvc-complex-type.2.4.a") || e.contains("unexpected element")
    );
    assert!(has_order_error, "should have ordering error, got: {:?}", wrong_errors);
}
