//! Integration tests for ISO Schematron validation.
//!
//! Exercises the full Schematron pipeline: schema parsing, document validation,
//! firing rules, variables, message interpolation, and phase selection.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use xmloxide::validation::schematron::{
    parse_schematron, validate_schematron, validate_schematron_with_phase,
};
use xmloxide::Document;

// ---------------------------------------------------------------------------
// Purchase order schema — realistic multi-pattern Schematron
// ---------------------------------------------------------------------------

const PURCHASE_ORDER_SCHEMA: &str = r#"
<schema xmlns="http://purl.oclc.org/dml/schematron"
        defaultPhase="full">

  <ns prefix="po" uri="urn:example:purchase-order"/>

  <let name="max_items" value="100"/>

  <phase id="quick">
    <active pattern="structure"/>
  </phase>

  <phase id="full">
    <active pattern="structure"/>
    <active pattern="business"/>
    <active pattern="warnings"/>
  </phase>

  <pattern id="structure">
    <rule context="/purchaseOrder">
      <assert test="@orderDate">Purchase order must have an orderDate attribute</assert>
      <assert test="@id">Purchase order must have an id attribute</assert>
      <assert test="shipTo">Purchase order must have a shipTo element</assert>
      <assert test="billTo">Purchase order must have a billTo element</assert>
      <assert test="items">Purchase order must have an items element</assert>
    </rule>

    <rule context="//shipTo | //billTo">
      <assert test="name">Address must have a name</assert>
      <assert test="street">Address must have a street</assert>
      <assert test="city">Address must have a city</assert>
      <assert test="country">Address must have a country</assert>
    </rule>

    <rule context="//item">
      <assert test="@partNum">Item must have a partNum attribute</assert>
      <assert test="productName">Item must have a productName</assert>
      <assert test="quantity">Item must have a quantity</assert>
      <assert test="price">Item must have a price</assert>
    </rule>
  </pattern>

  <pattern id="business">
    <rule context="/purchaseOrder/items">
      <let name="item_count" value="count(item)"/>
      <assert test="$item_count > 0">Order must have at least one item</assert>
      <assert test="$item_count &lt;= $max_items">Order cannot exceed <value-of select="$max_items"/> items (has <value-of select="$item_count"/>)</assert>
    </rule>

    <rule context="//item/quantity">
      <assert test="number(.) > 0">Quantity must be positive for item <value-of select="../productName"/></assert>
      <assert test="number(.) = floor(number(.))">Quantity must be a whole number for item <value-of select="../productName"/></assert>
    </rule>

    <rule context="//item/price">
      <assert test="number(.) >= 0">Price must not be negative for item <value-of select="../productName"/></assert>
    </rule>
  </pattern>

  <pattern id="warnings">
    <rule context="/purchaseOrder">
      <report test="comment">Order <value-of select="@id"/> has a comment attached</report>
    </rule>

    <rule context="//item">
      <report test="@urgency = 'rush'">Item <value-of select="productName"/> is marked as rush delivery</report>
    </rule>
  </pattern>
</schema>
"#;

fn valid_purchase_order() -> &'static str {
    r#"<?xml version="1.0"?>
<purchaseOrder id="PO-2026-001" orderDate="2026-03-14">
  <shipTo>
    <name>Alice Smith</name>
    <street>123 Maple Street</street>
    <city>Springfield</city>
    <state>IL</state>
    <zip>62704</zip>
    <country>US</country>
  </shipTo>
  <billTo>
    <name>Alice Smith</name>
    <street>123 Maple Street</street>
    <city>Springfield</city>
    <state>IL</state>
    <zip>62704</zip>
    <country>US</country>
  </billTo>
  <items>
    <item partNum="872-AA">
      <productName>Lawnmower</productName>
      <quantity>1</quantity>
      <price>148.95</price>
    </item>
    <item partNum="926-AA">
      <productName>Baby Monitor</productName>
      <quantity>2</quantity>
      <price>39.98</price>
    </item>
  </items>
</purchaseOrder>"#
}

// ---------------------------------------------------------------------------
// Integration tests
// ---------------------------------------------------------------------------

#[test]
fn test_valid_purchase_order() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    let doc = Document::parse_str(valid_purchase_order()).unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(result.is_valid, "valid PO should pass: {:?}", result.errors);
    assert!(result.warnings.is_empty());
}

#[test]
fn test_missing_required_elements() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    // Missing shipTo, billTo, and items
    let doc =
        Document::parse_str(r#"<purchaseOrder id="PO-001" orderDate="2026-01-01"/>"#).unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(!result.is_valid);
    // Should catch: missing shipTo, billTo, items
    assert!(
        result.errors.len() >= 3,
        "expected at least 3 errors, got {}: {:?}",
        result.errors.len(),
        result.errors
    );
}

#[test]
fn test_missing_address_fields() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    let doc = Document::parse_str(
        r#"<purchaseOrder id="PO-002" orderDate="2026-01-01">
  <shipTo>
    <name>Bob</name>
  </shipTo>
  <billTo>
    <name>Bob</name>
    <street>456 Oak Ave</street>
    <city>Chicago</city>
    <country>US</country>
  </billTo>
  <items>
    <item partNum="100-ZZ">
      <productName>Widget</productName>
      <quantity>1</quantity>
      <price>9.99</price>
    </item>
  </items>
</purchaseOrder>"#,
    )
    .unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(!result.is_valid);
    // shipTo is missing street, city, country
    let addr_errors: Vec<_> = result
        .errors
        .iter()
        .filter(|e| e.message.contains("Address must have"))
        .collect();
    assert!(
        addr_errors.len() >= 3,
        "expected at least 3 address errors, got {}: {:?}",
        addr_errors.len(),
        addr_errors
    );
}

#[test]
fn test_invalid_quantity_and_price() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    let doc = Document::parse_str(
        r#"<purchaseOrder id="PO-003" orderDate="2026-01-01">
  <shipTo>
    <name>Carol</name><street>1 St</street><city>NYC</city><country>US</country>
  </shipTo>
  <billTo>
    <name>Carol</name><street>1 St</street><city>NYC</city><country>US</country>
  </billTo>
  <items>
    <item partNum="AAA-01">
      <productName>Gadget</productName>
      <quantity>0</quantity>
      <price>-5.00</price>
    </item>
    <item partNum="BBB-02">
      <productName>Gizmo</productName>
      <quantity>3.5</quantity>
      <price>10.00</price>
    </item>
  </items>
</purchaseOrder>"#,
    )
    .unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(!result.is_valid);

    // Check for specific business rule violations
    let has_quantity_error = result
        .errors
        .iter()
        .any(|e| e.message.contains("Quantity must be positive"));
    assert!(has_quantity_error, "should catch zero quantity");

    let has_price_error = result
        .errors
        .iter()
        .any(|e| e.message.contains("Price must not be negative"));
    assert!(has_price_error, "should catch negative price");

    let has_whole_number_error = result
        .errors
        .iter()
        .any(|e| e.message.contains("whole number"));
    assert!(has_whole_number_error, "should catch fractional quantity");
}

#[test]
fn test_message_interpolation_with_context() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    let doc = Document::parse_str(
        r#"<purchaseOrder id="PO-004" orderDate="2026-01-01">
  <shipTo>
    <name>Dave</name><street>2 St</street><city>LA</city><country>US</country>
  </shipTo>
  <billTo>
    <name>Dave</name><street>2 St</street><city>LA</city><country>US</country>
  </billTo>
  <items>
    <item partNum="CCC-03">
      <productName>Sprocket</productName>
      <quantity>0</quantity>
      <price>5.00</price>
    </item>
  </items>
</purchaseOrder>"#,
    )
    .unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(!result.is_valid);

    // The error message should include the product name from value-of
    let quantity_error = result
        .errors
        .iter()
        .find(|e| e.message.contains("Quantity must be positive"))
        .expect("should have quantity error");
    assert!(
        quantity_error.message.contains("Sprocket"),
        "error message should include product name: {}",
        quantity_error.message
    );
}

#[test]
fn test_report_warnings() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    let doc = Document::parse_str(
        r#"<purchaseOrder id="PO-005" orderDate="2026-01-01">
  <shipTo>
    <name>Eve</name><street>3 St</street><city>SF</city><country>US</country>
  </shipTo>
  <billTo>
    <name>Eve</name><street>3 St</street><city>SF</city><country>US</country>
  </billTo>
  <comment>Please gift wrap</comment>
  <items>
    <item partNum="DDD-04" urgency="rush">
      <productName>Express Widget</productName>
      <quantity>1</quantity>
      <price>99.99</price>
    </item>
  </items>
</purchaseOrder>"#,
    )
    .unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(
        result.is_valid,
        "reports should not cause validation failure: {:?}",
        result.errors
    );
    // Should have warnings from both report rules
    assert!(
        result.warnings.len() >= 2,
        "expected at least 2 warnings, got {}: {:?}",
        result.warnings.len(),
        result.warnings
    );

    let has_comment_warning = result
        .warnings
        .iter()
        .any(|w| w.message.contains("PO-005") && w.message.contains("comment"));
    assert!(has_comment_warning, "should warn about comment on PO-005");

    let has_rush_warning = result
        .warnings
        .iter()
        .any(|w| w.message.contains("Express Widget") && w.message.contains("rush"));
    assert!(has_rush_warning, "should warn about rush delivery");
}

#[test]
fn test_phase_quick_skips_business_rules() {
    let schema = parse_schematron(PURCHASE_ORDER_SCHEMA).unwrap();
    // This document has valid structure but invalid business rules
    let doc = Document::parse_str(
        r#"<purchaseOrder id="PO-006" orderDate="2026-01-01">
  <shipTo>
    <name>Frank</name><street>4 St</street><city>Denver</city><country>US</country>
  </shipTo>
  <billTo>
    <name>Frank</name><street>4 St</street><city>Denver</city><country>US</country>
  </billTo>
  <items>
    <item partNum="EEE-05">
      <productName>Bad Item</productName>
      <quantity>0</quantity>
      <price>-1</price>
    </item>
  </items>
</purchaseOrder>"#,
    )
    .unwrap();

    // Quick phase only checks structure — should pass
    let quick = validate_schematron_with_phase(&doc, &schema, "quick");
    assert!(
        quick.is_valid,
        "quick phase should pass (structure only): {:?}",
        quick.errors
    );

    // Full phase checks everything — should fail on business rules
    let full = validate_schematron_with_phase(&doc, &schema, "full");
    assert!(
        !full.is_valid,
        "full phase should catch business rule violations"
    );
}

#[test]
fn test_firing_rule_semantics_integration() {
    // Test that within a pattern, the first matching rule wins
    let schema = parse_schematron(
        r#"
        <schema xmlns="http://purl.oclc.org/dml/schematron">
          <pattern>
            <rule context="//item[@special]">
              <assert test="@discount">Special items must have a discount</assert>
            </rule>
            <rule context="//item">
              <assert test="@price">Regular items must have a price</assert>
            </rule>
          </pattern>
        </schema>
        "#,
    )
    .unwrap();

    let doc = Document::parse_str(
        r#"<order>
  <item price="10"/>
  <item special="yes" discount="5"/>
  <item special="yes"/>
  <item/>
</order>"#,
    )
    .unwrap();

    let result = validate_schematron(&doc, &schema);
    assert!(!result.is_valid);

    // item[1]: no @special, matches rule 2 → needs @price → has it → OK
    // item[2]: @special, matches rule 1 → needs @discount → has it → OK
    // item[3]: @special, matches rule 1 → needs @discount → MISSING → error
    //   (does NOT match rule 2 because it already fired in rule 1)
    // item[4]: no @special, matches rule 2 → needs @price → MISSING → error
    assert_eq!(
        result.errors.len(),
        2,
        "expected exactly 2 errors, got: {:?}",
        result.errors
    );
}

#[test]
fn test_schema_level_variable_used_across_patterns() {
    let schema = parse_schematron(
        r#"
        <schema xmlns="http://purl.oclc.org/dml/schematron">
          <let name="currency" value="'USD'"/>
          <let name="min_order" value="10"/>

          <pattern id="p1">
            <rule context="/order">
              <assert test="@currency = $currency">Order currency must be <value-of select="$currency"/></assert>
            </rule>
          </pattern>

          <pattern id="p2">
            <rule context="/order">
              <assert test="count(item) >= $min_order">Need at least <value-of select="$min_order"/> items</assert>
            </rule>
          </pattern>
        </schema>
        "#,
    )
    .unwrap();

    let doc = Document::parse_str(r#"<order currency="EUR"><item/></order>"#).unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(!result.is_valid);

    let currency_error = result.errors.iter().any(|e| e.message.contains("USD"));
    assert!(currency_error, "should mention USD in error");

    let count_error = result.errors.iter().any(|e| e.message.contains("10"));
    assert!(count_error, "should mention min_order of 10");
}

#[test]
fn test_classic_namespace_schema() {
    // Validate that classic Schematron 1.5 namespace also works end-to-end
    let schema = parse_schematron(
        r#"
        <schema xmlns="http://www.ascc.net/xml/schematron">
          <pattern>
            <rule context="/doc">
              <assert test="title">Document must have a title</assert>
              <assert test="body">Document must have a body</assert>
            </rule>
          </pattern>
        </schema>
        "#,
    )
    .unwrap();

    let doc_valid =
        Document::parse_str("<doc><title>Hello</title><body>World</body></doc>").unwrap();
    assert!(validate_schematron(&doc_valid, &schema).is_valid);

    let doc_invalid = Document::parse_str("<doc><title>Hello</title></doc>").unwrap();
    let result = validate_schematron(&doc_invalid, &schema);
    assert!(!result.is_valid);
    assert_eq!(result.errors.len(), 1);
    assert!(result.errors[0].message.contains("body"));
}

#[test]
fn test_sum_attribute_values() {
    // Integration test for the XPath attribute NodeSet fix
    let schema = parse_schematron(
        r#"
        <schema xmlns="http://purl.oclc.org/dml/schematron">
          <pattern>
            <rule context="/invoice">
              <let name="total" value="sum(item/@amount)"/>
              <assert test="$total = number(@expected)">Total <value-of select="$total"/> does not match expected <value-of select="@expected"/></assert>
            </rule>
          </pattern>
        </schema>
        "#,
    )
    .unwrap();

    let doc = Document::parse_str(
        r#"<invoice expected="60"><item amount="10"/><item amount="20"/><item amount="30"/></invoice>"#,
    )
    .unwrap();
    let result = validate_schematron(&doc, &schema);
    assert!(
        result.is_valid,
        "sum of attributes should work: {:?}",
        result.errors
    );
}
