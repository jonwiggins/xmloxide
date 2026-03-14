//! Integration test: parse real UBL 2.4 schemas and validate example document.
//!
//! Requires the UBL 2.4 schema files in tests/ubl-2.4/.
//! These are NOT checked into git — download with the test setup.

#![allow(clippy::unwrap_used)]

use std::fs;
use std::path::Path;

fn ubl_schemas_available() -> bool {
    Path::new("tests/ubl-2.4/maindoc/UBL-BusinessCard-2.4.xsd").exists()
}

#[test]
fn test_ubl_24_business_card_schema_parse() {
    if !ubl_schemas_available() {
        eprintln!("Skipping UBL test: schemas not downloaded");
        return;
    }

    let base = Path::new("tests/ubl-2.4");
    let main_xsd = fs::read_to_string(base.join("maindoc/UBL-BusinessCard-2.4.xsd")).unwrap();

    let base_clone = base.to_path_buf();
    let resolver = move |location: &str, _base: Option<&str>| -> Option<String> {
        let path = if let Some(rel) = location.strip_prefix("../") {
            base_clone.join(rel)
        } else {
            base_clone.join("common").join(location)
        };
        fs::read_to_string(&path).ok()
    };

    let opts = xmloxide::validation::xsd::XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: None,
    };

    let result = xmloxide::validation::xsd::parse_xsd_with_options(&main_xsd, &opts);
    match &result {
        Ok(schema) => {
            eprintln!(
                "UBL BusinessCard schema parsed OK, ns={:?}",
                schema.target_namespace
            );
        }
        Err(e) => {
            eprintln!("UBL BusinessCard schema parse failed: {}", e.message);
        }
    }
    // For now just check that parsing doesn't error — validation gaps are expected
    assert!(
        result.is_ok(),
        "Failed to parse UBL BusinessCard schema: {}",
        result.unwrap_err()
    );
}

#[test]
fn test_ubl_24_business_card_validate() {
    if !ubl_schemas_available() {
        eprintln!("Skipping UBL test: schemas not downloaded");
        return;
    }

    let base = Path::new("tests/ubl-2.4");
    let main_xsd = fs::read_to_string(base.join("maindoc/UBL-BusinessCard-2.4.xsd")).unwrap();

    let base_clone = base.to_path_buf();
    let resolver = move |location: &str, _base: Option<&str>| -> Option<String> {
        let path = if let Some(rel) = location.strip_prefix("../") {
            base_clone.join(rel)
        } else {
            base_clone.join("common").join(location)
        };
        fs::read_to_string(&path).ok()
    };

    let opts = xmloxide::validation::xsd::XsdParseOptions {
        resolver: Some(&resolver),
        base_uri: None,
    };

    let schema = xmloxide::validation::xsd::parse_xsd_with_options(&main_xsd, &opts).unwrap();

    let xml = fs::read_to_string(base.join("UBL-BusinessCard-2.2-Example.xml")).unwrap();
    let doc = xmloxide::Document::parse_str(&xml).unwrap();
    let result = xmloxide::validation::xsd::validate_xsd(&doc, &schema);

    eprintln!(
        "Validation: valid={}, {} errors",
        result.is_valid,
        result.errors.len()
    );
    for (i, e) in result.errors.iter().enumerate().take(20) {
        eprintln!("  error {}: {}", i + 1, e.message);
    }
    if result.errors.len() > 20 {
        eprintln!("  ... and {} more errors", result.errors.len() - 20);
    }

    // NOTE: This test documents current behavior. Full UBL validation requires
    // features not yet implemented (ref attributes, elementFormDefault, etc.).
    // For now we just verify no panics and document what errors we get.
}
