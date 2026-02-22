#!/bin/bash
# Downloads the W3C XML Conformance Test Suite.
#
# The test files are placed in tests/conformance/xmlconf/.
# This directory is gitignored.

set -euo pipefail

SUITE_URL="https://www.w3.org/XML/Test/xmlts20130923.tar.gz"
TARGET_DIR="tests/conformance"
ARCHIVE_NAME="xmlts20130923.tar.gz"

cd "$(git rev-parse --show-toplevel)"

mkdir -p "$TARGET_DIR"

if [ -d "$TARGET_DIR/xmlconf" ]; then
    echo "Conformance suite already downloaded at $TARGET_DIR/xmlconf"
    echo "To re-download, remove the directory first:"
    echo "  rm -rf $TARGET_DIR/xmlconf"
    exit 0
fi

echo "Downloading W3C XML Conformance Test Suite..."
curl -fSL "$SUITE_URL" -o "$TARGET_DIR/$ARCHIVE_NAME"

echo "Extracting..."
tar -xzf "$TARGET_DIR/$ARCHIVE_NAME" -C "$TARGET_DIR"

rm "$TARGET_DIR/$ARCHIVE_NAME"

echo "Done. Test suite extracted to $TARGET_DIR/xmlconf/"
echo "Run conformance tests with: cargo test --test conformance"
