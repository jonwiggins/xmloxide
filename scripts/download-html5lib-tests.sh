#!/bin/bash
# Downloads the html5lib-tests suite for HTML5 parser conformance testing.
#
# The test files are placed in tests/html5lib-tests/.
# This directory is gitignored.

set -euo pipefail

REPO_URL="https://github.com/html5lib/html5lib-tests.git"
TARGET_DIR="tests/html5lib-tests"

cd "$(git rev-parse --show-toplevel)"

if [ -d "$TARGET_DIR" ]; then
    echo "html5lib-tests already downloaded at $TARGET_DIR"
    echo "To re-download, remove the directory first:"
    echo "  rm -rf $TARGET_DIR"
    exit 0
fi

echo "Cloning html5lib-tests suite..."
git clone --depth 1 "$REPO_URL" "$TARGET_DIR"

# Remove git metadata — we don't need it
rm -rf "$TARGET_DIR/.git"

echo "Done. Test suite cloned to $TARGET_DIR/"
echo "Run tokenizer tests with:          cargo test --test html5lib_tokenizer"
echo "Run tree construction tests with:   cargo test --test html5lib_tree_construction"
