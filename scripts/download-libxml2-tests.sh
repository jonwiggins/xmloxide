#!/usr/bin/env bash
# Downloads libxml2 test and result directories for compatibility testing.
#
# Usage: ./scripts/download-libxml2-tests.sh
#
# The test data is extracted to tests/libxml2-compat/libxml2/ and is
# gitignored. The download is skipped if the directory already exists.

set -euo pipefail

VERSION="2.13.6"
TARBALL_URL="https://download.gnome.org/sources/libxml2/2.13/libxml2-${VERSION}.tar.xz"
DEST_DIR="tests/libxml2-compat/libxml2"

cd "$(git rev-parse --show-toplevel)"

if [ -d "$DEST_DIR/test" ] && [ -d "$DEST_DIR/result" ]; then
    echo "libxml2 test data already present at $DEST_DIR"
    echo "To re-download, remove the directory first: rm -rf $DEST_DIR"
    exit 0
fi

echo "Downloading libxml2 ${VERSION} tarball..."
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

curl -sSL "$TARBALL_URL" -o "$TMPDIR/libxml2.tar.xz"

echo "Extracting test/ and result/ directories..."
mkdir -p "$DEST_DIR"

# Extract only the test/ and result/ directories from the tarball
tar -xf "$TMPDIR/libxml2.tar.xz" -C "$TMPDIR" \
    "libxml2-${VERSION}/test" \
    "libxml2-${VERSION}/result"

# Move into our destination
mv "$TMPDIR/libxml2-${VERSION}/test" "$DEST_DIR/test"
mv "$TMPDIR/libxml2-${VERSION}/result" "$DEST_DIR/result"

echo "Done. Test data extracted to $DEST_DIR"
echo "  test/  : $(find "$DEST_DIR/test" -type f | wc -l | tr -d ' ') files"
echo "  result/: $(find "$DEST_DIR/result" -type f | wc -l | tr -d ' ') files"
