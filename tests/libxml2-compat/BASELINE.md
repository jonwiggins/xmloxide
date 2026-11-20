# libxml2 Compatibility Test Baseline

This file tracks the expected pass rates for the libxml2 regression test suite
when run against xmloxide. These numbers serve as a regression detection
mechanism — pass counts should only go up.

## How to run

```sh
./scripts/download-libxml2-tests.sh
cargo test --test libxml2_compat -- --nocapture
```

The baseline below is for the pinned release (2.13.6). To run the harness
against another libxml2 release, point `XMLOXIDE_LIBXML2_DIR` at a directory
containing that release's `test/` and `result/` trees:

```sh
XMLOXIDE_LIBXML2_DIR=/path/to/libxml2-2.14.6 cargo test --test libxml2_compat -- --nocapture
```

libxml2 2.14 replaced its HTML tokenizer with an HTML5-conformant one, so
several of its `test/HTML` expectations (RCDATA `<title>`/`<textarea>`,
raw-text `<iframe>`/`<noframes>`/`<xmp>`, bogus comments for `<?...>` and
`<![...]>`, the full HTML5 entity table) intentionally differ from the
`xmloxide::html` parser, which follows the 2.13 model. Use `xmloxide::html5`
for WHATWG-conformant parsing (see issue #70).

## Current baseline

| Category | Passed | Total | Pass Rate | Notes |
|----------|--------|-------|-----------|-------|
| XML parse | All | All | 100% | Parse + serialize roundtrip |
| Namespaces | All | All | 100% | Namespace handling |
| Error detection | All | All | 100% | Expected parse failures |
| HTML parse | All | All | 100% | HTML parse + serialize |
| **Overall** | **119** | **119** | **100%** | **All categories combined** |

## Known skip categories

- **External entity tests** (`dtd*`, `ent*`, `valid*`): Require external entity
  resolution with file system access. Can be revisited with entity resolver.
- **Encoding tests** (`iso8859*`, `GB18030`, etc.): Require system iconv or
  extended encoding support beyond what `encoding_rs` provides.
- **XInclude, Catalog, Schema, RelaxNG**: Not yet compared (separate test
  categories in libxml2).

## Update process

After improving xmloxide's conformance, re-run the test suite and update the
numbers above. If pass counts decrease, investigate the regression before
merging.
