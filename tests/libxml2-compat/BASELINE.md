# libxml2 Compatibility Test Baseline

This file tracks the expected pass rates for the libxml2 regression test suite
when run against xmloxide. These numbers serve as a regression detection
mechanism — pass counts should only go up.

## How to run

```sh
./scripts/download-libxml2-tests.sh
cargo test --test libxml2_compat -- --nocapture
```

## Current baseline

> **Note:** Initial baseline — numbers will be updated after the first full run.

| Category | Passed | Total | Pass Rate | Notes |
|----------|--------|-------|-----------|-------|
| XML parse | TBD | TBD | TBD | Parse + serialize roundtrip |
| Namespaces | TBD | TBD | TBD | Namespace handling |
| Error detection | TBD | TBD | TBD | Expected parse failures |
| HTML parse | TBD | TBD | TBD | HTML parse + serialize |

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
