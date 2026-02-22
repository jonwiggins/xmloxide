## Summary

<!-- Briefly describe what this PR does and why. -->

## Type of Change

<!-- Check all that apply. -->

- [ ] Bug fix (corrects an existing issue)
- [ ] New feature (adds new functionality)
- [ ] Conformance fix (aligns behavior with the W3C spec or libxml2)
- [ ] Refactoring (no functional change)
- [ ] Documentation (docs, comments, examples)
- [ ] CI / tooling (build scripts, workflows, hooks)

## Spec Reference

<!-- If this change implements or fixes spec-defined behavior, cite the section.
     Delete this section if not applicable. -->

- Spec: <!-- e.g., XML 1.0 Fifth Edition -->
- Section: <!-- e.g., section 2.3 "Common Syntactic Constructs" -->

## Test Plan

<!-- Describe how you tested the change. Include new tests you added. -->

- [ ] Added unit tests
- [ ] Added roundtrip test (parse -> serialize -> parse -> compare)
- [ ] Added regression test for the bug being fixed
- [ ] Verified against W3C Conformance Test Suite
- [ ] Tested manually with `xmllint`

## Checklist

- [ ] Code is formatted (`cargo fmt --all`)
- [ ] No clippy warnings (`cargo clippy --all-targets --all-features -- -D warnings`)
- [ ] All tests pass (`cargo test --all-features`)
- [ ] Documentation updated for any public API changes
- [ ] Commit messages follow the `<module>: <summary>` convention
- [ ] No new dependencies added (or discussed and approved in an issue)
