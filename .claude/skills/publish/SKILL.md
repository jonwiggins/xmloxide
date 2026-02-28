---
name: publish
description: Build, verify, and publish the xmloxide crate to crates.io
disable-model-invocation: true
argument-hint: "[version]"
---

# Publish xmloxide to crates.io

Publish version `$ARGUMENTS` of the xmloxide crate. If no version argument is
provided, publish the version currently in Cargo.toml.

## Pre-flight checks

Run ALL of the following checks. If any fail, stop and report the issue.
Do NOT proceed to publishing if any check fails.

1. **Working tree clean**: `git status` must show no uncommitted changes.
   If there are changes, stop and ask the user to commit or stash them.

2. **Format check**: `cargo fmt --all -- --check`

3. **Lint check**: `cargo clippy --all-targets --all-features -- -D warnings`

4. **Full test suite**: `cargo test --all-features`
   All tests must pass (unit, FFI, integration, doc tests).

5. **Doc build**: `cargo doc --all-features --no-deps`
   Must build with no warnings.

6. **Version consistency**: If a version argument was provided, verify
   `Cargo.toml` has that version. If not, update it and commit the change.

7. **Changelog**: Verify CHANGELOG.md has an entry for this version.

8. **Dry run**: `cargo publish --dry-run --all-features`
   Must succeed with no errors (warnings about excluded test files are OK).

## Publish

After ALL pre-flight checks pass, show the user a summary:
- Version being published
- Number of tests passed
- Package size from dry run
- Any warnings from dry run

Then ask the user for final confirmation before proceeding.

Once confirmed:

1. **Tag the release**: `git tag -a v<VERSION> -m "Release v<VERSION>"`
2. **Publish to crates.io**: `cargo publish --all-features`
3. **Push the tag**: `git push origin v<VERSION>`
4. **Create GitHub release**: Use `gh release create v<VERSION> --title "v<VERSION>" --notes-file -`
   with the changelog entry for this version as the release notes.

## Post-publish verification

1. Check that the crate appears on crates.io: `https://crates.io/crates/xmloxide`
2. Report the published version and links to the user.

## Error handling

- If `cargo publish` fails due to authentication, tell the user to run
  `cargo login` first.
- If the tag already exists, ask the user whether to overwrite or skip tagging.
- If any pre-flight check fails, report exactly which check failed and why.
  Do NOT skip checks or proceed with a partial publish.
