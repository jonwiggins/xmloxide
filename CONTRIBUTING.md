# Contributing to xmloxide

Thank you for your interest in contributing to xmloxide. This guide explains how
to get involved, what we expect from contributions, and how to set up your
development environment.

xmloxide is a pure Rust reimplementation of libxml2 targeting full W3C XML 1.0
conformance. Contributions of all kinds are welcome: bug reports, test cases,
documentation improvements, and code changes.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [How to Contribute](#how-to-contribute)
- [Pull Request Process](#pull-request-process)
- [Code Style](#code-style)
- [Commit Messages](#commit-messages)
- [Testing](#testing)
- [Architecture Overview](#architecture-overview)
- [Spec References](#spec-references)
- [Security](#security)
- [License](#license)

## Code of Conduct

Be kind, be constructive, and be patient. This is a community project maintained
by volunteers. Treat everyone with respect regardless of experience level,
background, or the nature of their contribution. Harassment or abusive behavior
will not be tolerated.

## Getting Started

### Prerequisites

- **Rust** 1.81 or later (the minimum supported Rust version)
- **Git**
- A working internet connection (to download dev-dependencies on first build)

### Building

```sh
# Clone the repository
git clone https://github.com/jonwiggins/xmloxide.git
cd xmloxide

# Build the project
cargo build

# Run all tests
cargo test

# Run the linter
cargo clippy --all-targets --all-features -- -D warnings

# Check formatting
cargo fmt --all -- --check
```

### Installing Git Hooks

We provide a pre-commit hook that runs formatting checks, clippy, and the full
test suite before each commit. Install it with:

```sh
./scripts/install-hooks.sh
```

This ensures you catch issues locally before pushing.

## Development Setup

The project uses standard Cargo tooling with a few configuration files:

| File | Purpose |
|------|---------|
| `rustfmt.toml` | Formatting rules (max width 100, shorthand syntax) |
| `clippy.toml` | Clippy thresholds (argument count, type complexity) |
| `Cargo.toml` `[lints]` | Clippy pedantic enabled, `unwrap`/`expect` warned |
| `scripts/pre-commit` | Pre-commit hook (fmt + clippy + test + doc) |
| `.github/workflows/ci.yml` | CI pipeline (fmt, clippy, test on stable + MSRV, docs) |

## How to Contribute

### Reporting Bugs

Open an issue on GitHub with:

1. A minimal XML/HTML input that reproduces the problem
2. The expected behavior (what libxml2 does, or what the spec says)
3. The actual behavior (what xmloxide does)
4. Your Rust version and OS

### Suggesting Features

Open an issue describing the feature, its use case, and which part of the spec
it relates to (if applicable). Check the [phased implementation plan](#architecture-overview)
to see if it's already planned.

### Contributing Code

1. **Check existing issues** to see if someone is already working on it.
2. **Open an issue first** for non-trivial changes to discuss the approach.
3. **Fork the repository** and create a feature branch.
4. **Make your changes** following the guidelines below.
5. **Submit a pull request** against `main`.

### Current Project Status

xmloxide v0.1.0 implements the full feature set: XML/HTML parsing, DOM, SAX2,
XmlReader, XPath 1.0, DTD/RelaxNG/XSD validation, C14N, XInclude, XML Catalogs,
and C/C++ FFI. Contributions that improve conformance, performance, error
messages, documentation, or test coverage are especially welcome.

### Good First Contributions

Look for issues labeled `good-first-issue` or `help-wanted`. Other good entry
points:

- Adding test cases for edge cases in existing parsers
- Improving error messages with better source locations
- Adding doc examples to public API items
- Fixing clippy warnings that were selectively allowed

## Pull Request Process

1. **Create a focused PR.** Each PR should address one logical change. Split
   unrelated changes into separate PRs.

2. **Ensure CI passes.** Your PR must pass all CI checks:
   - `cargo fmt --all -- --check`
   - `cargo clippy --all-targets --all-features -- -D warnings`
   - `cargo test --all-features` (on both stable and MSRV 1.81)
   - `cargo doc --all-features --no-deps` with no warnings

3. **Write tests.** Bug fixes should include a regression test. New features
   should include unit tests and, where applicable, roundtrip tests
   (parse -> serialize -> parse -> assert equality).

4. **Update documentation.** If your change affects the public API, update the
   relevant doc comments. Every public item needs a doc comment.

5. **Reference the spec.** If your change implements or fixes behavior defined
   in a W3C spec, include the spec section reference (e.g., "XML 1.0 section 2.3").

6. **Fill out the PR template.** Describe what you changed, why, and how to
   test it.

PRs will be reviewed for correctness, conformance with the spec, code style,
and test coverage. Maintainers may request changes before merging.

## Code Style

All code must be formatted with `rustfmt` and pass `clippy` at the pedantic
level with zero warnings. The key rules:

### Formatting

- **Max line width:** 100 characters (configured in `rustfmt.toml`)
- **Use `?` shorthand** for error propagation (configured in `rustfmt.toml`)
- Run `cargo fmt --all` before committing

### Linting

- **Clippy pedantic** is enabled project-wide
- Specific pedantic lints are allowed only with justification (see `Cargo.toml`)
- `unwrap()` and `expect()` are **warned against** in library code — use `?` or
  return `Result` instead
- `unwrap()` is acceptable in tests
- `unsafe` blocks require a `// SAFETY:` comment explaining the invariant

### Naming

| Kind | Convention | Example |
|------|-----------|---------|
| Types | `PascalCase` | `NodeId`, `Document`, `ParseOptions` |
| Functions / methods | `snake_case` | `parse_str`, `root_element` |
| Constants | `SCREAMING_SNAKE_CASE` | `MAX_ENTITY_DEPTH` |
| Modules | `snake_case` | `tree`, `parser`, `xpath` |
| Newtype indices | Always use the newtype | `NodeId`, not `u32` |

### API Design

- Parse functions return `Result<Document, ParseError>` — never panic
- Navigation returns `Option<NodeId>` — missing nodes are `None`, not errors
- Traversal uses iterators: `children()`, `ancestors()`, `descendants()`
- All tree mutation goes through `&mut Document`
- Builder pattern for options: `ParseOptions::default().recover(true)`
- No global state — each `Document` is self-contained and `Send + Sync`

### Dependencies

We minimize external dependencies. Do not add new dependencies without
discussing it in an issue first. See the [dependencies policy](CLAUDE.md) for
rationale on what is and isn't allowed.

## Commit Messages

Follow this format:

```
<module>: <imperative summary>
```

Examples:

```
parser: handle CDATA sections inside mixed content
tree: add NodeId newtype with NonZeroU32
serial: fix attribute escaping for double quotes
ci: add clippy and tests to pre-commit hook
```

Rules:

- **Module prefix:** Use the module name that best describes the change
  (`parser`, `tree`, `serial`, `xpath`, `html`, `sax`, `encoding`, `ci`, `doc`,
  etc.)
- **Imperative mood:** "add", "fix", "handle", "implement" — not "added" or
  "adds"
- **Lowercase** after the colon
- **No trailing period**
- **One logical change per commit** — don't mix formatting fixes with feature
  work
- Keep the summary line under 72 characters
- Add a body separated by a blank line for non-obvious changes

## Testing

### Running Tests

```sh
# All tests
cargo test

# Tests for a specific module
cargo test tree::
cargo test parser::

# Run benchmarks
cargo bench
```

### Test Conventions

- **Unit tests** go in the same file as the code, inside a
  `#[cfg(test)] mod tests { ... }` block
- **Integration tests** go in the `tests/` directory
- **Test names** follow `test_<function>_<scenario>`:
  `test_parse_str_empty_document`, `test_node_append_child_to_leaf`
- **Roundtrip tests** are strongly encouraged: parse the input, serialize it,
  parse it again, and compare the trees
- Use `pretty_assertions` for comparing large strings or structures

### W3C Conformance Tests

The W3C XML Conformance Test Suite is our primary conformance benchmark. To
download and run it:

```sh
./scripts/download-conformance-suite.sh
cargo test --all-features conformance
```

The test suite files are gitignored and must be downloaded separately.

### What Needs Tests

- **Bug fixes:** Always include a regression test that fails without the fix
- **New parser features:** Valid input, malformed input (error recovery), and
  edge cases
- **New node types:** Parse and serialize roundtrip
- **XPath features:** Expression evaluation against known documents

## Architecture Overview

xmloxide uses **arena allocation with typed indices** for its tree
representation. All nodes live in a `Vec<NodeData>` owned by the `Document`,
and are referenced by `NodeId` (a `NonZeroU32` newtype). This provides O(1)
node access, cache-friendly layout, no reference counting, and safe bulk
deallocation.

The parser is a **hand-rolled recursive descent parser** (not combinator-based)
because:

1. libxml2's parser is recursive descent and we need identical behavior
2. Error recovery requires fine-grained control over parse state
3. Push/incremental parsing requires suspendable state
4. No abstraction overhead

For a detailed architecture description, module map, and phased implementation
plan, see [CLAUDE.md](CLAUDE.md).

## Spec References

These are the specifications xmloxide implements. When contributing parser or
serialization changes, reference the relevant section:

- [XML 1.0 (Fifth Edition)](https://www.w3.org/TR/xml/)
- [Namespaces in XML 1.0](https://www.w3.org/TR/xml-names/)
- [XPath 1.0](https://www.w3.org/TR/xpath-10/)
- [Canonical XML 1.0](https://www.w3.org/TR/xml-c14n/)
- [RelaxNG](https://relaxng.org/spec-20011203.html)
- [W3C XML Schema](https://www.w3.org/TR/xmlschema-1/)
- [HTML 4.01](https://www.w3.org/TR/html401/)
- [RFC 3986 (URI syntax)](https://www.rfc-editor.org/rfc/rfc3986)

## Security

If you discover a security vulnerability, please **do not** open a public
issue. Instead, report it privately using GitHub's
[private vulnerability reporting](https://github.com/jonwiggins/xmloxide/security/advisories/new)
feature on the repository.

Include:

- A minimal input that triggers the issue
- The observed behavior and potential impact
- Your suggested fix (if you have one)

We will acknowledge the report, coordinate a fix, and credit you in the
advisory (unless you prefer otherwise).

## License

By contributing to xmloxide, you agree that your contributions will be licensed
under the [MIT License](LICENSE), the same license as the project.
