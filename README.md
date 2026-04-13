# org-lark

`org-lark` exports Lark/Feishu cloud documents to readable Org files.

It uses a practical two-stage conversion:

1. Fetch Lark's Markdown export with `lark-cli docs +fetch`.
2. Normalize Lark-only components such as `callout`, `lark-table`, `grid`, `whiteboard`, `okr`, and `agenda`.
3. Convert the remaining ordinary Markdown to Org with Pandoc.

Pandoc alone is not enough because Lark's Markdown export contains custom XML-like blocks. For example, `lark-table` is not a real HTML table and gets flattened if it is passed directly to Pandoc.

## Requirements

- Emacs 27.1 or newer
- `lark-cli`, already configured and authorized
- `pandoc`

The exporter calls:

```sh
lark-cli docs +fetch --as user --doc DOC --format json
```

Media assets are downloaded with:

```sh
lark-cli docs +media-download --as user --token TOKEN --output PATH
```

Whiteboard thumbnails use the same command with `--type whiteboard`.

## Usage

Load the package:

```elisp
(add-to-list 'load-path "/path/to/org-lark")
(require 'org-lark)
```

Export a document interactively:

```elisp
M-x org-lark-export
```

Or from Lisp:

```elisp
(org-lark-export
 "https://your-lark-doc"
 "~/notes/lark-parser-test.org")
```

Export the URL or token at point:

```elisp
M-x org-lark-export-url-at-point
```

## Mapping Notes

Common Markdown constructs, including headings up to level 6, emphasis, links, checkboxes, and lists, are delegated to Pandoc.

Lark-specific mappings include:

| Lark export | Org output |
| --- | --- |
| `quote-container`, `quote` | `#+begin_quote` |
| `callout` | `#+begin_lark_callout` with `#+attr_org` |
| `equation` | LaTeX display math |
| `lark-table` without spans | native Org table |
| `lark-table` with `rowspan`/`colspan` | `#+begin_lark_table` by default |
| `grid`, `column` | `#+begin_lark_grid` and `#+begin_lark_column` |
| `image`, `whiteboard`, `file` | local `file:` links under `assets/` |
| `mention-doc`, `mention-user`, `sheet`, `task` | custom Org links |
| `chat-card`, `add-ons`, `source-synced`, `reference-synced` | named Org special blocks |
| `agenda`, `okr` | Org subtrees |
| unsupported block comments | Org comments |

Unknown angle-bracket placeholders such as `<domain>` and `<missing_scope>` are left alone unless their tag name is in the known Lark tag allowlist.

## Customization

Useful variables:

```elisp
(setq org-lark-cli-program "lark-cli")
(setq org-lark-pandoc-program "pandoc")
(setq org-lark-identity "user")
(setq org-lark-download-media t)
(setq org-lark-assets-directory-name "assets")
(setq org-lark-table-span-strategy 'special-block)
```

Use `M-x customize-group RET org-lark RET` for the full set.

## Tests

Tests live in `test/org-lark-test.el` and can be run with:

```sh
emacs -Q --batch -L . -l test/org-lark-test.el -f ert-run-tests-batch-and-exit
```

This workspace currently did not have `emacs` on `PATH` during initial implementation, so the test suite is designed for standard batch Emacs but may need to be run on a machine with Emacs installed.
