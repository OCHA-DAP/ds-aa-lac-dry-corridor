# Pages site (`gh-pages` branch)

**Live:** https://ocha-dap.github.io/ds-aa-lac-dry-corridor/

This branch *is* the site: GitHub Pages serves it as-is (legacy branch mode, no build step).
It follows the team's landing-page convention (ds-knowledge-base
`methods/static-data-apps.md`): a landing page at `/`, each product under its own path.

| Path | What | Source | Gate |
|---|---|---|---|
| `/` | Landing page (`index.html`, `assets/`) | edited here | none |
| `/book/` | Quarto book: trigger development & forecast evaluation, incl. the 2026 season review (chapter 14) | `analysis/2026_cadc_drought_v3/`, published by `publish_book.sh` | JS password prompt |
| `/season-review-2026/` | Redirect stub: the season review was a separate StatiCrypt page until it moved into the book | edited here | none |
| `/*.html` at the root | Redirect stubs: the book lived at the root until 2026-10-08 | edited here | none |

## Never run `quarto publish gh-pages`

It runs `git rm -r .` on this branch before copying the book in, so it deletes the landing
page and the redirect stubs. Publish the book with
`analysis/2026_cadc_drought_v3/publish_book.sh` instead. It only touches `book/` and refuses to
push if anything else changed.

## Adding a page

1. Commit a directory with an `index.html` (e.g. `my-product/index.html`).
2. Add a card to `index.html`: copy an existing `<a class="k">` block and change the href,
   title, blurb and foot.
3. Put a back-to-landing link at the top of the new page (snippet: ds-knowledge-base
   `methods/static-data-apps.md`, "Nested pages link back home"; the book's copy is
   `analysis/2026_cadc_drought_v3/home-link.html`). For an encrypted page, add it to the
   source *before* encrypting. StatiCrypt replaces the whole document on decrypt.
4. Declare the URL in the KB: `surfaces:` on `frameworks/lac-dry-corridor/2026-03-13.md`.

## What the gates protect

The book's password is checked by a script in the page, and the password is in the page
source. It keeps casual visitors out, nothing more. That now includes the 2026 season review,
which was StatiCrypt-encrypted while it was a separate page. The repository is public either
way, so the gate doesn't protect the analysis sources.
