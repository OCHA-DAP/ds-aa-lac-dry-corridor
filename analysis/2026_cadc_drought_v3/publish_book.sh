#!/usr/bin/env bash
# Publish the rendered book to /book/ on the gh-pages branch.
#
# Use this instead of `quarto publish gh-pages`, which runs `git rm -r .` on the whole
# gh-pages branch before copying the book in. That would delete the landing page and every
# other product on the site (layout: README.md on the gh-pages branch). This script only
# touches book/ and refuses to push if anything outside it changed.
#
# Usage (from anywhere in the repo):
#   analysis/2026_cadc_drought_v3/publish_book.sh              render, then publish
#   analysis/2026_cadc_drought_v3/publish_book.sh --no-render  publish the existing _book/
#   add --dry-run to either to see what would change without committing or pushing
set -euo pipefail

render=1
dry_run=0
for arg in "$@"; do
  case "$arg" in
    --no-render) render=0 ;;
    --dry-run) dry_run=1 ;;
    *) echo "usage: $0 [--no-render] [--dry-run]" >&2; exit 2 ;;
  esac
done

book_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(git -C "$book_dir" rev-parse --show-toplevel)"
source_rev="$(git -C "$repo_root" describe --always --dirty)"

if [ "$render" = 1 ]; then
  quarto render "$book_dir"
fi
if [ ! -f "$book_dir/_book/index.html" ]; then
  echo "error: $book_dir/_book/index.html not found; render the book first" >&2
  exit 1
fi

git -C "$repo_root" fetch origin gh-pages
worktree="$(mktemp -d)/gh-pages"
git -C "$repo_root" worktree add --detach "$worktree" origin/gh-pages
trap 'cd "$repo_root" && git worktree remove --force "$worktree"' EXIT

rsync -a --delete --exclude .DS_Store "$book_dir/_book/" "$worktree/book/"

cd "$worktree"
# -f because quarto output (html/css/js/png) matches global ignore patterns in many setups
git add -A -f -- book
outside="$(git status --porcelain --untracked-files=all | grep -v '^.. book/' || true)"
if [ -n "$outside" ]; then
  echo "error: refusing to publish, changes outside book/:" >&2
  echo "$outside" >&2
  exit 1
fi
if git diff --cached --quiet; then
  echo "Nothing to publish: book/ on gh-pages already matches _book/."
  exit 0
fi

git diff --cached --stat | tail -1
if [ "$dry_run" = 1 ]; then
  echo "Dry run: not committing or pushing."
  exit 0
fi
git commit -q -m "Publish book to /book/ from $source_rev"
git push origin HEAD:gh-pages
echo "Published: https://ocha-dap.github.io/ds-aa-lac-dry-corridor/book/"
