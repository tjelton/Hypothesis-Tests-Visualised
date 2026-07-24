#!/usr/bin/env bash
# Deploy the JavaScript static site to the gh-pages branch (GitHub Pages).
#
# The live site is served from the `gh-pages` branch. This script publishes the
# current `main` verbatim, plus a `.nojekyll` marker so Pages serves every file
# as-is (no Jekyll processing). It builds the deploy commit with git plumbing,
# so it never touches your working tree or switches branches, and parents the
# new commit on the existing gh-pages tip (a fast-forward -- old deploys are
# kept in history, no force-push).
#
# Usage (from the repo root, with main pushed to origin):
#   bash tools/deploy_gh_pages.sh
#
# Then check https://tjelton.github.io/Hypothesis-Tests-Visualised/

set -euo pipefail

SOURCE_REF="${1:-origin/main}"   # what to deploy (default: origin/main)
DEPLOY_BRANCH="gh-pages"
REMOTE="origin"

# Repo root, regardless of where the script is invoked from.
cd "$(git rev-parse --show-toplevel)"

echo "Fetching $REMOTE ..."
git fetch --quiet "$REMOTE"

SOURCE_TREE=$(git rev-parse "${SOURCE_REF}^{tree}")
echo "Deploying tree from $SOURCE_REF ($(git rev-parse --short "$SOURCE_REF"))"

# Build a temporary index = source tree + an empty .nojekyll at the root.
IDX="$(mktemp)"
rm -f "$IDX"                       # git read-tree wants the path free
trap 'rm -f "$IDX" "${IDX}.nojekyll" 2>/dev/null || true' EXIT
GIT_INDEX_FILE="$IDX" git read-tree "$SOURCE_TREE"
: > "${IDX}.nojekyll"
BLOB=$(git hash-object -w "${IDX}.nojekyll")
GIT_INDEX_FILE="$IDX" git update-index --add --cacheinfo 100644,"$BLOB",.nojekyll
NEW_TREE=$(GIT_INDEX_FILE="$IDX" git write-tree)

# Commit on top of the current gh-pages tip (fast-forward; history preserved).
PARENT=$(git rev-parse "${REMOTE}/${DEPLOY_BRANCH}")
NEW_COMMIT=$(git commit-tree "$NEW_TREE" -p "$PARENT" -m "Deploy JavaScript static site to gh-pages")

echo "New deploy commit: $(echo "$NEW_COMMIT" | cut -c1-10)"
git push "$REMOTE" "${NEW_COMMIT}:${DEPLOY_BRANCH}"
git branch -f "$DEPLOY_BRANCH" "$NEW_COMMIT" 2>/dev/null || true

echo "Deployed. GitHub Pages will rebuild shortly:"
echo "  https://tjelton.github.io/Hypothesis-Tests-Visualised/"
