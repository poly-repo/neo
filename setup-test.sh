#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source <(
    sed -n \
        '/^update_neo_checkout()/,/^}/p' \
        "$SCRIPT_DIR/setup.sh"
)

TEST_ROOT="$(mktemp -d)"
trap 'rm -rf "$TEST_ROOT"' EXIT

REMOTE="$TEST_ROOT/neo.git"
OLD_SOURCE="$TEST_ROOT/old-source"
NEW_SOURCE="$TEST_ROOT/new-source"
CHECKOUT="$TEST_ROOT/checkout"

git init --bare --initial-branch=main "$REMOTE" >/dev/null

git init --initial-branch=main "$OLD_SOURCE" >/dev/null
git -C "$OLD_SOURCE" config user.name "NEO setup test"
git -C "$OLD_SOURCE" config user.email "neo-setup@example.invalid"
echo old >"$OLD_SOURCE/version"
git -C "$OLD_SOURCE" add version
git -C "$OLD_SOURCE" commit -m "Old history" >/dev/null
git -C "$OLD_SOURCE" remote add origin "$REMOTE"
git -C "$OLD_SOURCE" push origin main >/dev/null

git clone "$REMOTE" "$CHECKOUT" >/dev/null
OLD_HEAD="$(git -C "$CHECKOUT" rev-parse HEAD)"

git init --initial-branch=main "$NEW_SOURCE" >/dev/null
git -C "$NEW_SOURCE" config user.name "NEO setup test"
git -C "$NEW_SOURCE" config user.email "neo-setup@example.invalid"
echo new >"$NEW_SOURCE/version"
git -C "$NEW_SOURCE" add version
git -C "$NEW_SOURCE" commit -m "Replacement history" >/dev/null
NEW_HEAD="$(git -C "$NEW_SOURCE" rev-parse HEAD)"
git -C "$NEW_SOURCE" remote add origin "$REMOTE"
git -C "$NEW_SOURCE" push --force origin main >/dev/null

update_neo_checkout "$CHECKOUT"

test "$(git -C "$CHECKOUT" rev-parse HEAD)" = "$NEW_HEAD"
test "$(git -C "$CHECKOUT" rev-parse "neo-setup-backup-${OLD_HEAD:0:12}")" \
    = "$OLD_HEAD"
test "$(cat "$CHECKOUT/version")" = "new"

echo local-change >"$CHECKOUT/local-change"
git -C "$OLD_SOURCE" push --force origin main >/dev/null

if update_neo_checkout "$CHECKOUT"; then
    echo "Expected a dirty checkout with replaced history to be rejected." >&2
    exit 1
fi

test "$(git -C "$CHECKOUT" rev-parse HEAD)" = "$NEW_HEAD"
test "$(cat "$CHECKOUT/local-change")" = "local-change"

echo "setup.sh checkout update tests passed"
