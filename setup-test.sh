#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source <(
    sed -n \
        -e '/^update_neo_checkout()/,/^}/p' \
        -e '/^install_extension_sources()/,/^)/p' \
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

NEO_EXTENSIONS_SOURCE="$TEST_ROOT/neo-extensions-source"
MAV_EXTENSIONS_SOURCE="$TEST_ROOT/mav-extensions-source"
INSTALLED_NEO="$TEST_ROOT/installed-neo"

git init --initial-branch=main "$NEO_EXTENSIONS_SOURCE" >/dev/null
git -C "$NEO_EXTENSIONS_SOURCE" config user.name "NEO setup test"
git -C "$NEO_EXTENSIONS_SOURCE" config user.email "neo-setup@example.invalid"
mkdir -p "$NEO_EXTENSIONS_SOURCE/extensions/neo/example"
echo neo >"$NEO_EXTENSIONS_SOURCE/extensions/neo/example/source"
git -C "$NEO_EXTENSIONS_SOURCE" add extensions
git -C "$NEO_EXTENSIONS_SOURCE" commit -m "NEO extension source" >/dev/null

git init --initial-branch=main "$MAV_EXTENSIONS_SOURCE" >/dev/null
git -C "$MAV_EXTENSIONS_SOURCE" config user.name "NEO setup test"
git -C "$MAV_EXTENSIONS_SOURCE" config user.email "neo-setup@example.invalid"
mkdir -p "$MAV_EXTENSIONS_SOURCE/extensions/mav/example"
echo mav >"$MAV_EXTENSIONS_SOURCE/extensions/mav/example/source"
git -C "$MAV_EXTENSIONS_SOURCE" add extensions
git -C "$MAV_EXTENSIONS_SOURCE" commit -m "Mav extension source" >/dev/null

mkdir -p "$INSTALLED_NEO"
install_extension_sources \
    "$INSTALLED_NEO" \
    "$NEO_EXTENSIONS_SOURCE" \
    "$MAV_EXTENSIONS_SOURCE"

test "$(cat "$INSTALLED_NEO/extensions/extensions/neo/example/source")" = "neo"
test "$(cat "$INSTALLED_NEO/extensions/extensions/mav/example/source")" = "mav"

echo "setup.sh tests passed"
