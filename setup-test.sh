#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source <(
    sed -n \
        -e '/^ensure_user_local_bin_on_path()/,/^}/p' \
        -e '/^update_neo_checkout()/,/^}/p' \
        -e '/^install_extension_sources()/,/^)/p' \
        "$SCRIPT_DIR/setup.sh"
)

TEST_ROOT="$(mktemp -d)"
trap 'rm -rf "$TEST_ROOT"' EXIT

(
    HOME="$TEST_ROOT/path-home"
    PATH="/usr/bin:/bin"

    ensure_user_local_bin_on_path
    test "$PATH" = "$HOME/.local/bin:/usr/bin:/bin"

    ensure_user_local_bin_on_path
    test "$PATH" = "$HOME/.local/bin:/usr/bin:/bin"
)

(
    HOME="$TEST_ROOT/path-home"
    PATH="/usr/bin:$HOME/.local/bin:/bin"

    ensure_user_local_bin_on_path
    test "$PATH" = "/usr/bin:$HOME/.local/bin:/bin"
)

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

REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
FAKE_ANSIBLE="$TEST_ROOT/ansible-playbook"
CAPTURE_DIR="$TEST_ROOT/ansible-capture"
mkdir -p "$CAPTURE_DIR"

test -x "$SCRIPT_DIR/scripts/install-emacs"
test -x "$SCRIPT_DIR/scripts/install-fonts"

printf '%s\n' \
    '#!/usr/bin/env bash' \
    'set -euo pipefail' \
    'printf "%s\n" "$0" >"$CAPTURE_DIR/runner"' \
    'printf "%s\n" "$PWD" >"$CAPTURE_DIR/cwd"' \
    'printf "%s\n" "${ANSIBLE_CONFIG:-}" >"$CAPTURE_DIR/config"' \
    'printf "%s\n" "$@" >"$CAPTURE_DIR/args"' \
    >"$FAKE_ANSIBLE"
chmod +x "$FAKE_ANSIBLE"

(
    cd "$TEST_ROOT"
    CAPTURE_DIR="$CAPTURE_DIR" ANSIBLE_PLAYBOOK_BIN="$FAKE_ANSIBLE" \
        "$SCRIPT_DIR/scripts/install-emacs"
)
mapfile -t emacs_args <"$CAPTURE_DIR/args"
test "$(cat "$CAPTURE_DIR/cwd")" = "$REPO_ROOT"
test "$(cat "$CAPTURE_DIR/config")" = "$REPO_ROOT/ansible.cfg"
test "${emacs_args[0]}" = \
    "$REPO_ROOT/infra/ansible/playbooks/emacs/emacs.yaml"
test "${emacs_args[1]}" = "-e"
test "${emacs_args[2]}" = "emacs_version_name=master-gtk3"
test "${emacs_args[3]}" = "--tags"
test "${emacs_args[4]}" = "emacs"

CAPTURE_DIR="$CAPTURE_DIR" ANSIBLE_PLAYBOOK_BIN="$FAKE_ANSIBLE" \
    "$SCRIPT_DIR/scripts/install-emacs" 30.2
mapfile -t emacs_args <"$CAPTURE_DIR/args"
test "${emacs_args[2]}" = "emacs_version_name=30.2-gtk3"

(
    cd "$TEST_ROOT"
    CAPTURE_DIR="$CAPTURE_DIR" ANSIBLE_PLAYBOOK_BIN="$FAKE_ANSIBLE" \
        "$SCRIPT_DIR/scripts/install-fonts"
)
mapfile -t font_args <"$CAPTURE_DIR/args"
test "$(cat "$CAPTURE_DIR/cwd")" = "$REPO_ROOT"
test "$(cat "$CAPTURE_DIR/config")" = "$REPO_ROOT/ansible.cfg"
test "${font_args[0]}" = \
    "$REPO_ROOT/infra/ansible/playbooks/fonts/fonts.yaml"

STANDALONE_NEO="$TEST_ROOT/standalone-neo"
mkdir -p \
    "$STANDALONE_NEO/scripts" \
    "$STANDALONE_NEO/ansible/roles" \
    "$STANDALONE_NEO/ansible/playbooks/emacs" \
    "$STANDALONE_NEO/ansible/playbooks/fonts"
cp "$SCRIPT_DIR/scripts/install-emacs" "$STANDALONE_NEO/scripts/"
cp "$SCRIPT_DIR/scripts/install-fonts" "$STANDALONE_NEO/scripts/"
printf '%s\n' '[defaults]' 'roles_path = ./ansible/roles' \
    >"$STANDALONE_NEO/ansible.cfg"

(
    cd "$TEST_ROOT"
    CAPTURE_DIR="$CAPTURE_DIR" ANSIBLE_PLAYBOOK_BIN="$FAKE_ANSIBLE" \
        "$STANDALONE_NEO/scripts/install-emacs"
)
mapfile -t emacs_args <"$CAPTURE_DIR/args"
test "$(cat "$CAPTURE_DIR/cwd")" = "$STANDALONE_NEO"
test "$(cat "$CAPTURE_DIR/config")" = "$STANDALONE_NEO/ansible.cfg"
test "${emacs_args[0]}" = \
    "$STANDALONE_NEO/ansible/playbooks/emacs/emacs.yaml"

mkdir -p "$STANDALONE_NEO/.neo-python/bin"
touch "$STANDALONE_NEO/.neo-python/bin/python"
chmod +x "$STANDALONE_NEO/.neo-python/bin/python"

(
    cd "$TEST_ROOT"
    CAPTURE_DIR="$CAPTURE_DIR" ANSIBLE_PLAYBOOK_BIN="$FAKE_ANSIBLE" \
        "$STANDALONE_NEO/scripts/install-fonts"
)
mapfile -t font_args <"$CAPTURE_DIR/args"
test "$(cat "$CAPTURE_DIR/cwd")" = "$STANDALONE_NEO"
test "$(cat "$CAPTURE_DIR/config")" = "$STANDALONE_NEO/ansible.cfg"
test "${#font_args[@]}" = 3
test "${font_args[0]}" = \
    "$STANDALONE_NEO/ansible/playbooks/fonts/fonts.yaml"
test "${font_args[1]}" = "-e"
test "${font_args[2]}" = \
    "font_downloader_python=$STANDALONE_NEO/.neo-python/bin/python"

cp "$FAKE_ANSIBLE" \
    "$STANDALONE_NEO/.neo-python/bin/ansible-playbook"
PATH_ANSIBLE="$TEST_ROOT/path-ansible"
mkdir -p "$PATH_ANSIBLE"
printf '%s\n' '#!/usr/bin/env bash' 'exit 99' \
    >"$PATH_ANSIBLE/ansible-playbook"
chmod +x "$PATH_ANSIBLE/ansible-playbook"

CAPTURE_DIR="$CAPTURE_DIR" PATH="$PATH_ANSIBLE:/usr/bin:/bin" \
    "$STANDALONE_NEO/scripts/install-fonts"
test "$(cat "$CAPTURE_DIR/runner")" = \
    "$STANDALONE_NEO/.neo-python/bin/ansible-playbook"

CAPTURE_DIR="$CAPTURE_DIR" PATH="$PATH_ANSIBLE:/usr/bin:/bin" \
    "$STANDALONE_NEO/scripts/install-emacs"
test "$(cat "$CAPTURE_DIR/runner")" = \
    "$STANDALONE_NEO/.neo-python/bin/ansible-playbook"

grep -Fq '"$NEO_DIR/scripts/install-emacs" master' "$SCRIPT_DIR/setup.sh"
grep -Fq '"$NEO_DIR/scripts/install-fonts"' "$SCRIPT_DIR/setup.sh"
grep -Fq 'TARGET_DIR="$HOME/neo"' "$SCRIPT_DIR/setup.sh"
grep -Fq '"$VENV_PATH/bin/pip" install -r "$REQ_FILE"' "$SCRIPT_DIR/setup.sh"
grep -Fq 'ANSIBLE_PLAYBOOK_BIN="$VENV_PATH/bin/ansible-playbook"' \
    "$SCRIPT_DIR/setup.sh"

echo "setup.sh tests passed"
