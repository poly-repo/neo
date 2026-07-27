#!/bin/bash
# Ehi, Emacs, this is *-* shell-script -*-

set -euo pipefail

ensure_user_local_bin_on_path() {
    local user_local_bin="$HOME/.local/bin"

    case ":${PATH:-}:" in
        *":$user_local_bin:"*) ;;
        *) export PATH="$user_local_bin${PATH:+:$PATH}" ;;
    esac
}

ensure_user_local_bin_on_path

update_neo_checkout() {
    local target_dir="$1"
    local upstream="origin/main"

    git -C "$target_dir" fetch origin main

    if git -C "$target_dir" merge-base --is-ancestor HEAD "$upstream"; then
        git -C "$target_dir" merge --ff-only "$upstream"
        return
    fi

    if [ -n "$(git -C "$target_dir" status --porcelain --untracked-files=all)" ]; then
        echo "ERROR: $target_dir has local changes and its remote history was replaced." >&2
        echo "Preserve or remove those changes, then run setup.sh again." >&2
        return 1
    fi

    local old_head
    local backup_branch
    old_head="$(git -C "$target_dir" rev-parse HEAD)"
    backup_branch="neo-setup-backup-${old_head:0:12}"

    if ! git -C "$target_dir" show-ref --verify --quiet \
        "refs/heads/$backup_branch"; then
        git -C "$target_dir" branch "$backup_branch" "$old_head"
    fi

    echo "Remote history changed; preserving the previous checkout as $backup_branch."
    git -C "$target_dir" reset --hard "$upstream"
}

install_extension_sources() (
    local target_dir="$1"
    local neo_extensions_url="${2:-https://github.com/poly-repo/neo-extensions.git}"
    local mav_extensions_url="${3:-https://github.com/poly-repo/mav-extensions.git}"
    local temporary_dir
    local extensions_dir="$target_dir/extensions/extensions"

    temporary_dir="$(mktemp -d)"
    trap 'rm -rf "$temporary_dir"' EXIT

    git clone --depth 1 "$neo_extensions_url" \
        "$temporary_dir/neo-extensions"
    git clone --depth 1 "$mav_extensions_url" \
        "$temporary_dir/mav-extensions"

    mkdir -p "$extensions_dir"
    mv "$temporary_dir/neo-extensions/extensions/neo" "$extensions_dir/"
    mv "$temporary_dir/mav-extensions/extensions/mav" "$extensions_dir/"
)

# ==============================================================================
#  NEO SETUP
#  
#  DISCLAIMER: Running untrusted code from the intertubes is the digital 
#  equivalent of eating a mysterious floor-fry. But we promise—pinky swear—
#  that this script is strictly for setting up your ~/neo directory and 
#  Emacs environment. No magic, no malfeasance, just code.
# ==============================================================================

cd "$HOME"

TARGET_DIR="$HOME/neo"
REPO_URL="https://github.com/poly-repo/neo.git"
VENV_PATH="$TARGET_DIR/.neo-python"
REQ_FILE="$TARGET_DIR/requirements.txt"

if [ ! -d "$TARGET_DIR" ]; then
    echo "Directory $TARGET_DIR does not exist. Cloning..."
    git clone "$REPO_URL" "$TARGET_DIR"
    install_extension_sources "$TARGET_DIR"
elif [ -d "$TARGET_DIR/.git" ]; then
    echo "Directory $TARGET_DIR exists and is a git repo. Pulling updates..."
    update_neo_checkout "$TARGET_DIR"
else
    echo "Directory $TARGET_DIR exists but is not a git repository. Assuming it was synced locally and skipping git update."
fi

if [ ! -d "$VENV_PATH" ]; then
    echo "Creating virtual environment in $VENV_PATH..."
    sudo apt install python3-venv

    python3 -m venv "$VENV_PATH"
    
    echo "Upgrading pip in new venv..."
    "$VENV_PATH/bin/pip" install --upgrade pip
else
    echo "Virtual environment already exists."
fi

if [ -f "$REQ_FILE" ]; then
    echo "Installing requirements from $REQ_FILE..."
    "$VENV_PATH/bin/pip" install -r "$REQ_FILE"
else
    echo "Warning: $REQ_FILE not found, skipping pip install."
fi

NEO_DIR="$(cd "$TARGET_DIR" && pwd)"

# True if there's no usable Emacs on PATH: missing entirely, or version <=
# 32.0. The threshold is intentionally set above the documented functional
# minimum (30.2, see README.md) to steer people toward a fresh master build
# rather than because older versions don't work.
emacs_needs_build() {
    if ! command -v emacs >/dev/null 2>&1; then
        return 0
    fi
    local version
    version="$(emacs --version | head -n1 | grep -oE '[0-9]+\.[0-9]+' | head -n1)"
    if [ -z "$version" ]; then
        return 0
    fi
    # Numeric (not string) comparison: sort -V orders "9.0" before "10.0".
    local lowest
    lowest="$(printf '%s\n32.0\n' "$version" | sort -V | head -n1)"
    [ "$lowest" != "32.0" ] || [ "$version" = "32.0" ]
}

# ask_yes_no PROMPT
# Reads from /dev/tty (not stdin, which under `curl | bash` is the piped
# script itself, not the terminal). Returns 0=yes, 1=no, 2=non-interactive
# (no /dev/tty available, e.g. CI) -- callers must check $? explicitly rather
# than `if ask_yes_no ...`, which would collapse the 2 case into "no" and
# silently skip without explanation.
ask_yes_no() {
    local prompt="$1"
    local reply
    # Testing -r /dev/tty isn't enough: the device node exists and is
    # readable by permission bits even with no controlling terminal (e.g.
    # under setsid), so a plain `</dev/tty` redirect fails loudly at read
    # time instead of being caught here. Actually opening the fd is the only
    # reliable non-interactive check; the { } group is needed to suppress
    # the "No such device" message bash prints for a failed exec redirect
    # before a same-line `2>/dev/null` would take effect.
    if ! { exec 3</dev/tty; } 2>/dev/null; then
        return 2
    fi
    read -r -p "$prompt [y/N] " reply <&3
    exec 3<&-
    case "$reply" in
        [yY] | [yY][eE][sS]) return 0 ;;
        *) return 1 ;;
    esac
}

if emacs_needs_build; then
    echo ""
    echo "No Emacs >= 32.0 was found on PATH. NEO can build the latest Emacs"
    echo "from master for you via Ansible. Building will:"
    echo "  - install ansible into $VENV_PATH (if not already present)"
    echo "  - sudo apt install ~60 build-dependency packages (gcc, libgccjit-N-dev,"
    echo "    libgtk, libtree-sitter build tooling, etc.)"
    echo "  - clone and build emacs-libvterm under ~/.cache/emacs-libvterm"
    echo "  - download and build a pinned tree-sitter runtime under ~/.cache/tree-sitter"
    echo "  - shallow-clone Emacs master and 'sudo make install' it into"
    echo "    ~/.local/emacs-master-<date>-gtk3"
    echo "  - install the configured Google and Nerd Font families"
    echo ""

    set +e
    ask_yes_no "Build Emacs master (gtk3) now?"
    build_reply=$?
    set -e

    manual_build_cmd="\"$NEO_DIR/scripts/install-emacs\" master && \"$NEO_DIR/scripts/install-fonts\""

    if [ "$build_reply" -eq 2 ]; then
        echo "Non-interactive shell (no /dev/tty); skipping the build offer."
        echo "Run this yourself later to build Emacs master:"
        echo "  $manual_build_cmd"
    elif [ "$build_reply" -eq 0 ]; then
        # Keep NEO's optional Ansible tooling in its own environment. The
        # helper scripts invoke it directly, so users do not need to activate
        # the environment in their shell.
        if [ ! -x "$VENV_PATH/bin/ansible-playbook" ]; then
            echo "Installing ansible into $VENV_PATH..."
            "$VENV_PATH/bin/pip" install ansible
        fi
        ANSIBLE_PLAYBOOK_BIN="$VENV_PATH/bin/ansible-playbook"

        export ANSIBLE_PLAYBOOK_BIN
        "$NEO_DIR/scripts/install-emacs" master
        "$NEO_DIR/scripts/install-fonts"

        set +e
        ask_yes_no "Install a daily cron job to keep this Emacs build fresh?"
        cron_reply=$?
        set -e

        manual_cron_cmd="\"$NEO_DIR/ansible/scripts/o-emacs-daily-cron\" install"
        if [ "$cron_reply" -eq 2 ]; then
            echo "Non-interactive shell (no /dev/tty); skipping the cron offer."
            echo "Run this yourself later to install the daily cron job:"
            echo "  $manual_cron_cmd"
        elif [ "$cron_reply" -eq 0 ]; then
            "$NEO_DIR/ansible/scripts/o-emacs-daily-cron" install
        else
            echo "Skipping the daily cron job."
        fi
    else
        echo "Skipping the Emacs build. Run this yourself any time:"
        echo "  $manual_build_cmd"
    fi
fi
