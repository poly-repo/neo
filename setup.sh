#!/bin/bash
# Ehi, Emacs, this is *-* shell-script -*-

# ==============================================================================
#  NEO SETUP
#  
#  DISCLAIMER: Running untrusted code from the intertubes is the digital 
#  equivalent of eating a mysterious floor-fry. But we promise—pinky swear—
#  that this script is strictly for setting up your ~/neo directory and 
#  Emacs environment. No magic, no malfeasance, just code.
# ==============================================================================

cd $HOME

TARGET_DIR="neo"
REPO_URL="https://github.com/poly-repo/neo.git"
VENV_PATH="$TARGET_DIR/.python"
REQ_FILE="$TARGET_DIR/requirements.txt"

if [ ! -d "$TARGET_DIR" ]; then
    echo "Directory $TARGET_DIR does not exist. Cloning..."
    git clone "$REPO_URL" "$TARGET_DIR"
elif [ -d "$TARGET_DIR/.git" ]; then
    echo "Directory $TARGET_DIR exists and is a git repo. Pulling updates..."
    git -C "$TARGET_DIR" pull
else
    echo "ERROR: Directory $TARGET_DIR exists but is NOT a git repository." >&2
    exit 1
fi

if [ ! -d "$VENV_PATH" ]; then
    echo "Creating virtual environment in $VENV_PATH..."
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
