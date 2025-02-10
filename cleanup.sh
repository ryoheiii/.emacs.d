#!/bin/bash

# ターゲットディレクトリ
VAR_DIR="./var"
ELISP_DIR="./loads/elisp"

echo "Cleaning Emacs auto created files..."

# var ディレクトリ削除
if [ -d "$VAR_DIR" ]; then
    echo "Removing $VAR_DIR ..."
    rm -rf "$VAR_DIR"
fi

# loads/elisp 内のファイルを削除
if [ -d "$ELISP_DIR" ]; then
    echo "Removing contents of $ELISP_DIR ..."
    rm -rf "$ELISP_DIR"/*
fi

echo "Emacs cleanup complete."
