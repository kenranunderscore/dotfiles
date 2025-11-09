#!/usr/bin/env bash

set -e

dotfile_dir="$1"
source="$dotfile_dir/$(cut -d'/' -f5- <<<"$2")"
destination="$4/$3"

if [ ! -d "$dotfile_dir" ]; then
    echo "✗ '$dotfile_dir' does not exist"
    exit 1
fi

if [ -L "$destination" ]; then
    if [ $(readlink "$destination") = "$source" ]; then
        echo "✓ $destination: nothing to do"
        exit 0
    else
        echo "✗ $destination: clash with existing symlink"
        exit 1
    fi
fi

if [ -e "$destination" ]; then
    echo "✗ $destination: clash"
    exit 1
fi

$DRY_RUN_CMD mkdir -p "$(dirname "$destination")"
if [ -d "$source" ]; then
    $DRY_RUN_CMD ln -snf "$source" "$destination"
else
    $DRY_RUN_CMD ln -s "$source" "$destination"
fi

echo "✓ $destination: created"
