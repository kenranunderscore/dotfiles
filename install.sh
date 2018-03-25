#!/usr/bin/env bash

dotfile_dir=~/dotfiles
backup_dir=~/dotfiles_backup
targets=".zshrc .Xresources"

echo "Creating $backup_dir..."
mkdir -p $backup_dir
echo "Changing to $dotfile_dir..."
cd $dotfile_dir

echo "Backing up and creating symlinks..."
for target in $targets; do
    mv ~/$target $backup_dir
    ln -s $dotfile_dir/$target ~/$target
done

