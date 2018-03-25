#!/usr/bin/env bash

dotfile_dir=~/dotfiles
backup_dir=~/backup_dotfiles
targets=".zshrc .Xresources .vimrc .vim .emacs"

echo "Creating $backup_dir..."
mkdir -p $backup_dir
echo "Changing to $dotfile_dir..."
cd $dotfile_dir

echo "Backing up and creating symlinks..."
for target in $targets; do
    mv ~/$target $backup_dir
    ln -s $dotfile_dir/$target ~/$target
done

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
