#!/usr/bin/env zsh

ANTIGEN_REPO=$HOME/.antigen

if [[ ! -a $ANTIGEN_REPO ]]; then
	git clone https://github.com/zsh-users/antigen.git $ANTIGEN_REPO
fi

source $ANTIGEN_REPO/antigen.zsh

antigen use oh-my-zsh
antigen theme candy

antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting

if [[ "$OSTYPE" == "darwin"* ]]; then
	antigen bundle osx
fi

antigen apply
