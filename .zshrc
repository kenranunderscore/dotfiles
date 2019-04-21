#!/usr/bin/env zsh

ANTIGEN=$HOME/.antigen
if [[ ! -a $ANTIGEN ]]; then
	git clone https://github.com/zsh-users/antigen.git $ANTIGEN
fi

source $ANTIGEN/antigen.zsh

antigen use oh-my-zsh
antigen theme tobyjamesthomas/pi

antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting

if [[ "$OSTYPE" == "darwin"* ]]; then
	antigen bundle osx
fi

antigen apply
