HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

ZPLUGIN_HOME=$HOME/.zplugin

if [[ ! -f $ZPLUGIN_HOME/bin/zplugin.zsh ]]; then
    git clone https://github.com/psprint/zplugin $ZPLUGIN_HOME/bin
    zcompile $ZPLUGIN_HOME/bin/zplugin/zsh
fi

source $ZPLUGIN_HOME/bin/zplugin.zsh

autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

setopt promptsubst

zplugin light willghatch/zsh-saneopt

zplugin snippet OMZ::lib/theme-and-appearance.zsh
zplugin snippet OMZ::lib/completion.zsh
zplugin snippet OMZ::plugins/git/git.plugin.zsh

zplugin ice wait"0" atload"_zsh_autosuggest_start"
zplugin light zsh-users/zsh-autosuggestions

zplugin ice wait"1" lucid
zplugin load psprint/zsh-navigation-tools

zplugin ice blockf
zplugin light zsh-users/zsh-completions

zplugin ice pick"async.zsh" src"pure.zsh"
zplugin light sindresorhus/pure

zplugin ice wait"1" lucid as"program" make"!PREFIX=$ZPFX install" \
    atclone"cp contrib/fzy-* $ZPFX/bin/" \
    pick"$ZPFX/bin/fzy*"
zplugin light jhawthorn/fzy

zplugin ice wait lucid
zplugin load hlissner/zsh-autopair

zplugin ice wait"0" atinit"zpcompinit"
zplugin light zdharma/fast-syntax-highlighting

export GPG_TTY=$(tty)

if [ -d $HOME/.nix-profile ]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

if (( $+commands[opam] )) then
    eval `opam env`
fi
