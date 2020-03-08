HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

ZPLUGIN_HOME=$HOME/.zinit

if [[ ! -f $ZPLUGIN_HOME/bin/zinit.zsh ]]; then
    git clone https://github.com/zdharma/zinit $ZPLUGIN_HOME/bin
fi

source $ZPLUGIN_HOME/bin/zinit.zsh

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

setopt promptsubst

zinit light willghatch/zsh-saneopt

zinit snippet OMZ::lib/theme-and-appearance.zsh
zinit snippet OMZ::lib/completion.zsh
zinit snippet OMZ::lib/key-bindings.zsh
zinit snippet OMZ::plugins/git/git.plugin.zsh

# zinit ice wait"0" atload"_zsh_autosuggest_start"
# zinit light zsh-users/zsh-autosuggestions

zinit ice wait"1" lucid
zinit load psprint/zsh-navigation-tools

zinit ice blockf
zinit light zsh-users/zsh-completions

PURE_PROMPT_SYMBOL="λ"
zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure
zstyle ':prompt:pure:prompt:success' color yellow

zinit ice wait"1" lucid as"program" make"!PREFIX=$ZPFX install" \
    atclone"cp contrib/fzy-* $ZPFX/bin/" \
    pick"$ZPFX/bin/fzy*"
zinit light jhawthorn/fzy

zinit ice wait lucid
zinit load hlissner/zsh-autopair

zinit ice wait"0" atinit"zpcompinit"
zinit light zsh-users/zsh-syntax-highlighting

export GPG_TTY=$(tty)

if (( $+commands[opam] )) then
    eval `opam env`
fi

# if [ -d $HOME/.nix-profile ]; then
    # source $HOME/.nix-profile/etc/profile.d/nix.sh
# fi
