typeset -U path
path=(
    $HOME/.yarn/bin
    $HOME/.cargo/bin
    $HOME/.cabal/bin
    $HOME/.ghcup/bin
    $HOME/.local/bin
    $HOME/.nix-profile/bin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /usr/sbin
    /bin
    /sbin
)

[[ -f /bin/zsh ]] && export SHELL=/bin/zsh
[[ -f /usr/bin/zsh ]] && export SHELL=/usr/bin/zsh
[[ -f /usr/local/bin/zsh ]] && export SHELL=/usr/local/bin/zsh

alias s="sudo"

alias cp="command cp -i"
alias mv="command mv -i"

alias l="ls -GFAtr --color"
alias la="ls -AF --color"
alias ll="l -l"
alias lh="l -H"
alias lr="l -R"
alias lk="la -l"

alias sl="ln -sf"
alias md="mkdir -p"

alias pdflatex="TEXINPUTS=\"$HOME/active-group/howto/tex:\" pdflatex"

function mc () {
    md $1
    cd $1
}
