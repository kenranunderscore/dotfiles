GOPATH=$HOME/go

typeset -U path
path=(
    /run/wrappers/bin
    $HOME/.yarn/bin
    $HOME/.cargo/bin
    $HOME/.cabal/bin
    $HOME/.ghcup/bin
    $HOME/.local/bin
    $HOME/.nix-profile/bin
    /run/current-system/sw/bin
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

# Aliases to manage emacs clients:
#
# Open an emacs frame in the current terminal.
alias e="TERM=xterm-24bit emacsclient -a '' -t"
#
# Create a new graphical emacs frame, not tied to the shell.
alias ee="emacsclient -a '' -c -n"
#
alias ec="emacsclient -a '' -c"
#
# Kill the emacs daemon.
alias ekill="emacsclient -e '(kill-emacs)'"
#
# Open a magit-status terminal frame to handle git commands.
alias eg="e -e '(magit-status)'"

alias l="ls -GFAtr --color"
alias la="ls -AF --color"
alias ls="ls -G --color"
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
