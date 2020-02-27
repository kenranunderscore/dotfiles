GOPATH=$HOME/go

typeset -U path
path=(
    $GOPATH/bin
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

# Aliases to manage emacs clients:
#
# Create a new graphical emacs frame, not tied to the shell.
# This should be the command to run for the first and 'main'
# emacs instance.
alias ee="emacsclient -a '' -c -n"
#
# Create a new graphical emacs frame that returns control
# to the shell after finishing. Meant as a replacement
# for use of 'vi'/'vim'.
alias e="emacsclient -a '' -c"
#
# Open a file inside an existing emacs frame.
alias em="emacsclient -a '' -n"
#
# Softly kill the emacs daemon.
alias ekill="emacsclient -e '(save-buffers-kill-emacs)'"
#
# Open a magit-status popup to handle git commands
alias eg="emacsclient -e -c '(magit-status)'"

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
