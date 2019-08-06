typeset -U path
path=(
    $HOME/.cargo/bin
    $HOME/.cabal/bin
    $HOME/.ghcup/bin
    /usr/local/bin
    /usr/bin
    /bin
)

[[ -f /bin/zsh ]] && export SHELL=/bin/zsh
[[ -f /usr/bin/zsh ]] && export SHELL=/usr/bin/zsh
[[ -f /usr/local/bin/zsh ]] && export SHELL=/usr/local/bin/zsh
