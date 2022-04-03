precmd () {
    local nix_tag
    if [[ x"$IN_NIX_SHELL" == "x" ]]; then
        nix_tag=""
    else
        nix_tag="[nix]"
    fi

    PR_NIX_SHELL=%F{240}$nix_tag%f
}

PROMPT='%(?..%F{red}[$?]%f)$PR_NIX_SHELL%2~%# '
