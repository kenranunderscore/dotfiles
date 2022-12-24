function open_file_in_neovim \
    --description 'Choose a file and open it in Neovim'
    set fd fd --type f --color never .
    set fzf fzf --prompt "nvim "

    set target $($fd 2>/dev/null | $fzf)
    if test -n "$target"
        nvim $target
    end

    commandline -f repaint
end
