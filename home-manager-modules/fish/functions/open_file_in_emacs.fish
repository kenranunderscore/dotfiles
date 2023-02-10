function open_file_in_emacs \
    --description 'Choose a file and open it with emacsclient'
    set fd fd --type f --color never .
    set fzf fzf --prompt "emacsclient -a '' -c "

    set target $($fd 2>/dev/null | $fzf)
    if test -n "$target"
        emacsclient -a '' -c  $target
    end

    commandline -f repaint
end
