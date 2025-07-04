function open_file_in_editor \
    --description 'Choose a file and open it with emacsclient'
    set fd fd --type f --color never .
    set fzf fzf --prompt "$EDITOR "

    set target $($fd 2>/dev/null | $fzf)
    if test -n "$target"
        eval $EDITOR $target
    end

    commandline -f repaint
end
