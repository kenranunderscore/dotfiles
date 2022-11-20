function command_history_search \
    --description 'Reverse-search the command history'
    set fzf fzf --layout reverse --height 30% --border sharp --prompt "Command: "
    set command (history | $fzf)
    if test -n $command
        commandline $command
    end
end
