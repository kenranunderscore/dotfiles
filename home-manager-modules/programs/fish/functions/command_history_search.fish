function command_history_search \
    --description 'Reverse-search the command history'
    set fzf fzf --no-sort --exact --prompt "Command: "
    set command (history | $fzf)
    if test -n "$command"
        commandline $command
    end
    commandline -f repaint
end
