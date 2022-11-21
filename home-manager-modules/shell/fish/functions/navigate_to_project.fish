function navigate_to_project \
    --description 'Navigate to one of the known project directories'
    set project_dirs ~/projects \
                     ~/ag \
                     ~/tmpdev \
                     ~/projects/forks
    set fd fd --type d --max-depth 1 . --color never
    set fzf fzf --layout reverse --height 30% --border sharp --prompt "Navigate to project: "
    
    for i in (seq (count $project_dirs))
        set -a projects $($fd $project_dirs[$i] 2>/dev/null || true)
    end
    set -a projects ~/dotfiles
    
    set target (echo -e (string join "\n" $projects) | $fzf)
    if test -n "$target"
        cd $target
    end
    commandline -f repaint
end
