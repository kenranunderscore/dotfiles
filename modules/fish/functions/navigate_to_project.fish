function navigate_to_project \
    --description 'Navigate to one of the known project directories'
    set fd fd --type d --max-depth 1 --color never .
    set fzf fzf --prompt "Navigate to project: "

    function is_git_repo
        set -l dir "$argv[1]"
        set -l gitdir "$dir/.git"
        test -d "$gitdir"; or test -f "$gitdir"
    end

    set project_dirs ~/projects \
                     ~/ag \
                     ~/tmpdev
    set projects dotfiles

    for dir in $project_dirs
        set subdirs ($fd $dir 2>/dev/null)
        for sub in $subdirs
            if is_git_repo "$sub"
                set p (string replace --regex "^$HOME/" "" "$sub")
                set -a projects "$p"
            else
                for possible_worktree in ($fd $sub 2>/dev/null)
                    if is_git_repo "$possible_worktree"
                        set p (string replace --regex "^$HOME/" "" "$possible_worktree")
                        set -a projects "$p"
                    end
                end
            end
        end
        set -a projects $($fd $project_dirs[$i] 2>/dev/null | sed 's/\/home\/\w\+\///')
    end

    set target $(printf "%s\n" $projects | $fzf)
    if test -n "$target"
        cd ~/$target
    end
    commandline -f repaint
end
