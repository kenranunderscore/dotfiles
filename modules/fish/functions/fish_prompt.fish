function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
  
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_show_informative_status 1
    set -g __fish_git_prompt_use_informative_chars 0
    set -g __fish_git_prompt_showupstream informative
    set -g __fish_git_prompt_hide_untrackedfiles 1

    set -g __fish_git_prompt_char_cleanstate ""
    set -g __fish_git_prompt_char_stateseparator " "
    set -g __fish_git_prompt_char_dirtystate "*"
    set -g __fish_git_prompt_char_invalidstate "#"
    set -g __fish_git_prompt_char_stagedstate "+"
    set -g __fish_git_prompt_char_stashstate "\$"
    set -g __fish_git_prompt_char_untrackedfiles "…"
    set -g __fish_git_prompt_char_upstream_ahead ">"
    set -g __fish_git_prompt_char_upstream_behind "<"
    set -g __fish_git_prompt_char_upstream_diverged "<>"

    set -g __fish_git_prompt_color_branch green --bold
    set -g __fish_git_prompt_color_dirtystate blue
    set -g __fish_git_prompt_color_stagedstate yellow
    set -g __fish_git_prompt_color_invalidstate red
    set -g __fish_git_prompt_color_cleanstate green --bold

    # Virtualenv (any)
    if set -q MY_VENV
        printf '%s ' "(venv)"
    end

    set -l color_cwd
    set -l suffix
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        else
            set color_cwd red
        end
        set suffix '#'
    else
        set color_cwd magenta
        set suffix '%'
    end
  
    # PWD
    set_color $color_cwd
    echo -n (prompt_pwd)
    set_color normal
  
    printf '%s ' (fish_git_prompt)
  
    set -l status_color (set_color $fish_color_status)
    set -l statusb_color (set_color --bold $fish_color_status)
    set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
    echo -n $prompt_status
    set_color normal
  
    echo -n "$suffix "
end
