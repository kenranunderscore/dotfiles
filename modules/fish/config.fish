if not status is-interactive
    exit
end

set fish_greeting
bind \cp navigate_to_project
bind \cr command_history_search
bind \cv open_file_in_emacs

fish_add_path --path $HOME/.config/emacs/bin
fish_add_path --path $HOME/.local/bin

set -l joined (string join " " $fish_complete_path)
set -l prev_joined (string replace --regex "[^\s]*generated_completions.*" "" $joined)
set -l post_joined (string replace $prev_joined "" $joined)
set -l prev (string split " " (string trim $prev_joined))
set -l post (string split " " (string trim $post_joined))
set fish_complete_path $prev "$XDG_DATA_HOME/fish/home-manager_generated_completions" $post

direnv hook fish | source
