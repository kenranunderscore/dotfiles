set -U fisher_path $__fish_config_dir/fisher

set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..]
for file in $fisher_path/conf.d/*.fish
    source $file
end

if test -d "$fisher_path" || set -q fisher_install_lock
    exit
end

set -U fisher_install_lock

echo "fisher: installing..."
curl -sL --silent https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source
echo "fisher: ✓"

set packages \
    jorgebucaran/fisher \
    jorgebucaran/autopair.fish \
    meaningful-ooo/sponge \
    decors/fish-colored-man

for pkg in $packages
    fisher install $pkg
end

set -e fisher_install_lock
