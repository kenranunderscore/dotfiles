if not status is-interactive
    exit
end

abbr --add rr rm -rf
abbr --add s sudo
abbr --add serve-this python3 -m http.server
abbr --add single-monitor xrandr --output HDMI-0 --off
abbr --add v nvim
abbr --add d cd ~/dotfiles
abbr --add doom ~/.config/emacs/bin/doom
abbr --add dual-monitor xrandr --output HDMI-0 --off && xrandr --auto && xrandr --output HDMI-0 --primary --output eDP-1-1 --mode 1920x1080 --right-of HDMI-0
abbr --add md mkdir -p
abbr --add sy systemctl
abbr --add syu systemctl --user
abbr --add jo journalctl -u
abbr --add jou journalctl --user -u

# Emacs
abbr --add e "emacsclient --alternate-editor '' --create-frame --no-wait"
abbr --add ec "emacsclient --alternate-editor '' --no-wait"
abbr --add eg "emacsclient --alternate-editor '' --create-frame --eval '(magit-status)'"
abbr --add ekill "env -u ALTERNATE_EDITOR emacsclient --eval '(kill-emacs)' 2>/dev/null"
abbr --add et "emacsclient --alternate-editor '' --tty"

# Direnv
abbr --add dea direnv allow
abbr --add ded direnv deny
abbr --add der direnv reload

# Docker
abbr --add drun docker run -it --rm
abbr --add drunl docker run -it --rm --mount type=bind,source=$(pwd),target=/foo
abbr --add dsp docker system prune

# Nix
abbr --add n nix
abbr --add nb nix build
abbr --add nbt --set-cursor nix build this#%
abbr --add nb. --set-cursor nix build .#%
abbr --add nd nix develop
abbr --add nd. --set-cursor nix develop .#%
abbr --add nf nix flake
abbr --add nfi nix flake init
abbr --add nfl nix flake lock
abbr --add nfc nix flake check
abbr --add nfu nix flake update
abbr --add nr nix run
abbr --add nrt --set-cursor nix run this#%
abbr --add nr. --set-cursor nix run .#%
abbr --add nrb nixos-rebuild boot --use-remote-sudo --flake ~/dotfiles/
abbr --add nrs nixos-rebuild switch --use-remote-sudo --flake ~/dotfiles/
abbr --add ns nix shell
abbr --add nst --set-cursor nix shell this#% -c fish

# Git
abbr --add g git
abbr --add ga git add
abbr --add gap git add -p
abbr --add gb git branch -v
abbr --add gc git commit -v
abbr --add gca git commit -v --amend
abbr --add gcl git clone
abbr --add gcm --set-cursor git commit -v -m \"%\"
abbr --add gd git diff
abbr --add gds git diff --staged
abbr --add gf git fetch
abbr --add gfa git fetch --all
abbr --add gfu git fetch upstream
abbr --add gi git init
abbr --add gl git pull
abbr --add glo git log --oneline --decorate
abbr --add glog git log --oneline --decorate --graph
abbr --add gm git merge
abbr --add gp git push
abbr --add gpra git pull --rebase --autostash
abbr --add gr git remote
abbr --add gra git remote add
abbr --add grb git rebase
abbr --add grs git restore
abbr --add grss git restore --staged
abbr --add grv git remote -v
abbr --add gs git status --short
abbr --add gst git status
abbr --add gsw git switch
abbr --add gw git worktree
abbr --add gwa git worktree add
abbr --add gwl git worktree list
