{ pkgs }:

{
  # nixos
  nrs = "nixos-rebuild switch --use-remote-sudo --flake ~/dotfiles/";

  # direnv
  dea = "direnv allow";
  ded = "direnv deny";
  der = "direnv reload";

  # Create a new graphical emacs frame, not tied to a shell
  e = "emacsclient --alternate-editor '' --create-frame --no-wait";
  # Attach to existing emacs frame
  ec = "emacsclient --alternate-editor '' --no-wait";
  # Kill the emacs daemon
  ekill = "emacsclient --eval '(kill-emacs)'";
  # Try using emacs/magit as replacement for CLI git
  eg =
    "emacsclient --alternate-editor '' --create-frame --eval '(magit-status)'";

  # git
  g = "git";
  ga = "git add";
  gap = "git add -p";
  gb = "git branch -v";
  gc = "git commit -v";
  gca = "git commit -v --amend";
  gcl = "git clone";
  gcm = "git commit -v -m";
  gd = "git diff";
  gds = "git diff --staged";
  gf = "git fetch";
  gfa = "git fetch --all";
  gfu = "git fetch upstream";
  gi = "git init";
  gl = "git pull";
  glog = "git log --oneline --decorate --graph";
  glo = "git log --oneline --decorate";
  gm = "git merge";
  gp = "git push";
  gpu = "git push --set-upstream origin";
  gr = "git remote";
  gra = "git remote add";
  grb = "git rebase";
  grs = "git restore";
  grss = "git restore --staged";
  grv = "git remote -v";
  gs = "git status --short";
  gst = "git status";
  gsw = "git switch";
  gpra = "git pull --rebase --autostash";

  # nix
  n = "nix";
  nf = "nix flake";
  nb = "nix build";
  nd = "nix develop";
  nr = "nix run";
  ns = "nix shell";

  # systemd
  sy = "systemctl";
  syu = "systemctl --user";
  jo = "journalctl";
  jou = "journalctl --user";

  # docker
  drun = "docker run -it --rm";
  dsp = "docker system prune";

  # misc
  md = "mkdir -p";
  s = "sudo";
  rr = "rm -rf";
  serve-this = "${pkgs.lib.getExe pkgs.python3} -m http.server";
  dual-monitor-setup =
    "xrandr --auto && xrandr --output HDMI-0 --primary --output DP-2 --mode 1920x1080 --rate 72.01 --right-of HDMI-0 && systemctl --user restart polybar";
  hc = "herbstclient";
}
