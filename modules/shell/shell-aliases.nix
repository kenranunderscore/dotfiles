{
  dea = "direnv allow";
  ded = "direnv deny";
  # create a new graphical emacs frame, not tied to a shell
  e = "emacsclient --alternate-editor '' --create-frame --no-wait";
  # kill the emacs daemon
  ekill = "emacsclient --eval '(kill-emacs)'";
  # try using emacs/magit as replacement for CLI git
  eg =
    "emacsclient --alternate-editor '' --create-frame --eval '(magit-status)'";

  # git aliases
  g = "git";
  ga = "git add";
  gap = "git add -p";
  gc = "git commit -v";
  gca = "git commit -v --amend";
  gcl = "git clone";
  gcm = "git commit -v -m";
  gd = "git diff";
  gds = "git diff --staged";
  gf = "git fetch";
  gfa = "git fetch --all";
  gi = "git init";
  gl = "git pull";
  glog = "git log --oneline --decorate --graph";
  glo = "git log --oneline --decorate";
  gm = "git merge";
  gp = "git push";
  gr = "git remote";
  gra = "git remote add";
  grb = "git rebase";
  grs = "git restore";
  grss = "git restore --staged";
  grv = "git remote -v";
  gs = "git status --short";
  gst = "git status";
  gsw = "git switch";
  gupa = "git pull --rebase --autostash";

  # misc
  md = "mkdir -p";
  s = "sudo";
  hs = "home-manager switch";
}
