{
  # create a new graphical emacs frame, not tied to a shell
  e = "emacsclient --alternate-editor '' --create-frame --no-wait";
  # kill the emacs daemon
  ekill = "emacsclient --eval '(kill-emacs)'";
  # try using emacs/magit as replacement for CLI git
  eg = "emacsclient --eval '(magit-status)'";

  # git aliases
  g = "git";
  ga = "git add";
  gc = "git commit -v";
  gca = "git commit -v --amend";
  gcm = "git commit -v -m";
  gd = "git diff";
  gds = "git diff --staged";
  gf = "git fetch";
  gl = "git pull";
  glog = "git log --oneline --decorate --graph";
  glo = "git log --oneline --decorate";
  gm = "git merge";
  gp = "git push";
  grb = "git rebase";
  grs = "git restore";
  grss = "git restore --staged";
  gs = "git status --short";
  gst = "git status";
  gsw = "git switch";
  gupa = "git pull --rebase --autostash";

  # misc
  md = "mkdir -p";
  s = "sudo";
  hs = "home-manager switch";
}
