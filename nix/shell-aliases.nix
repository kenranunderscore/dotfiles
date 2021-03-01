{
  # create a new graphical emacs frame, not tied to a shell
  e = "emacsclient --alternate-editor '' --create-frame --no-wait";
  # kill the emacs daemon
  ekill = "emacsclient --eval '(kill-emacs)'";

  # git aliases
  g = "git";
  ga = "git add";
  gc = "git commit -v";
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
  gs = "git status --short";
  gst = "git status";
  gupa = "git pull -r --autostash";

  # misc
  md = "mkdir -p";
}
