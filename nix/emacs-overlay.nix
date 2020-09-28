# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "4ae245ce817bcd017de987022e1b179635b21b67";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
