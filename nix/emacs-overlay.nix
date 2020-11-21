# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "1b1fc523f4286a28193f70bfdbf6c2098e3e8967";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
