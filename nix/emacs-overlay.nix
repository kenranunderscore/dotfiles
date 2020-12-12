# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "16fc85a9522705b7ff1e479cf06975d99ebb7b55";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
