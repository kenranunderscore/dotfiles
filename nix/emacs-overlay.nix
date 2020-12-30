# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "208c2bd388eee9b58dd56dd4ba1db863fee1dcba";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
