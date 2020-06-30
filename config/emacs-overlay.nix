# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "0fed589bd0208643ae273e44c8fab092fc43fd30";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
