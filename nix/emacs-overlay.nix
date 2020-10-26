# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "a69588a3f7de6d68f20cea21562ab7f6f91a400a";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
