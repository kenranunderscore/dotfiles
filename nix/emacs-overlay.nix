# This overlay enables us to use unstable, HEAD, or even gcc emacs.
let rev = "dbec54615414ada3fe63b69f7e8d81828ce4b64a";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
}))
