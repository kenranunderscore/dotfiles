# This overlay enables us to use neovim HEAD, or in general recent commits.
let rev = "350d5517833d0906f8e90bb882db45460ac5b830";
in (import (builtins.fetchTarball {
  url = "https://github.com/nix-community/neovim-nightly-overlay/archive/${rev}.tar.gz";
}))
