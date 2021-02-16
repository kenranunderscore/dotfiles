# This overlay enables us to use neovim HEAD, or in general recent commits.
let rev = "cf6e8a71c34b6dea741f920f9e6fe4b5328fb2b5";
in (import (builtins.fetchTarball {
  url =
    "https://github.com/nix-community/neovim-nightly-overlay/archive/${rev}.tar.gz";
}))
