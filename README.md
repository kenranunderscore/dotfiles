# My home configuration

I use [home-manager](https://github.com/rycee/home-manager) to manage my
dotfiles and available programs.

## Bootstrapping

`nix-shell -p git gnumake --run 'git clone --recurse-submodules
https://github.com/kenranunderscore/dotfiles && cd dotfiles && make bootstrap'`

Won't work the first time because `home-manager` will still be missing.
Relogging is the workaround for now.
