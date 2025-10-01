# Overview

This repository represents the state of the environment(s) I'm currently using
for development, learning and, to some degree, entertainment. Currently there
are mostly Linux machines (one WSL2 environment that I need to include at some
point) I manage this way:

- [atuan](./nixos/atuan): my home PC running NixOS
- [tuon](./users/void@tuon): Void Linux on my current work laptop
- [gentoo](./users/kenran@gentoo): Gentoo Linux on my current work laptop (dual boot with Void above)
- [angband](./nixos/angband): a tiny notebook (64 GB of disk space only) I use when
  traveling
- [paln](./nixos/paln): a tiny [Hetzner Cloud](https://www.hetzner.com/cloud)
  VM I use for synchronization and small backups

The actual (development) environments are managed with
[home-manager](https://github.com/nix-community/home-manager).
`home-manager` makes it possible and easy to bundle binaries / packages with their
respective configuration (usually files inside the home directory or
`$XDG_CONFIG_HOME`).

In the past I was using macOS as well as Ubuntu, but have since fully
transitioned. Note that using a nearly identical setup powered by the Nix
package manager and `home-manager` is not that different. Only a handful of things
are actually managed by the underlying system anymore. (If you're interested,
you should look up how to create a `homeConfigurations.<username>` output in your
`flake.nix`, replacing the `nixosConfigurations.<hostname>` one.)

## Repository and flake structure
### NixOS systems

The [nixos](./nixos) directory contains the machine/host-specific pieces of configuration.
Every subdirectory will lead to a flake output under `nixosConfigurations`, with
the attribute name being the name of the directory.

The directories itself follow a fixed convention:

- `configuration.nix` and `hardware-configuration.nix` (automatically created when
  installing NixOS on the machine for the first time) together constitute the
  NixOS configuration
- `home.nix` is the home manager configuration
- `custom.nix` contains everything that should be passed to all the NixOS and home
  manager modules as arguments; usually this (only) means things that should be
  known everywhere, like my username

### Modules

The [modules](./modules) directory contains the home manager NixOS modules that
all my configurations share (but might choose to disable or enable on a
per-machine basis).

### Home-manager on non-NixOS

The [users](./users) directory contains `home-manager`-only configurations for my non-NixOS
machines. At the moment my work laptop runs both NixOS and Void Linux, and on
the latter I use `home-manager` first-class.

Install a new generation with

```shell
home-manager switch --flake ~/dotfiles --impure
```

(The `--impure` argument is due to the fact that on non-NixOS systems,
applications using OpenGL (or similar) often fail on startup;
[nixGL](https://github.com/nix-community/nixGL) helps with this problem, but
installing it requires this parameter).

## Building/switching to a new generation

I usually use an alias (or `fish` abbreviation in my case) in my shell to
switch to the next generation after changing things in my configuration. The
actual command is one the following, depending on whether it's a NixOS or other
system:

```shell
nixos-rebuild switch --use-remote-sudo --flake ~/dotfiles
```

or

```shell
home-manager switch --flake ~/dotfiles --impure
```

(The `--impure` is necessary so `nixGL` can pick up the OpenGL drivers to build
against.)

The `--flake ~/dotfiles` part works because of two reasons:

- I always clone this repository into my `$HOME`.
- The command performs a lookup on the hostname of the machine; if the `flake.nix`
  contains an output `nixosConfigurations.<hostname>`, this is the configuration
  that will be used.

It's also possible to only switch into the next `home-manager` generation for the
current user without adding a new system generation using the following command
(which is yet another shell alias):

```shell
nix shell "~/dotfiles/#nixosConfigurations.$(hostname).config.home-manager.users.$USER.home.activationPackage" \
          --command home-manager-generation
```

Note, however, that without creating a new system generation these changes will
not be "activated" upon next boot.

# Emacs configuration

A special case of a program with a relatively huge accompanying configuration
is GNU Emacs. My literate Emacs configurations (very much always a
work-in-progress) can be found in the following places:

- [Vanilla Emacs](file:modules/emacs/emacs.d/config.org).
- [Doom Emacs](file:modules/doom/doom/config.org).

I used to use Nix to manage all of my Emacs packages, but have since switched
to [`straight.el`](https://github.com/radian-software/straight.el), as it
allows me to more quickly change my config, try out new packages on the fly
without cluttering anything, and even debug packages. In the case of Doom Emacs
I just follow the pinned versions of Doom's packages.

# Neovim configuration

I've recently switched (back) to mainly Neovim (I basically switch between
Emacs and Neovim once every 1-2 years). The configuration can be found
[here](./modules/neovim/nvim). I'm using the [neovim nightly
overlay](https://github.com/nix-community/neovim-nightly-overlay) to get a
bleeding-edge version of Neovim via Nix. Dependencies are managed with
`mini.deps` from [mini.nvim](https://github.com/nvim-mini/mini.nvim/).
