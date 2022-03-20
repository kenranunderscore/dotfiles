{
  description = "My system configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    privateConfig.url =
      "git+ssh://git@github.com/kenranunderscore/private-config";

    # Editors
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-overlay.url = "github:nix-community/neovim-nightly-overlay";

    # zsh plugins
    zsh-abbr = {
      url = "github:olets/zsh-abbr";
      flake = false;
    };
    zsh-autopair = {
      url = "github:hlissner/zsh-autopair";
      flake = false;
    };
    zsh-autosuggestions = {
      url = "github:zsh-users/zsh-autosuggestions";
      flake = false;
    };
    zsh-syntax-highlighting = {
      url = "github:zsh-users/zsh-syntax-highlighting";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      overlays = [
        (final: prev: {
          # Shorter than prev.lib.extend (f: p: ...), but I don't know
          # if there's another difference.
          lib = prev.lib // { my = import ./lib { inherit (final) lib; }; };
        })
        inputs.emacs-overlay.overlay
        inputs.neovim-overlay.overlay
      ];
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit overlays system;
      };
      inherit (pkgs) lib;
    in {
      nixosConfigurations = let machines = lib.my.readDirNames ./hosts;
      in builtins.foldl' (acc: hostname:
        acc // {
          ${hostname} =
            lib.my.mkNixosSystem { inherit hostname system inputs pkgs; };
        }) { } machines;
    };
}
