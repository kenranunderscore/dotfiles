{
  description = "My system configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    privateConfig = {
      url = "git+ssh://git@github.com/kenranunderscore/private-config";
      flake = false;
    };

    # Editors
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    # zsh plugins
    zsh-autopair = {
      url = "github:hlissner/zsh-autopair";
      flake = false;
    };
    zsh-autosuggestions = {
      url = "github:zsh-users/zsh-autosuggestions";
      flake = false;
    };
    zsh-history-substring-search = {
      url = "github:zsh-users/zsh-history-substring-search";
      flake = false;
    };
    zsh-syntax-highlighting = {
      url = "github:zsh-users/zsh-syntax-highlighting";
      flake = false;
    };

    # Fonts
    sf-mono = {
      url = "github:supercomputra/sf-mono-font";
      flake = false;
    };
    twilio-sans-mono = {
      url = "github:twilio/twilio-sans-mono";
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
      ];
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit overlays system;
      };
      inherit (pkgs) lib;
    in {
      formatter.${system} = pkgs.nixfmt;
      nixosConfigurations = let machines = lib.my.readDirNames ./hosts;
      in builtins.foldl' (acc: hostname:
        acc // {
          ${hostname} =
            lib.my.mkNixosSystem { inherit hostname system inputs pkgs; };
        }) { } machines;
    };
}
