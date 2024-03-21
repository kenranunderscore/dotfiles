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
    nixgl = {
      url = "github:kenranunderscore/nixgl";
      inputs.nixpkgs.follows = "nixpkgs";
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
      overlays =
        [ inputs.emacs-overlay.overlays.default inputs.nixgl.overlays.default ];
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit overlays system;
      };
      inherit (pkgs) lib;
    in {
      formatter.${system} = pkgs.nixfmt;
      lib = import ./lib { inherit (pkgs) lib; };
      nixosConfigurations = let machines = self.lib.readDirNames ./hosts;
      in builtins.foldl' (acc: hostname:
        acc // {
          ${hostname} =
            self.lib.mkNixosSystem { inherit hostname system inputs pkgs; };
        }) { } machines;
    };
}
