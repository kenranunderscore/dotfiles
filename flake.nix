{
  description = "My system configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-overlay.url = "github:nix-community/neovim-nightly-overlay";
    notmuch = {
      url =
        "github:notmuch/notmuch?rev=d25dafb4c2f26d9f7ae67ca603181238514e6e97";
      flake = false;
    };
    privateConfig.url =
      "git+ssh://git@github.com/kenranunderscore/private-config";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      overlays = [
        inputs.emacs-overlay.overlay
        inputs.neovim-overlay.overlay
        (_final: prev: {
          notmuch =
            prev.notmuch.overrideAttrs (_old: { src = inputs.notmuch; });
        })
      ];
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit overlays system;
      };
      specialArgs = { inherit inputs; };
    in {
      nixosConfigurations = {
        atuan = nixpkgs.lib.nixosSystem {
          inherit system pkgs specialArgs;
          modules = [
            ./system-configurations/atuan
            home-manager.nixosModules.home-manager
            {
              home-manager.users.kenran = import ./hosts/atuan/home.nix;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = false;
              home-manager.extraSpecialArgs = specialArgs;
            }
          ];
        };
        paln = nixpkgs.lib.nixosSystem {
          inherit system pkgs specialArgs;
          modules = [
            ./system-configurations/paln
            home-manager.nixosModules.home-manager
            {
              home-manager.users.kenran = import ./hosts/paln/home.nix;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = false;
              home-manager.extraSpecialArgs = specialArgs;
            }
          ];
        };
      };

      homeConfigurations = {
        gont = home-manager.lib.homeManagerConfiguration {
          inherit system pkgs;
          configuration = import ./hosts/gont/home.nix;
          homeDirectory = "/home/johannes";
          username = "johannes";
          stateVersion = "21.03";
          extraSpecialArgs = specialArgs;
        };
      };
    };
}
