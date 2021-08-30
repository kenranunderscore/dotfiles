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
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }: {
    nixosConfigurations = let specialArgs = { inherit inputs; };
    in {
      atuan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./system-configurations/atuan
          home-manager.nixosModules.home-manager
          {
            home-manager.users.kenran = import ./hosts/atuan/home.nix;
            home-manager.useGlobalPkgs = false;
            home-manager.useUserPackages = false;
            home-manager.extraSpecialArgs = specialArgs;
          }
          { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
        ];
        inherit specialArgs;
      };
    };
  };
}
