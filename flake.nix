{
  description = "My system configurations";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable"; };

  outputs = inputs@{ self, nixpkgs }: {
    nixosConfigurations = {
      atuan = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./system-configurations/atuan ];
        specialArgs = { inherit inputs; };
      };
    };
  };
}
