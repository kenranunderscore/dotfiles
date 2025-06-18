{
  # Get a list of absolute paths of all files/directories in a given directory.
  readDirNames =
    dir: builtins.map (path: dir + "/${path}") (builtins.attrNames (builtins.readDir dir));

  # Create a NixOS configuration attrset of the form { myHostname = conf },
  # which can be used as a flake output in `nixosConfigurations`.
  mkNixosSystem =
    {
      dir,
      system,
      inputs,
      pkgs,
    }:
    let
      hostname = builtins.baseNameOf dir;
      custom = (import (dir + /custom.nix)) // {
        inherit hostname;
      };
      username = custom.username;
      specialArgs = {
        inherit inputs custom;
      };
    in
    {
      "${hostname}" = inputs.nixpkgs.lib.nixosSystem {
        inherit system pkgs specialArgs;
        modules = [
          inputs.self.nixosModules.symlink-config
          (dir + /configuration.nix)
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              users.${username} = import (dir + /home.nix);
              useGlobalPkgs = true;
              useUserPackages = false;
              extraSpecialArgs = specialArgs;
            };
          }
        ];
      };
    };

  # Create a home-manager configuration attrset of the form { username = conf },
  # which can be used as a flake output in `homeConfigurations`.
  mkHomeConfiguration =
    {
      dir,
      inputs,
      pkgs,
    }:
    let
      userAtHost = builtins.baseNameOf dir;
      parts = pkgs.lib.splitString "@" userAtHost;
      username = builtins.head parts;
      hostname = builtins.elemAt parts 1;
      custom = import (dir + /custom.nix) // {
        inherit username hostname;
      };
    in
    {
      "${username}" = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          inherit inputs custom;
        };
        modules = [
          inputs.self.nixosModules.symlink-config
          (dir + /home.nix)
        ];
      };
    };

  # Create a shell script derivation based on an existing package, calling the
  # respective package with nixGL, thus making it work on non-NixOS.
  createNixGLWrapper =
    pkgs: drv:
    let
      inherit (pkgs.lib) getExe;
      name = drv.meta.mainProgram or drv.pname;
    in
    pkgs.writeShellScriptBin name ''
      exec ${pkgs.nixgl.auto.nixGLDefault}/bin/nixGL ${getExe drv} $@
    '';
}
