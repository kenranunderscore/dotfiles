{ config, lib, ... }:

let
  inherit (lib) types;
  cfg = config.symlink-config;
in
{
  options.symlink-config = {
    dotfileDir = lib.mkOption {
      type = types.str;
      default = "$HOME/dotfiles";
      description = "Absolute path to your dotfiles";
    };
    files = lib.mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            source = lib.mkOption {
              type = types.path;
              description = "Source file or directory";
            };
            destination = lib.mkOption {
              type = types.str;
              description = "Absolute destination path (will be shell-expanded)";
            };
          };
        }
      );
      default = [ ];
      description = "A list of file specs to symlink directly into your $HOME";
    };
  };

  config.home.activation.symlinkCustomConfigFiles = lib.hm.dag.entryAfter [ "writeBoundary" ] (
    lib.concatMapStringsSep "\n" (
      { source, destination }:
      ''${./symlink.sh} "${cfg.dotfileDir}" "${builtins.toString source}" "${destination}"''
    ) cfg.files
  );
}
