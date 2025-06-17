{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.brave;
in
{
  options.my.brave = {
    enable = lib.mkEnableOption "brave";

    wrapWithNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      let
        brave =
          if cfg.wrapWithNixGL then inputs.self.lib.createNixGLWrapper pkgs pkgs.brave else pkgs.brave;
      in
      [ brave ];
  };
}
