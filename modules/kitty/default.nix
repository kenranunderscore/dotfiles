{
  inputs,
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.kitty;
in
{
  options.modules.kitty = {
    enable = lib.mkEnableOption "kitty";

    wrapWithNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      let
        kitty =
          if cfg.wrapWithNixGL then inputs.self.lib.createNixGLWrapper pkgs pkgs.kitty else pkgs.kitty;
      in
      [ kitty ];

    xdg.configFile."kitty/kitty.conf".source = ./kitty.conf;
  };
}
