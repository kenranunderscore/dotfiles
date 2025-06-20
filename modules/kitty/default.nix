{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.kitty;
in
{
  options.my.kitty = {
    enable = lib.mkEnableOption "kitty";

    wrapWithNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./kitty.conf;
        destination = "kitty/kitty.conf";
        xdg = true;
      }
    ];

    home.packages =
      let
        kitty =
          if cfg.wrapWithNixGL then inputs.self.lib.createNixGLWrapper pkgs pkgs.kitty else pkgs.kitty;
      in
      [ kitty ];
  };
}
