{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.ghostty;
in
{
  options.my.ghostty = {
    enable = lib.mkEnableOption "ghostty";
    withPackage = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./config;
        destination = "ghostty/config";
        xdg = true;
      }
    ];

    home.packages = lib.optional cfg.withPackage pkgs.ghostty;
  };
}
