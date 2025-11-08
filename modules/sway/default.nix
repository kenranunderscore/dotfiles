{ config, lib, ... }:

let
  cfg = config.my.sway;
in
{
  options.my.sway = {
    enable = lib.mkEnableOption "sway";
    configFile = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = cfg.configFile;
        destination = "sway/config";
        xdg = true;
      }
    ];
  };
}
