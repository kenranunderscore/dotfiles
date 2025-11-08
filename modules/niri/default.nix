{ config, lib, ... }:

let
  cfg = config.my.niri;
in
{
  options.my.niri = {
    enable = lib.mkEnableOption "niri";
    configFile = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = cfg.configFile;
        destination = "niri/config.kdl";
        xdg = true;
      }
    ];
  };
}
