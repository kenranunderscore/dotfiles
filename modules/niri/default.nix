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

  config = {
    symlink-config.files = [
      {
        source = cfg.configFile;
        destination = "niri/config.kdl";
        xdg = true;
      }
    ];
  };
}
