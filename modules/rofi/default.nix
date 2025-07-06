{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.rofi;
in
{
  options.my.rofi.enable = lib.mkEnableOption "rofi";

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./config.rasi;
        destination = "rofi/config.rasi";
        xdg = true;
      }
      {
        source = ./themes;
        destination = ".local/share/rofi/themes";
      }
    ];

    home.packages = [ pkgs.rofi ];
  };
}
