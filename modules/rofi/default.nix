{
  custom,
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
    programs = {
      rofi = {
        enable = true;
        cycle = true;
        font = "${custom.font.name} ${toString custom.font.size}";
        location = "center";
        terminal = "${lib.getExe pkgs.kitty}";
        theme = ./naga.rasi;
        extraConfig = {
          modi = "run,drun,ssh,window";
        };
      };
    };
  };
}
