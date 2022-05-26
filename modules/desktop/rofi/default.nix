{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.rofi;
in {
  options.modules.desktop.rofi.enable = lib.mkEnableOption "rofi";

  config = lib.mkIf cfg.enable {
    programs = {
      rofi = {
        enable = true;
        cycle = true;
        font = "Courier Prime 18";
        location = "center";
        terminal = "${pkgs.kitty}/bin/kitty";
        theme = ./kenran.rasi;
        extraConfig = { modi = "run,drun,ssh,window"; };
      };
    };
  };
}
