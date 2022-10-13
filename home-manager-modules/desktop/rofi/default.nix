{ custom, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.rofi;
in {
  options.modules.desktop.rofi.enable = lib.mkEnableOption "rofi";

  config = lib.mkIf cfg.enable {
    programs = {
      rofi = {
        enable = true;
        cycle = true;
        font = "${custom.font.name} ${toString custom.font.size}";
        location = "center";
        terminal = "${lib.getExe pkgs.kitty}";
        theme = ./kenran.rasi;
        extraConfig = { modi = "run,drun,ssh,window"; };
      };
    };
  };
}
