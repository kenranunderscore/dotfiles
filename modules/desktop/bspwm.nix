{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.bspwm;
in {
  options.modules.desktop.bspwm = {
    enable = mkEnableOption "bspwm";

    configDir = mkOption {
      type = types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.polybar pkgs.sxhkd ];
    xdg.configFile = {
      "bspwm/bspwmrc".source = "${cfg.configDir}/bspwmrc";
      "polybar/config".source = "${cfg.configDir}/polybar";
      "sxhkd/sxhkdrc".source = "${cfg.configDir}/sxhkdrc";
    };
  };
}
