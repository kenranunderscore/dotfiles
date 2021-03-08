{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.bspwm;
in {
  options.modules.programs.bspwm = {
    enable = mkEnableOption "bspwm";

    configDir = mkOption {
      type = types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile = {
      "bspwm/bspwmrc".source = "${cfg.configDir}/bspwmrc";
      "polybar/config".source = "${cfg.configDir}/polybar";
      "sxhkd/sxhkdrc".source = "${cfg.configDir}/sxhkdrc";
    };
  };
}
