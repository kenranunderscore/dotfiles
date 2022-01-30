{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.polybar;
in {
  options.modules.desktop.polybar = { enable = lib.mkEnableOption "polybar"; };

  config = lib.mkIf cfg.enable {
    home.packages = let
      myPolybar = pkgs.polybar.override {
        alsaSupport = true;
        githubSupport = true;
        pulseSupport = true;
      };
    in [ pkgs.polybar ];
    xdg.configFile."polybar/config".source = ./config.ini;
  };
}
