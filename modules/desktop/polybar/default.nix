{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.polybar;
in {
  options.modules.desktop.polybar = { enable = lib.mkEnableOption "polybar"; };

  config = lib.mkIf cfg.enable {
    # Using services.polybar doesn't work for me.  It's probably due
    # to using startx, so I'd have to start it manually.
    home.packages = let
      myPolybar = pkgs.polybar.override {
        alsaSupport = true;
        githubSupport = true;
        pulseSupport = true;
      };
    in [ myPolybar ];
    xdg.configFile."polybar/config".source = ./config.ini;
  };
}
