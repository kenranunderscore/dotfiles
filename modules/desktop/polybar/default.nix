{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.polybar;
in {
  options.modules.desktop.polybar = { enable = lib.mkEnableOption "polybar"; };

  config = lib.mkIf cfg.enable {
    services.polybar = let
      myPolybar = pkgs.polybar.override {
        alsaSupport = true;
        githubSupport = true;
        pulseSupport = true;
        i3GapsSupport = true;
      };
    in {
      enable = true;
      package = myPolybar;
      # script = "polybar -q -r top & polybar -q -r bottom &";
      script = "polybar -q -r main";
      config = ./config.ini;
    };
  };
}
