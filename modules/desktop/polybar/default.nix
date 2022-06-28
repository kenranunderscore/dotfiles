{ config, lib, pkgs, ... }:

let
  cfg = config.modules.desktop.polybar;
  i3 = config.modules.desktop.i3.enable;
in {
  options.modules.desktop.polybar = {
    enable = lib.mkEnableOption "polybar";

    withBattery = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

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
      script = ''
        for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
          ${if i3 then "true" else "false"} && MONITOR=$m polybar -r top &
          MONITOR=$m polybar -r bottom &
        done
      '';
      config = let
        configFile = if i3 then ./config-i3.nix else ./config-herbstluftwm.nix;
      in import configFile cfg.withBattery pkgs;
    };
  };
}
