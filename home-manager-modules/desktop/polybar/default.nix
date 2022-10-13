{ custom, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.polybar;
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
          MONITOR=$m polybar -r top &
          MONITOR=$m polybar -r bottom &
        done
      '';
      config = import ./config-i3.nix {
        inherit (cfg) withBattery;
        inherit (custom) font;
        inherit pkgs;
      };
    };
  };
}
