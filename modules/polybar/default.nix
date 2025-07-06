{
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.polybar;
in
{
  options.my.polybar = {
    enable = lib.mkEnableOption "polybar";

    withBattery = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./polybar.sh;
        destination = "polybar/polybar.sh";
        xdg = true;
      }
    ];

    services.polybar =
      let
        myPolybar = pkgs.polybar.override {
          i3Support = true;
          alsaSupport = true;
          githubSupport = true;
          pulseSupport = true;
        };
      in
      {
        enable = true;
        package = myPolybar;
        script = ''
          for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
            MONITOR=$m polybar -r main &
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
