{ config, lib, pkgs, ... }:

let cfg = config.modules.spectrwm;
in {
  options.modules.spectrwm = {
    enable = lib.mkEnableOption "spectrwm";

    terminal = lib.mkOption {
      type = lib.types.str;
      default = "${lib.getExe pkgs.kitty}";
    };
  };

  config =
    lib.mkIf cfg.enable { home.packages = [ pkgs.spectrwm pkgs.xlockmore ]; };
}
