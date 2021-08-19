{ config, lib, pkgs, ... }:

let cfg = config.modules.programs.imwheel;
in {
  options.modules.programs.imwheel = { enable = lib.mkEnableOption "imwheel"; };

  config = lib.mkIf cfg.enable {
    home = {
      packages = [ pkgs.imwheel ];
      file.".imwheelrc".source = ../../config/imwheelrc;
    };
  };
}
