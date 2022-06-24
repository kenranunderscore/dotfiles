{ config, lib, pkgs, ... }:

let cfg = config.modules.programs.imwheel;
in {
  options.modules.programs.imwheel = { enable = lib.mkEnableOption "imwheel"; };

  config = lib.mkIf cfg.enable {
    home = {
      packages = [ pkgs.imwheel ];
      file.".imwheelrc".source = ./imwheelrc;
    };

    systemd.user.services.imwheel = {
      Unit.Description = "IMWheel";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        Type = "simple";
        ExecStart = "${lib.getExe pkgs.imwheel} -d";
        ExecStop = "pkill imwheel";
        Environment = "XAUTHORITY=%h/.Xauthority";
        RemainAfterExit = "yes";
      };
    };
  };
}
