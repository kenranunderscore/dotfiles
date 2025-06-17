{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.bat;
in
{
  options.my.bat.enable = lib.mkEnableOption "bat";

  config = lib.mkIf cfg.enable {
    programs.bat = {
      enable = true;
      config.theme = "ansi";
    };
  };
}
