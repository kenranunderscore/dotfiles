{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.bat;
in
{
  options.modules.bat.enable = lib.mkEnableOption "bat";

  config = lib.mkIf cfg.enable {
    programs.bat = {
      enable = true;
      config.theme = "ansi";
    };
  };
}
