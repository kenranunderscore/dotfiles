{ config, lib, ... }:

{
  options.my.bat.enable = lib.mkEnableOption "bat";

  config = lib.mkIf config.my.bat.enable {
    programs.bat = {
      enable = true;
      config.theme = "ansi";
    };
  };
}
