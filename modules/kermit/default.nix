{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.kermit.enable = lib.mkEnableOption "kermit";

  config = lib.mkIf config.my.kermit.enable {
    symlink-config.files = [
      {
        source = ./kermit.conf;
        destination = ".config/kermit.conf";
        xdg = false;
      }
    ];
    home.packages = [ pkgs.kermit-terminal ];
  };
}
