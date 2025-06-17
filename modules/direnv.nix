{ config, lib, ... }:

{
  options.my.direnv.enable = lib.mkEnableOption "direnv";

  config = lib.mkIf config.my.direnv.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      # Automatically enabled, thus readonly:
      # enableFishIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
