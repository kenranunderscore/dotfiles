{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv = { enable = mkEnableOption "direnv"; };

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
