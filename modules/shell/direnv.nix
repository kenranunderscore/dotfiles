{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv.enable = lib.mkEnableOption "direnv";

  config = lib.mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
