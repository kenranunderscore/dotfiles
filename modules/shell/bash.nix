{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.bash;
in {
  options.modules.shell.bash = { enable = mkEnableOption "bash"; };

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      historyIgnore = [ "ls" "cd" "exit" ];
      shellAliases = import ./shell-aliases.nix;
    };
  };
}
