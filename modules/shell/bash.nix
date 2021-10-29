{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.bash;
in {
  options.modules.shell.bash.enable = lib.mkEnableOption "bash";

  config = lib.mkIf cfg.enable {
    programs.bash = {
      enable = true;
      historyIgnore = [ "ls" "cd" "exit" ];
      shellAliases = import ./shell-aliases.nix;
    };
  };
}
