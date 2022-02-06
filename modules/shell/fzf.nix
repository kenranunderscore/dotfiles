{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.fzf;
in {
  options.modules.shell.fzf.enable = lib.mkEnableOption "fzf";

  config = lib.mkIf cfg.enable {
    programs.fzf = {
      enable = true;
      enableFishIntegration = config.modules.shell.fish.enable;
      enableBashIntegration = config.modules.shell.bash.enable;
      enableZshIntegration = config.modules.shell.zsh.enable;
    };
  };
}
