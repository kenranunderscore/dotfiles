{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = {
    enable = mkEnableOption "tmux";

    shellPath = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      escapeTime = 0;
      historyLimit = 50000;
      newSession = true;
      terminal = "xterm-24bit";
      resizeAmount = 10;
      shell = cfg.shellPath;
      extraConfig = ''
        set-option -g renumber-windows on
        set -sa terminal-overrides "xterm*:Tc,alacritty:Tc"
      '';
    };
  };
}
