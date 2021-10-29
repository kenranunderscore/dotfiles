{ config, lib, pkgs, ... }:

let
  cfg = config.modules.shell.tmux;
  types = lib.types;
in {
  options.modules.shell.tmux = {
    enable = lib.mkEnableOption "tmux";

    shellPath = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      escapeTime = 0;
      historyLimit = 50000;
      newSession = true;
      terminal = "xterm-256color";
      resizeAmount = 10;
      shell = cfg.shellPath;
      extraConfig = ''
        set-option -g renumber-windows on
        set -sa terminal-overrides "xterm*:Tc,alacritty:Tc"
      '';
    };
  };
}
