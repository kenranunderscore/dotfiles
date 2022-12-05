{ config, lib, pkgs, ... }:

let
  cfg = config.modules.tmux;
  types = lib.types;
in {
  options.modules.tmux = { enable = lib.mkEnableOption "tmux"; };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      escapeTime = 0;
      historyLimit = 50000;
      newSession = true;
      terminal = "xterm-256color";
      resizeAmount = 10;
      extraConfig = ''
        set-option -g renumber-windows on
        set -sa terminal-overrides "xterm*:Tc,alacritty:Tc"
      '';
    };
  };
}
