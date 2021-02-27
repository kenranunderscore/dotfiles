shellPath:
{ ... }:

{
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    historyLimit = 50000;
    newSession = true;
    terminal = "xterm-24bit";
    resizeAmount = 10;
    shell = "${shellPath}";
    extraConfig = ''
      set-option -g renumber-windows on
      set -sa terminal-overrides "xterm*:Tc,alacritty:Tc"
    '';
  };
}
