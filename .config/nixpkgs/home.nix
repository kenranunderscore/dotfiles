{ config, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
  username = if isDarwin then "maier" else "kenran";
  homeDirectory = if isDarwin then "/Users/maier" else "/home/kenran";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = { inherit username homeDirectory; };

  nixpkgs.config.firefox.enableFlash = false;

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Johannes Maier";
    userEmail =
      if isDarwin
      then "johannes.maier@active-group.de"
      else "johb.maier@gmail.com";
    ignores = [];
    signing.signByDefault = true;
    signing.key = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
    extraConfig = {
      pull.rebase = "false";
      core.editor = "vim";
    };
  };

  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.hack-font;
      name = "Hack";
    };
    settings = {
      font_size = "12.0";
      adjust_line_height = 1;
      scrollback_lines = 50000;
      hide_window_decorations = true;
      remember_window_size = false;
      initial_window_width = 800;
      initial_window_height = 520;
      enable_audio_bell = false;
      # Base16 Atelier Estuary - kitty color config
      # Scheme by Bram de Haan (http://atelierbramdehaan.nl)
      background = "#22221b";
      foreground = "#929181";
      selection_background = "#929181";
      selection_foreground = "#22221b";
      url_color = "#878573";
      cursor = "#929181";
      active_border_color = "#6c6b5a";
      inactive_border_color = "#302f27";
      active_tab_background = "#22221b";
      active_tab_foreground = "#929181";
      inactive_tab_background = "#302f27";
      inactive_tab_foreground = "#878573";
      tab_bar_background = "#302f27";
      color0 = "#22221b";
      color1 = "#ba6236";
      color2 = "#7d9726";
      color3 = "#a5980d";
      color4 = "#36a166";
      color5 = "#5f9182";
      color6 = "#5b9d48";
      color7 = "#929181";
      color8 = "#6c6b5a";
      color9 = "#ae7313";
      color10 = "#302f27";
      color11 = "#5f5e4e";
      color12 = "#878573";
      color13 = "#e7e6df";
      color14 = "#9d6c7c";
      color15 = "#f4f3ec";
    };
  };

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    historyLimit = 50000;
    newSession = true;
    terminal = "xterm-24bit";
    resizeAmount = 10;
    extraConfig = ''
set-option -g renumber-windows on
set -sa terminal-overrides "xterm*:Tc,alacritty:Tc"
    '';
  };

  programs.irssi = {
    enable = true;
    networks = {
      freenode = {
        server = {
          address = "chat.freenode.net";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = false;
          };
        };
        nick = "kenran";
        channels = {
          nixos.autoJoin = true;
          haskell.autoJoin = true;
          zsh.autoJoin = true;
          nim.autoJoin = true;
          "##crawl".autoJoin = true;
        };
      };
    };
  };

  home.packages = with pkgs; [
    direnv
    emacs
    lorri
    ripgrep
  ];

  home.stateVersion = "20.09";
}
