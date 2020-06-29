{ config, pkgs, ... }:

with import <home-manager/modules/lib/dag.nix> { lib = pkgs.lib; };
let
  isDarwin = pkgs.stdenv.isDarwin;
  username = if isDarwin then "maier" else "kenran";
  homeDirectory = if isDarwin then "/Users/maier" else "/home/kenran";
  pwd = builtins.toPath ./.;
  osPrivatePath = if isDarwin then ./private/macos else ./private/linux;
  shellPath = "${pkgs.zsh}/bin/zsh";
in {
  # Config for nixpkgs when used by home-manager.
  nixpkgs.config = import ./config-files/nixpkgs-config.nix;

  programs = {
    home-manager.enable = true;

    git = {
      package = pkgs.gitAndTools.gitFull;
      enable = true;
      userName = "Johannes Maier";
      userEmail = if isDarwin then
        "johannes.maier@active-group.de"
      else
        "johb.maier@gmail.com";
      ignores = [ ];
      signing.signByDefault = true;
      signing.key = if isDarwin then
        "0x4DC80C3B727DC1EE"
      else
        "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
      extraConfig = {
        pull.rebase = "false";
        core.editor = "vim";
      };
    };

    kitty = {
      enable = true;
      font = {
        package = pkgs.hack-font;
        name = "Hack";
      };
      settings = {
        shell = shellPath + (if isDarwin then " --login" else "");
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

    tmux = {
      enable = true;
      escapeTime = 0;
      historyLimit = 50000;
      newSession = true;
      terminal = "xterm-24bit";
      resizeAmount = 10;
      extraConfig = ''
        set-option -g renumber-windows on
        set -sa terminal-overrides "xterm*:Tc,alacritty:Tc"
        set -g default-shell ${shellPath}
      '';
    };

    irssi = {
      enable = true;
      extraConfig = ''
        servers = (
          {
            address = "chat.freenode.net";
            chatnet = "freenode";
            port = "6697";
            use_tls = "yes";
            tls_cert = "${private/irssi.pem}";
            tls_verify = "no";
            autoconnect = "yes";
          }
        );

        chatnets = { freenode = { type = "IRC"; }; };

        channels = (
          { name = "#linux"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#haskell"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#nixos"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#home-manager"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#zsh"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#nim"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "##crawl"; chatnet = "freenode"; autojoin = "yes"; }
        );

        settings = {
          "fe-common/core" = {
            theme = "h3rbz";
          };
        };
      '';
    };

    ssh.enable = true;
    gpg.enable = true;
    zsh.enable = true;
  };

  xresources.properties = {
    "Xft.dpi" = 96;
    "Xft.antialias" = true;
    "Xft.rgba" = "rgb";
    "Xft.hinting" = true;
    "Xft.hintstyle" = "hintfull";

    # A basic default colorscheme; useful for roguelike games for instance.
    "*.foreground" = "#fcfcfc";
    "*.background" = "#313133";
    "*.cursorColor" = "#cfcfc";
    "*.color0" = "#181819";
    "*.color8" = "#77747f";
    "*.color1" = "#ef2917";
    "*.color9" = "#ef2917";
    "*.color2" = "#97cc04";
    "*.color10" = "#97cc04";
    "*.color3" = "#ffad05";
    "*.color11" = "#ffad05";
    "*.color4" = "#2d7dd2";
    "*.color12" = "#2d7dd2";
    "*.color5" = "#f230aa";
    "*.color13" = "#f230aa";
    "*.color6" = "#5fbff9";
    "*.color14" = "#5fbff9";
    "*.color7" = "#94949f";
    "*.color15" = "#fcfcfc";
  };

  xdg.configFile = {
    "doom" = {
      source = ./config-files/doom;
      recursive = true;
    };
    "nixpkgs/config.nix".source = ./config-files/nixpkgs-config.nix;
  };

  services.gpg-agent = {
    enable = !isDarwin;
    enableSshSupport = true;
  };

  home = {
    stateVersion = "20.09";

    inherit username homeDirectory;

    packages = with pkgs; [
      direnv
      emacs
      gnumake
      lorri
      nixfmt
      nix-prefetch-git
      ripgrep
      xorg.xkbcomp
    ];

    file = {
      # The private key file is linked to directly during activation.
      ".ssh/id_rsa.pub".source = osPrivatePath + "/id_rsa.pub";

      ".vimrc".source = ./config-files/vimrc;
      ".xinitrc".source = ./config-files/xinitrc;
      ".zshrc".source = ./config-files/zshrc;
      ".zshenv".source = ./config-files/zshenv;

      ".irssi/h3rbz.theme".source = ./config-files/h3rbz.theme;
    };

    # We symlink our git submodule to circumvent a nix store directory being
    # read-only. Maybe there's a way to still use fetchFromGitHub...
    activation = {
      symlinkDoomDir = dagEntryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -snf ${pwd}/config-files/doom-emacs $HOME/.emacs.d
      '';

      correctKeyPermissions = dagEntryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD cd ${pwd}/private && \
        $DRY_RUN_CMD chmod 400 *.pem **/*.key **/id_rsa*
      '';

      addXterm24bitTerminfo =
        let tic = if isDarwin then "/usr/bin/tic" else "tic";
        in dagEntryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ${tic} -x -o ~/.terminfo ${
            ./config-files/xterm-24bit.terminfo
          }
        '';

      addSshKey = let privateKeyPath = osPrivatePath + "/id_rsa";
      in dagEntryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -sf ${
          builtins.toPath privateKeyPath
        } $HOME/.ssh/id_rsa && \
        $DRY_RUN_CMD ssh-add $HOME/.ssh/id_rsa
      '';

      importGpgKey = dagEntryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD gpg --import ${osPrivatePath + "/gpg.key"}
      '';
    };
  };
}
