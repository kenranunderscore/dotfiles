{ config, lib, pkgs, ... }:

let
  cfg = config.hosts.base;
  dag = import <home-manager/modules/lib/dag.nix> { inherit lib; };
in {
  options.hosts.base = {
    username = lib.mkOption { type = lib.types.str; };

    homeDirectory = lib.mkOption {
      type = lib.types.str;
      default = builtins.toPath "/home/${cfg.username}";
    };

    privateDir = lib.mkOption { type = lib.types.path; };

    shellPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };

    gpgKey = lib.mkOption {
      type = lib.types.str;
      default = null;
    };
  };

  imports = [ ../modules ];

  config = {
    # Config for nixpkgs when used by home-manager.
    nixpkgs = { config = import ../nix/nixpkgs-config.nix; };

    modules = {
      email.enable = true;
      programs = {
        emacs.enable = true;
        irssi.enable = true;
        kitty = {
          enable = true;
          shellPath = cfg.shellPath;
        };
        neovim.enable = true;
        qutebrowser.enable = true;
      };
      shell = {
        bash.enable = true;
        bat.enable = true;
        direnv.enable = true;
        fish.enable = true;
        git = {
          enable = true;
          gpgKey = cfg.gpgKey;
        };
        tmux = {
          enable = true;
          shellPath = cfg.shellPath;
        };
        zsh.enable = true;
      };
    };

    fonts.fontconfig.enable = true;

    # TODO move SSH into dedicated module
    programs = {
      gpg.enable = true;
      home-manager.enable = true;
      password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (e: [ e.pass-import ]);
        settings = {
          PASSWORD_STORE_DIR = "~/.password-store";
          PASSWORD_STORE_CLIP_TIME = "30";
          PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
          PASSWORD_STORE_KEY = cfg.gpgKey;
        };
      };
      ssh = {
        enable = true;
        matchBlocks = {
          "sync" = {
            host = "sync";
            hostname = "157.90.159.76";
            user = "kenran";
            compression = true;
          };
        };
      };
    };

    xdg.configFile = {
      "nixpkgs/config.nix".source = ../nix/nixpkgs-config.nix;
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
      "*.cursorColor" = "#fcfcfc";
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

    home = {
      username = cfg.username;
      homeDirectory = cfg.homeDirectory;

      stateVersion = "21.03";

      packages = with pkgs; [
        cacert
        curl
        darcs
        fd
        gcc
        gnumake
        htop
        lorri
        manpages
        mercurial
        nixfmt
        nix-index
        nix-output-monitor
        nix-prefetch-git
        plantuml
        racket
        ripgrep
        rlwrap
        rsync
        sbcl
        tree
        unzip
        vim
        wget

        # Fonts
        anonymousPro
        camingo-code
        cantarell_fonts
        cascadia-code
        fira-code
        hermit
        ibm-plex
        inconsolata
        jetbrains-mono
        ubuntu_font_family
        source-code-pro
        terminus_font
        terminus_font_ttf
      ];

      file = {
        ".sbclrc".source = ../config/sbclrc;
        # The private key file is linked to directly during activation.
        ".ssh/id_rsa.pub".source = cfg.privateDir + "/id_rsa.pub";
        ".vimrc".source = ../config/vimrc;
      };

      activation = {
        addXterm24bitTerminfo =
          let tic = if pkgs.stdenv.isDarwin then "/usr/bin/tic" else "tic";
          in dag.dagEntryAfter [ "writeBoundary" ] ''
            $DRY_RUN_CMD ${tic} -x -o ~/.terminfo ${
              ../config/xterm-24bit.terminfo
            }
          '';

        handlePrivateKeys = let privateKeyPath = cfg.privateDir + "/id_rsa";
        in dag.dagEntryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ln -sf ${
            builtins.toPath privateKeyPath
          } $HOME/.ssh/id_rsa && \
          $DRY_RUN_CMD cd ${builtins.toPath ../.}/private && \
          $DRY_RUN_CMD chmod 400 *.pem **/*.key **/id_rsa* && \
          $DRY_RUN_CMD ssh-add $HOME/.ssh/id_rsa && \
          $DRY_RUN_CMD eval "$(ssh-agent)" && \
          $DRY_RUN_CMD gpg --import ${cfg.privateDir + "/gpg.key"}
        '';
      };
    };
  };
}
