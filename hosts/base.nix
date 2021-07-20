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
        kitty = {
          enable = true;
          inherit (cfg) shellPath;
        };
        neovim.enable = true;
        toggleKeyboardLayout.enable = true;
        qutebrowser.enable = true;
        weechat.enable = true;
      };
      shell = {
        bash.enable = true;
        bat.enable = true;
        direnv.enable = true;
        fish.enable = true;
        git = {
          enable = true;
          inherit (cfg) gpgKey;
        };
        pass = {
          enable = true;
          inherit (cfg) gpgKey;
        };
        ssh.enable = true;
        tmux = {
          enable = true;
          inherit (cfg) shellPath;
        };
        zsh.enable = true;
      };
    };

    fonts.fontconfig.enable = true;

    programs = {
      gpg.enable = true;
      home-manager.enable = true;
    };

    xdg.configFile = {
      "nixpkgs/config.nix".source = ../nix/nixpkgs-config.nix;
      "starship.toml".source = ../config/starship.toml;
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
        neofetch
        nixfmt
        nix-index
        nix-info
        nix-output-monitor
        nix-prefetch-git
        openssl
        plantuml
        ranger
        ripgrep
        rlwrap
        rsync
        rxvt-unicode
        sbcl
        scrot
        tree
        unzip
        vim
        wget
        xst

        # Fonts
        anonymousPro
        camingo-code
        cantarell_fonts
        cascadia-code
        fira-code
        fontmatrix # font explorer
        go-font
        hermit
        ibm-plex
        inconsolata
        iosevka
        jetbrains-mono
        nerdfonts
        roboto-mono
        source-code-pro
        terminus_font
        ubuntu_font_family
        unifont
        uw-ttyp0
      ];

      file = {
        ".sbclrc".source = ../config/sbclrc;
        # The private key file is linked to directly during activation.
        ".ssh/id_rsa.pub".source = cfg.privateDir + "/id_rsa.pub";
        ".vimrc".source = ../config/vimrc;
        ".Xresources".source = ../config/Xresources;
      };

      activation = {
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
