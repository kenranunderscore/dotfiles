{ config, lib, pkgs, ... }:

let cfg = config.hosts.base;
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
    modules = {
      email.enable = true;
      programs = {
        emacs.enable = true;
        kitty = {
          enable = true;
          inherit (cfg) shellPath;
        };
        neovim.enable = true;
        qutebrowser.enable = true;
        weechat.enable = true;
      };
      shell = {
        bash.enable = true;
        bat.enable = true;
        direnv.enable = true;
        fish.enable = true;
        fzf.enable = true;
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

    fonts.fontconfig.enable = lib.mkForce true;

    programs = {
      gpg.enable = true;
      home-manager.enable = true;
      # TODO(Johannes): Customize colors?
      jq.enable = true;
    };

    xdg.configFile = { };

    home = {
      username = cfg.username;
      homeDirectory = cfg.homeDirectory;

      sessionVariables = rec {
        EDITOR = "emacsclient -a '' -c";
        VISUAL = EDITOR;
      };

      packages = with pkgs; [
        broot
        cacert
        curl
        darcs
        feh
        fd
        firefox-bin
        gnumake
        graphviz
        htop
        manpages
        neofetch
        niv
        nixfmt
        nix-prefetch-git
        openssl
        ripgrep
        rlwrap
        rsync
        rxvt-unicode
        scrot
        sqlite
        tree
        unzip
        vim
        wget
        xcape
        xst
      ];

      file = {
        ".sbclrc".source = ../config/sbclrc;
        ".vimrc".source = ../config/vimrc;
        ".Xresources".source = ../config/Xresources;
      };

      activation = {
        importGpgKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --import ${
            cfg.privateDir + "/gpg.key"
          }
        '';
      };
    };
  };
}
