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
        git.enable = true;
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
    };

    xdg.configFile = { "starship.toml".source = ../config/starship.toml; };

    home = {
      username = cfg.username;
      homeDirectory = cfg.homeDirectory;

      stateVersion = "21.03";

      packages = with pkgs; [
        cacert
        curl
        darcs
        fd
        fzf
        gcc
        gnumake
        graphviz
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
        sqlite
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
        hack-font
        hermit
        ibm-plex
        inconsolata
        iosevka
        jetbrains-mono
        meslo-lg
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
        ".vimrc".source = ../config/vimrc;
        ".Xresources".source = ../config/Xresources;
      };

      activation = {
        importGpgKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD gpg --import ${cfg.privateDir + "/gpg.key"}
        '';
      };
    };
  };
}
