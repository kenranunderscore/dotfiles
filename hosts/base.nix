{ inputs, customConfig, config, lib, pkgs, ... }:

let cfg = config.hosts.base;
in {
  options.hosts.base = {
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
        firefox.enable = true;
        kitty = {
          enable = true;
          inherit (cfg) shellPath;
        };
        neovim.enable = true;
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

    xdg = {
      enable = true;
      configFile = { };
      mime.enable = true;
    };

    home = let inherit (customConfig) username;
    in {
      inherit username;
      homeDirectory = "/home/${username}";

      file.".crawlrc".source = "${inputs.crawlrc}/.crawlrc";

      sessionVariables = rec {
        EDITOR = "emacsclient -a '' -c";
        VISUAL = EDITOR;
      };

      packages = with pkgs; [
        binutils
        cacert
        curl
        fd
        feh
        file
        gnumake
        graphviz
        gh
        htop
        jq
        man-pages
        neofetch
        nixfmt
        openssl
        perl
        ripgrep
        rlwrap
        rsync
        rxvt-unicode
        scrot
        shellcheck
        shfmt
        tree
        unzip
        watchexec
        wget
      ];

      activation = {
        importGpgKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ${lib.getExe pkgs.gnupg} --import ${
            cfg.privateDir + "/gpg.key"
          }
        '';
      };
    };
  };
}
