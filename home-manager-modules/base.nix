{ inputs, custom, config, lib, pkgs, ... }:

let
  cfg = config.modules.base;
  privateDir = "${inputs.privateConfig}";
in {
  options.modules.base = {
    gpgKey = lib.mkOption {
      type = lib.types.str;
      default = null;
    };
  };

  imports = [ ./. ];

  config = {
    nix.registry = {
      this.flake = inputs.nixpkgs;
      n.to = {
        id = "nixpkgs";
        type = "indirect";
      };
    };

    modules = {
      alacritty.enable = true;
      firefox.enable = true;
      kitty.enable = true;
      bash.enable = true;
      bat.enable = true;
      direnv.enable = true;
      email.enable = true;
      fish.enable = true;
      fzf.enable = true;
      pass = {
        enable = true;
        inherit (cfg) gpgKey;
      };
      ssh.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };

    fonts.fontconfig.enable = lib.mkForce true;

    programs = {
      gpg.enable = true;
      home-manager.enable = true;
      # TODO(Johannes): Customize colors?
      jq.enable = true;
      nix-index = {
        enable = true;
        enableFishIntegration = true;
        enableZshIntegration = true;
        enableBashIntegration = true;
      };
    };

    xdg = {
      enable = true;
      mime.enable = true;
    };

    home = let inherit (custom) username;
    in {
      inherit username;
      homeDirectory = "/home/${username}";

      sessionVariables = rec {
        EDITOR = "emacsclient -c";
        VISUAL = EDITOR;
        ALTERNATE_EDITOR = "";
        KENRAN_IRC_CERTS = "${privateDir}/irc";
        OPENSSL_DIR = "${pkgs.openssl.dev}";
      };

      packages = with pkgs; [
        autoconf
        automake
        binutils
        cacert
        cmake
        curl
        fd
        feh
        fennel
        file
        fontconfig.dev
        gh
        gnumake
        graphviz
        htop
        jq
        man-pages
        neofetch
        nixfmt
        nixpkgs-review
        openssl.dev
        pciutils
        perl
        pkg-config
        ripgrep
        rlwrap
        rsync
        rxvt-unicode
        scrot
        shellcheck
        shfmt
        tree
        unzip
        usbutils
        watchexec
        wget
        xcolor
        xorg.xkill
      ];

      activation = {
        importGpgKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ${lib.getExe pkgs.gnupg} --import ${
            privateDir + "/gpg.key"
          }
        '';
      };
    };
  };
}
