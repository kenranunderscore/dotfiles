{
  inputs,
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.base;
  privateDir = "${inputs.privateConfig}";
in
{
  options.my.base = {
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
      d.to = {
        owner = "kenranunderscore";
        repo = "dotfiles";
        type = "github";
      };
    };

    my = {
      firefox.enable = true;
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
    };

    xdg = {
      enable = true;
      mime.enable = true;
    };

    home =
      let
        inherit (custom) username;
      in
      {
        inherit username;
        homeDirectory = "/home/${username}";

        sessionVariables = rec {
          EDITOR = "nvim";
          VISUAL = EDITOR;
          ALTERNATE_EDITOR = "";
          KENRAN_IRC_CERTS = "${privateDir}/irc";
          OPENSSL_DIR = "${pkgs.openssl.dev}";
        };

        packages = with pkgs; [
          binutils
          cacert
          cmake
          curl
          emmet-language-server
          fd
          feh
          fennel
          file
          fontconfig.dev
          gh
          gnumake
          htop
          jq
          just
          kdePackages.konsole
          man-pages
          ncdu
          nil
          nixfmt-rfc-style
          nixpkgs-review
          openssl.dev
          parallel
          pciutils
          perl
          ripgrep
          rlwrap
          rsync
          scrot
          shellcheck
          shfmt
          time
          timer
          tree
          nettools
          unzip
          usbutils
          watchexec
          wget
          xcolor
          xorg.xkill
        ];

        activation = {
          importGpgKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            $DRY_RUN_CMD ${lib.getExe pkgs.gnupg} --import ${privateDir + "/gpg.key"}
          '';
        };
      };
  };
}
