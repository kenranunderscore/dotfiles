{ inputs, custom, config, lib, pkgs, ... }:

let
  cfg = config.hosts.base;
  privateDir = "${inputs.privateConfig}";
in {
  options.hosts.base = {
    gpgKey = lib.mkOption {
      type = lib.types.str;
      default = null;
    };
  };

  imports = [ ./. ];

  config = {
    modules = {
      alacritty.enable = true;
      emacs.enable = true;
      firefox.enable = true;
      kitty.enable = true;
      neovim.enable = true;
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
    };

    xdg = {
      enable = true;
      configFile = { };
      mime.enable = true;
    };

    home = let inherit (custom) username;
    in {
      inherit username;
      homeDirectory = "/home/${username}";

      file.".crawlrc".source = "${inputs.crawlrc}/.crawlrc";

      sessionVariables = rec {
        EDITOR = "nvim";
        VISUAL = EDITOR;
        KENRAN_IRC_CERTS = "${privateDir}/irc";
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
            privateDir + "/gpg.key"
          }
        '';
      };
    };
  };
}
