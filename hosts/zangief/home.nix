{ inputs, config, lib, pkgs, ... }:

{
  imports = [ ../base.nix ../../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base = {
    username = "johannes";
    privateDir = "${inputs.privateConfig}/linux";
    shellPath = "${pkgs.fish}/bin/fish";
    gpgKey = "9AC78C1A48681583";
  };

  modules = {
    desktop = {
      i3.enable = false;
      sway.enable = true;
    };
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    programs = {
      emacs = {
        emacsVersion = "git";
        nativeComp = true;
      };
      imwheel.enable = true;
      kitty = {
        enable = true;
        useLoginShell = false;
      };
    };
    shell.git.email = "johannes.maier@active-group.de";
  };

  # Configured here because it's not used elsewhere yet
  # (wayland-native).
  programs.foot = {
    enable = true;
    server.enable = false;
    settings = rec {
      main = {
        # Default is 'foot'; setting this seems like the safe choice for
        # lots of SSH'ing.
        term = "xterm-256color";
        shell = "${pkgs.fish}/bin/fish";
        font = "Meslo LG M:size=13";
        dpi-aware = "no";
      };
      scrollback = { lines = 25000; };
      colors = {
        alpha = "1.0";
        foreground = "0ac30a";
        background = "040404";
        regular0 = "707370";
        bright0 = "545454";
        regular1 = "dc7612";
        bright1 = "ff0000";
        regular2 = "eec900";
        bright2 = "00ff00";
        regular3 = "b3ee3a";
        bright3 = "ffff00";
        regular4 = "00cdcd";
        bright4 = "0000ff";
        regular5 = "f474b4";
        bright5 = "ff00ff";
        regular6 = "00cdcd";
        bright6 = "00ffff";
        regular7 = "f4f4f4";
        bright7 = "e5e5e5";
      };
      cursor = {
        blink = "no";
        style = "block";
        color = "${colors.background} ${colors.foreground}";
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
    syncthing.enable = true;
  };

  home.packages = let
    liblinphoneWithOlderSoci = pkgs.liblinphone.override {
      soci = pkgs.soci.overrideAttrs (old: rec {
        pname = "soci";
        version = "4.0.1";
        src = pkgs.fetchFromGitHub {
          owner = "SOCI";
          repo = pname;
          rev = version;
          sha256 = "sha256-d4GtxDaB+yGfyCnbvnLRUYcrPSMkUF7Opu6+SZd8opM=";
        };
      });
    };
    linphonePatched =
      pkgs.linphone.override { liblinphone = liblinphoneWithOlderSoci; };
  in with pkgs; [
    broot
    cloc
    dbeaver
    dhall
    discord
    element-desktop
    google-chrome-beta
    feh
    firefox-bin
    keepass
    keepassx
    leiningen
    linphonePatched
    mattermost-desktop
    mercurial
    racket
    sieve-connect
    subversion
    thunderbird
  ];

  home.stateVersion = "22.05";
}
