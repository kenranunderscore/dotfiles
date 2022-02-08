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
      i3 = {
        enable = true;
        additionalStartupCommands = [{
          command =
            "xrandr --output DP-2 --primary --mode 1920x1080 --rate 72.01 --right-of HDMI-0";
          always = false;
        }];
      };
      polybar.enable = true;
      picom.enable = true;
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
    cloc
    dbeaver
    dhall
    discord
    element-desktop
    google-chrome-beta
    keepass
    keepassx
    leiningen
    linphonePatched
    mattermost-desktop
    mercurial
    pavucontrol
    sieve-connect
    subversion
    thunderbird
  ];

  home.stateVersion = "22.05";

  xsession.enable = true;
}
