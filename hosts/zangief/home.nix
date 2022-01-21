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
    desktop.i3.enable = true;
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

  home.packages = with pkgs; [
    cloc
    dbeaver
    dhall
    discord
    google-chrome-beta
    feh
    firefox-bin
    keepass
    keepassx
    leiningen
    linphone
    mattermost-desktop
    mercurial
    racket
    sieve-connect
    subversion
    thunderbird
    xorg.xkbcomp
  ];

  home.stateVersion = "22.05";

  xsession = {
    enable = true;
    windowManager.command = "${pkgs.i3}/bin/i3";
  };

  xsession.windowManager.i3 = {
    # FIXME Make startup commands configurable, and merge
    config.startup = [
      {
        command = "setxkbmap -layout us -variant altgr-intl";
        always = true;
      }
      {
        command = "xrandr --output DP-2 --rate 72.01 --right-of HDMI-0";
        always = true;
      }
      {
        command = "~/.fehbg";
        always = true;
      }
      {
        command =
          "xinput set-prop 'SYNA1202:00 06CB:CD64 Touchpad' 'libinput Natural Scrolling Enabled' 1";
        always = true;
      }
      {
        command =
          "xinput set-prop 'SYNA1202:00 06CB:CD64 Touchpad' 'libinput Tapping Enabled' 1";
        always = true;
      }
      {
        command =
          "xinput set-prop 'Logitech USB-PS/2 Optical Mouse' 'libinput Natural Scrolling Enabled' 1";
        always = true;
      }
      {
        command =
          "xinput set-prop 'Razer  Razer Abyssus' 'libinput Natural Scrolling Enabled' 1";
        always = true;
      }
    ];
  };
}
