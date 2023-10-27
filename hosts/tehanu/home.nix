{ inputs, config, lib, pkgs, ... }:

let email = "johannes.maier@active-group.de";
in {
  imports = [ ../../home-manager-modules/base.nix ../../home-manager-modules ];

  modules = {
    base.gpgKey = "9AC78C1A48681583";
    herbstluftwm.enable = false;
    i3 = {
      enable = true;
      withGaps = false;
      workspaces = [
        {
          name = "1:main";
          output = "HDMI-0";
        }
        {
          name = "2:web";
          output = "HDMI-0";
        }
        {
          name = "3";
          output = "HDMI-0";
        }
        {
          name = "4";
          output = "HDMI-0";
        }
        {
          name = "5";
          output = "HDMI-0";
        }
        {
          name = "6";
          output = "eDP-1-1";
        }
        {
          name = "7";
          output = "eDP-1-1";
        }
        {
          name = "8";
          output = "eDP-1-1";
        }
        {
          name = "9:mattermost";
          output = "eDP-1-1";
          assigns = [{ class = "Mattermost"; }];
        }
      ];
      startupCommands = [
        {
          command = "xset r rate 200 70";
          always = true;
          notification = false;
        }
        {
          command = ''
            xrandr --output HDMI-0 --off \
            && xrandr --auto \
            && xrandr --output HDMI-0 --primary --output eDP-1-1 --mode 1920x1080 --right-of HDMI-0 \
            && systemctl --user restart polybar \
            && ~/.fehbg
          '';
          always = true;
          notification = false;
        }
      ];
    };
    rofi.enable = true;
    neovim.enable = lib.mkForce false;
    sbcl.enable = true;
    polybar = {
      enable = true;
      withBattery = true;
    };
    picom.enable = false;
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    doom = {
      enable = true;
      emacsVersion = "stable";
    };
    # imwheel seems to be responsible for stuttering scrolling
    imwheel.enable = false;
    nyxt.enable = true;
    git.email = "johannes.maier@mailbox.org";
  };

  xsession.enable = true;

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
    syncthing.enable = true;
  };

  home.packages = with pkgs; [
    # citrix_workspace
    brave
    cloc
    dbeaver
    dhall
    discord
    dmenu
    element-desktop
    google-chrome-beta
    keepass
    keepassxc
    libreoffice
    linphone
    mattermost-desktop
    nushell
    obs-studio
    pavucontrol
    prismlauncher
    racket
    sieve-connect
    steam
    subversion
    thunderbird
    twitch-cli
    vlc
    wine
    wireshark
  ];

  home.stateVersion = "22.05";
}
