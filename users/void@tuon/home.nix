{ inputs, config, lib, pkgs, ... }:

let email = "johannes.maier@active-group.de";
in {
  imports = [ ../../modules/base.nix ../../modules ];

  modules = {
    base.gpgKey = "9AC78C1A48681583";
    brave = {
      enable = true;
      wrapWithNixGL = true;
    };
    herbstluftwm.enable = false;
    i3 = {
      enable = true;
      withGaps = false;
      terminal = "wezterm";
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
            && ~/.fehbg
          '';
          always = true;
          notification = false;
        }
      ];
    };
    kitty.wrapWithNixGL = true;
    rofi.enable = true;
    neovim.enable = true;
    sbcl.enable = true;
    polybar = {
      enable = false;
      withBattery = true;
    };
    picom.enable = false;
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    doom.enable = false;
    emacs.enable = true;
    nyxt.enable = false;
    git.email = "johannes.maier@mailbox.org";
    gcli.enable = true;
    wezterm = {
      enable = true;
      withPackage = false;
    };
  };

  home.packages = with pkgs; [
    cloc
    dmenu
    keepass
    keepassxc
    nixgl.auto.nixGLDefault
    opam
    pavucontrol
    sieve-connect
    subversion
    thunderbird
    twitch-cli
    xst
  ];

  home.stateVersion = "23.11";
}
