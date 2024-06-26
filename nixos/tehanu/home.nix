{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

let
  email = "johannes.maier@active-group.de";
in
{
  imports = [
    ../../modules/base.nix
    ../../modules
  ];

  modules = {
    bash.enable = true;
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
          assigns = [ { class = "Mattermost"; } ];
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
    wezterm.enable = true;
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
    dbeaver-bin
    dhall
    discord
    dmenu
    element-desktop
    google-chrome
    keepass
    keepassxc
    libreoffice
    linphone
    mattermost-desktop
    obs-studio
    pavucontrol
    pinta
    prismlauncher
    qutebrowser
    sieve-connect
    steam
    subversion
    thunderbird
    twitch-cli
    vlc
    wine
    wireshark
    xst

    # Languages that should rather live globally, or are sometimes useful to
    # have available always
    opam
  ];

  home.stateVersion = "22.05";
}
