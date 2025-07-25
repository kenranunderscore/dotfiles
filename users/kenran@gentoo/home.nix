{ pkgs, ... }:

{
  imports = [
    ../../modules/base.nix
    ../../modules
  ];

  my = {
    base.gpgKey = "9AC78C1A48681583";
    brave = {
      enable = true;
      wrapWithNixGL = false;
    };
    ghostty.enable = false;
    herbstluftwm.enable = false;
    kitty.enable = false;
    rofi.enable = false;
    neovim.enable = false;
    sbcl = {
      enable = false;
      withPackage = false;
    };
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
    doom = {
      enable = false;
      includePkg = false;
    };
    emacs = {
      enable = false;
      includePkg = false;
    };
    nyxt.enable = false;
    git.email = "johannes.maier@mailbox.org";
    mercurial = {
      enable = true;
      email = "johannes.maier@mailbox.org";
    };
    wezterm = {
      enable = true;
      withPackage = true;
    };
    i3 = {
      enable = true;
      withGaps = false;
      terminal = "nixGL wezterm";
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
  };

  home.packages = with pkgs; [
    ansible
    bun
    cloc
    dmenu
    gcli
    keepass
    keepassxc
    nixgl.auto.nixGLDefault
    nodejs_latest
    pavucontrol
    subversion
    thunderbird
  ];

  home.stateVersion = "23.11";
}
