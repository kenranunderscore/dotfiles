{ pkgs, ... }:

{
  imports = [
    ../../modules/base.nix
    ../../modules
  ];

  my = {
    base.gpgKey = "9AC78C1A48681583";
    bash.enable = true;
    brave = {
      enable = true;
      wrapWithNixGL = false;
    };
    alacritty.enable = true;
    ghostty.enable = true;
    kermit.enable = true;
    herbstluftwm.enable = false;
    i3 = {
      enable = true;
      withGaps = true;
      terminal = "konsole";
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
          command = "~/.config/polybar/polybar.sh";
          always = false;
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
    kitty = {
      enable = true;
      wrapWithNixGL = true;
    };
    rofi.enable = true;
    neovim = {
      enable = true;
      includePkg = true;
    };
    sbcl = {
      enable = true;
      withPackage = false;
    };
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
    doom.enable = true;
    emacs.enable = false;
    nyxt.enable = false;
    git.email = "johannes.maier@mailbox.org";
    mercurial = {
      enable = true;
      email = "johannes.maier@mailbox.org";
    };
    wezterm = {
      enable = true;
      withPackage = false;
    };
    zellij.enable = true;
  };

  home.packages = with pkgs; [
    ansible
    bun
    cloc
    clojure
    dbeaver-bin
    dmenu
    element-desktop
    fontforge-gtk
    gcli
    jfrog-cli
    keepass
    keepassxc
    leiningen
    nixgl.auto.nixGLDefault
    nixVersions.latest
    nixos-rebuild
    nodejs
    pavucontrol
    remmina
    sbt
    sieve-connect
    steam
    subversion
    thunderbird
    twitch-cli
    xst
  ];

  home.stateVersion = "23.11";
}
