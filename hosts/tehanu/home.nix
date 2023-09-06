{ inputs, config, lib, pkgs, ... }:

let email = "johannes.maier@active-group.de";
in {
  imports = [ ../../home-manager-modules/base.nix ../../home-manager-modules ];

  targets.genericLinux = { enable = true; };

  modules = {
    base.gpgKey = "9AC78C1A48681583";
    herbstluftwm.enable = false;
    i3 = {
      enable = true;
      withGaps = true;
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
          command = "~/.fehbg";
          always = false;
          notification = false;
        }
        {
          command = "xset r rate 200 65";
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
    emacs.emacsVersion = "git";
    # imwheel seems to be responsible for stuttering scrolling
    imwheel.enable = false;
    nyxt.enable = true;
    git.email = email;
  };

  programs = {
    mercurial = {
      enable = true;
      userEmail = email;
      userName = "Johannes Maier";
      # FIXME: always enable git, and use its config here, or extract
      ignores = [
        # Vim
        "*.swp"
        # Direnv
        ".direnv/"
        ".envrc"
        # macOS
        ".DS_Store"
        # Emacs: backup, auto-save, lock files, directory-local
        # variables
        "*~"
        "\\#*\\#"
        ".\\#*"
        ".dir-locals.el"
      ];
      aliases = { p = "pull -u"; };
    };
  };

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
    leiningen
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
    wireshark
    zoom-us
  ];

  home.stateVersion = "22.05";

  xsession.enable = true;
}
