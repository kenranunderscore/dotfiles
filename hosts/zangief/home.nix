{ inputs, config, lib, pkgs, ... }:

let email = "johannes.maier@active-group.de";
in {
  imports = [ ../../modules/base.nix ../../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base.gpgKey = "9AC78C1A48681583";

  modules = {
    desktop = {
      rofi.enable = true;
      i3 = {
        enable = true;
        workspaces = [
          {
            name = "1:main/emacs";
            output = "HDMI-0";
          }
          {
            name = "2:web";
            output = "HDMI-0";
            assigns =
              [ { class = "Google-chrome-beta"; } { class = "firefox"; } ];
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
            name = "5:chat";
            output = "HDMI-0";
            assigns = [{ class = "Element"; }];
          }
          {
            name = "6";
            output = "DP-2";
          }
          {
            name = "7";
            output = "DP-2";
          }
          {
            name = "8:calendar";
            output = "DP-2";
            assigns = [{ class = "thunderbird"; }];
          }
          {
            name = "9:mattermost";
            output = "DP-2";
            assigns = [{ class = "Mattermost"; }];
          }
        ];
        startupCommands = [
          {
            command =
              "xrandr --output HDMI-0 --primary --output DP-2 --mode 1920x1080 --rate 72.01 --right-of HDMI-0; exec ~/.fehbg";
            always = false;
            notification = false;
          }
          # I could use ${pkgs.foo}/bin/foo here for instance, but I'm
          # fine with these kinds of commands failing should I decide
          # to get rid of any of the respective programs.
          {
            command = "mattermost-desktop";
            always = false;
            notification = false;
          }
          {
            command = "google-chrome-beta";
            always = false;
            notification = false;
          }
          {
            command = "thunderbird";
            always = false;
            notification = false;
          }
          {
            command = "i3-msg 'workspace 9:mattermost'";
            always = false;
            notification = false;
          }
          {
            command = "i3-msg 'workspace 1:main'";
            always = false;
            notification = false;
          }
          {
            command = ''
              i3-msg "workspace 5:chat; exec emacs -f start-irc"
            '';
            notification = false;
          }
          {
            command = "xset r rate 200 55";
            always = true;
            notification = false;
          }
        ];
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
      emacs.emacsVersion = "git";
      imwheel.enable = true;
      nyxt.enable = true;
    };
    shell.git.email = email;
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
    citrix_workspace
    cloc
    dbeaver
    dhall
    discord
    element-desktop
    google-chrome-beta
    keepass
    keepassx
    leiningen
    linphone
    mattermost-desktop
    pavucontrol
    polymc
    sieve-connect
    subversion
    thunderbird
    wireshark
  ];

  home.stateVersion = "22.05";

  xsession.enable = true;
}
