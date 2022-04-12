{ inputs, config, lib, pkgs, ... }:

let email = "johannes.maier@active-group.de";
in {
  imports = [ ../base.nix ../../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base = {
    privateDir = "${inputs.privateConfig}/linux";
    shellPath = "${pkgs.zsh}/bin/zsh";
    gpgKey = "9AC78C1A48681583";
  };

  modules = {
    desktop = {
      i3 = {
        enable = true;
        workspaces = [
          {
            number = 1;
            label = "main";
            output = "HDMI-0";
          }
          {
            number = 2;
            label = "web";
            output = "HDMI-0";
            assigns =
              [ { class = "Google-chrome-beta"; } { class = "firefox"; } ];
          }
          {
            number = 3;
            label = "3";
            output = "HDMI-0";
          }
          {
            number = 4;
            label = "4";
            output = "HDMI-0";
          }
          {
            number = 5;
            label = "irc/matrix";
            output = "HDMI-0";
            assigns = [{ class = "Element"; }];
          }
          {
            number = 6;
            label = "6";
            output = "DP-2";
          }
          {
            number = 7;
            label = "7";
            output = "DP-2";
          }
          {
            number = 8;
            label = "calendar";
            output = "DP-2";
            assigns = [{ class = "Thunderbird"; }];
          }
          {
            number = 9;
            label = "mattermost";
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
      emacs = {
        emacsVersion = "git";
        nativeComp = true;
      };
      imwheel.enable = true;
      nyxt.enable = true;
      kitty = {
        enable = true;
        useLoginShell = false;
      };
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
    cmus
    coq_8_14
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
    pavucontrol
    sieve-connect
    subversion
    thunderbird
  ];

  home.stateVersion = "22.05";

  xsession.enable = true;
}
