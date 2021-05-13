{ config, lib, pkgs, ... }:

let
  nixGLSource = let rev = "7d6bc1b21316bab6cf4a6520c2639a11c25a220e";
  in builtins.fetchTarball {
    url = "https://github.com/guibou/nixGL/archive/${rev}.tar.gz";
    sha256 = "02y38zmdplk7a9ihsxvnrzhhv7324mmf5g8hmxqizaid5k5ydpr3";
  };
  nixGL = (pkgs.callPackage "${nixGLSource}/nixGL.nix" { }).nixGLNvidia;
in rec {
  imports = [ ../base.nix ../../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base = {
    username = "johannes";
    privateDir = ../../private/gont;
    shellPath = "${pkgs.fish}/bin/fish";
    gpgKey = "DDB27C847E768551";
  };

  modules = {
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
      isSyncServer = false;
    };
    programs = {
      bspwm = {
        enable = true;
        configDir = ./bspwm;
      };
      emacs = {
        emacsVersion = "git";
        nativeComp = true;
      };
      kitty = {
        enable = true;
        useLoginShell = false;
        fontSize = "10.0";
      };
      qutebrowser = {
        package = pkgs.writeShellScriptBin "qb" ''
          #!/usr/bin/env sh
          ${nixGL}/bin/nixGLNvidia ${pkgs.qutebrowser}/bin/qutebrowser "$@"
        '';
      };
    };
    shell = { git = { email = "johannes.maier@active-group.de"; }; };
  };

  programs = {
    rofi.enable = true;
    msmtp.enable = true;
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
    syncthing.enable = true;
  };

  home.packages = with pkgs; [
    dhall
    feh
    keepass
    keepassx
    kittyWrapped
    leiningen
    linphone
    mercurial
    subversion
    xorg.xkbcomp
  ];

  xsession.windowManager = {
    i3 = {
      enable = true;
      config = rec {
        modifier = "Mod4";
        startup = [
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
        terminal = "${pkgs.xst}/bin/xst -e ${pkgs.fish}/bin/fish";
        window.titlebar = false;
        keybindings =
          let mod = config.xsession.windowManager.i3.config.modifier;
          in lib.mkOptionDefault {
            # Use normal vim keys for moving between windows.
            "${mod}+h" = "focus left";
            "${mod}+l" = "focus right";
            "${mod}+j" = "focus down";
            "${mod}+k" = "focus up";
            "${mod}+Shift+h" = "move left";
            "${mod}+Shift+l" = "move right";
            "${mod}+Shift+j" = "move down";
            "${mod}+Shift+k" = "move up";
            "${mod}+v" = "split v";
            "${mod}+s" = "split h";
            "${mod}+t" = "exec ${terminal}";
            "${mod}+space" = lib.mkForce ''
              exec "${pkgs.rofi}/bin/rofi --no-startup-id -show drun -modi drun,run -show-icons"'';
            "${mod}+z" = "mode $mode_gaps";
          };
        bars = [{
          position = "top";
          mode = "dock";
          fonts = [ "Hack 10" ];
          statusCommand = "${pkgs.i3status}/bin/i3status";
          trayOutput = null;
          workspaceButtons = true;
          workspaceNumbers = true;
          hiddenState = "hide";
        }];
      };
    };
  };

  # wrap kitty with nixGL, since it does not expose a 'package' option
  nixpkgs.overlays = [
    (_: super: {
      kittyWrapped = pkgs.writeShellScriptBin "ky" ''
        #!/usr/bin/env sh
        ${nixGL}/bin/nixGLNvidia ${super.kitty}/bin/kitty "$@"
      '';
    })
  ];
}
