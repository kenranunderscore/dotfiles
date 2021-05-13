{ config, lib, pkgs, ... }:

{
  imports = [ ../base.nix ../../modules ];
  hosts.base = {
    username = "kenran";
    privateDir = ../../private/linux;
    shellPath = "${pkgs.fish}/bin/fish";
    gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
  };

  modules = {
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
      isSyncServer = false;
    };
    programs = {
      emacs.emacsVersion = "git";
      emacs.nativeComp = true;
      kitty = {
        useLoginShell = false;
        fontSize = "10.0";
      };
    };
    shell = { git.email = "johb.maier@gmail.com"; };
  };

  programs = {
    rofi.enable = true;
    msmtp.enable = true;
    i3status = {
      enable = true;
      enableDefault = true;
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  xsession.windowManager = {
    i3 = {
      enable = true;
      config = rec {
        modifier = "Mod4";
        startup = [ ];
        terminal = "${pkgs.xst}/bin/xst -e ${pkgs.fish}/bin/fish";
        window = { titlebar = false; };
        keybindings = lib.mkOptionDefault {
          # Use normal vim keys for moving between windows.
          "${modifier}+h" = "focus left";
          "${modifier}+l" = "focus right";
          "${modifier}+j" = "focus down";
          "${modifier}+k" = "focus up";
          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+l" = "move right";
          "${modifier}+Shift+j" = "move down";
          "${modifier}+Shift+k" = "move up";
          "${modifier}+v" = "split v";
          "${modifier}+s" = "split h";
          "${modifier}+t" = "exec ${terminal}";
          "${modifier}+space" = lib.mkForce ''
            exec "rofi --no-startup-id -show drun -modi drun,run -show-icons"'';
        };
        bars = [{
          position = "top";
          mode = "dock";
          fonts = [ "Hack 10" ];
          statusCommand = "i3status";
          trayOutput = null;
          workspaceButtons = true;
          workspaceNumbers = true;
          hiddenState = "hide";
        }];
      };
    };
  };

  home.packages = with pkgs; [ dmenu pandoc xorg.xkbcomp ];
}
