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
      emacs.emacsVersion = "gcc";
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
      package = pkgs.i3-gaps;
      config = rec {
        modifier = "Mod4";
        startup = [ ];
        terminal = "${pkgs.kitty}/bin/kitty";
        keybindings =
          let mod = config.xsession.windowManager.i3.config.modifier;
          in lib.mkOptionDefault {
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
        gaps = let testGap = 15;
        in {
          smartGaps = false;
          top = testGap;
          right = testGap;
          bottom = testGap;
          left = testGap;
          inner = testGap;
          outer = testGap;
          horizontal = testGap;
          vertical = testGap;
        };
      };
    };
  };

  home.packages = with pkgs; [ dmenu pandoc xorg.xkbcomp ];
}
