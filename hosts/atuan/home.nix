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
      package = pkgs.i3-gaps;
      config = rec {
        modifier = "Mod4";
        startup = [ ];
        terminal = "kitty";
        keybindings =
          let mod = config.xsession.windowManager.i3.config.modifier;
          in lib.mkOptionDefault {
            # I only really use 5 workspaces. This frees up the
            # hotkeys for future use, maybe to control the
            # gaps/layout/toggles.
            "${mod}+6" = null;
            "${mod}+7" = null;
            "${mod}+8" = null;
            "${mod}+9" = null;
            "${mod}+0" = null;
            "${mod}+Shift+6" = null;
            "${mod}+Shift+7" = null;
            "${mod}+Shift+8" = null;
            "${mod}+Shift+9" = null;
            "${mod}+Shift+0" = null;
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
              exec "rofi --no-startup-id -show drun -modi drun,run -show-icons"'';
            "${mod}+z" = "mode $mode_gaps";
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
        gaps = {
          smartGaps = false;
          # top = testGap;
          # right = testGap;
          # bottom = testGap;
          # left = testGap;
          inner = 15;
          outer = 0;
          # horizontal = testGap;
          # vertical = testGap;
        };
      };
      extraConfig = ''
        set $mode_gaps: (o)uter, (i)nner
        set $mode_gaps_outer Outer Gaps: +|-|0 (local), Shift + +|-|0 (global)
        set $mode_gaps_inner Inner Gaps: +|-|0 (local), Shift + +|-|0 (global)

        mode "$mode_gaps" {
          bindsym o      mode "$mode_gaps_outer"
          bindsym i      mode "$mode_gaps_inner"
          bindsym Return mode "$mode_gaps"
          bindsym Escape mode "default"
        }

        mode "$mode_gaps_outer" {
          bindsym plus        gaps outer current plus 5
          bindsym minus       gaps outer current minus 5
          bindsym 0           gaps outer current set 0
          bindsym Shift+plus  gaps outer all plus 5
          bindsym Shift+minus gaps outer all minus 5
          bindsym Shift+0     gaps outer all set 0
          bindsym Return mode "$mode_gaps"
          bindsym Escape mode "default"
        }

        mode "$mode_gaps_inner" {
          bindsym plus        gaps inner current plus 5
          bindsym minus       gaps inner current minus 5
          bindsym 0           gaps inner current set 0
          bindsym Shift+plus  gaps inner all plus 5
          bindsym Shift+minus gaps inner all minus 5
          bindsym Shift+0     gaps inner all set 0
          bindsym Return mode "$mode_gaps"
          bindsym Escape mode "default"
        }
      '';
    };
  };

  home.packages = with pkgs; [ dmenu pandoc xorg.xkbcomp ];
}
