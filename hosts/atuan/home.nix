{ inputs, config, lib, pkgs, ... }:

{
  imports = [ ../base.nix ../../modules ];

  hosts.base = {
    privateDir = "${inputs.privateConfig}/linux";
    shellPath = "${pkgs.zsh}/bin/zsh";
    gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
  };

  modules = {
    desktop = {
      rofi.enable = true;
      i3 = {
        enable = false;
        workspaces = [
          { name = "1:main/emacs"; }
          {
            name = "2:web";
            assigns = [{ class = "firefox"; }];
          }
          { name = "3"; }
          { name = "4"; }
          { name = "5:chat"; }
          { name = "6"; }
          { name = "7"; }
          { name = "8"; }
          { name = "9"; }
        ];
        startupCommands = [
          {
            command = "~/.fehbg";
            always = false;
            notification = false;
          }
          {
            command = "firefox";
            notification = false;
          }
          {
            # Workaround: otherwise i3 module will be empty at first
            command = "systemctl --user restart polybar";
            notification = false;
          }
          {
            command = "i3-msg 'workspace 5:chat; exec kitty weechat'";
            notification = false;
          }
          {
            command = "i3-msg 'workspace 1:main/emacs; exec kitty'";
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
    games.dwarfFortress.enable = true;
    programs = {
      emacs.emacsVersion = "git";
      emacs.nativeComp = true;
      kitty = { useLoginShell = false; };
      nyxt.enable = true;
      sbcl.enable = true;
    };
    shell.git.email = "johannes.maier@mailbox.org";
  };

  xsession.windowManager.herbstluftwm = let
    mod = "Mod4";
    prependMod = lib.mapAttrs' (key: lib.nameValuePair "${mod}-${key}");
    resizeStep = "0.05";
  in rec {
    enable = true;
    settings = {
      window_border_width = 0;
      window_border_active_color = "#0033aa";
    };
    # Add real tags manually later.
    tags = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" ];
    keybinds = prependMod {
      o = "split right 0.5";
      Shift-o = "split left 0.5";
      u = "split bottom 0.5";
      Shift-u = "split top 0.5";
      l = "focus right";
      h = "focus left";
      k = "focus up";
      j = "focus down";
      Shift-l = "shift right";
      Shift-h = "shift left";
      Shift-k = "shift up";
      Shift-j = "shift down";
      Control-l = "resize right ${resizeStep}";
      Control-h = "resize left ${resizeStep}";
      Control-k = "resize up ${resizeStep}";
      Control-j = "resize down ${resizeStep}";
      t = "spawn kitty";
      space = "spawn rofi -disable-history -show run";
      r = "remove";
      s = "floating toggle";
      f = "fullscreen toggle";
      p = "pseudotile toggle";
    };
    mousebinds = prependMod {
      B1 = "move";
      B2 = "zoom";
      B3 = "resize";
    };
    extraConfig = ''
      for i in ${lib.escapeShellArgs tags}; do
        if ! [ -z "$i" ]; then
          index=$(expr $i - 1)
          herbstclient keybind "${mod}-$i" use_index "$index"
          herbstclient keybind "${mod}-Shift-$i" move_index "$index"
        fi
      done
      herbstclient use_index 0

      ~/.fehbg
    '';
  };

  services = {
    syncthing.enable = true;
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  xsession.enable = true;

  home = {
    packages = with pkgs; [ cmus cmusfm ];
    stateVersion = "21.03";
  };
}
