{ inputs, config, lib, pkgs, ... }:

{
  imports = [ ../base.nix ../../modules ];

  hosts.base = {
    privateDir = "${inputs.privateConfig}";
    shellPath = "${pkgs.zsh}/bin/zsh";
    gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
  };

  modules = {
    desktop = {
      rofi.enable = true;
      i3 = {
        enable = true;
        withGaps = false;
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
            # Workaround: otherwise i3 module will be empty at first
            command = "systemctl --user restart polybar";
            notification = false;
          }
          {
            command = "xset r rate 200 55";
            always = true;
            notification = false;
          }
          {
            command = "setxkbmap -option 'caps:ctrl_modifier'";
            notification = false;
            always = true;
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
    fonts = {
      enable = true;
      withCustomBuilds = false;
    };
    programs = {
      emacs.emacsVersion = "stable";
      kitty = { useLoginShell = false; };
      nyxt.enable = true;
      sbcl.enable = true;
    };
    shell.git.email = "johannes.maier@mailbox.org";
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
    packages = with pkgs; [ xorg.xkbcomp xcape ];
    stateVersion = "21.03";
  };
}
