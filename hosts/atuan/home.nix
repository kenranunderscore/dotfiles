{ inputs, config, lib, pkgs, ... }:

{
  imports = [ ../../home-manager-modules/base.nix ../../home-manager-modules ];

  hosts.base.gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";

  modules = {
    desktop = {
      rofi.enable = true;
      i3 = {
        enable = true;
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
            command = "i3-msg 'workspace 1:main; exec kitty'";
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
    games.dwarfFortress.enable = true;
    programs = {
      emacs.emacsVersion = "git";
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
    packages = with pkgs; [
      (angband.override { enableSdl2 = true; })
      cmus
      cmusfm
      element-desktop
      obs-studio
      pavucontrol
    ];
    stateVersion = "21.03";
  };
}
