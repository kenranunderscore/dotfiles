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
            command = "i3-msg 'workspace 5:chat; exec kitty weechat'";
            notification = false;
          }
          {
            command = "i3-msg 'workspace 1:main/emacs; exec kitty'";
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
