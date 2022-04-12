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
        workspaces = builtins.map (n: {
          number = n;
          label = toString n;
          output = "HDMI-0";
        }) [ 1 2 3 4 5 6 7 8 9 ];
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
