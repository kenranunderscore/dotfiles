{ inputs, config, lib, pkgs, ... }:

{
  imports = [ ../base.nix ../../modules ];

  hosts.base = {
    username = "kenran";
    privateDir = "${inputs.privateConfig}/linux";
    shellPath = "${pkgs.fish}/bin/fish";
    gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
  };

  modules = {
    desktop = { i3.enable = true; };
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
    };
    shell.git.email = "johannes.maier@mailbox.org";
  };

  xsession = {
    windowManager.i3.config.startup = lib.mkForce [
      {
        command = "xrandr --dpi 96";
        always = false;
      }
      {
        command = "setxkbmap -layout us -variant altgr-intl";
        always = false;
      }
    ];
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  home = {
    packages = with pkgs; [ pandoc xorg.xkbcomp firefox-bin ];
    stateVersion = "21.03";
  };
}
