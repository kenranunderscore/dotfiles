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
    desktop = { i3.enable = true; };
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
      isSyncServer = false;
    };
    games.dwarfFortress.enable = true;
    programs = {
      emacs.emacsVersion = "git";
      emacs.nativeComp = true;
      kitty = {
        useLoginShell = false;
        fontSize = "12.0";
      };
    };
    shell = { git.email = "johb.maier@gmail.com"; };
  };

  programs = { msmtp.enable = true; };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  home.packages = with pkgs; [ dmenu pandoc xorg.xkbcomp ];
}
