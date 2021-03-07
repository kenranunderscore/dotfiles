{ config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base = {
    username = "johannes";
    privateDir = ../private/linux;
    shellPath = "${pkgs.fish}/bin/fish";
  };

  modules = {
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    programs = {
      kitty = {
        useLoginShell = false;
        fontSize = "12.0";
      };
    };
    shell = {
      git = {
        email = "johb.maier@gmail.com";
        gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
      };
    };
  };

  programs = { rofi.enable = true; };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  home.packages = with pkgs; [
    htop
    manpages
    nextcloud-client
    polybar
    sxhkd
    xorg.xkbcomp
  ];
}
