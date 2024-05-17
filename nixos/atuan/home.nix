{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../../modules/base.nix
    ../../modules
  ];

  modules = {
    bash.enable = true;
    base.gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
    rofi.enable = true;
    herbstluftwm.enable = false;
    i3.enable = true;
    polybar.enable = true;
    picom.enable = false;
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    dwarfFortress.enable = false;
    nyxt.enable = false;
    sbcl.enable = false;
    git.email = "johannes.maier@mailbox.org";
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
    packages = with pkgs; [ pavucontrol ];
    stateVersion = "21.03";
  };
}
