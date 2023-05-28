{ inputs, config, lib, pkgs, ... }:

{
  imports = [ ../../home-manager-modules/base.nix ../../home-manager-modules ];

  modules = {
    base.gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
    rofi.enable = true;
    herbstluftwm.enable = true;
    polybar.enable = true;
    picom.enable = false;
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    dwarfFortress.enable = true;
    emacs.emacsVersion = "git";
    nyxt.enable = true;
    sbcl.enable = true;
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
    packages = with pkgs; [
      (angband.override { enableSdl2 = true; })
      cmus
      cmusfm
      element-desktop
      obs-studio
      pavucontrol
      rustup
    ];
    stateVersion = "21.03";
  };
}
