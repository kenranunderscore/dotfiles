{ pkgs, ... }:

{
  imports = [
    ../../modules/base.nix
    ../../modules
  ];

  my = {
    base.gpgKey = "9AC78C1A48681583";
    bash.enable = true;
    brave = {
      enable = false;
      wrapWithNixGL = false;
    };
    alacritty.enable = true;
    ghostty.enable = false;
    kermit.enable = true;
    herbstluftwm.enable = false;
    i3.enable = false;
    kitty = {
      enable = false;
      wrapWithNixGL = true;
    };
    rofi.enable = true;
    kakoune.enable = true;
    neovim = {
      enable = true;
      includePkg = true;
    };
    sbcl = {
      enable = true;
      withPackage = false;
    };
    polybar.enable = false;
    picom.enable = false;
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    doom.enable = true;
    emacs.enable = false;
    nyxt.enable = false;
    git.email = "johannes.maier@mailbox.org";
    mercurial = {
      enable = true;
      email = "johannes.maier@mailbox.org";
    };
    wezterm = {
      enable = false;
      withPackage = false;
    };
  };

  home.packages = with pkgs; [
    ansible
    cloc
    clojure
    dmenu
    element-desktop
    fontforge-gtk
    gcli
    jfrog-cli
    keepass
    keepassxc
    leiningen
    neovide
    nixgl.auto.nixGLDefault
    nixVersions.latest
    nixos-rebuild
    nodejs
    pavucontrol
    pijul
    sieve-connect
    signal-desktop-bin
    subversion
    thunderbird
    (vivaldi.override { proprietaryCodecs = true; })
  ];

  home.stateVersion = "23.11";
}
