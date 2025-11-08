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
    foot = {
      enable = true;
      includePkg = false;
    };
    ghostty.enable = false;
    herbstluftwm.enable = false;
    kermit.enable = false;
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
    niri = {
      enable = true;
      configFile = ../../modules/niri/config.kdl;
    };
    sbcl = {
      enable = true;
      withPackage = false;
    };
    sway = {
      enable = true;
      configFile = ../../modules/sway/config;
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
    signal-desktop
    subversion
    thunderbird
    (vivaldi.override { proprietaryCodecs = true; })
  ];

  home.stateVersion = "23.11";
}
