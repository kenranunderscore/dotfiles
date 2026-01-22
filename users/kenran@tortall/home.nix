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
    alacritty = {
      enable = true;
      wrapWithNixGL = true;
    };
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
      configFile = ./niri/config.kdl;
    };
    sbcl = {
      enable = true;
      withPackage = false;
    };
    sway = {
      enable = true;
      configFile = ./sway/config;
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
    vivaldi = {
      enable = true;
      wrapWithNixGL = true;
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
    jujutsu
    keepass
    keepassxc
    lazygit
    leiningen
    neovide
    nixgl.auto.nixGLDefault
    nixVersions.latest
    nixos-rebuild
    nodejs
    pavucontrol
    pijul
    prismlauncher
    sieve-connect
    signal-desktop
    subversion
    thunderbird
  ];

  home.stateVersion = "23.11";
}
