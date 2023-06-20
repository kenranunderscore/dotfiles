{ inputs, config, lib, pkgs, ... }:

let email = "johannes.maier@active-group.de";
in {
  imports = [ ../../home-manager-modules/base.nix ../../home-manager-modules ];

  targets.genericLinux = { enable = true; };

  modules = {
    base.gpgKey = "9AC78C1A48681583";
    herbstluftwm.enable = true;
    rofi.enable = true;
    neovim.enable = lib.mkForce false;
    sbcl.enable = true;
    polybar = {
      enable = true;
      withBattery = true;
    };
    picom.enable = false;
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    fonts.enable = true;
    emacs.emacsVersion = "git";
    imwheel.enable = true;
    nyxt.enable = true;
    git.email = email;
  };

  programs = {
    mercurial = {
      enable = true;
      userEmail = email;
      userName = "Johannes Maier";
      # FIXME: always enable git, and use its config here, or extract
      ignores = [
        # Vim
        "*.swp"
        # Direnv
        ".direnv/"
        ".envrc"
        # macOS
        ".DS_Store"
        # Emacs: backup, auto-save, lock files, directory-local
        # variables
        "*~"
        "\\#*\\#"
        ".\\#*"
        ".dir-locals.el"
      ];
      aliases = { p = "pull -u"; };
    };
    vim.enable = lib.mkForce false;
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
    syncthing.enable = true;
  };

  home.packages = let
    googleChromeBin = pkgs.writeShellScriptBin "google-chrome" ''
      google-chrome-stable $@
    '';
  in with pkgs; [
    # citrix_workspace
    cloc
    dbeaver
    dhall
    discord
    element-desktop
    google-chrome-beta
    googleChromeBin
    keepass
    keepassxc
    leiningen
    linphone
    mattermost-desktop
    pavucontrol
    prismlauncher
    racket
    sieve-connect
    subversion
    teams
    thunderbird
    vlc
    wireshark
    zoom-us
  ];

  home.stateVersion = "22.05";

  xsession.enable = true;
}
