{ config, pkgs, ... }:

with import <home-manager/modules/lib/dag.nix> { lib = pkgs.lib; };
let
  isDarwin = pkgs.stdenv.isDarwin;
  username = if isDarwin then "maier" else "kenran";
  homeDirectory = if isDarwin then "/Users/maier" else "/home/kenran";
  pwd = builtins.toPath ./.;
  osPrivatePath = if isDarwin then ./private/macos else ./private/linux;
  shellPath = "${pkgs.zsh}/bin/zsh";
in {
  # Config for nixpkgs when used by home-manager.
  nixpkgs = {
    config = import ./config/nixpkgs-config.nix;
    overlays = [ (import ./config/emacs-overlay.nix) ];
  };

  imports = [
    (import ./nix/email.nix isDarwin)
    (import ./nix/git.nix isDarwin)
    (import ./nix/kitty.nix isDarwin shellPath)
    (import ./nix/tmux.nix shellPath)
    ./nix/irssi.nix
  ];

  fonts.fontconfig.enable = true;

  programs = {
    home-manager.enable = true;
    rofi.enable = !isDarwin;
    ssh.enable = true;
    gpg.enable = true;
    zsh.enable = true;
    mbsync.enable = true;
    msmtp.enable = true;
    password-store.enable = true;
  };

  xresources.properties = {
    "Xft.dpi" = 96;
    "Xft.antialias" = true;
    "Xft.rgba" = "rgb";
    "Xft.hinting" = true;
    "Xft.hintstyle" = "hintfull";

    # A basic default colorscheme; useful for roguelike games for instance.
    "*.foreground" = "#fcfcfc";
    "*.background" = "#313133";
    "*.cursorColor" = "#cfcfc";
    "*.color0" = "#181819";
    "*.color8" = "#77747f";
    "*.color1" = "#ef2917";
    "*.color9" = "#ef2917";
    "*.color2" = "#97cc04";
    "*.color10" = "#97cc04";
    "*.color3" = "#ffad05";
    "*.color11" = "#ffad05";
    "*.color4" = "#2d7dd2";
    "*.color12" = "#2d7dd2";
    "*.color5" = "#f230aa";
    "*.color13" = "#f230aa";
    "*.color6" = "#5fbff9";
    "*.color14" = "#5fbff9";
    "*.color7" = "#94949f";
    "*.color15" = "#fcfcfc";
  };

  xdg.configFile = {
    "bspwm/bspwmrc".source = ./config/bspwmrc;
    "doom" = {
      source = ./config/doom;
      recursive = true;
    };
    "nixpkgs/config.nix".source = ./config/nixpkgs-config.nix;
    "polybar/config".source = ./config/polybar;
    "sxhkd/sxhkdrc".source = ./config/sxhkdrc;
  };

  services = {
    gpg-agent = {
      enable = !isDarwin;
      enableSshSupport = true;
    };
  };

  home = {
    stateVersion = "20.09";

    inherit username homeDirectory;

    packages = let
      basePackages = with pkgs; [
        bat
        curl
        direnv
        gnumake
        jetbrains-mono
        lorri
        mu
        nixfmt
        nix-prefetch-git
        pijul
        plantuml
        ripgrep
        rsync
        vim
        wget
        xorg.xkbcomp
      ];
      darwinPackages = with pkgs; [
        pkgs.emacsUnstable
      ];
      linuxPackages = with pkgs; [
        emacsUnstable
        firefox-bin
        nextcloud-client
        polybar
        sxhkd
      ];
    in basePackages ++ (if isDarwin then [ ] else linuxPackages)
    ++ (if isDarwin then darwinPackages else [ ]);

    file = {
      ".irssi/h3rbz.theme".source = ./config/h3rbz.theme;
      # The private key file is linked to directly during activation.
      ".ssh/id_rsa.pub".source = osPrivatePath + "/id_rsa.pub";
      ".vimrc".source = ./config/vimrc;
      ".zshrc".source = ./config/zshrc;
      ".zshenv".source = ./config/zshenv;
    };

    # We symlink our git submodule to circumvent a nix store directory being
    # read-only. Maybe there's a way to still use fetchFromGitHub...
    activation = {
      symlinkAndSyncDoom = dagEntryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -snf ${pwd}/config/doom-emacs $HOME/.emacs.d && \
        $DRY_RUN_CMD ~/.emacs.d/bin/doom sync
      '';

      symlinkMacOSApps = let
        action = if isDarwin then
          "$DRY_RUN_CMD ln -snf $HOME/.nix-profile/Applications/*.app ~/Applications/"
        else
          "";
      in dagEntryAfter [ "writeBoundary" ] action;

      addXterm24bitTerminfo =
        let tic = if isDarwin then "/usr/bin/tic" else "tic";
        in dagEntryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ${tic} -x -o ~/.terminfo ${./config/xterm-24bit.terminfo}
        '';

      handlePrivateKeys = let privateKeyPath = osPrivatePath + "/id_rsa";
      in dagEntryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -sf ${
          builtins.toPath privateKeyPath
        } $HOME/.ssh/id_rsa && \
        $DRY_RUN_CMD cd ${pwd}/private && \
        $DRY_RUN_CMD chmod 400 *.pem **/*.key **/id_rsa* && \
        $DRY_RUN_CMD ssh-add $HOME/.ssh/id_rsa && \
        $DRY_RUN_CMD gpg --import ${osPrivatePath + "/gpg.key"}
      '';
    };
  };
}
