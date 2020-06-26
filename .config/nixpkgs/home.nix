{ config, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
  username = if isDarwin then "maier" else "kenran";
  homeDirectory = if isDarwin then "/Users/maier" else "/home/kenran";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = { inherit username homeDirectory; };

  nixpkgs.config.firefox.enableFlash = false;

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Johannes Maier";
    userEmail =
      if isDarwin
      then "johannes.maier@active-group.de"
      else "johb.maier@gmail.com";
    ignores = [];
    extraConfig = {
      pull.rebase = "false";
      core.editor = "vim";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  home.packages = with pkgs; [
    direnv
    lorri
    ripgrep
    vim
  ];
}
