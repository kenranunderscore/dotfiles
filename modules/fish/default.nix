{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.fish;
in
{
  options.my.fish.enable = lib.mkEnableOption "fish";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.eza ];

    programs.fish = {
      enable = true;
      plugins = [ ];
      shellAbbrs = import ../shell-aliases.nix { inherit pkgs; };
      interactiveShellInit = ''
        set fish_greeting
        bind \cp navigate_to_project
        bind \cr command_history_search
        bind \cv open_file_in_emacs

        fish_add_path --path $HOME/.config/emacs/bin
        fish_add_path --path $HOME/.local/bin
      '';
      shellAliases = {
        ls = "eza";
        l = "eza -lbF --group-directories-first --icons";
        ll = "eza -lbGF --group-directories-first --icons";
        la = "eza -labF --group-directories-first --icons";
        lla = "eza -labGF --group-directories-first --icons";
      };
    };

    # Manage functions manually
    xdg.configFile = {
      "fish/functions" = {
        source = ./functions;
        recursive = true;
      };
    };
  };
}
