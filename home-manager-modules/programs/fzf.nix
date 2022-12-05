{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.fzf;
in {
  options.modules.shell.fzf.enable = lib.mkEnableOption "fzf";

  config = lib.mkIf cfg.enable {
    home = {
      packages = [ pkgs.fzf ];
      sessionVariables = {
        FZF_DEFAULT_OPTS =
          "--border sharp --height 40% --layout reverse --select-1 --exit-0 --preview-window ',border-sharp'";
        FZF_ALT_C_OPTS = "--preview 'tree -C {} | head -n 50'";
        FZF_CTRL_T_OPTS = "--prompt 'Choose file: '";
        FZF_TMUX = "1";
      };
    };
  };
}
