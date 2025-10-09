{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.fzf.enable = lib.mkEnableOption "fzf";

  config = lib.mkIf config.my.fzf.enable {
    home = {
      packages = [ pkgs.fzf ];
      sessionVariables = {
        FZF_DEFAULT_OPTS = "--exact  --extended  --select-1  --exit-0 --cycle --bind ctrl-p:up,ctrl-n:down,alt-p:prev-history,alt-n:next-history --layout reverse  --height 40%  --border sharp  --preview-window ',border-sharp'";
        FZF_ALT_C_OPTS = "--preview 'tree -C {} | head -n 50'";
        FZF_CTRL_T_OPTS = "--prompt 'Choose file: '";
        FZF_TMUX = "1";
      };
    };
  };
}
