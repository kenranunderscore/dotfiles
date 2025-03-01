{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.git;
  types = lib.types;
in
{
  options.modules.git = {
    email = lib.mkOption {
      type = types.str;
      default = "";
    };
  };

  config = {
    programs.git = {
      package = pkgs.gitAndTools.gitFull;
      enable = true;
      userName = "Johannes Maier";
      userEmail = cfg.email;
      aliases = {
        co = "checkout";
        pushf = "push --force-with-lease";
      };
      includes = [
        {
          condition = "gitdir:~/ag/";
          contents = {
            user.email = "johannes.maier@active-group.de";
          };
        }
      ];
      ignores = [
        # (n)vim
        "*.swp"
        ".exrc"
        ".nvimrc"

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
      signing = {
        signByDefault = true;
        key = "~/.ssh/id_ed25519.pub";
      };
      extraConfig = {
        core.askPass = "";
        gpg.format = "ssh";
        init.defaultBranch = "main";
        merge.conflictstyle = "diff3";
        pull.rebase = "true";
        push.autoSetupRemote = "true";
        submodule.recurse = "true";
        url = {
          "https://github.com/" = {
            insteadOf = "gh:";
          };
          "git@github.com:kenranunderscore/" = {
            insteadOf = "gh:/";
          };
          "git@github.com:" = {
            insteadOf = "ghssh:";
          };
          "https://gitlab.com/" = {
            insteadOf = "gl:";
          };
          "ssh://git@gitlab.active-group.de:1022/ag/" = {
            insteadOf = "ag:";
          };
          "git@github.com:active-group/" = {
            insteadOf = "aggh:";
          };
        };
      };
    };
  };
}
