{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.git;
  types = lib.types;
in
{
  options.my.git = {
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

        # JetBrains IDEs
        ".idea/"
      ];
      signing = {
        signByDefault = true;
        key = "~/.ssh/id_ed25519.pub";
      };
      extraConfig = {
        branch.sort = "-committerdate";
        column.sort = "auto";
        core.askPass = "";
        diff = {
          algorithm = "histogram";
          mnemonicPrefix = true;
          renames = true;
        };
        fetch = {
          prune = true;
          pruneTags = true;
          all = true;
        };
        gpg.format = "ssh";
        init.defaultBranch = "main";
        merge.conflictstyle = "zdiff3";
        pull.rebase = "true";
        push.autoSetupRemote = "true";
        rebase = {
          autoSquash = true;
          autoStash = true;
          updateRefs = true;
        };
        submodule.recurse = "true";
        tag.sort = "version:refname";
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
