{ config, ... }:

let
  inherit (config.xdg) dataHome configHome cacheHome;
in
{
  # Any static configuration files that should be put into $XDG_CONFIG_HOME
  symlink-config.files = [
    {
      source = ./fourmolu.yaml;
      destination = "fourmolu/fourmolu.yaml";
      xdg = true;
    }
    {
      source = ./npm_config;
      destination = "npm/config";
      xdg = true;
    }
  ];

  # Some programs behave according to XDG conventions when certain environment
  # variables are set. Let's put all of those here; they're not really annoying
  # even if I don't use the corresponding programs much or at all anymore, but
  # prevent cluttering of $HOME in case I quickly try something.
  home.sessionVariables = {
    # Haskell
    STACK_XDG = "1";
    GHCUP_USE_XDG_DIRS = "1";

    # Rust
    CARGO_HOME = "${dataHome}/cargo";
    RUSTUP_HOME = "${dataHome}/rustup";

    # Python (thanks hlissner)
    IPYTHONDIR = "${configHome}/ipython";
    PIP_CONFIG_FILE = "${configHome}/pip/pip.conf";
    PIP_LOG_FILE = "${dataHome}/pip/log";
    PYTHONSTARTUP = "${configHome}/python/pythonrc";
    JUPYTER_CONFIG_DIR = "${configHome}/jupyter";

    # JavaScript / NodeJS
    NPM_CONFIG_USERCONFIG = "${configHome}/npm/config";
    NPM_CONFIG_CACHE = "${cacheHome}/npm";
    NPM_CONFIG_TMP = "\${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/npm";
    NPM_CONFIG_PREFIX = "${cacheHome}/npm";
    NODE_REPL_HISTORY = "${cacheHome}/node/repl_history";

    # Nim
    NIMBLE_DIR = "${dataHome}/nimble";

    # OCaml
    OPAMROOT = "${dataHome}/opam";
    DUNE_CACHE_ROOT = "${cacheHome}/dune}";
  };
}
