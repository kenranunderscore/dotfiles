{ ... }:

{
  # Any static configuration files that should be put into $XDG_CONFIG_HOME
  xdg.configFile = {
    "fourmolu/fourmolu.yaml".source = ./fourmolu.yaml;
    "npm/config".source = ./npm_config;
  };

  # Some programs behave according to XDG conventions when certain environment
  # variables are set. Let's put all of those here; they're not really annoying
  # even if I don't use the corresponding programs much or at all anymore, but
  # prevent cluttering of $HOME in case I quickly try something.
  home = {
    sessionVariables = {
      # Haskell
      STACK_XDG = "1";
      GHCUP_USE_XDG_DIRS = "1";

      # Rust
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
      RUSTUP_HOME = "$XDG_DATA_HOME/rustup";

      # Python (thanks hlissner)
      IPYTHONDIR = "$XDG_CONFIG_HOME/ipython";
      PIP_CONFIG_FILE = "$XDG_CONFIG_HOME/pip/pip.conf";
      PIP_LOG_FILE = "$XDG_DATA_HOME/pip/log";
      PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
      JUPYTER_CONFIG_DIR = "$XDG_CONFIG_HOME/jupyter";

      # JavaScript / NodeJS
      NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
      NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
      NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
      NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
      NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";

      # Nim
      NIMBLE_DIR = "$XDG_DATA_HOME/nimble";
    };
  };
}
