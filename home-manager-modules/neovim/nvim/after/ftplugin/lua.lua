vim.opt_local.formatprg =
    "tee /tmp/format_lua | stylua --config-path ./.stylua.toml --stdin-filepath % 2>/dev/null - || cat /tmp/format_lua"
