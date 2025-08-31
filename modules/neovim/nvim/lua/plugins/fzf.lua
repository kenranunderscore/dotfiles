return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  cmd = "FzfLua",
  config = function()
    local small_window = {
      width = 0.5,
    }
    require("fzf-lua").setup {
      fzf_opts = {
        -- Override some values that are otherwise inherited from
        -- my shell's $FZF_DEFAULT_OPTS
        ["--no-select-1"] = true,
        ["--no-exit-0"] = true,
        ["--layout"] = "default",
      },
      winopts = {
        height = 0.4,
        row = 0.5,
        col = 0.5,
      },
      files = {
        previewer = false,
        winopts = small_window,
      },
      oldfiles = {
        previewer = false,
        winopts = small_window,
      },
      buffers = {
        previewer = false,
        winopts = small_window,
      },
      grep = {
        winopts = {
          width = 0.9,
          height = 0.6,
          row = 0.5,
          col = 0.5,
          preview = {
            horizontal = "right:40%",
          },
        },
      },
    }
  end,
  keys = {
    { "<leader>pf", "<cmd>FzfLua files<cr>", desc = "Find file" },
    { "<leader>b", "<cmd>FzfLua buffers<cr>", desc = "Select buffer" },
    { "<leader>ps", "<cmd>FzfLua live_grep_native<cr>", desc = "Find file" },
  },
}
