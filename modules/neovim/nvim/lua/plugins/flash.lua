return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {
    jump = { autojump = false },
    modes = { search = { enabled = true } },
  },
  keys = {
    {
      "s",
      mode = { "n", "x", "o" },
      function()
        require("flash").jump()
      end,
      desc = "Flash",
    },
  },
}
