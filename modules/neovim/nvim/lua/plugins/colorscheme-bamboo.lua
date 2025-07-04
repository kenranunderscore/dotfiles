return {
  "ribru17/bamboo.nvim",
  lazy = false,
  config = function()
    require("bamboo").setup({
      style = "multiplex",
    })
    require("bamboo").load()
  end,
}
