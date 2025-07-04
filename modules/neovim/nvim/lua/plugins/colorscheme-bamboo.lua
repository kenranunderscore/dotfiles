return {
  "ribru17/bamboo.nvim",
  lazy = false,
  config = function()
    require("bamboo").setup({
      style = "vulgaris",
    })
    require("bamboo").load()
  end,
}
