local M = {}

local colors = {
    bg = "#040404",
    bg_green = "#041a04",
    fg = "#0ac30a",
    fg_dark = "#078807",
    yellow = "#eec900",
    cyan = "#00cdcd",
    string = "#b3ee3a",
    orange = "#ff9000",
    comment = "#707370",
    blue = "#01018a",
    purple = "#cc59d2",
}

function M.colorscheme()
    vim.cmd("hi clear")
    vim.o.background = "dark"
    vim.o.termguicolors = true
    vim.g.colors_name = "naga"

    -- terminal colors, let's make it similar to kitty
    vim.g.terminal_color_0 = colors.fg_dark
    vim.g.terminal_color_8 = "#545454"
    vim.g.terminal_color_7 = colors.fg
    vim.g.terminal_color_15 = colors.fg
    vim.g.terminal_color_1 = colors.orange
    vim.g.terminal_color_9 = colors.orange
    vim.g.terminal_color_2 = colors.fg
    vim.g.terminal_color_10 = colors.fg
    vim.g.terminal_color_3 = colors.yellow
    vim.g.terminal_color_11 = colors.yellow
    vim.g.terminal_color_4 = colors.blue
    vim.g.terminal_color_12 = colors.blue
    vim.g.terminal_color_5 = colors.purple
    vim.g.terminal_color_13 = colors.purple
    vim.g.terminal_color_6 = colors.cyan
    vim.g.terminal_color_14 = colors.cyan

    vim.api.nvim_set_hl(0, "Fg", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Grey", { fg = colors.comment })
    vim.api.nvim_set_hl(0, "Red", { fg = "#f00000" })
    vim.api.nvim_set_hl(0, "Orange", { fg = colors.orange })
    vim.api.nvim_set_hl(0, "Yellow", { fg = colors.yellow })
    vim.api.nvim_set_hl(0, "Green", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Blue", { fg = colors.blue })
    vim.api.nvim_set_hl(0, "Purple", { fg = colors.purple })
    vim.api.nvim_set_hl(0, "Normal", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Grey", { fg = colors.comment })
end

return M
