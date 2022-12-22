local M = {}

local colors = {
    bg = "#040404",
    bg_green = "#041a04",
    fg = "#0ac30a",
    fg_dark = "#078807",
    yellow = "#eec900",
    gold = "#eead0e",
    cyan = "#00cdcd",
    string = "#b3ee3a",
    purple = "#cc59d2",
    orange = "#ff9000",
    comment = "#707370",
    comment_light = "#099590",
    comment_dark = "#353835",
    docstring = "#698b22",
    gray = "#aabaaa",
    dark_blue = "#01018a",
    sea_green = "#3cb371",
    orange_red = "#ff4500",
    red = "#ff1500",
    whitespace_fg = "#555f55",
    block = "#060606",
    block_light = "#252525",
    olivedrab3 = "#9acd32",
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
    vim.api.nvim_set_hl(0, "Red", { fg = colors.red })
    vim.api.nvim_set_hl(0, "Orange", { fg = colors.orange })
    vim.api.nvim_set_hl(0, "Yellow", { fg = colors.yellow })
    vim.api.nvim_set_hl(0, "Green", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Blue", { fg = colors.blue })
    vim.api.nvim_set_hl(0, "Purple", { fg = colors.purple })
    vim.api.nvim_set_hl(0, "Normal", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Grey", { fg = colors.comment })

    vim.api.nvim_set_hl(0, "Constant", { fg = colors.purple })
    vim.api.nvim_set_hl(0, "Type", { fg = colors.olivedrab3 })
    vim.api.nvim_set_hl(0, "PreProc", { fg = colors.orange })
    vim.api.nvim_set_hl(0, "String", { fg = colors.string })
    vim.api.nvim_set_hl(0, "Character", { fg = colors.string })
    vim.api.nvim_set_hl(0, "Special", { fg = colors.docstring })
    vim.api.nvim_set_hl(0, "Identifier", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Operator", { fg = colors.fg })
    vim.api.nvim_set_hl(0, "Statement", { fg = colors.cyan })
    vim.api.nvim_set_hl(0, "Keyword", { fg = colors.yellow })
    vim.api.nvim_set_hl(0, "Comment", { fg = colors.comment })

    vim.api.nvim_set_hl(0, "NonText", { fg = colors.whitespace_fg })
    vim.api.nvim_set_hl(0, "Visual", { fg = colors.fg, bg = colors.dark_blue })
    vim.api.nvim_set_hl(0, "Search", { fg = colors.bg, bg = colors.fg_dark })
    vim.api.nvim_set_hl(0, "IncSearch", { fg = colors.bg, bg = colors.fg })
    vim.api.nvim_set_hl(0, "LineNr", { fg = colors.block_light })
    vim.api.nvim_set_hl(0, "LineNrAbove", { fg = colors.block_light })
    vim.api.nvim_set_hl(0, "LineNrBelow", { fg = colors.block_light })
    vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#006400" })
    vim.api.nvim_set_hl(0, "MatchParen", { bg = colors.red })
    vim.api.nvim_set_hl(0, "Todo", { fg = colors.purple, bg = colors.bg, bold = true, underline = true })

    vim.api.nvim_set_hl(0, "Pmenu", { bg = "#080808" })
    vim.api.nvim_set_hl(0, "PmenuSel", { bg = colors.dark_blue, fg = colors.fg })
    vim.api.nvim_set_hl(0, "PmenuSbar", { bg = colors.comment_dark })
    vim.api.nvim_set_hl(0, "PmenuThumb", { bg = colors.fg })
    vim.api.nvim_set_hl(0, "TabLine", { fg = colors.comment_dark, bg = colors.bg, italic = true })
    vim.api.nvim_set_hl(0, "TabLineSel", { fg = colors.fg_dark, bg = colors.bg, underline = true })
    vim.api.nvim_set_hl(0, "TabLineFill", { bg = colors.bg })
    vim.api.nvim_set_hl(0, "CursorLine", { bg = colors.bg_green })
    vim.api.nvim_set_hl(0, "CursorColumn", { bg = colors.bg_green })
end

return M
