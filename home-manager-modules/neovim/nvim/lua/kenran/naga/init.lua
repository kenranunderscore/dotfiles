local M = {}
local colors = require("kenran.naga.colors")

function M.colorscheme()
    vim.cmd("hi clear")
    vim.o.background = "dark"
    vim.o.termguicolors = true
    vim.g.colors_name = "naga"
    local hl = vim.api.nvim_set_hl

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

    hl(0, "Normal", { fg = colors.fg, bg = colors.bg })
    hl(0, "Fg", { fg = colors.fg })
    hl(0, "Grey", { fg = colors.comment })
    hl(0, "Red", { fg = colors.red })
    hl(0, "Orange", { fg = colors.orange })
    hl(0, "Yellow", { fg = colors.yellow })
    hl(0, "Green", { fg = colors.fg })
    hl(0, "Blue", { fg = colors.blue })
    hl(0, "Purple", { fg = colors.purple })
    hl(0, "Grey", { fg = colors.comment })

    hl(0, "Constant", { fg = colors.purple })
    hl(0, "Type", { fg = colors.olivedrab3 })
    hl(0, "PreProc", { fg = colors.orange })
    hl(0, "String", { fg = colors.string })
    hl(0, "Character", { fg = colors.string })
    hl(0, "Special", { fg = colors.docstring })
    hl(0, "Identifier", { fg = colors.fg })
    hl(0, "Operator", { fg = colors.fg })
    hl(0, "Statement", { fg = colors.cyan })
    hl(0, "Keyword", { fg = colors.yellow })
    hl(0, "Comment", { fg = colors.comment })

    hl(0, "NonText", { fg = colors.whitespace_fg })
    hl(0, "Visual", { fg = colors.fg, bg = colors.dark_blue })
    hl(0, "Search", { fg = colors.bg, bg = colors.fg_dark })
    hl(0, "IncSearch", { fg = colors.bg, bg = colors.gold })
    hl(0, "LineNr", { fg = colors.bg_green })
    hl(0, "LineNrAbove", { fg = colors.block_light })
    hl(0, "LineNrBelow", { fg = colors.block_light })
    hl(0, "CursorLineNr", { fg = "#006400" })
    hl(0, "MatchParen", { bg = colors.red })
    hl(0, "Todo", { fg = colors.purple, bg = colors.bg, bold = true, underline = true })

    hl(0, "Pmenu", { bg = "#080808" })
    hl(0, "PmenuSel", { bg = colors.dark_blue, fg = colors.fg })
    hl(0, "PmenuSbar", { bg = colors.comment_dark })
    hl(0, "PmenuThumb", { bg = colors.fg })
    hl(0, "TabLine", { fg = colors.comment_dark, bg = colors.bg, italic = true })
    hl(0, "TabLineSel", { fg = colors.fg_dark, bg = colors.bg, underline = true })
    hl(0, "TabLineFill", { bg = colors.bg })
    hl(0, "CursorLine", { bg = colors.bg_green })
    hl(0, "CursorColumn", { bg = colors.bg_green })
    hl(0, "SignColumn", { bg = colors.bg, fg = colors.comment_dark })
    hl(0, "DiffAdd", { fg = colors.fg })
    hl(0, "DiffChange", { fg = colors.purple })
    hl(0, "DiffDelete", { fg = colors.red })
    hl(0, "DiffText", { fg = colors.red })

    hl(0, "fugitiveHunk", { fg = colors.comment_dark })
    hl(0, "diffAdded", { fg = colors.fg })
    hl(0, "diffRemoved", { fg = colors.orange_red })
    hl(0, "diffLine", { fg = colors.comment_light })
    hl(0, "diffSubName", { fg = colors.comment_light })
    hl(0, "fugitiveUnstagedModifier", { fg = colors.yellow })
    hl(0, "fugitiveUntrackedModifier", { fg = colors.yellow })
    hl(0, "fugitiveStagedModifier", { fg = colors.yellow })
    hl(0, "fugitiveUnstagedSection", { fg = colors.fg, italic = true })
    hl(0, "fugitiveUntrackedSection", { fg = colors.fg, italic = true })
    hl(0, "fugitiveStagedSection", { fg = colors.fg, italic = true })
    hl(0, "fugitiveUnstagedHeading", { fg = colors.orange, bold = true })
    hl(0, "fugitiveUntrackedHeading", { fg = colors.orange, bold = true })
    hl(0, "fugitiveStagedHeading", { fg = colors.orange, bold = true })
    hl(0, "fugitiveHeader", { fg = colors.yellow, bold = true })
    hl(0, "fugitiveHeading", { fg = colors.orange, bold = true })
    hl(0, "fugitiveHash", { fg = colors.yellow })
    hl(0, "fugitiveSymbolicRef", { fg = colors.cyan, bold = true })
end

return M
