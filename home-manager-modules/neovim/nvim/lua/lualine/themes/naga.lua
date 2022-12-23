local M = {}
local colors = require("kenran.naga.colors")

M.normal = {
    a = { bg = colors.fg, fg = colors.bg },
    b = { bg = "#090909", fg = colors.gold },
    c = { bg = "#090909", fg = colors.sea_green },
}
M.insert = {
    a = { bg = colors.orange_red, fg = colors.bg },
    b = { bg = "#090909", fg = colors.gold },
    c = { bg = "#090909", fg = colors.sea_green },
}
M.command = {
    a = { bg = colors.purple, fg = colors.bg },
    b = { bg = "#090909", fg = colors.gold },
    c = { bg = "#090909", fg = colors.sea_green },
}
M.terminal = {
    a = { bg = colors.fg, fg = colors.bg },
    b = { bg = "#090909", fg = colors.gold },
    c = { bg = "#090909", fg = colors.sea_green },
}
M.visual = {
    a = { bg = colors.gold, fg = colors.bg },
    b = { bg = "#090909", fg = colors.gold },
    c = { bg = "#090909", fg = colors.sea_green },
}
M.replace = {
    a = { bg = colors.fg, fg = colors.bg },
    b = { bg = "#090909", fg = colors.gold },
    c = { bg = "#090909", fg = colors.sea_green },
}
M.inactive = {
    a = { bg = colors.fg, fg = colors.comment },
    b = { bg = "#090909", fg = colors.comment },
    c = { bg = "#090909", fg = colors.comment },
}

return M
