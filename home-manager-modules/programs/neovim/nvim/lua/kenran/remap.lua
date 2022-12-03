local M = {}

local function bind(op, outer_opts)
    return function(lhs, rhs, opts)
        opts = vim.tbl_extend("force", outer_opts or {}, opts or {})
        vim.keymap.set(op, lhs, rhs, opts)
    end
end

M.nmap = bind("n", {noremap = false})
M.nnoremap = bind("n")
M.vnoremap = bind("v")
M.xnoremap = bind("x")
M.inoremap = bind("i")

return M
