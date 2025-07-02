local opt = vim.opt_local
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.smartindent = true
opt.formatprg = "stylua --stdin-filepath % 2>&1 -"
