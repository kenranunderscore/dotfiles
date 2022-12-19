vim.api.nvim_create_augroup("autofmt", {})
vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function()
        if vim.g.autoformat == 1 and vim.bo.formatprg ~= "" then
            local view = vim.fn.winsaveview()
            vim.cmd("exe 'norm gggqG'")
            vim.fn.winrestview(view)
        end
    end,
})
