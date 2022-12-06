local lspconfig = require("lspconfig")
local cmp_nvim_lsp = require("cmp_nvim_lsp")

local capabilities = cmp_nvim_lsp.default_capabilities()
lspconfig.util.default_config = vim.tbl_deep_extend("force", lspconfig.util.default_config, {
    capabilities = capabilities,
})

require("lspsaga").init_lsp_saga()

local on_attach = function(_, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    local bufopts = { silent = true, buffer = bufnr }
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
    vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
    vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, bufopts)
    vim.keymap.set("n", "<leader>ln", vim.diagnostic.goto_next, bufopts)
    vim.keymap.set("n", "<leader>lp", vim.diagnostic.goto_prev, bufopts)
    vim.keymap.set("n", "<leader>ld", vim.diagnostic.open_float, bufopts)
    vim.keymap.set("n", "<leader>lw", vim.lsp.buf.workspace_symbol, bufopts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
    vim.keymap.set("n", "<leader>lf", vim.lsp.buf.references, bufopts)
    vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename, bufopts)
    vim.keymap.set("n", "<leader>li", "<cmd>LspInfo<cr>", bufopts)
    vim.keymap.set("n", "<C-h>", vim.lsp.buf.signature_help, bufopts)
end

lspconfig.rust_analyzer.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}

lspconfig.ocamllsp.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}

lspconfig.sumneko_lua.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
        Lua = {
            runtime = {
                version = "LuaJIT",
            },
            diagnostics = {
                globals = { "vim" },
            },
            workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
            },
            telemetry = {
                enable = false,
            }
        }
    }
}

lspconfig.hls.setup {
    on_attach = on_attach,
}

lspconfig.clojure_lsp.setup {
    on_attach = on_attach,
}
