local lspconfig = require("lspconfig")
local cmp_nvim_lsp = require("cmp_nvim_lsp")

local capabilities = cmp_nvim_lsp.default_capabilities()
lspconfig.util.default_config = vim.tbl_deep_extend("force", lspconfig.util.default_config, {
    capabilities = capabilities,
})

require("lspsaga").init_lsp_saga()

local nnoremap = require("kenran.remap").nnoremap
nnoremap("<leader>e", vim.diagnostic.open_float)
nnoremap("<leader>e", vim.diagnostic.goto_prev)

local on_attach = function(_, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    nnoremap("gd", vim.lsp.buf.definition)
    nnoremap("gD", vim.lsp.buf.declaration)
    nnoremap("gt", vim.lsp.buf.type_definition)
    nnoremap("gi", vim.lsp.buf.implementation)
    nnoremap("<leader>la", vim.lsp.buf.code_action)
    nnoremap("<leader>ln", vim.diagnostic.goto_next)
    nnoremap("<leader>lp", vim.diagnostic.goto_prev)
    nnoremap("<leader>ld", vim.diagnostic.open_float)
    nnoremap("<leader>lw", vim.lsp.buf.workspace_symbol)
    nnoremap("K", vim.lsp.buf.hover)
    nnoremap("<leader>lf", vim.lsp.buf.references)
    nnoremap("<leader>lr", vim.lsp.buf.rename)
    nnoremap("<C-h>", vim.lsp.buf.signature_help)
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

local haskell_tools = require("haskell-tools")
haskell_tools.setup {
    hls = {
        on_attach = on_attach,
    },
}
