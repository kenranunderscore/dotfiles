local lspconfig = require("lspconfig")
local cmp_nvim_lsp = require("cmp_nvim_lsp")
require("lspsaga").init_lsp_saga()

local nnoremap = require("kenran.remap").nnoremap
nnoremap("<leader>e", vim.diagnostic.open_float)
nnoremap("<leader>e", vim.diagnostic.goto_prev)

local on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    nnoremap("gd", vim.lsp.buf.definition)
    nnoremap("gD", vim.lsp.buf.declaration)
    nnoremap("<leader>la", vim.lsp.buf.code_action)
    nnoremap("<leader>ln", vim.diagnostic.goto_next)
    nnoremap("<leader>lp", vim.diagnostic.goto_prev)
    nnoremap("<leader>ld", vim.diagnostic.open_float)
    nnoremap("<leader>lw", vim.lsp.buf.workspace_symbol)
    nnoremap("<leader>lh", vim.lsp.buf.hover)
    nnoremap("<leader>lf", vim.lsp.buf.references)
    nnoremap("<leader>lr", vim.lsp.buf.rename)
    nnoremap("<C-h>", vim.lsp.buf.signature_help)
end

lspconfig["rust_analyzer"].setup {
    on_attach = on_attach,
}

lspconfig["ocamllsp"].setup {
    on_attach = on_attach,
}

lspconfig["sumneko_lua"].setup {
    cmd = { "lua-lsp" },
    on_attach = on_attach,
    settings = {
        runtime = {
            version = "LuaJIT",
        },
        diagnostics = {
            globals = { "vim" },
        },
        workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
        },
        telemetry = {
            enable = false,
        }
    }
}

local haskell_tools = require("haskell-tools")
haskell_tools.setup {
    hls = {
        on_attach = on_attach,
    },
}
