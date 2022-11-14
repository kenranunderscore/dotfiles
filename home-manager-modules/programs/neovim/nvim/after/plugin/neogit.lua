local neogit = require("neogit")
neogit.setup {
    disable_signs = false,
    disable_hints = false,
    disable_context_highlighting = false,
    disable_commit_confirmation = true,
}

local nnoremap = require("kenran.remap").nnoremap

nnoremap("<leader>gs", function() neogit.open({}) end)
