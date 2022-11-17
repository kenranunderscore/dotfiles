local neogit = require("neogit")
neogit.setup {
    disable_signs = false,
    disable_hints = false,
    disable_context_highlighting = false,
    disable_commit_confirmation = true,
    disable_builtin_notifications = false,
    sections = {
        stashes = false,
        recent = false,
    },
    signs = {
        section = { "", "" },
        item = { "", "" },
        hunk = { "", "" },
    },
}

local nnoremap = require("kenran.remap").nnoremap

nnoremap("<leader>gs", neogit.open)
