local setup, neogit = pcall(require, "neogit")
if not setup then
    print("neogit setup failed")
    return
end

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

vim.keymap.set("n", "<leader>gs", neogit.open)