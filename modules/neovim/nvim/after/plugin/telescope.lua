local telescope_setup, telescope = pcall(require, "telescope")
if not telescope_setup then
    print("Package 'telescope' failed to load")
    return
end

local borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }

telescope.setup {
    defaults = {
        border = true,
        borderchars = borderchars,
        layout_strategy = "horizontal",
    },
    pickers = {
        find_files = { previewer = false, layout_config = { height = 0.3 } },
        git_files = { previewer = false, layout_config = { height = 0.3 } },
        buffers = { previewer = false, layout_config = { height = 0.3 } },
        live_grep = { layout_config = { height = 0.9, width = 0.95 } },
        grep_string = { layout_config = { height = 0.9, width = 0.95 } },
    },
    extensions = {
        fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
        },
        ["ui-select"] = {
            require("telescope.themes").get_cursor {
                borderchars = borderchars,
            },
        },
    },
}

telescope.load_extension("fzf")
telescope.load_extension("ui-select")

local function git_files_with_fallback()
    vim.fn.system("git rev-parse --is-inside-work-tree")
    if vim.v.shell_error == 0 then
        require("telescope.builtin").git_files {}
    else
        require("telescope.builtin").find_files {}
    end
end

vim.keymap.set("n", "<leader>fp", git_files_with_fallback)
vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>")
vim.keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep<cr>")
vim.keymap.set("n", "<leader>fs", "<cmd>Telescope grep_string<cr>")
vim.keymap.set("n", "<leader>bb", "<cmd>Telescope buffers<cr>")
vim.keymap.set("n", "<leader>fq", "<cmd>Telescope quickfix<cr>")
