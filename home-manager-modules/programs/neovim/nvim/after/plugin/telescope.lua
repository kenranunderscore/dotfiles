local telescope = require("telescope")

telescope.setup {
    pickers = { 
        git_files = {
            theme = "dropdown",
        },
        find_files = {
            theme = "dropdown",
        },
        buffers = {
            theme = "dropdown",
        },
        man_pages = {
            theme = "dropdown",
        },
        live_grep = {
            theme = "dropdown",
        },
    },
    extensions = {
        ["ui-select"] = {
            require("telescope.themes").get_cursor {}
        },
        fzf = {
            fuzzy = false,
        },
    },
}

local nnoremap = require("kenran.remap").nnoremap

project_files = function()
    local opts = {}
    vim.fn.system("git rev-parse --is-inside-work-tree")
    if vim.v.shell_error == 0 then
        require("telescope.builtin").git_files(opts)
    else
        require("telescope.builtin").find_files(opts)
    end
end

-- Things I use often; trying out a key scheme different from what I'm used to
-- in Emacs
nnoremap("<leader>fp", project_files)
nnoremap("<leader>ff", function()
    require("telescope.builtin").find_files()
end)
nnoremap("<leader>bb", function()
    require("telescope.builtin").buffers()
end)
nnoremap("<leader>fg", function()
    require("telescope.builtin").live_grep()
end)
nnoremap("<leader>fs", function()
    require("telescope.builtin").grep_string()
end)
nnoremap("<leader>fq", function()
    require("telescope.builtin").quickfix()
end)

-- Misc
nnoremap("<leader>hm", function()
    require("telescope.builtin").man_pages()
end)

telescope.load_extension("ui-select")
telescope.load_extension("fzf")
