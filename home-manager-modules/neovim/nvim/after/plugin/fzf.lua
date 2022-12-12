local fzf = require("fzf-lua")

local small_win = {
    height = 0.30,
    row = 0.5,
}

fzf.setup {
    winopts = {
        border = "single",
        preview = {
            layout = "vertical",
            delay = 60,
            scrollbar = false,
        },
    },
    fzf_opts = {
        ["--info"] = "hidden",
        ["--select-1"] = false,
    },
    files = {
        prompt = "File: ",
        previewer = false,
        winopts = small_win,
        fzf_opts = {
            ["--info"] = "inline",
        },
    },
    git = {
        files = {
            prompt = "File (git): ",
            winopts = small_win,
            previewer = false,
            fzf_opts = {
                ["--info"] = "inline",
            },
        },
    },
    grep = {
        prompt = "rg: ",
        input_prompt = "Search string: ",
        -- this is needed for live_grep* to work
        exec_empty_query = true,
    },
    buffers = {
        prompt = "Buffer: ",
        previewer = false,
        winopts = small_win,
    },
    oldfiles = {
        prompt = "Oldfile: ",
    },
    colorschemes = {
        prompt = "Colorscheme: ",
        live_preview = true,
        winopts = {
            height = 0.5,
            width = 0.25,
        },
    },
    lsp = {
        prompt_postfix = ": ",
    },
}

vim.keymap.set("n", "<leader>fp", fzf.git_files)
vim.keymap.set("n", "<leader>ff", fzf.files)
vim.keymap.set("n", "<leader>b", fzf.buffers)
vim.keymap.set("n", "<leader>fg", fzf.live_grep_native)
vim.keymap.set("n", "<leader>fs", fzf.grep_cword)
vim.keymap.set("n", "<leader>fq", fzf.quickfix)
