local fzf = require("fzf-lua")

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
        fzf_opts = {
            ["--info"] = "inline",
        },
    },
    git = {
        files = {
            prompt = "File (git): ",
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

fzf.register_ui_select()

local nnoremap = require("kenran.remap").nnoremap
nnoremap("<leader>fp", fzf.git_files)
nnoremap("<leader>ff", fzf.files)
nnoremap("<leader>bb", fzf.buffers)
nnoremap("<leader>fg", fzf.live_grep_native)
nnoremap("<leader>fs", fzf.grep_cword)
nnoremap("<leader>fq", fzf.quickfix)