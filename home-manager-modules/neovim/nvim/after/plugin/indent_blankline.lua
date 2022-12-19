local indent_blankline_setup, indent_blankline = pcall(require, "indent_blankline")
if not indent_blankline_setup then
    print("Package 'indent_blankline' failed to load")
    return
end

indent_blankline.setup {
    show_current_context = false,
    show_current_context_start = false,
}
