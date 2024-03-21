local indent_blankline_setup, indent_blankline = pcall(require, "ibl")
if not indent_blankline_setup then
    print("Package 'indent_blankline' failed to load")
    return
end

indent_blankline.setup {}
