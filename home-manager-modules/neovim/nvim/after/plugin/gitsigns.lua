local gitsigns_setup, gitsigns = pcall(require, "gitsigns")
if not gitsigns_setup then
    print("Package 'gitsigns' failed to load")
    return
end

gitsigns.setup()
