vim.g.neovide_cursor_vfx_mode = "railgun"
vim.g.neovide_cursor_vfx_particle_lifetime = 1.0
vim.g.neovide_cursor_vfx_particle_density = 1.0

local function make_guifont(font, size)
  return font .. ":h" .. size
end

local current_font = "PragmataPro Mono"

vim.o.guifont = make_guifont(current_font, 17)

local function set_font_size(maybe_size)
  if maybe_size then
    local size = tonumber(maybe_size)
    if size then
      vim.opt.guifont = make_guifont(current_font, size)
    else
      vim.notify("Invalid size", vim.log.levels.ERROR, {})
    end
  end
end

vim.api.nvim_create_user_command("SetFontSize", function(opts)
  set_font_size(opts.args)
end, { nargs = 1 })
