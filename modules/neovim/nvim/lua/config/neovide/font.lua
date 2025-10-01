local function make_guifont(font, size)
  return font .. ":h" .. size
end

local current_font = "PragmataPro Mono"
local default_size = 17
local current_size = default_size

local function update_guifont()
  vim.o.guifont = make_guifont(current_font, current_size)
end
update_guifont()

local function set_font_size(maybe_size)
  if maybe_size then
    local size = tonumber(maybe_size)
    if size then
      current_size = size
    else
      current_size = default_size
    end
    update_guifont()
  end
end

vim.api.nvim_create_user_command("SetFontSize", function(opts)
  set_font_size(opts.args)
end, { nargs = "?" })
local fonts = {
  "Cascadia Code",
  "Consolas",
  "Courier Prime",
  "DejaVu Sans Mono",
  "Fantasque Sans Mono",
  "Fira Code",
  "Fixedsys Excelsior 3.01",
  "Iosevka Comfy",
  "Iosevka",
  "JetBrains Mono",
  "Julia Mono",
  "Lucida Console",
  "Maple Mono",
  "PragmataPro Mono",
  "Source Code Pro",
  "TX-02",
  "Ubuntu Sans Mono",
}

vim.api.nvim_create_user_command("SwitchFont", function(_)
  require("fzf-lua").fzf_exec(fonts, {
    prompt = "Font: ",
    actions = {
      ["default"] = function(selected)
        if selected and #selected > 0 then
          current_font = selected[1]
          update_guifont()
        end
      end,
    },
  })
end, {})
