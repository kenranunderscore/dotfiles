-- Add the path to my user installation of Fennel to the Lua path
local nix_lua_share = vim.fn.expand("$HOME/.nix-profile/share/lua/5.2/?.lua")
package.path = package.path .. ";" .. nix_lua_share

-- Moonwalk allows me to write Fennel wherever I could write Lua or Vimscript
local moonwalk_setup, moonwalk = pcall(require, "moonwalk")
if not moonwalk_setup then
    print("Package 'moonwalk' failed to load")
    return
end

-- Add loader for Fennel -> Lua compilation
moonwalk.add_loader("fnl", function(src)
    local fennel_setup, fennel = pcall(require, "fennel")
    if not fennel_setup then
        print("Cannot load fennel.lua")
        return
    end
    return fennel.compileString(src)
end)
