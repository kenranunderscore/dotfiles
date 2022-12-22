local impatient_setup, _ = pcall(require, "impatient")
if not impatient_setup then
    print("Package 'impatient' failed to load")
end

require("kenran.settings")
require("kenran.plugins")
require("kenran.fennel_setup")
