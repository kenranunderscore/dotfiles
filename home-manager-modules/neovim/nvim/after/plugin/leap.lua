local leap_setup, leap = pcall(require, "leap")
if not leap_setup then
    print("Package 'leap' failed to load")
    return
end

leap.add_default_mappings()
