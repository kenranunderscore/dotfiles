function groot --description 'Jump to the root directory of a git project'
    set root $(git rev-parse --show-toplevel 2>/dev/null)
    # TODO: search up the directory tree to find parent repos?

    if test -n "$root"
        cd "$root"
    end
end
