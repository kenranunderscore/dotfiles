function fish_should_add_to_history \
    --description 'Decide whether a term should be added to the history'
    for cmd in fg bg
        string match -qr "^$cmd\$" -- $argv; and return 1
    end
    return 0
end
