function navigate_to_project \
    --description 'Navigate to one of the known project directories'
    set target (select_project.pl)
    if test $status -eq 0
        cd "$target"
    end
    commandline -f repaint
end
