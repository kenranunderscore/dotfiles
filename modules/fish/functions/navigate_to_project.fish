function navigate_to_project \
    --description 'Navigate to one of the known project directories'
    cd (select_project.pl)
    commandline -f repaint
end
