function mc --description 'Create directory and cd into it'
    command mkdir -p $argv

    if test $status = 0
        cd $argv[(count $argv)]
    end
end
