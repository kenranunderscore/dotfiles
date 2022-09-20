# The function assumes I'm always passing the repo URL as first
# argument.  If there's a target directory as well (that is, there's
# at least two arguments to `git clone`) then it has to be the last
# argument to the call, and not start with a double dash.

function gclone --description 'Clone a git repository and cd into it'
    git clone $argv

    if [ (count $argv) -gt 1 ] && ! [ (string match -- '--*' $argv[-1]) ]
        cd $argv[-1]
    else
        cd (basename $argv[1] '.git')
    end
end
