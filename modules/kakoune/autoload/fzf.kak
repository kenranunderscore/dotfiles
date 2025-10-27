define-command fzf -shell-script-completion %{printf "%s\n" -input-cmd -exec -fzf-extra-args} -params .. %{
    evaluate-commands %sh{
        while [ $# -gt 0 ]; do
            case "$1" in
                (-input-cmd)
                    shift
                    input_cmd="$1"
                    ;;
                (-exec)
                    shift
                    exec_cmd="$1"
                    ;;
                (-fzf-extra-args)
                    shift
                    fzf_extra_args="$1"
                    ;;
            esac
            shift
        done

        fzf_cmd="fzf --exact --height 100% $fzf_extra_args"
        result="/tmp/kak_fzf_${kak_session}_result"
        pid="/tmp/kak_fzf_${kak_session}.pid"
        touch "$pid"
        cmd="export FZF_DEFAULT_OPTS=\"\${FZF_DEFAULT_OPTS/--select-1}\"; echo \$\$ >$pid; $input_cmd | $fzf_cmd >$result; rm $pid"
        echo "$cmd">/tmp/debug
        printf "tmuxv bash -c '%s'\n" "$cmd"

        ( while [ -f "$pid" ]; do sleep 0.05; done
          if [ -s "$result" ]; then
              res=$(cat "$result")
              echo "evaluate-commands -client $kak_client %{ $exec_cmd '$res' }" | kak -p "$kak_session"
          fi
          rm -f "$result" "$pid"
        ) >/dev/null 2>&1 </dev/null &
    }
}

define-command fzf-project-file %{
    fzf -input-cmd "fd --type f 2>/dev/null" -exec "edit"
}

define-command fzf-switch-buffer %{
    evaluate-commands %sh{
        eval "set -- ${kak_quoted_buflist:?}"
        buffers="$kak_bufname"
        for buf in "$@"; do
            [ "$buf" = "$kak_bufname" ] && continue
            buffers="${buffers}\n$buf"
        done
        echo "fzf -input-cmd 'echo \"$buffers\"' -exec buffer -fzf-extra-args \"--header-lines 1\""
    }
}
