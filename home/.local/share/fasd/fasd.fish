function __fasd_init -e fish_preexec
  command fasd --proc (command fasd --sanitize $argv) > "/dev/null" 2>&1 &
end

function __fasd_print_completions
  set cmd (commandline -op)
  if test (count $cmd) -ge 2
    command fasd -l $argv $cmd[2..-1]
  else
    command fasd -l $argv
  end
end

function fasd_cd
  if test (count $argv) -le 1
    command fasd $argv
  else
    set -l ret (command fasd -e 'printf %s' $argv)
    test -z "$ret"; and return
    if test -d "$ret"
      set_color green; echo "$ret"; set_color normal
      cd "$ret"
    else
      set_color red; echo "$ret"; set_color normal
    end
  end
end

function a
  command fasd -a $argv
end

function d
  command fasd -d $argv
end

function f
  command fasd -f $argv
end

function s
  command fasd -si $argv
end

function sd
  command fasd -si $argv
end

function sf
  command fasd -si $argv
end

function j
  fasd_cd -d $argv
end

function ea
  command fasd $argv -e "emacsclient -c -a emacs"
end

function ed
  command fasd -d $argv -e "emacsclient -c -a emacs"
end

function ef
  command fasd -f $argv -e "emacsclient -c -a emacs"
end

complete -c fasd -a "(__fasd_print_completions)" -f
complete -c fasd_cd -a "(__fasd_print_completions -d)" -f -A
complete -c a -a "(__fasd_print_completions -a)" -f -A
complete -c d -a "(__fasd_print_completions -d)" -f -A
complete -c f -a "(__fasd_print_completions -f)" -f -A
complete -c s -a "(__fasd_print_completions)" -f -A
complete -c sd -a "(__fasd_print_completions -d)" -f -A
complete -c sf -a "(__fasd_print_completions -f)" -f -A
complete -c j -a "(__fasd_print_completions -d)" -f -A
complete -c ea -a "(__fasd_print_completions)" -f -A
complete -c ed -a "(__fasd_print_completions -d)" -f -A
complete -c ef -a "(__fasd_print_completions -f)" -f -A
