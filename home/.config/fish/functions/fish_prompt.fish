function fish_prompt --description 'Write out the prompt'
  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end

  # User (not logged in or remote)
  if not contains (whoami) (who -m | cut -d ' ' -f1)
    set_color red
    printf "[$USER]"
    set_color normal
    printf "  "
  else if begin set -q SSH_CLIENT; or set -q SSH_TTY; end
    set_color red
    printf "[$USER]"
    set_color normal
    printf "  "
  end

  # Hostname (remote only)
  if begin set -q SSH_CLIENT; or set -q SSH_TTY; end
    if not set -q __fish_prompt_hostname
      set -g __fish_prompt_hostname (hostname | cut -d . -f 1)
    end
    set_color --bold magenta
    printf "[$__fish_prompt_hostname]"
    set_color normal
    printf "  "
  end

  # PWD
  set_color $fish_color_cwd
  printf "[%s]" (prompt_pwd)
  set_color normal
  printf "  "

  # GIT
  if __fish_git_prompt >/dev/null ^&1
    printf "%s" (__fish_git_prompt "[%s]")
    set_color normal
    printf "  "
  end

  # versions
  if begin command -s node >/dev/null ^&1
      or command -s rbenv >/dev/null ^&1
    end
    set_color green
    printf "["

    # node
    if command -s node >/dev/null ^&1
      printf "N:%s" (node -v | grep -Po "[\d.]+")
    end

    if begin command -s node >/dev/null ^&1
      and command -s rbenv >/dev/null ^&1
      end
      printf " "
    end

    # rbenv
    if command -s rbenv >/dev/null ^&1
      printf "R:%s" (rbenv version | grep -Po "^[^\s]+")
      if rbenv gemset active >/dev/null ^&1
        printf "-%s" (rbenv gemset active | grep -Po "^[^\s]+")
      end
    end

    printf "]"
    set_color normal
    printf "  "
  end

  printf "\n"

  # VI mode
  switch "$fish_key_bindings"
  case '*_vi_*' '*_vi'
    switch $fish_bind_mode
    case default
      set_color --bold white -b yellow
      echo -n ' NORMAL '
      set_color normal
      echo -n '  '
    case visual
      set_color --bold white -b magenta
      echo -n ' VISUAL '
      set_color normal
      echo -n '  '
    end
  end

  # Suffix
  set_color --bold white
  switch $USER
  case root toor
    printf "#"
  case '*'
    printf "\$"
  end

  set_color normal
  echo -n ' '
end
