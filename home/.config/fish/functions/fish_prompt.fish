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
  else if set -q SSH_CLIENT | set -q SSH_TTY
    set_color red
    printf "[$USER]"
    set_color normal
    printf "  "
  end

  # Hostname (remote only)
  if set -q SSH_CLIENT | set -q SSH_TTY
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

  # rbenv
  if command -s rbenv >/dev/null ^&1
    set_color green
    printf "[%s" (rbenv version | grep -Po "^[^\s]+")
    if rbenv gemset active >/dev/null ^&1
      printf " %s" (rbenv gemset active | grep -Po "^[^\s]+")
    end
    printf "]"
    set_color normal
    printf "  "
  end

  # VI mode
  switch "$fish_key_bindings"
  case '*_vi_*' '*_vi'
    switch $fish_bind_mode
    case default
      set_color --bold red
      echo -n '[normal]'
    case insert
      set_color yellow
      echo -n '[insert]'
    case visual
      set_color magenta
      echo -n '[visual]'
    end
    set_color normal
    echo -n '  '
  end

  # Suffix
  set_color white
  switch $USER
  case root toor
    printf "\n#"
  case '*'
    printf "\n\$"
  end

  set_color normal
  echo -n ' '
end
