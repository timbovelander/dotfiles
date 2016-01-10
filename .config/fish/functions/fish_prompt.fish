function fish_prompt --description 'Write out the prompt'
  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end

  # User
  set_color red
  printf "[$USER]"

  # Hostname
  if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
  end
  set_color normal
  printf "  "
  set_color --bold magenta
  printf "[$__fish_prompt_hostname]"

  # PWD
  set_color normal
  printf "  "
  set_color $fish_color_cwd
  printf "[%s]" (prompt_pwd)

  # GIT
  if __fish_git_prompt >/dev/null ^&1
    set_color normal
    printf "  %s" (__fish_git_prompt "[%s]")
  end

  # rbenv
  if command -s rbenv >/dev/null ^&1
    set_color normal
    printf "  "
    set_color green
    printf "[%s" (rbenv version | grep -Po "^[^\s]+")
    if rbenv gemset active >/dev/null ^&1
      printf " %s" (rbenv gemset active | grep -Po "^[^\s]+")
    end
    printf "]"
  end

  # VI mode
  switch "$fish_key_bindings"
  case '*_vi_*' '*_vi'
    set_color normal
    echo -n '  '
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
