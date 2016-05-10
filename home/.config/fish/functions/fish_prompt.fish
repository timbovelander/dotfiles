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

  # nodejs
  if command -s node >/dev/null ^&1
    set_color green
    printf "[%s]" (node -v | grep -Po "[\d.]+")
    set_color normal
    printf "  "
  end

  # rbenv
  if command -s rbenv >/dev/null ^&1
    set_color yellow
    printf "[%s" (rbenv version | grep -Po "^[^\s]+")
    if rbenv gemset active >/dev/null ^&1
      printf " %s" (rbenv gemset active | grep -Po "^[^\s]+")
    end
    printf "]"
    set_color normal
    printf "  "
  end

  printf "\n"

  # VI mode and suffix
  set_color --bold white
  switch "$fish_key_bindings"
  case '*_vi_*' '*_vi'
    switch $fish_bind_mode
    case default
      printf "N"
    case visual
      printf "V"
    case '*'
      switch $USER
      case root toor
        printf "#"
      case '*'
        printf "\$"
      end
    end
  end
  set_color normal
  printf " "
end
