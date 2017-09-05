function fish_prompt --description 'Write out the prompt'
  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end

  # User (not logged in or remote)
  set -l who (who -m | cut -d ' ' -f1)
  set -l whoami (whoami)
  if begin test -n "$who" -a -n "$whoami"; and not contains "$whoami" "$who"; end
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

  # maven project
  if begin command -s xmllint >/dev/null ^&1; and test -f "pom.xml"; end
    set -l version (xmllint --xpath '/*[local-name()="project"]/*[local-name()="version"]/text()' pom.xml ^/dev/null)
    if test $status -eq 0
      set_color yellow
      printf "[%s]" (echo $version)
      set_color normal
      printf "  "
    end
  end

  # node project
  if begin command -s npm >/dev/null ^&1; and command -s ramda >/dev/null ^&1; end
    set -l packageJson (npm root | sed s/node_modules/package.json/)
    if test -f $packageJson
      set_color green
      printf "[%s]" (cat $packageJson | ramda -o raw "prop \version")
      set_color normal
      printf "  "
    end
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
