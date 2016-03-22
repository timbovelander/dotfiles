function fasd_init -e fish_preexec
  fasd --proc (fasd --sanitize $argv | sed "s/\s/\n/g") > /dev/null 2>&1
end

function ea
  set -l any (fasd -a -e "printf %s" "$argv")
  set_color blue
  echo "$any"
  set_color normal
  eval e "$any"
end

function ed
  set -l dir (fasd -d -e "printf %s" "$argv")
  set_color blue
  echo "$dir"
  set_color normal
  eval e "$dir"
end

function ef
  set -l file (fasd -f -e "printf %s" "$argv")
  set_color blue
  echo "$file"
  set_color normal
  eval e "$file"
end

function j
  set -l dir (fasd -d -e "printf %s" "$argv")
  set_color blue
  echo "$dir"
  set_color normal
  eval cd "$dir"
end
