function less
  if test -f /usr/share/vim/vim74/macros/less.sh
    /usr/share/vim/vim74/macros/less.sh $argv
  else
    /usr/bin/less $argv
  end
end
