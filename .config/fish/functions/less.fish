function less
  if test -f /usr/share/vim/vim82/macros/less.sh
    /usr/share/vim/vim82/macros/less.sh $argv
  else if test -f /usr/share/vim/vim74/macros/less.sh
    /usr/share/vim/vim74/macros/less.sh $argv
  else
    command less $argv
  end
end
