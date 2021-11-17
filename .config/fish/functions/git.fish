function git
  if test (pwd) = "$HOME"
    command git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" $argv
  else
    command git $argv
  end
end