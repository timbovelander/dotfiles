# find directories
function fd {
  find -type d -name "*$1*"
}

# find files
function ff {
  find -type f -name "*$1*"
}

# alias less to vimless if available
if [ -f /usr/share/vim/vim74/macros/less.sh ]; then
  alias less="/usr/share/vim/vim74/macros/less.sh"
fi

# show file with pygmentize or cat
function show {
  echo ''
  if command -v pygmentize >/dev/null 2>&1; then
    pygmentize -g "$1"
  else
    cat "$1"
  fi
  echo ''
}
