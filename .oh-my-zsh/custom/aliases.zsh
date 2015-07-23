# alias cat to pygmentize if available
if command -v pygmentize >/dev/null 2>&1; then
  alias cat="pygmentize -g"
fi

# alias less to vimless if available
if [ -f /usr/share/vim/vim74/macros/less.sh ]; then
  alias less="/usr/share/vim/vim74/macros/less.sh"
fi
