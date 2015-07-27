# find directories
alias fd="find -type d -name"

# find files
alias ff="find -type f -name"

# alias for git
alias g="git"

# alias less to vimless if available
if [ -f /usr/share/vim/vim74/macros/less.sh ]; then
  alias less="/usr/share/vim/vim74/macros/less.sh"
fi

# show file with pygmentize or cat
if command -v pygmentize &>/dev/null; then
  alias show="pygmentize -g"
else
  alias show="cat"
fi
