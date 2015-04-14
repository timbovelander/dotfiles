# Pygmentize a file
function ccat() {
  printf '\n\n'
  pygmentize "$1"
  printf '\n'
}

# Create directory and cd into it
function md() {
  mkdir -p "$1"
  cd "$_"
}

# Open with nautilus
function o() {
  if (($# == 0))
    then
      nautilus .
    else
      nautilus "$1"
  fi
}

# Open with sublime text
function s() {
  if (($# == 0))
    then
      subl .
    else
      subl "$1"
  fi
}
