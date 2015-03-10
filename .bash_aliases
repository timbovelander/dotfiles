# Easier navigation: .., ..., ...., .....
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# Shortcuts
alias dl="cd ~/Downloads"

# List files
alias l="ls -lF --color"
alias la="ls -laF --color"
alias lsd="ls -lF --color | grep --color=never '^d'"
alias ls="command ls --color"

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Get week number
alias week='date +%W'

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"

# Trim new lines and copy to clipboard
alias c="tr -d '\n' | xclip -selection c"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
	alias "$method"="lwp-request -m '$method'"
done

# Make Grunt print stack traces by default
command -v grunt > /dev/null && alias grunt="grunt --stack"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"
