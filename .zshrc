# set the oh-my-zsh location
export ZSH=$HOME/.oh-my-zsh

# set theme
ZSH_THEME="tim"

# disabled auto update for oh-my-zsh
DISABLE_AUTO_UPDATE="true"

# set default date format for oh-my-zsh
HIST_STAMPS="dd.mm.yyyy"

# set PATH variables
export PATH="$PATH:$HOME/.rvm/bin"

# enable oh-my-zsh plugins
plugins=(autojump bower mvn npm rvm sudo vagrant web-search)

# start oh-my-zsh
source $ZSH/oh-my-zsh.sh
