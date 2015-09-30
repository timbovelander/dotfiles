# use vi-mode in zsh
bindkey -v

# kill lag when changing mode
export KEYTIMEOUT=1

# redraw prompt when vi-mode changes
function zle-line-init zle-keymap-select {
  zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# define normal mode indicator
if [[ "$ZSH_THEME_VI_MODE_NORMAL" == "" ]]; then
  ZSH_THEME_VI_MODE_NORMAL="%{$bg[magenta]$fg_bold[white]%} NORMAL %{$reset_color%}"
fi

# define insert mode indicator
if [[ "$ZSH_THEME_VI_MODE_INSERT" == "" ]]; then
  ZSH_THEME_VI_MODE_INSERT="%{$bg[yellow]$fg_bold[white]%} INSERT %{$reset_color%}"
fi

# define function to call in theme
function vi_mode_prompt_info() {
  VI_MODE="${KEYMAP/vicmd/$ZSH_THEME_VI_MODE_NORMAL}"
  VI_MODE="${VI_MODE/(main|viins)/$ZSH_THEME_VI_MODE_INSERT}"
  echo $VI_MODE
}
