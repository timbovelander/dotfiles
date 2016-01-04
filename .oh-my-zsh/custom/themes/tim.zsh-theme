PROMPT='%{$fg[red]%}%n%{$reset_color%}'
PROMPT+=' @ '
PROMPT+='%{$fg[yellow]%}%m%{$reset_color%}'
PROMPT+=' : '
PROMPT+='%{$fg[blue]%}%~%{$reset_color%}'
PROMPT+='$(git_prompt_info)$(git_prompt_status)%{$reset_color%}'

if which rvm-prompt &> /dev/null; then
  PROMPT+=' | %{$fg[green]%}$(~/.rvm/bin/rvm-prompt)%{$reset_color%}'
fi

PROMPT+=$'\n$(vi_mode_prompt_info) %# '

ZSH_THEME_GIT_PROMPT_PREFIX=" - %{$fg[cyan]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_GIT_PROMPT_UNTRACKED=" ❓"
ZSH_THEME_GIT_PROMPT_ADDED=" ✚"
ZSH_THEME_GIT_PROMPT_MODIFIED=" ✱"
ZSH_THEME_GIT_PROMPT_RENAMED=" →"
ZSH_THEME_GIT_PROMPT_DELETED=" ❌"
ZSH_THEME_GIT_PROMPT_STASHED=" 💲"
ZSH_THEME_GIT_PROMPT_UNMERGED=" ⚠"
ZSH_THEME_GIT_PROMPT_AHEAD=" ↑"
ZSH_THEME_GIT_PROMPT_BEHIND=" ↓"
ZSH_THEME_GIT_PROMPT_DIVERGED=" ↕"

ZSH_THEME_VI_MODE_NORMAL="%{$bg[yellow]$fg_bold[white]%} NORMAL %{$reset_color%}"
ZSH_THEME_VI_MODE_INSERT="%{$bg[black]$fg_bold[blue]%} INSERT %{$reset_color%}"
