RPS1='$(vi_mode_prompt_info)'

PROMPT='%{$fg[red]%}%n%{$reset_color%}'
PROMPT+=' @ '
PROMPT+='%{$fg[yellow]%}%m%{$reset_color%}'
PROMPT+=' : '
PROMPT+='%{$fg[blue]%}%~%{$reset_color%}'
PROMPT+='$(git_prompt_info)$(git_prompt_status)%{$reset_color%}'

if which rvm-prompt &> /dev/null; then
  PROMPT+=' | %{$fg[green]%}$(~/.rvm/bin/rvm-prompt)%{$reset_color%}'
fi

PROMPT+=$'\n%# '

ZSH_THEME_GIT_PROMPT_PREFIX=" - %{$fg[cyan]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_GIT_PROMPT_UNTRACKED="❓"
ZSH_THEME_GIT_PROMPT_ADDED="✚"
ZSH_THEME_GIT_PROMPT_MODIFIED="✱"
ZSH_THEME_GIT_PROMPT_RENAMED="→"
ZSH_THEME_GIT_PROMPT_DELETED="❌"
ZSH_THEME_GIT_PROMPT_STASHED="💲"
ZSH_THEME_GIT_PROMPT_UNMERGED="⚠"
ZSH_THEME_GIT_PROMPT_AHEAD="↑"
ZSH_THEME_GIT_PROMPT_BEHIND="↓"
ZSH_THEME_GIT_PROMPT_DIVERGED="↕"
