if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="blue"; fi

PROMPT='%{$fg[$NCOLOR]%}[%n@%m]%{$reset_color%}'
PROMPT+='  %{$fg[yellow]%}[%~]%{$reset_color%}'

if which rvm-prompt &> /dev/null; then
  PROMPT+='  %{$fg[magenta]%}[$(~/.rvm/bin/rvm-prompt)]%{$reset_color%}'
fi

PROMPT+='$(git_prompt_info) %{$fg[white]%}%{$bg[cyan]%}$(git_prompt_status)%{$reset_color%}'
PROMPT+=$'\n%# '

ZSH_THEME_GIT_PROMPT_PREFIX="  %{$fg[cyan]%}["
ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_GIT_PROMPT_ADDED="+"
ZSH_THEME_GIT_PROMPT_DELETED="-"
ZSH_THEME_GIT_PROMPT_MODIFIED="*"
ZSH_THEME_GIT_PROMPT_UNTRACKED="?"
