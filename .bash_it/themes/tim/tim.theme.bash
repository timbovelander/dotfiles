#!/usr/bin/env bash
SCM_THEME_PROMPT_DIRTY=""
SCM_THEME_PROMPT_CLEAN=""
SCM_THEME_PROMPT_PREFIX="${normal} - ${cyan}"
SCM_THEME_PROMPT_SUFFIX=""

SCM_GIT_UNSTAGED_CHAR="!:"
SCM_GIT_STAGED_CHAR="$:"

SCM_NONE_CHAR="\$"

RVM_THEME_PROMPT_PREFIX="${normal} | ${green}"
RVM_THEME_PROMPT_SUFFIX=""

function prompt_command() {
    #PS1="${bold_cyan}$(scm_char)${green}$(scm_prompt_info)${purple}$(ruby_version_prompt) ${yellow}\h ${reset_color}in ${green}\w ${reset_color}\n${green}→${reset_color} "
    #PS1="\n${yellow}$(ruby_version_prompt) ${purple}\h ${reset_color}in ${green}\w\n${bold_cyan}$(scm_char)${green}$(scm_prompt_info) ${green}→${reset_color} "
  PS1="${red}\u"
  PS1+="${normal} @ "
  PS1+="$(color reset magenta fg bright)\h"
  PS1+="${normal} : "
  PS1+="${blue}\w"
  PS1+="$(scm_prompt_info)"
  PS1+="$(ruby_version_prompt)"
  PS1+="${normal}\n$(scm_char) "
}

PROMPT_COMMAND=prompt_command;
