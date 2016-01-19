# disable greeting message
set fish_greeting

# use vi key bindings
set fish_key_bindings fish_vi_key_bindings

# colors
set fish_color_normal normal
set fish_color_command green
set fish_color_quote cyan
set fish_color_redirection yellow
set fish_color_end normal
set fish_color_error red
set fish_color_param blue
set fish_color_comment green --bold
set fish_color_match cyan --bold
set fish_color_search_match --background=black
set fish_color_operator red --bold
set fish_color_escape normal
set fish_color_selection --background=black
set fish_color_cwd blue
set fish_color_autosuggestion green --bold
set fish_pager_color_prefix white
set fish_pager_color_completion normal
set fish_pager_color_description yellow --bold
set fish_pager_color_progress yellow

# git_prompt
set __fish_git_prompt_show_informative_status true
set __fish_git_prompt_char_upstream_ahead ' ↑'
set __fish_git_prompt_char_upstream_behind ' ↓'
set __fish_git_prompt_char_stateseparator ''
set __fish_git_prompt_char_cleanstate ''
set __fish_git_prompt_char_dirtystate ' *'
set __fish_git_prompt_char_stagedstate ' +'
set __fish_git_prompt_char_invalidstate ' #'
set __fish_git_prompt_char_stashstate ' $'
set __fish_git_prompt_char_untrackedfiles ' ?'
set __fish_git_prompt_color cyan

# environment variables
set -x TERM "xterm-256color"
set -x EDITOR "vim"
set -x VISUAL "vim"

# autojump
if test -e /usr/share/autojump/autojump.fish
  source /usr/share/autojump/autojump.fish
end

# rbenv
if test -d "$HOME/.rbenv"
  set PATH "$HOME/.rbenv/bin" $PATH
  status --is-interactive; and . (rbenv init -|psub)
end

# aliases
alias fd "find -type d -name"
alias ff "find -type f -name"
alias g "git"
alias l "ls -la"
alias search "ag --pager 'less'"