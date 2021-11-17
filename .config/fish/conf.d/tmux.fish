if status is-interactive; and test (command -s tmux); and not set -q TMUX
    tmux attach -t base || tmux new -s base
end
