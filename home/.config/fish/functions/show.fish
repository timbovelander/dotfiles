function show
    command -s pygmentize >/dev/null ^&1
    and pygmentize $argv ^/dev/null
    or command -s highlight >/dev/null ^&1
    and highlight -O ansi $argv ^/dev/null
    or cat $argv
end
