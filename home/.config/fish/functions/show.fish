function show
  if command -s pygmentize >/dev/null ^&1
    pygmentize $argv ^/dev/null; or cat $argv
  else
    cat $argv
  end
end
