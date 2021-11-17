function rm
  if command -s trash-put >/dev/null 2>&1
    command trash-put $argv
  else if command -s trash >/dev/null 2>&1
    command trash $argv
  else
    command rm $argv
  end
end
