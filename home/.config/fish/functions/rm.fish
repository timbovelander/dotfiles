function rm
  if command -s trash-put >/dev/null ^&1
    trash-put $argv
  else if command -s trash >/dev/null ^&1
    trash $argv
  else
    /bin/rm $argv
  end
end
