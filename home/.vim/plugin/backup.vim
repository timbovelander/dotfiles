if !empty(glob("$HOME/.vim/backup"))
  set backup
  set backupdir=$HOME/.vim/backup

  augroup backup
    " change backup file extension, add timestamp
    autocmd BufWritePre * let &backupext = "-" . strftime("%s") . ".vimbackup"
    " remove old backup files on write file
    autocmd BufWritePost * call RemoveOldBackupFiles()
  augroup END

  function! RemoveOldBackupFiles()
    let filename = expand('%:t')
    let find = "find $HOME/.vim/backup -name " . filename . "\\*.vimbackup"
    let sort = "sort -n"
    let filter = "head -n -10"
    let remove = "xargs rm -f --"

    execute "silent !" . find . " | " . sort . " | " . filter . " | " . remove
  endfunction
endif
