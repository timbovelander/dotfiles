nnoremap <silent> <leader>/ :call ReplaceInBuffer()<CR>

function! ReplaceInBuffer()
  call inputsave()
  let search = input('Search: ')
  let replace = input('Replace with: ')
  call inputrestore()

  execute "%s/" . search . "/" . replace . "/gc"
endfunction
