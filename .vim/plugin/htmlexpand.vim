augroup extendhtmltag
  autocmd FileType html,html.handlebars,jsp,php inoremap <expr> <CR> ExpandHtmlTag()
augroup END

function! ExpandHtmlTag()
  let line = getline(".")
  let col = col(".")
  let first = line[col-2]
  let second = line[col-1]
  let third = line[col]

  if first ==# ">"
    if second ==# "<" && third ==# "/"
      return "\<CR>\<C-o>==\<C-o>O"
    else
      return "\<CR>"
    endif
  else
    return "\<CR>"
  endif
endfunction
