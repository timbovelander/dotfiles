" ===== Appearance
set linespace=6
set guifont=Droid\ Sans\ Mono\ 11
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

" ===== Zoom
nnoremap <silent> <leader>0 :call AdjustFontSize(0)<CR>
nnoremap <silent> <leader>- :call AdjustFontSize(-2)<CR>
nnoremap <silent> <leader>= :call AdjustFontSize(2)<CR>

function! AdjustFontSize(amount)
  let defaultfontsize = 11
  let minfontsize = 9
  let maxfontsize = 19
  let fontname = substitute(&guifont, '^\(.* \)\([1-9][0-9]*\)$', '\1', '')
  let cursize = substitute(&guifont, '^\(.* \)\([1-9][0-9]*\)$', '\2', '')

  if a:amount == 0
    let newsize = defaultfontsize
  else
    let newsize = cursize + a:amount
  endif

  if newsize < minfontsize
    let newsize = minfontsize
  elseif newsize > maxfontsize
    let newsize = maxfontsize
  endif

  let newfont = fontname . newsize
  let &guifont = newfont
endfunction
