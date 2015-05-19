" ===== Plugins
" required for Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

" start Vundle
call vundle#begin()

" Plugins
Plugin 'gmarik/vundle.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/html5.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'mattn/emmet-vim'
Plugin 'valloric/youcompleteme'
Plugin 'marijnh/tern_for_vim'
Plugin 'raimondi/delimitmate'

" end Vundle
call vundle#end()

" required for Vundle
filetype plugin indent on


" ===== Global
" ignore case on file & dir completion
set wildignorecase

" always show status line
set laststatus=2


" ===== Theme
" enable syntax highlighting
syntax enable

" set color scheme
colorscheme solarized
set background=light

" set font
if has('gui_running')
  set guifont=Droid\ Sans\ Mono\ 10
endif


" ===== UI
" enable line number
set number

" show command in bottom bar
set showcmd

" highlight current line
set cursorline

" do not show gui toolbar and scroll bars
set guioptions-=T
set guioptions-=r
set guioptions-=L


" ===== Custom keybindings
" insert a newline with enter & shift-enter
nnoremap <CR> o<Esc>
nnoremap <S-Enter> O<Esc>

" navigate buffers using ctrl-tab
nnoremap <C-Tab> :bn<CR>


" ===== Plugin: Airline
" set seperators
let g:airline_left_sep=' '
let g:airline_right_sep=' '

" enable tab line extension
let g:airline#extensions#tabline#enabled = 1


" ===== Plugin: YouCompleteMe
" create custom semantic triggers to show autocomplete
let g:ycm_semantic_triggers =  {
  \  'css,scss': ['re!^\s*', 're!:\s*'],
  \  'html': ['<', '</', 're!<.*\s'],
  \}


" ===== Plugin: DelimitMate
" on enter, place cursor in the middle of brackets
let delimitMate_expand_cr = 1


" ===== Filetype settings
" create an augroup so autocmds are only applied once
augroup vimrc

  " clear all previous autocmd's
  autocmd!

  " enable omnifunc autocomplete
  autocmd FileType css set omnifunc=csscomplete#CompleteCSS
  autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
  autocmd Filetype javascript set omnifunc=tern#Complete

  " enable emmet-completion on TAB
  autocmd FileType css,html,html.mustache imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

  " indentation fix for html
  autocmd FileType html inoremap <expr> <CR> ExpandHtmlTag()

  " set aliases
  autocmd BufRead,BufNewFile *.scss set filetype=scss.css
  autocmd BufRead,BufNewFile *.mustache set filetype=html.mustache

augroup END


" ===== Functions
" expand html tag
function! ExpandHtmlTag()
  let line   = getline(".")
  let col    = col(".")
  let first  = line[col-2]
  let second = line[col-1]
  let third  = line[col]

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
