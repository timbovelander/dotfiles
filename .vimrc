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
Plugin 'justinmk/vim-gtfo'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/html5.vim'
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


" ===== Custom keybindings
" insert a newline with enter & shift-enter
nmap <CR> o<Esc>
nmap <S-Enter> O<Esc>


" ===== Plugin: YouCompleteMe
" create custom semantic triggers to show autocomplete
let g:ycm_semantic_triggers =  {
  \  'css,scss': ['re!^\s*', 're!:\s*'],
  \  'html': ['<', 're!<.*\s'],
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
  autocmd FileType css,html imap <Tab> <Plug>(emmet-expand-abbr)

  " set aliases
  autocmd BufRead,BufNewFile *.scss set filetype=scss.css

augroup END
