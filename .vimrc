" ===== Plugins
" required for Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

" start Vundle
call vundle#begin()

" Plugins
Plugin 'gmarik/Vundle.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/html5.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'mattn/emmet-vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'marijnh/tern_for_vim'
Plugin 'Raimondi/delimitMate'
Plugin 'Valloric/MatchTagAlways'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-rvm'

" end Vundle
call vundle#end()


" ===== General
" enable filetype plugins
filetype plugin indent on

" change split behaviour, split right and below
set splitbelow
set splitright


" ===== Theme
" enable syntax highlighting
syntax enable

" set color scheme
colorscheme solarized
set background=light

if has('gui_running')

  " set font
  set guifont=Droid\ Sans\ Mono\ 10

  " set linespace
  set linespace=6

endif


" ===== UI
" minimal number of screen lines to keep above and below the cursor
set scrolloff=7

" configure backspace to work the same as in other editors
set backspace=2

" ignore case on file & dir completion
set wildignorecase

" always show status line
set laststatus=2

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

" find as you type search
set incsearch

" highlight search matches
set hlsearch


" ===== Custom keybindings
" change background color light/dark
map <F12> :let &background = ( &background == "dark"? "light" : "dark" )<CR>

" insert a newline with enter & shift-enter
nnoremap <CR> o<Esc>
nnoremap <S-Enter> O<Esc>

" navigate buffers using ctrl-tab
nnoremap <C-Tab> :bn<CR>

" navigate windows with ctrl-arrow
nnoremap <C-Left> <C-W><Left>
nnoremap <C-Right> <C-W><Right>
nnoremap <C-Up> <C-W><Up>
nnoremap <C-Down> <C-W><Down>

" create splits with ctrl-shift-arrow
nnoremap <C-S-Left> :topleft vnew<CR>
nnoremap <C-S-Right> :botright vnew<CR>
nnoremap <C-S-Up> :topleft new<CR>
nnoremap <C-S-Down> :botright new<CR>

" explorer attached to ctrl-e
nnoremap <C-e> :Explore<CR>

" redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>


" ===== Plugin: netrw (vim default)
" use tree view
let g:netrw_liststyle=3


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


" ===== Plugin: ctrlp
" make results scrollable
let g:ctrlp_match_window='results:100'


" ===== Autocmd's
" create an augroup so autocmds are only applied once
augroup vimrc

  " clear all previous autocmd's
  autocmd!

  " run Rvm on startup to set rvm right
  autocmd VimEnter * Rvm

  " set aliases
  "autocmd BufRead,BufNewFile *.scss set filetype=scss.css
  "autocmd BufRead,BufNewFile *.mustache set filetype=html.mustache

  " enable omnifunc autocomplete
  autocmd FileType css set omnifunc=csscomplete#CompleteCSS
  autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
  autocmd Filetype javascript set omnifunc=tern#Complete

  " enable emmet-completion on TAB
  autocmd FileType css,html,html.mustache imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

  " indentation fix for html
  autocmd FileType html,html.mustache imap <expr> <CR> ExpandHtmlTag()

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
