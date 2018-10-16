" ===== Plugins
set nocompatible
set runtimepath+=$HOME/.vim/bundle/Vundle.vim

filetype off

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-sensible'
Plugin 'morhetz/gruvbox'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'wellle/targets.vim'
Plugin 'othree/html5.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/yajs.vim'
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'dag/vim-fish'
Plugin 'jiangmiao/auto-pairs'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'tpope/vim-commentary'

call vundle#end()

" ===== Defaults
filetype plugin indent on

set hidden
set ignorecase
set noswapfile
set shell=bash
set smartcase
set splitbelow
set splitright
set wildignorecase

" ===== Appearance
colorscheme gruvbox

set background=dark
set number

" ===== Specific settings
augroup specific
  autocmd FileType jsp set commentstring=<%--%s--%>

  autocmd BufRead,BufNewFile *.jspf setfiletype jsp
  autocmd BufRead,BufNewFile *.schema setfiletype json
  autocmd BufRead,BufNewFile *.template setfiletype html.handlebars
  autocmd BufRead,BufNewFile *.tag setfiletype html

  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup END

" ===== Keybindings
let mapleader = "\<Space>"

nnoremap <CR> o<Esc>
nnoremap <S-CR> O<Esc>

nnoremap <HOME> ^
inoremap <HOME> <C-O>^

nnoremap <leader>d "+d
vnoremap <leader>d "+d
nnoremap <leader>p "+p
vnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>P "+P
nnoremap <leader>y "+y
vnoremap <leader>y "+y

nnoremap <silent> <leader>w/ :botright vsplit<CR>
nnoremap <silent> <leader>w- :botright split<CR>
nnoremap <silent> <leader>wh :wincmd h<CR>
nnoremap <silent> <leader>wj :wincmd j<CR>
nnoremap <silent> <leader>wk :wincmd k<CR>
nnoremap <silent> <leader>wl :wincmd l<CR>
nnoremap <silent> <leader>w<Left> :wincmd h<CR>
nnoremap <silent> <leader>w<Down> :wincmd j<CR>
nnoremap <silent> <leader>w<Up> :wincmd k<CR>
nnoremap <silent> <leader>w<Right> :wincmd l<CR>
nnoremap <silent> <leader>wc :close<CR>

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" ===== Plugin settings
let g:mustache_abbreviations = 1
