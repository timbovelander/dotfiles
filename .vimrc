" ===== Plugins
set nocompatible
set runtimepath+=$HOME/.vim/bundle/Vundle.vim

filetype off

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-sensible'
Plugin 'altercation/vim-colors-solarized'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/neocomplete.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'moll/vim-bbye'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'wellle/targets.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'othree/html5.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/yajs.vim'
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'dag/vim-fish'
Plugin 'jiangmiao/auto-pairs'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'ternjs/tern_for_vim'
Plugin 'scrooloose/syntastic'
Plugin 'mattn/emmet-vim'
Plugin 'mattn/webapi-vim'
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
colorscheme solarized

set background=dark
set number

" ===== Specific settings
augroup specific
  autocmd FileType jsp set commentstring=<%--%s--%>

  autocmd BufRead,BufNewFile *.jspf setfiletype jsp
  autocmd BufRead,BufNewFile *.schema setfiletype json
  autocmd BufRead,BufNewFile *.template setfiletype html.handlebars
  autocmd BufRead,BufNewFile *.tag setfiletype html

  autocmd BufReadPost fugitive://* set bufhidden=delete
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup END

" ===== Keybindings
let mapleader = "\<Space>"

nnoremap <silent> <F2> :Explore<CR>
nnoremap <silent> <F11> :only<CR>

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

imap <C-Tab> <Plug>(emmet-expand-abbr)
vmap <C-Tab> <Plug>(emmet-expand-abbr)

nnoremap <silent> \ :Unite -buffer-name=files -no-split grep:.<CR>
nnoremap <silent> <C-p> :Unite -buffer-name=files -direction=botright -start-insert -winheight=10 file_rec/async<CR>

nnoremap <silent> <leader>b :Unite -buffer-name=buffers -direction=botright -winheight=10 buffer<CR>
nnoremap <silent> <C-Tab> :lclose<Bar>bnext<CR>
nnoremap <silent> ]b :lclose<Bar>bnext<CR>
nnoremap <silent> [b :lclose<Bar>bprevious<CR>
nnoremap <silent> ]B :lclose<Bar>bfirst<CR>
nnoremap <silent> [B :lclose<Bar>blast<CR>
nnoremap <silent> <leader>qb :lclose<Bar>Bdelete<CR>
nnoremap <silent> <C-q> :lclose<Bar>Bdelete<CR>

nnoremap <silent> <leader>l :lopen<CR>
nnoremap <silent> ]l :lnext<CR>
nnoremap <silent> [l :lprevious<CR>
nnoremap <silent> ]L :lfirst<CR>
nnoremap <silent> [L :llast<CR>
nnoremap <silent> <leader>ql :lclose<CR>

nnoremap <silent> <leader>q :copen<CR>
nnoremap <silent> ]q :cnext<CR>
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]Q :cfirst<CR>
nnoremap <silent> [Q :clast<CR>
nnoremap <silent> <leader>qq :cclose<CR>

nnoremap <silent> <A-h> :topleft vsplit<CR>
nnoremap <silent> <A-j> :botright split<CR>
nnoremap <silent> <A-k> :topleft split<CR>
nnoremap <silent> <A-l> :botright vsplit<CR>
nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>
nnoremap <silent> <A-Left> :topleft vsplit<CR>
nnoremap <silent> <A-Down> :botright split<CR>
nnoremap <silent> <A-Up> :topleft split<CR>
nnoremap <silent> <A-Right> :botright vsplit<CR>
nnoremap <silent> <C-Left> :wincmd h<CR>
nnoremap <silent> <C-Down> :wincmd j<CR>
nnoremap <silent> <C-Up> :wincmd k<CR>
nnoremap <silent> <C-Right> :wincmd l<CR>
nnoremap <silent> <A-q> :close<CR>

nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>ge :if &diff<Bar>Gedit<Bar>only<Bar>else<Bar>Gedit<Bar>endif<CR>
nnoremap <silent> <leader>gl :silent Glog -- %<CR>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>

nnoremap <silent> <leader>xt :silent !x-terminal-emulator --working-directory=%:p:h &<CR>

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" ===== Commands
command Clear nohl<Bar>redraw

" ===== Plugin settings
let g:unite_enable_auto_select = 0
let g:unite_source_buffer_time_format = "(%d-%m-%Y %H:%M:%S)"
let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '-g', '']
let g:unite_source_grep_command = "ag"
let g:unite_source_grep_default_opts = "--line-numbers --nocolor --nogroup"
let g:unite_force_overwrite_statusline = 0

let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_auto_close_preview = 1

let g:airline_left_sep = ''
let g:airline_right_sep = ''

let g:mustache_abbreviations = 1

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = 'eslint_d'
let g:syntastic_scss_checkers = ['scss_lint']

let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.emmet.json')), "\n"))
let g:user_emmet_leader_key = '<C-e>'

let g:tern_show_argument_hints = 'on_move'
