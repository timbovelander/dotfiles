" ===== Plugins
" required for Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

" start Vundle
call vundle#begin()

" Plugins
" plugin management
Plugin 'gmarik/Vundle.vim'
" color scheme
Plugin 'altercation/vim-colors-solarized'
" session management
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'
" ui utils
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'FelikZ/ctrlp-py-matcher'
Plugin 'bling/vim-airline'
Plugin 'moll/vim-bbye'
Plugin 'mileszs/ack.vim'
" vim utils
Plugin 'tpope/vim-surround'
Plugin 'wellle/targets.vim'
" git integration
Plugin 'tpope/vim-fugitive'
" code syntax
Plugin 'othree/html5.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'groenewege/vim-less'
Plugin 'othree/yajs.vim'
Plugin 'othree/javascript-libraries-syntax.vim'
" code completion, extension, linting, ...
Plugin 'jiangmiao/auto-pairs'
Plugin 'Valloric/YouCompleteMe'
Plugin 'marijnh/tern_for_vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'scrooloose/syntastic'
Plugin 'mattn/emmet-vim'
Plugin 'mattn/webapi-vim'
Plugin 'tpope/vim-commentary'

" end Vundle
call vundle#end()

" enable Matchit plugin
runtime macros/matchit.vim

" ===== General
" enable filetype plugins
filetype plugin indent on

" change split behaviour, split right and below
set splitbelow
set splitright

" disable backup. using git
set nobackup
set nowb
set noswapfile

" set number of history records
set history=200

" set nrformats to decimal only, do not use octal or hex notation
set nrformats=

" hide buffers automatically
set hidden

" enable omnifunc autocompletion
set omnifunc=syntaxcomplete#Complete

" ===== Theme
" enable syntax highlighting
syntax enable

" set color scheme
colorscheme solarized
set background=dark

if has('gui_running')

  " set font
  set guifont=Droid\ Sans\ Mono\ 11

  " set linespace
  set linespace=6

endif

" ===== UI
" minimal number of screen lines to keep above and below the cursor
set scrolloff=7

" configure backspace to delete autoindent, end-of-lines and past insert
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

" do not show gui menubar, toolbar and scroll bars
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

" find as you type search
set incsearch

" highlight search matches
set hlsearch

" ===== Custom mappings
" map leader key
let mapleader = "\<Space>"

" center screen
nmap <leader><SPACE> zz

" insert newlines in normal mode
nmap <CR> o<Esc>
nmap <S-Enter> O<Esc>

" set home to go to first non-whitespace character
nmap <HOME> ^
imap <HOME> <Esc>^i

" close buffer
nnoremap <silent> <C-q> :lclose<CR>:Bdelete<CR>

" navigate buffers
nmap <silent> <C-Tab> :lclose<CR>:bnext<CR>
nmap <silent> <C-S-Tab> :lclose<CR>:bprevious<CR>
nmap <silent> ]b :lclose<CR>:bnext<CR>
nmap <silent> [b :lclose<CR>:bprevious<CR>
nmap <silent> ]B :lclose<CR>:bfirst<CR>
nmap <silent> [B :lclose<CR>:blast<CR>

" create window(split)
nmap <A-Left> :topleft vsplit<CR>
nmap <A-Down> :botright split<CR>
nmap <A-Up> :topleft split<CR>
nmap <A-Right> :botright vsplit<CR>

" close window
nmap <A-q> :close<CR>

" navigate windows
nmap <C-Left> <C-W><Left>
nmap <C-Down> <C-W><Down>
nmap <C-Up> <C-W><Up>
nmap <C-Right> <C-W><Right>

" create new tab
nmap <leader>t :tabnew<CR>

" close tab
nmap <leader>q :tabclose<CR>

" navigate tabs
nmap ]t :tabnext<CR>
nmap [t :tabprevious<CR>
nmap ]T :tablast<CR>
nmap [T :tabfirst<CR>

" open file explorer
map <silent> <F2> <Esc>:NERDTreeToggle<CR>
map <silent> <C-F2> <Esc>:NERDTreeFind<CR>

" redraws the screen and removes any search highlighting
nnoremap <silent> <C-l> :nohl<CR><C-l>

" cut, copy and paste to/from clipboard
nmap <leader>d "+d
vmap <leader>d "+d
nmap <leader>p "+p
vmap <leader>p "+p
nmap <leader>y "+y
vmap <leader>y "+y

" expand path of the active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" search in project
nnoremap \ :Ack!<SPACE>

" ===== Plugin: Airline
" set seperators
let g:airline_left_sep=' '
let g:airline_right_sep=' '

" enable tab line extension
let g:airline#extensions#tabline#enabled = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" ===== Plugin: YouCompleteMe
" create custom semantic triggers to show autocomplete
let g:ycm_semantic_triggers =  {
  \  'css,less,scss': ['re!^\s*', 're!:\s*'],
  \  'html': ['<', '</', 're!<.*\s'],
  \}

" ===== Plugin: ctrlp
" make results scrollable
let g:ctrlp_match_window='results:40'

" set max depth to search with ctrlp
let g:ctrlp_max_depth=40

" set max files as results of ctrlp (0 = unlimited)
let g:ctrlp_max_files=0

" use ag to index files to ctrlp command also ignores files defined in .gitignore
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

" use ctrlp-py-matcher plugin as ctrlp matcher
let g:ctrlp_match_func = {'match': 'pymatcher#PyMatch'}

" ===== Plugin: Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" use html tidy width html5 support
let g:syntastic_html_tidy_exec = 'tidy5'

" ignore specific html tidy errors
let g:syntastic_html_tidy_ignore_errors = ['trimming empty <i>']

" ===== Plugin: vim-session
" disable autosave session
let g:session_autosave = 'no'

" disable autorestore session
let g:session_autoload = 'no'

" ===== Plugin: mustache
" enable abbreviations
let g:mustache_abbreviations = 1

" ===== Plugin: ack.vim
" use ag instead of ack
let g:ackprg = 'ag --vimgrep'

" close quickfix list when opening a result
let g:ack_autoclose = 1

" ===== Plugin: emmet.vim
" load custom emmet settings
let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.emmet.json')), "\n"))

" set emmet key bindings
let g:user_emmet_leader_key = '<C-e>'
let g:user_emmet_expandabbr_key = '<C-tab>'

" ===== Autocmd's
" create an augroup so autocmds are only applied once
augroup vimrc

  " clear all previous autocmd's
  autocmd!

  " associate unknown file extensions with filetypes
  autocmd BufRead,BufNewFile *.template setfiletype html.handlebars
  autocmd BufRead,BufNewFile *.jspf setfiletype jsp

  " indentation fix for html
  autocmd FileType html,html.handlebars,jsp,php imap <expr> <CR> ExpandHtmlTag()

  " Set comments for jsp
  autocmd FileType jsp set commentstring=<%--%s--%>

  " In the quickfix window, <CR> is used to jump to the error under the
  " cursor, so undefine the mapping there.
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>

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
