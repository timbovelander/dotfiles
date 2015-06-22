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
Plugin 'bling/vim-airline'
Plugin 'moll/vim-bbye'
" vim utils
Plugin 'tpope/vim-surround'
Plugin 'wellle/targets.vim'
Plugin 'Lokaltog/vim-easymotion'
" code syntax
Plugin 'othree/html5.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'groenewege/vim-less'
Plugin 'pangloss/vim-javascript'
Plugin 'othree/javascript-libraries-syntax.vim'
" code completion, extension, linting, ...
Plugin 'jiangmiao/auto-pairs'
Plugin 'Valloric/YouCompleteMe'
Plugin 'marijnh/tern_for_vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'scrooloose/syntastic'
Plugin 'mattn/emmet-vim'

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

" disable backup. using git & btrfs snapshots
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
set background=light

" enable ToggleBG function
call togglebg#map("<F12>")

if has('gui_running')

  " set font
  set guifont=Droid\ Sans\ Mono\ 10

  " set linespace
  set linespace=6

  " open maximized
  set lines=999 columns=999

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

" ===== Custom keybindings
" center screen
nmap <SPACE> zz

" insert newlines in normal mode
nmap <CR> o<Esc>
nmap <S-Enter> O<Esc>

" set home to go to first non-whitespace character
nmap <HOME> ^
imap <HOME> <Esc>^i

" navigate buffers
nmap <silent> <C-Tab> :lclose<CR>:bnext<CR>
nmap <silent> <C-S-Tab> :lclose<CR>:bprevious<CR>
nmap <silent> ]b :lclose<CR>:bnext<CR>
nmap <silent> [b :lclose<CR>:bprevious<CR>
nmap <silent> ]B :lclose<CR>:bfirst<CR>
nmap <silent> [B :lclose<CR>:blast<CR>

" close buffer
nnoremap <silent> <leader>q :lclose<CR>:Bdelete<CR>

" navigate windows
nmap <C-Left> <C-W><Left>
nmap <C-Down> <C-W><Down>
nmap <C-Up> <C-W><Up>
nmap <C-Right> <C-W><Right>

" create splits
nmap <A-Left> :topleft vsplit<CR>
nmap <A-Down> :botright split<CR>
nmap <A-Up> :topleft split<CR>
nmap <A-Right> :botright vsplit<CR>

" close window
nmap <A-q> :close<CR>

" open file explorer
nmap <silent> <C-e> :NERDTreeCWD<CR>

" redraws the screen and removes any search highlighting
nnoremap <silent> <C-l> :nohl<CR><C-l>

" copy and paste from clipboard
nmap <leader>p "+gp
vmap <leader>p "+gp
nmap <leader>y "+y
vmap <leader>y "+y

" easymotion line mappings
map <leader>h <Plug>(easymotion-linebackward)
map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)
map <leader>l <Plug>(easymotion-lineforward)
map <leader><Left> <leader>h
map <leader><Down> <leader>j
map <leader><Up> <leader>k
map <leader><Right> <leader>l

" easymotion search
map <leader>f <Plug>(easymotion-s2)

" ===== Plugin: Airline
" set seperators
let g:airline_left_sep=' '
let g:airline_right_sep=' '

" enable tab line extension
let g:airline#extensions#tabline#enabled = 1

" ===== Plugin: YouCompleteMe
" create custom semantic triggers to show autocomplete
let g:ycm_semantic_triggers =  {
  \  'css,less,scss': ['re!^\s*', 're!:\s*'],
  \  'html': ['<', '</', 're!<.*\s'],
  \}

" ===== Plugin: ctrlp
" make results scrollable
let g:ctrlp_match_window='results:20'

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

" ===== Plugin: vim-easymotion
" disable default mappings
let g:EasyMotion_do_mapping = 0

" ignore case
let g:EasyMotion_smartcase = 1

" ===== Plugin: vim-session
" disable autosave session
let g:session_autosave = 'no'

" disable autorestore session
let g:session_autoload = 'no'

" ===== Autocmd's
" create an augroup so autocmds are only applied once
augroup vimrc

  " clear all previous autocmd's
  autocmd!

  " enable emmet-completion
  autocmd FileType css,html imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

  " indentation fix for html
  autocmd FileType html imap <expr> <CR> ExpandHtmlTag()

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
