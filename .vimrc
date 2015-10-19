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
Plugin 'tpope/vim-obsession'
Plugin 'tungd/unite-session'
" ui utils
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'moll/vim-bbye'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/unite.vim'
" vim utils
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'wellle/targets.vim'
" git integration
Plugin 'tpope/vim-fugitive'
" code syntax
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'mustache/vim-mustache-handlebars'
" code completion, extension, linting, ...
Plugin 'jiangmiao/auto-pairs'
Plugin 'Shougo/neocomplete.vim'
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
set history=1000

" set nrformats to decimal only, do not use octal or hex notation
set nrformats=

" hide buffers automatically
set hidden

" enable omnifunc autocompletion
set omnifunc=syntaxcomplete#Complete

" timeout key codes
set ttimeout
set ttimeoutlen=100

" autoread files when they have changed outside of ViM
set autoread

" show lastline instead of @-lines
set display=lastline

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

" do not show gui menubar, toolbar and scroll bars
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

" find as you type search
set incsearch

" highlight search matches
set hlsearch

" enable smarttab
set smarttab

" enable wildmenu (command mode completion)
set wildmenu

" ===== Custom key mappings
" map leader key
let mapleader = "\<Space>"

" center screen
nnoremap <leader><SPACE> zz

" insert newlines in normal mode
nnoremap <CR> o<Esc>
nnoremap <S-CR> O<Esc>

" set home to go to first non-whitespace character
nnoremap <HOME> ^
inoremap <HOME> <C-O>^

" close buffer
nnoremap <silent> <C-q> :lclose<CR>:Bdelete<CR>

" navigate buffers
nnoremap <silent> <C-Tab> :lclose<CR>:bnext<CR>
nnoremap <silent> <C-S-Tab> :lclose<CR>:bprevious<CR>
nnoremap <silent> ]b :lclose<CR>:bnext<CR>
nnoremap <silent> [b :lclose<CR>:bprevious<CR>
nnoremap <silent> ]B :lclose<CR>:bfirst<CR>
nnoremap <silent> [B :lclose<CR>:blast<CR>

" create window(split)
nnoremap <silent> <A-Left> :topleft vsplit<CR>
nnoremap <silent> <A-Down> :botright split<CR>
nnoremap <silent> <A-Up> :topleft split<CR>
nnoremap <silent> <A-Right> :botright vsplit<CR>

" close window
nnoremap <silent> <A-q> :close<CR>

" navigate windows
nnoremap <silent> <C-Left> :wincmd h<CR>
nnoremap <silent> <C-Down> :wincmd j<CR>
nnoremap <silent> <C-Up> :wincmd k<CR>
nnoremap <silent> <C-Right> :wincmd l<CR>

" create new tab
nnoremap <silent> <leader>t :tabnew<CR>

" close tab
nnoremap <silent> <leader>q :tabclose<CR>

" navigate tabs
nnoremap <silent> ]t :tabnext<CR>
nnoremap <silent> [t :tabprevious<CR>
nnoremap <silent> ]T :tablast<CR>
nnoremap <silent> [T :tabfirst<CR>

" new session
nnoremap <silent> <leader>s :Obsession "$HOME/.vim/sessions/" .  matchstr(getcwd(),"[^/]*$")<CR>

" open file explorer
nnoremap <silent> <F2> :NERDTreeToggle<CR>
nnoremap <silent> <C-F2> :NERDTreeFind<CR>

" redraws the screen and removes any search highlighting
nnoremap <silent> <C-l> :nohl<CR><C-l>

" cut, copy and paste to/from clipboard
nnoremap <leader>d "+d
vnoremap <leader>d "+d
nnoremap <leader>p "+p
vnoremap <leader>p "+p
nnoremap <leader>y "+y
vnoremap <leader>y "+y

" expand path of the active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" fuzzy finder
nnoremap <silent> <C-p> :Unite -buffer-name=files -direction=botright -start-insert -winheight=10 buffer file_rec/async<CR>
nnoremap <silent> <C-A-p> :Unite -buffer-name=sessions -direction=botright -start-insert -winheight=10 session<CR>

" search in project
nnoremap <silent> \ :Unite -buffer-name=files -no-split -auto-preview grep:.<CR>

" open external applications
" open terminal emulator in directory of current buffer
nnoremap <silent> <leader>xt :silent !x-terminal-emulator --working-directory=%:p:h &<CR>

" enter visual mode from normal and insert mode
nnoremap <S-Left> v<left>
nnoremap <S-Down> v<Down>
nnoremap <S-Up> v<Up>
nnoremap <S-Right> v<Right>
inoremap <S-Left> <Esc>v<left>
inoremap <S-Down> <Esc>v<Down>
inoremap <S-Up> <Esc>v<Up>
inoremap <S-Right> <Esc>v<Right>
vnoremap <S-Left> <left>
vnoremap <S-Down> <Down>
vnoremap <S-Up> <Up>
vnoremap <S-Right> <Right>

" ===== Plugin: Airline
" set seperators
let g:airline_left_sep=' '
let g:airline_right_sep=' '

" enable tabline extension
let g:airline#extensions#tabline#enabled = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" ===== Plugin: Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" use html tidy with html5 support
let g:syntastic_html_tidy_exec = 'tidy5'

" ignore specific html tidy errors
let g:syntastic_html_tidy_ignore_errors = ['trimming empty <i>']

" use eslint_d as javascript linter
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = 'eslint_d'

" ===== Plugin: mustache
" enable abbreviations
let g:mustache_abbreviations = 1

" ===== Plugin: emmet.vim
" load custom emmet settings
let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.emmet.json')), "\n"))

" set emmet key bindings
let g:user_emmet_leader_key = '<C-e>'
let g:user_emmet_expandabbr_key = '<C-tab>'

" ===== Plugin: unite.vim
" do not skip first result
let g:unite_enable_auto_select = 0

" set date format
let g:unite_source_buffer_time_format = "(%d-%m-%Y %H:%M:%S) "

" use ag command (automatically ignores files in .agignore and .gitignore)
let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '-g', '']
let g:unite_source_grep_command = "ag"
let g:unite_source_grep_default_opts = "--line-numbers --nocolor --nogroup"

" ===== Plugin: unite-session
" do not autosave session, this is done by Obsession plugin
let g:unite_session_force_no_update = 1

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
