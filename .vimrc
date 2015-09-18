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
Plugin 'bling/vim-airline'
Plugin 'moll/vim-bbye'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/vimproc.vim'
" vim utils
Plugin 'tpope/vim-repeat'
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
nnoremap <C-Left> <C-W><Left>
nnoremap <C-Down> <C-W><Down>
nnoremap <C-Up> <C-W><Up>
nnoremap <C-Right> <C-W><Right>

" create new tab
nnoremap <silent> <leader>t :tabnew<CR>

" close tab
nnoremap <silent> <leader>q :tabclose<CR>

" navigate tabs
nnoremap <silent> ]t :tabnext<CR>
nnoremap <silent> [t :tabprevious<CR>
nnoremap <silent> ]T :tablast<CR>
nnoremap <silent> [T :tabfirst<CR>

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
nnoremap <silent> <C-p> :Unite -buffer-name=files -direction=botright -start-insert -winheight=10 file_rec/async<CR>

" search in project
nnoremap <silent> \ :Unite -buffer-name=files -no-split -auto-preview grep:.<CR>

" open external applications
" open terminal emulator in directory of current buffer
nnoremap <silent> <leader>xt :silent !x-terminal-emulator --working-directory=%:p:h &<CR>

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
