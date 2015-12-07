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
" ui utils
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/unite.vim'
Plugin 'moll/vim-bbye'
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

" enable backup
set backup
set backupdir=~/.vim/backup
set nowritebackup

" disable swap file (recovery)
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

" insert newlines in normal mode
nnoremap <CR> o<Esc>
nnoremap <S-CR> O<Esc>

" set home to go to first non-whitespace character
nnoremap <HOME> ^
inoremap <HOME> <C-O>^

" cut, copy and paste to/from clipboard
nnoremap <leader>d "+d
vnoremap <leader>d "+d
nnoremap <leader>p "+p
vnoremap <leader>p "+p
nnoremap <leader>y "+y
vnoremap <leader>y "+y

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

" redraws the screen and removes any search highlighting
nnoremap <silent> <C-l> :nohl<CR><C-l>

" buffers
nnoremap <silent> <leader>b :Unite -buffer-name=buffers -direction=botright -winheight=10 buffer<CR>
nnoremap <silent> <C-Tab> :lclose<Bar>bnext<CR>
nnoremap <silent> ]b :lclose<Bar>bnext<CR>
nnoremap <silent> [b :lclose<Bar>bprevious<CR>
nnoremap <silent> ]B :lclose<Bar>bfirst<CR>
nnoremap <silent> [B :lclose<Bar>blast<CR>
nnoremap <silent> <leader>qb :lclose<Bar>Bdelete<CR>
nnoremap <silent> <C-q> :lclose<Bar>Bdelete<CR>
" location list
nnoremap <silent> <leader>l :lopen<CR>
nnoremap <silent> ]l :lnext<CR>
nnoremap <silent> [l :lprevious<CR>
nnoremap <silent> ]L :lfirst<CR>
nnoremap <silent> [L :llast<CR>
nnoremap <silent> <leader>ql :lclose<CR>
" quickfix list
nnoremap <silent> <leader>q :copen<CR>
nnoremap <silent> ]q :cnext<CR>
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]Q :cfirst<CR>
nnoremap <silent> [Q :clast<CR>
nnoremap <silent> <leader>qq :cclose<CR>
" windows
nnoremap <silent> <A-Left> :topleft vsplit<CR>
nnoremap <silent> <A-Down> :botright split<CR>
nnoremap <silent> <A-Up> :topleft split<CR>
nnoremap <silent> <A-Right> :botright vsplit<CR>
nnoremap <silent> <C-Left> :wincmd h<CR>
nnoremap <silent> <C-Down> :wincmd j<CR>
nnoremap <silent> <C-Up> :wincmd k<CR>
nnoremap <silent> <C-Right> :wincmd l<CR>
nnoremap <silent> <A-q> :close<CR>

" fugitive git commands
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>ge :if &diff<Bar>Gedit<Bar>only<Bar>else<Bar>Gedit<Bar>endif<CR>
nnoremap <silent> <leader>gl :silent Glog -- %<CR>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>

" function key bindings
" open file explorer
nnoremap <silent> <F2> :NERDTreeToggle<CR>
nnoremap <silent> <C-F2> :NERDTreeFind<CR>
" show buffer fullscreen
nnoremap <silent> <F11> :only<CR>

" expand path of the active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" fuzzy finder
nnoremap <silent> <C-p> :Unite -buffer-name=files -direction=botright -start-insert -winheight=10 file_rec/async<CR>

" search in project
nnoremap <silent> \ :Unite -buffer-name=files -no-split grep:.<CR>
nnoremap <silent> <leader>/ :call ReplaceInBuffer()<CR>

" open external applications
" open terminal emulator in directory of current buffer
nnoremap <silent> <leader>xt :silent !x-terminal-emulator --working-directory=%:p:h &<CR>

" adjust font size
nnoremap <silent> <leader>0 :call AdjustFontSize(0)<CR>
nnoremap <silent> <leader>- :call AdjustFontSize(-2)<CR>
nnoremap <silent> <leader>= :call AdjustFontSize(2)<CR>

" emmet key bindings
imap <C-Tab> <Plug>(emmet-expand-abbr)
vmap <C-Tab> <Plug>(emmet-expand-abbr)

" ===== Plugin: Solarized
" map ToggleBG key
call togglebg#map('<F10>')

" ===== Plugin: Airline
" set seperators
let g:airline_left_sep=' '
let g:airline_right_sep=' '

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

" set emmet leader key
let g:user_emmet_leader_key = '<C-e>'

" ===== Plugin: unite.vim
" do not skip first result
let g:unite_enable_auto_select = 0

" set date format
let g:unite_source_buffer_time_format = "(%d-%m-%Y %H:%M:%S)"

" use ag command (automatically ignores files in .agignore and .gitignore)
let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '-g', '']
let g:unite_source_grep_command = "ag"
let g:unite_source_grep_default_opts = "--line-numbers --nocolor --nogroup"

" ===== Plugin: NeoComplete
" add semantic triggers
let g:ycm_semantic_triggers =  {
\  'html,html.handlebars,jsp': ['<', 're!<.+\s'],
\  'css': ['re!{\s', 're!^\s+', 're!:\s'],
\}

" ===== Autocmd's
" create an augroup so autocmds are only applied once
augroup vimrc

  " clear all previous autocmd's
  autocmd!

  " change backup file extension, add timestamp
  autocmd BufWritePre * let &backupext = "-" . strftime("%s") . ".vimbackup"
  " remove old backup files on write file
  autocmd BufWritePost * call RemoveOldBackupFiles()

  " associate unknown file extensions with filetypes
  autocmd BufRead,BufNewFile *.jspf setfiletype jsp
  autocmd BufRead,BufNewFile *.template setfiletype html.handlebars
  autocmd BufRead,BufNewFile *.tag setfiletype html

  " indentation fix for html
  autocmd FileType html,html.handlebars,jsp,php imap <expr> <CR> ExpandHtmlTag()

  " Set comments for jsp
  autocmd FileType jsp set commentstring=<%--%s--%>

  " In the quickfix window, <CR> is used to jump to the error under the
  " cursor, so undefine the mapping there.
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>

  " automatically remove fugitive buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete

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

" search and replace in buffer
function! ReplaceInBuffer()
  call inputsave()
  let search = input('Search: ')
  let replace = input('Replace with: ')
  call inputrestore()

  execute "%s/" . search . "/" . replace . "/gc"
endfunction

function! RemoveOldBackupFiles()
  let filename = expand('%:t')
  let find = "find $HOME/.vim/backup -name " . filename . "\\*.vimbackup"
  let sort = "sort -n"
  let filter = "head -n -10"
  let remove = "xargs rm --"

  execute "silent !" . find . " | " . sort . " | " . filter . " | " . remove
endfunction

function! AdjustFontSize(amount)
  if has("gui_gtk2") && has("gui_running")
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
  else
    echoerr "You need to run the GTK2 version of Vim to use this function."
  endif
endfunction
