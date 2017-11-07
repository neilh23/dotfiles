" reload with :so ~/.vimrc (or :so % if editing)

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

set path+=/usr/include/alsa/

call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'jceb/vim-orgmode'

Plugin 'tpope/vim-speeddating'

Plugin 'majutsushi/tagbar'

Plugin 'taglist.vim'

Plugin 'benmills/vimux'

" Allow repeating plugin maps with .
Plugin 'tpope/vim-repeat'

" change surrounding quotes etc
Plugin 'tpope/vim-surround'

" auto-detect indentation in current file
Plugin 'tpope/vim-sleuth'

" golang
Plugin 'fatih/vim-go'

" http://vimawesome.com/plugin/easymotion
" TODO - go through configuration ...
" Plugin 'Lokaltog/vim-easymotion'

"Plugin 'tpope/vim-fugitive'

" http://www.vim.org/scripts/script.php?script_id=3252
" Plugin 'L9'
" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'

" insert mode auto-completion for quotes, parenthesis etc
" https://github.com/Raimondi/delimitMate
" Plugin 'Raimondi/delimitMate'

Plugin 'scrooloose/syntastic'

Plugin 'tomasr/molokai'
"Plugin 'humiaozuzu/TabBar'

Plugin 'xolox/vim-misc'
" https://github.com/xolox/vim-easytags
" requires exuberant-ctags to be installed
" Plugin 'xolox/vim-easytags'
" Plugin 'majutsushi/tagbar'

" Plugin 'kien/ctrlp.vim'

" Plugin 'tpope/vim-rails'
" Plugin 'tpope/vim-bundler'

" Plugin 'vim-perl/vim-perl'
Plugin 'vim-ruby/vim-ruby'
Plugin 'pangloss/vim-javascript'
Plugin 'othree/html5.vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'chrisbra/csv.vim'
Plugin 'elzr/vim-json'

"Plugin 'maksimr/vim-jsbeautify'

" Plugin 'w0rp/ale'
Plugin 'junegunn/fzf'


" Plugin 'vim-scripts/Vim-R-plugin'
 
Plugin 'Align'
" Plugin 'SQLUtilities'

" Plugin 'freitass/todo.txt-vim'

" wrapper for the silver searcher ('ag') plugin for vim
Plugin 'rking/ag.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line



" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif


" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set backup		" keep a backup file
set backupdir=~/.vim/backup

" from http://vim.wikia.com/wiki/Keep_incremental_backups_of_edited_files
let myvar = strftime("%F_%H%M%S")
let myvar = "set backupext=_".myvar
execute myvar

set directory=~/.vim/tmp " Swap file directory
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
set number

" Don't use Ex mode, use Q for formatting
map Q gq

" In an xterm the mouse should work quite well, thus enable it.
set mouse=a

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

set shiftwidth=2
set tabstop=4
set smartindent
"set breakindent

" Keep permanent undo info! :-)
set undofile                " Save undo's after file closes
set undodir=~/.vim/undo " where to save undo histories - need to do mkdir to get it work
set undolevels=1000         " How many undos
set undoreload=10000        " number of lines to save for undo
silent !mkdir -p ~/.vim/tmp
silent !mkdir -p ~/.vim/undo
silent !mkdir -p ~/.vim/backup
" don't keep backups over a month ...
silent !find ~/.vim/backup/ -ctime +30 -delete

" show matching brackets
autocmd FileType perl set showmatch

" check perl code with :make
autocmd FileType perl set makeprg=perl\ -Wc\ %\ $*
autocmd FileType perl set errorformat=%f:%l:%m
autocmd FileType perl set autowrite

set background=dark
colorscheme molokai

if &diff
  colorscheme evening
endif

set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"use a strong cipher for encrypting files,
"default is 'zip'
setlocal cm=blowfish2

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_ruby_checkers          = ['rubocop', 'mri']
let g:syntastic_ruby_checkers          = ['rubocop']
" default java checker (javac) takes aaaaaages
let g:syntastic_java_checkers          = ['checkstyle']

let g:syntastic_javascript_checkers          = ['eslint']

let g:syntastic_perl_checkers=['perl', 'perlcritic']
" let g:syntastic_perl_checkers=['perl']

let g:syntastic_enable_perl_checker = 1

" let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_go_checkers = ['golint', 'govet', 'go']
" let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

let g:syntastic_enable_go_checker = 1

" let g:syntastic_c_checkers          = ['gcc', 'splint']

" ----- xolox/vim-easytags settings -----
" Where to look for tags files
set tags=./tags;,~/.vimtags
" Sensible defaults
let g:easytags_events = ['BufReadPost', 'BufWritePost']
let g:easytags_async = 1
let g:easytags_dynamic_files = 2
let g:easytags_resolve_links = 1
let g:easytags_suppress_ctags_warning = 1

" ----- majutsushi/tagbar settings -----
" Open/close tagbar with \b
nmap <silent> <leader>b :TagbarToggle<CR>
" Uncomment to open tagbar automatically whenever possible
"autocmd BufEnter * nested :call tagbar#autoopen(0)


" ----- jistr/vim-nerdtree-tabs -----
" Open/close NERDTree Tabs with \t
nmap <silent> <leader>t :NERDTreeTabsToggle<CR>
" To have NERDTree always open on startup
let g:nerdtree_tabs_open_on_console_startup = 0
let g:nerdtree_tabs_open_on_gui_startup = 0

let delimitMate_expand_cr = 1
augroup mydelimitMate
  au!
  au FileType markdown let b:delimitMate_nesting_quotes = ["`"]
  au FileType tex let b:delimitMate_quotes = ""
  au FileType tex let b:delimitMate_matchpairs = "(:),[:],{:},`:'"
  au FileType python let b:delimitMate_nesting_quotes = ['"', "'"]
augroup END

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
" Look in .svn .git etc
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

" js beautify
autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
" for html
autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
" for css or scss
autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>

" for sql
autocmd FileType sql noremap <buffer> <c-f> :% SQLUFormatter<cr>

" for xml
autocmd FileType xml noremap <buffer> <c-f> :% ! tidy -utf8 -xml -w 180 -i -c -q -asxml <cr>

au! BufRead,BufNewFile *.json set filetype=json
augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  autocmd FileType json set softtabstop=2 tabstop=8
  autocmd FileType json set expandtab
""  autocmd FileType json set foldmethod=syntax
augroup END

au! BufRead,BufNewFile Berksfile set filetype=ruby
au! BufRead,BufNewFile Vagrantfile set filetype=ruby

if has("gui_running")
  " GUI is running or is about to start.
  " Maximize gvim window (for an alternative on Windows, see simalt below).
  set lines=999 columns=999
else
  " This is console Vim.
  if exists("+lines")
    set lines=50
  endif
  if exists("+columns")
    set columns=100
  endif
endif

" set this at the end, because something seemed to be overriding it ...
" I hate tabs!
set expandtab


" nicked from https://github.com/Morantron/dotfiles/blob/master/vimrc#L229
function! SendToNearestPane(type, ...)
  let reg_save = @@

  "if exists("g:VimuxRunnerIndex") "use this to open vimux runner
  "inteligently

  if a:0  " Invoked from Visual mode, use gv command.
    silent exe "normal! gvy"
  elseif a:type == 'line'
    silent exe "normal! '[V']y"
  else
    silent exe "normal! :%y"
  endif

  call VimuxSendText(@@)

  let @@ = reg_save
endfunction

nnoremap \\ :<C-U>call SendToNearestPane(1)<CR>
vmap \\ :<C-U>call SendToNearestPane(visualmode(), 1)<CR>
