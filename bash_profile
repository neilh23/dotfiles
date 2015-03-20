export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim

export PS1="\u@\h \w> "'\033]2;'"\u@\h \w"'\007'

export PATH="~/bin:$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

[ -f ~/.bash_local ] && source ~/.bash_local
