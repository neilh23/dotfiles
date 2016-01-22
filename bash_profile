export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim

#export PS1="\u@\h \w> \\["'\033]2;'"\u@\h \w"'\007\]'
export PS1="\n\u@\h \w\n> "

alias t=todo.sh

# ... because ;-)
alias ':wq'=exit
complete -F _todo t

#handle £ signs in java source code ...
export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF8

if [[ -d ~/bin ]]; then
  export PATH=~/bin:${PATH}
fi

export HISTIGNORE='ls:pwd:date:exit:logout: *:history:pwd:kill*:rm*'

PWSAFE_IGNORE=parcellite
export PWSAFE_IGNORE

if [ -d ~/.bash/completions ]; then
  GLOBIGNORE="*~":"*/tags"
  for c in ~/.bash/completions/*; do
    [ -f $c ] && source $c
  done
fi

if [ -d ~/.bash/modules ]; then
  GLOBIGNORE="*~":"*/tags"
  for c in ~/.bash/modules/*; do
    [ -f $c ] && source $c
  done
fi

export LESS="-R x 4"
export SAL_USE_VCLPLUGIN=gen

[ -f ~/.bash_local ] && source ~/.bash_local

if [[ -s ~/.rvm/scripts/rvm ]]; then
  . ~/.rvm/scripts/rvm # This loads RVM into a shell session.
  export PATH=~/bin:$HOME/.rvm/bin:$PATH # Add RVM to PATH for scripting
fi

# increase Berkshelf timeout
export SOLVE_TIMEOUT=120

if [ -f ~/.ssh-agent ]; then
  . ~/.ssh-agent
fi
