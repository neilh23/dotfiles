export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim

export PS1="\u@\h \w> "'\033]2;'"\u@\h \w"'\007'

#handle £ signs in java source code ...
export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF8

if [[ -s ~/.rvm/scripts/rvm ]]; then
  . ~/.rvm/scripts/rvm # This loads RVM into a shell session.
  export PATH=~/bin:$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
fi

if [[ -d ~/bin ]]; then
  export PATH=~/bin:${PATH}
fi

export HISTIGNORE='ls:pwd:date:exit:logout: *:history:pwd:kill*:rm*'

PWSAFE_IGNORE=parcellite
export PWSAFE_IGNORE

[ -f ~/.bash_local ] && source ~/.bash_local