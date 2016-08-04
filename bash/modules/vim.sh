# lets you cut and paste from stack traces, so
# vim blah.java:120 takes you to line 120
# note: technically, 'blah.java:120' could be a legitimate filename, but ...
VIM=${VIM:-$( which vim )}
vim() {
    if [[ $# == 1 ]] && [[ $1 =~ ([^:]*):([0-9]*) ]]; then
	${VIM} +${BASH_REMATCH[2]} "${BASH_REMATCH[1]}"
    else
	${VIM} $*
    fi
}
