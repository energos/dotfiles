# ~/.bash_profile -*-Shell-script-*-

# https://wiki.gentoo.org/wiki/X_without_Display_Manager
if ! shopt -q login_shell; then
    echo Somehow this is a non-bash or non-login shell.
    sleep 5
    exit 1
fi

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]] ; then
    . ~/.bashrc
fi

# Put your fun stuff here...

eval $(grep '^NAME=' /etc/os-release)
case $NAME in
    *BSD)
        # echo "$NAME is a *BSD"
        ttyv=/dev/ttyv0
        if [ -x /usr/bin/fortune ] ; then /usr/bin/fortune freebsd-tips ; fi
        ;;
    Gentoo)
        # echo "$NAME is Gentoo"
        ttyv=/dev/tty1
        # user's Racket
        [[ -d ${HOME}/racket/bin ]] && PATH="${HOME}/racket/bin:${PATH}"
        [[ -d ${HOME}/racket/man ]] && MANPATH="${HOME}/racket/man:${MANPATH}"
        # user's .local/bin
        [[ -d ${HOME}/.local/bin ]] && PATH="${HOME}/.local/bin:${PATH}"
        [[ -d ${HOME}/.local/share/man ]] && MANPATH="${HOME}/.local/share/man:${MANPATH}"
        # user's bin
        [[ -d ${HOME}/bin ]] && PATH="${HOME}/bin:${PATH}"
        export PATH
        export MANPATH
        # add /usr/local/share/info to INFOPATH
        [[ -d /usr/local/share/info ]] && export INFOPATH="/usr/local/share/info:${INFOPATH}"
        ;;
    Void)
        # echo "$NAME is Void Linux"
        ttyv=/dev/tty1
        # user's .local/bin
        [[ -d ${HOME}/.local/bin ]] && PATH="${HOME}/.local/bin:${PATH}"
        # user's bin
        [[ -d ${HOME}/bin ]] && PATH="${HOME}/bin:${PATH}"
        export PATH
        ;;
    "Devuan GNU/Linux")
        # echo "$NAME is Devuan!"
        ttyv=/dev/tty1
        # user's .local/bin
        [[ -d ${HOME}/.local/bin ]] && PATH="${HOME}/.local/bin:${PATH}"
        # user's bin
        [[ -d ${HOME}/bin ]] && PATH="${HOME}/bin:${PATH}"
        export PATH
        ;;
    *)
        echo "Welcome to $NAME"
        unset MANPAGER
        ;;
esac
unset NAME

# And now we start X11
if [[ -f ~/.pqprc ]]; then
    eval $(grep '^START_X11=' ~/.pqprc)
    eval $(grep '^DPI=' ~/.pqprc)
fi
if [[ "${START_X11}" == "1" ]]; then
    # [[ -t 0 && "$(tty)" == "${ttyv}" && ! $DISPLAY ]] && exec startx -- $([[ -n ${XDG_VTNR} ]] && echo -n vt${XDG_VTNR}\ )-dpi ${DPI:=96}
    if [[ -t 0 && "$(tty)" == "${ttyv}" && ! $DISPLAY ]]; then
        echo -n "Starting X"
        sleep 1
        echo -n "."
        sleep 1
        echo -n "."
        sleep 1
        echo "."
        sleep 1
        echo "...right now!"
        startx -- $([[ -n ${XDG_VTNR} ]] && echo -n vt${XDG_VTNR}\ )-dpi ${DPI:=96}
    fi
fi
unset ttyv
unset DPI
unset START_X11
