# /root/.bashrc -*-Shell-script-*-
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here...

unset PROMPT_COMMAND

# preserve history
HISTFILESIZE=100000
HISTSIZE=100000
shopt -s histappend

# don't include duplicates in history
# don't include lines beggining with spaces
HISTCONTROL=ignoreboth

# list size in megabytes
export BLOCKSIZE=M

# your favorite editor
export EDITOR=zile

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

alias emacs='emacs -Q -nw -nbc --eval "(progn\
(setq visible-cursor nil)\
(menu-bar-mode -1)\
(defalias \`yes-or-no-p \`y-or-n-p)\
(setq scroll-step 1)\
(setq scroll-preserve-screen-position t)\
(setq scroll-error-top-bottom t)\
(setq-default tab-always-indent \`complete)\
(setq-default indent-tabs-mode nil)\
(setq backward-delete-char-untabify-method nil)\
(global-set-key (kbd \"C-z\") \`undo)\
(global-set-key (kbd \"<clearline>\") \`end-of-buffer)\
)" "$@"'

alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias mc='mc -d'
alias less='less -i'
alias zless='zless -i'

stty -ixon

# prompt
if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
    # Add git branch to bash prompt
    if [[ -r /usr/share/git/git-prompt.sh ]] ; then
        source /usr/share/git/git-prompt.sh
        # from /etc/bash/bashrc
        # PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
        # from /usr/share/git/git-prompt.sh
        # PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
        #
        # slice, mix and shake:
        PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w\[\033[01;35m\]$(__git_ps1 " (%s)") \[\033[01;34m\]\$\[\033[00m\] '
        GIT_PS1_SHOWDIRTYSTATE=1
    else
        PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
    fi
fi
