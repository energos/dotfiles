# ~/.bashrc -*-Shell-script-*-
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

# Load host/user specific configuration
if [[ -f ~/.pqprc ]]; then
    eval $(grep '^ALSA_CARD=' ~/.pqprc)
    eval $(grep '^DPI=' ~/.pqprc)
fi

# preserve history
HISTFILESIZE=100000
HISTSIZE=100000
shopt -s histappend

# don't include duplicates in history
# don't include lines beggining with spaces
HISTCONTROL=ignoreboth

# list size in megabytes
export BLOCKSIZE=M

# your favorite editor is ~/bin/emacs-nox
export EDITOR=emacs-nox

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

alias idf_env='. $HOME/esp/esp-idf/export.sh ; export ESPPORT=/dev/ttyUSB0'
alias emacs='emacs-nox'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias mc='mc -d'
alias less='less -i'
alias zless='zless -i'
alias avrdude='avrdude 2>&1'
alias poweroff='pqp poweroff'
alias reboot='pqp reboot'
# If DPI is not defined (from ~/.pqprc) default to DPI=96
alias startX="startx -- -dpi ${DPI:=96}"
unset DPI

export MC_XDG_OPEN=nohup-open

# https://github.com/hedning/nix-bash-completions
[[ -d $HOME/.nix-profile ]] && export XDG_DATA_DIRS="$HOME/.nix-profile/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"

# Export ALSA_CARD if defined in ~/.pqprc
[[ -n $ALSA_CARD ]] && export ALSA_CARD

stty -ixon

# FreeBSD
[[ -r /usr/local/share/bash-completion/bash_completion.sh ]] && source /usr/local/share/bash-completion/bash_completion.sh
# Debian/Devuan
[[ -r /etc/bash_completion ]] && source /etc/bash_completion

# prompt
if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
    # Add git branch to bash prompt
    # try alternative locations if file does not exist
    git_prompt=/usr/share/git/git-prompt.sh
    [[ -r $git_prompt ]] || git_prompt=/usr/local/share/git-core/contrib/completion/git-prompt.sh
    [[ -r $git_prompt ]] || git_prompt=/usr/lib/git-core/git-sh-prompt
    if [[ -r $git_prompt ]] ; then
        source $git_prompt
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

# emacs vterm
# https://github.com/akermu/emacs-libvterm
if [[ "${INSIDE_EMACS}" == "vterm" ]] ; then
    vterm_printf() {
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }
    function clear() {
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
    vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
            vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
            shift
        done
        vterm_printf "51;E$vterm_elisp"
    }
    find_file() {
        vterm_cmd find-file "$(realpath "${@:-.}")"
    }
    say() {
        vterm_cmd message "%s" "$*"
    }
    vterm_prompt_end(){
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi
