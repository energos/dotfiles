#!/usr/bin/env bash

# Load host/user specific configuration
if [[ -f ~/.pqprc ]]; then
    eval $(grep '^HYPERVISOR=' ~/.pqprc)
    eval $(grep '^XRANDR=' ~/.pqprc)
    eval $(grep '^SYNERGY_SERVER=' ~/.pqprc)
    eval $(grep '^KEYBOARD=' ~/.pqprc)
fi

# Generates the Openbox menu
[[ -x ~/.config/openbox/menu_root.sh ]] && if [[ $MENU_DYNAMIC == no ]]
then
    ~/.config/openbox/menu_root.sh -r > ~/.config/openbox/menu_root.xml
else
    ~/.config/openbox/menu_root.sh -d > ~/.config/openbox/menu_root.xml
fi

# Evaluate XRANDR if defined (in ~/.pqprc)
[[ $XRANDR =~ ^xrandr[[:space:]] ]] && eval $XRANDR

# https://superuser.com/questions/418699/
DPI=$(xdpyinfo | awk -F'[ x]+' '/resolution:/{print $3}')

THRESHOLD=${THRESHOLD:-120}
[[ $DPI -gt $THRESHOLD ]] && HDPI="-DHIGH_DPI" || HDPI=""
xrdb $HDPI -DXSERVER_DPI=$DPI ~/.Xresources

# Get KEYBOARD if defined (in ~/.pqprc) and set X keymap accordingly
case "$KEYBOARD" in
    br)
        setxkbmap -model abnt2,pc104 -layout br,us -variant abnt2,intl -option -option grp:shifts_toggle,grp_led:caps,terminate:ctrl_alt_bksp,caps:none,lv3:ralt_switch_multikey
        ;;
    pt)
        setxkbmap -model pc105 -layout pt,us -variant basic,intl -option -option grp:shifts_toggle,grp_led:caps,terminate:ctrl_alt_bksp,caps:none,lv3:ralt_switch_multikey
        ;;
    *)
        setxkbmap -model pc104 -layout us,us -variant intl,basic -option -option grp:shifts_toggle,grp_led:caps,terminate:ctrl_alt_bksp,caps:none,lv3:ralt_switch_multikey
        ;;
esac

if [ -f "$HOME/.Xmodmap" ]; then
    xmodmap "$HOME/.Xmodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Get HYPERVISOR if defined (in ~/.pqprc)
case $HYPERVISOR in
    vbox)
        # If host is a VirtualBox guest
        # run Guest Additions client
        [[ -x $(command -v VBoxClient-all) ]] && VBoxClient-all
        [[ -x $(command -v spice-vdagent) ]] && spice-vdagent
        # turn off screensaver
        xset s off -dpms
        # Why? Why? Pause is detected as Num_Lock???
        xmodmap -e 'keycode 77 = F15'   # Remap Num_Lock
        # scroll wheel issues in FreeBSD guests
        xmodmap -e 'pointer = 1 2 3 4 5 0 0 0 0 0 0 0'
        # Automatically set screen resolution
        xrandr --output $(xrandr | awk '/\<connected\>/ {print $1}') --auto
        ;;
    *)
        # If host is not a VirtualBox guest
        # Get synergy server name from ~/.pqprc
        # Do not start synergy if SYNERGY_SERVER is empty or not defined
        if [[ -n $SYNERGY_SERVER ]]; then
            # start a synergy server or connect to a existing one
            [[ $(hostname) == $SYNERGY_SERVER ]] && synergys -d ERROR || synergyc -d ERROR $SYNERGY_SERVER
        fi
        ;;
esac

xhost +si:localuser:$USER
# Workaround for slock authentication issue
xhost +local:

# Avoid kglobalaccel and xbindkeys conflict
(sleep 2 && qdbus org.kde.kglobalaccel /kglobalaccel blockGlobalShortcuts true) &

export QT_QPA_PLATFORMTHEME=qt5ct
export QT_FONT_DPI=$DPI
export QT_ENABLE_HIGHDPI_SCALING=0

# https://wiki.gentoo.org/wiki/Elogind#startx_integration
launch=$(command -v ck-launch-session)
if [[ -z $launch ]]; then
    launch="dbus-launch --exit-with-session"
fi
export WINDOW_MANAGER=${WINDOW_MANAGER:-openbox}
case $WINDOW_MANAGER in
    openbox)    exec $launch openbox-session;;
    plasma)     exec $launch startplasma-x11;;
    icewm)      exec $launch icewm-session;;
    emacs)      exec $launch emacs -mm --debug-init -l ~/.emacs.d/exwm-desktop.el;;
    w10)        VirtualBoxVM -startvm "W10" -fullscreen;;
    bsd)        VirtualBoxVM -startvm "Beastie" -fullscreen;;
    *)          exec $WINDOW_MANAGER;;
esac
