#!/bin/bash

DEBUGPORT=${DEBUGPORT:-""}
USER_ICONS=$HOME/.local/share/icons
ICONS=/usr/share/icons/Adwaita/48x48

# Get default navigator
# Exclude some sub-menus if this is a crappy host
[[ -f ~/.pqprc ]] && . ~/.pqprc
navigator=${NAVIGATOR:-firefox}

# Ugly, ugly, very ugly
case $navigator in
    falkon)
        navigator_icon=/usr/share/icons/hicolor/48x48/apps/falkon.png
        ;;
    qutebrowser)
        navigator_icon=/usr/share/icons/hicolor/scalable/apps/qutebrowser.svg
        ;;
    netsurf)
        navigator_icon=/usr/share/pixmaps/netsurf.xpm
        ;;
    midori)
        navigator_icon=/usr/share/icons/hicolor/22x22/apps/midori.png
        ;;
    dillo)
        navigator_icon=/usr/share/pixmaps/dillo.png
        ;;
    firefox)
        navigator_icon=/usr/share/pixmaps/aurora.png
        ;;
    *)
        navigator_icon=${ICONS}/apps/web-browser.png
        ;;
esac

do_exit=1

# parse options
while getopts ":prd" option; do
    case $option in
        p)  [[ -n $DEBUGPORT ]] && echo "p: Build a pipe menu" > $DEBUGPORT
            # build a pipe menu
            pipe_menu=1
            root_menu=0
            dynamic_menu=0
            do_exit=0
            ;;
        r)  [[ -n $DEBUGPORT ]] && echo "r: Build a root menu" > $DEBUGPORT
            # build a root menu
            pipe_menu=0
            root_menu=1
            dynamic_menu=0
            do_exit=0
            ;;
        d)  [[ -n $DEBUGPORT ]] && echo "d: Build a dynamic root menu" > $DEBUGPORT
            # build a dynamic root menu
            pipe_menu=0
            root_menu=1
            dynamic_menu=1
            do_exit=0
            ;;
        \?) [[ -n $DEBUGPORT ]] && echo "?: ${0##*/}: Invalid option -$OPTARG" > $DEBUGPORT
            tty -s && echo "${0##*/}: Invalid option -$OPTARG" >&2
            exit 1
            ;;
    esac
done
shift $((OPTIND - 1))

if [[ $do_exit == 1 ]]
then
    [[ -n $DEBUGPORT ]] && echo "?: ${0##*/}: No valid option found" > $DEBUGPORT
    tty -s && echo "${0##*/}: No valid option found" >&2
    exit 1
fi

# ROOT Menu Header
[[ $root_menu == 1 ]] && cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<openbox_menu xmlns="http://openbox.org/3.4/menu">
EOF

[[ $root_menu == 1 && $dynamic_menu != 1 ]] && cat <<EOF
<menu id="root-menu" label="Openbox 3">
EOF

# Pipe Menu Header
[[ $pipe_menu == 1 ]] && cat <<EOF
<openbox_pipe_menu>
EOF

if [[ $dynamic_menu == 0 ]]
then
    # ********************
    # * Static Root Menu *
    # ********************

    # Menu Title
    cat <<EOF
<separator label="Gentoo! @ ${HOSTNAME}" />
<separator />
EOF

    # Terminal & File Manager
    cat <<EOF
<item label="Terminal (urxvt)" icon="${USER_ICONS}/Utilities-terminal.svg">
<action name="Execute"><command>urxvtcd -name Terminal</command></action> </item>
<item label="Terminal (xterm)" icon="${ICONS}/apps/utilities-terminal.png">
<action name="Execute"><command>xterm</command></action> </item>
<item label="Midnight Commander" icon="${USER_ICONS}/mc.svg" >
<action name="Execute"><command>pqp mc</command></action> </item>
<item label="File Manager" icon="${ICONS}/apps/system-file-manager.png" >
<action name="Execute"><command>pcmanfm</command></action> </item>
<separator />
EOF

    # Internet
    cat <<EOF
<item label="${navigator^}" icon="${navigator_icon}">
<action name="Execute"><command>pqp -n navigator</command></action> </item>
<menu id="browsers" label="Browsers" icon="${ICONS}/categories/applications-internet.png" execute="~/.config/openbox/menu_browsers.sh" />
<separator />
EOF

    # Editors
    cat <<EOF
<item label="Emacs" icon="/usr/share/pixmaps/emacs.png">
<action name="Execute"><command>emacsclient -c -n -a "emacs"</command></action> </item>
<item label="Geany" icon="/usr/share/icons/hicolor/48x48/apps/geany.png">
<action name="Execute"><command>geany</command></action> </item>
<separator />
EOF

    # Electronics/CAD
    [[ $MENU_ELECTRONICS != no ]] && cat <<EOF
<menu id="cad" label="Electronics" icon="/usr/share/icons/hicolor/scalable/categories/applications-electronics.svg" execute="~/.config/openbox/menu_electronics.sh" />
<separator />
EOF

    # Multimedia
    cat <<EOF
<menu id="multimedia" label="Multimedia" icon="${ICONS}/categories/applications-multimedia.png" execute="~/.config/openbox/menu_multimedia.sh" />
<separator />
EOF

    # Office
    cat <<EOF
<item label="Calibre EBook Manager" icon="/usr/share/icons/hicolor/48x48/apps/calibre-gui.png">
<action name="Execute"><command>calibre --detach</command></action> </item>
<item label="Open Office Calc" icon="/usr/share/icons/hicolor/48x48/apps/openoffice4-calc.png">
<action name="Execute"><command>oocalc</command></action> </item>
<item label="Open Office Writer" icon="/usr/share/icons/hicolor/48x48/apps/openoffice4-writer.png">
<action name="Execute"><command>oowriter</command></action> </item>
<separator />
EOF

    # Virtual Machines
    [[ $MENU_VIRTUALIZATION != no ]] && cat <<EOF
<menu id="virtualization" label="Virtualization" icon="${ICONS}/places/folder.png" execute="~/.config/openbox/menu_virtualization.sh" />
<separator />
EOF

    # Run command
    cat <<EOF
<item label="Run Command" icon="${ICONS}/actions/document-send.png">
<action name="Execute"><command>gmrun</command></action> </item>
<separator />
EOF

    # Applications
    cat <<EOF
<menu id="applications" icon="${ICONS}/places/folder.png" />
<separator />
EOF

    # The Manuals
    cat <<EOF
<menu id="manuals" icon="${USER_ICONS}/Blue_question_mark_icon.svg" />
<separator />
EOF

    # Openbox
    cat <<EOF
<menu id="openbox" icon="/usr/share/pixmaps/openbox.png" />
<separator />
EOF

    # Desktops
    cat <<EOF
<menu id="client-list-menu" icon="${ICONS}/emblems/emblem-desktop.png"/>
<separator />
EOF

    # Logout
    cat <<EOF
<item label="Log Out" icon="/usr/share/icons/Adwaita/16x16/actions/application-exit.png">
<action name="Exit"> <prompt>yes</prompt> </action> </item>
EOF

else

    # *********************
    # * Dynamic Root Menu *
    # *********************
    cat <<EOF
<menu id="root-menu" label="Openbox 3" execute="~/.config/openbox/menu_root.sh -p">
EOF

fi

# Pipe Menu Footer
[[ $pipe_menu == 1 ]] && cat <<EOF
</openbox_pipe_menu>
EOF

# ROOT Menu Footer
[[ $root_menu == 1 ]] && cat <<EOF
</menu>
</openbox_menu>
EOF

exit 0
