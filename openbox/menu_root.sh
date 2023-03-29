#!/usr/bin/env bash

DEBUGPORT=${DEBUGPORT:-""}

# Get default navigator
# Exclude some sub-menus if this is a crappy host
[[ -f ~/.pqprc ]] && . ~/.pqprc > /dev/null
navigator=${NAVIGATOR:-firefox}

# Ugly, ugly, very ugly
case $navigator in
    falkon)
        navigator_icon=${OS_SHARE}/icons/hicolor/48x48/apps/falkon.png
        ;;
    qutebrowser)
        navigator_icon=${OS_SHARE}/icons/hicolor/scalable/apps/qutebrowser.svg
        ;;
    netsurf)
        navigator_icon=${OS_SHARE}/pixmaps/netsurf.xpm
        ;;
    midori)
        navigator_icon=${OS_SHARE}/icons/hicolor/22x22/apps/midori.png
        ;;
    dillo)
        navigator_icon=${OS_SHARE}/pixmaps/dillo.png
        ;;
    firefox)
        navigator_icon=${OS_SHARE}/pixmaps/firefox.png
        ;;
    *)
        navigator_icon=${OS_ICONS}/apps/48/internet-web-browser.svg
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
<separator label="${OS_NAME}! @ ${HOSTNAME}" />
<separator />
EOF

    # Terminal & File Manager
    cat <<EOF
<item label="URxvt Terminal" icon="${USER_ICONS}/Utilities-terminal.svg">
<action name="Execute"><command>pqp -n</command></action> </item>
<menu id="Terminals" label="Terminals" icon="${USER_ICONS}/utilities-terminal.svg">
<item label="URxvt" icon="${USER_ICONS}/Utilities-terminal.svg">
<action name="Execute"><command>pqp -n</command></action> </item>
<item label="XTerm" icon="${OS_ICONS}/apps/48/xterm.svg">
<action name="Execute"><command>xterm</command></action> </item>
EOF
    [[ -x $(command -v konsole) ]] && cat <<EOF
<item label="Konsole" icon="${USER_ICONS}/utilities-terminal.svg">
<action name="Execute"><command>pqp -n konsole</command></action> </item>
EOF
   cat <<EOF
</menu> <separator />
<item label="Midnight Commander" icon="${USER_ICONS}/mc.svg">
<action name="Execute"><command>pqp -n mc</command></action> </item>
EOF
    [[ -x $(command -v dolphin) ]] && cat <<EOF
<item label="Dolphin File Manager" icon="${OS_ICONS}/apps/48/system-file-manager.svg">
<action name="Execute"><command>dolphin</command></action> </item>
EOF
    [[ -x $(command -v thunar) ]] && cat <<EOF
<item label="Thunar File Manager" icon="${OS_ICONS}/apps/48/system-file-manager.svg" >
<action name="Execute"><command>thunar</command></action> </item>
EOF
    [[ -x $(command -v pcmanfm) ]] && cat <<EOF
<item label="PCManFM File Manager" icon="${OS_ICONS}/apps/48/system-file-manager.svg" >
<action name="Execute"><command>pcmanfm</command></action> </item>
EOF
    cat <<EOF
<separator />
EOF

    # Internet
    cat <<EOF
<item label="${navigator^}" icon="${navigator_icon}">
<action name="Execute"><command>pqp -n navigator</command></action> </item>
<menu id="browsers" label="Browsers" icon="${OS_ICONS}/apps/48/internet-web-browser.svg" execute="~/.config/openbox/menu_browsers.sh" />
<separator />
EOF

    # Editors
    cat <<EOF
<item label="Emacs" icon="${OS_ICONS}/apps/48/emacs.svg">
<action name="Execute"><command>pqp -n emacs</command></action> </item>
<item label="Geany" icon="${OS_SHARE}/icons/hicolor/48x48/apps/geany.png">
<action name="Execute"><command>geany</command></action> </item>
EOF
    [[ -x $(command -v kate) ]] && cat <<EOF
<item label="Kate" icon="${OS_ICONS}/apps/48/kate.svg">
<action name="Execute"><command>kate</command></action> </item>
EOF
    cat <<EOF
<separator />
EOF

    # Electronics/CAD
    [[ $MENU_ELECTRONICS != no ]] && cat <<EOF
<menu id="cad" label="Electronics" icon="${USER_ICONS}/applications-electronics.svg" execute="~/.config/openbox/menu_electronics.sh" />
<separator />
EOF

    # Multimedia
    cat <<EOF
<menu id="multimedia" label="Multimedia" icon="${OS_ICONS}/categories/32/applications-multimedia.svg" execute="~/.config/openbox/menu_multimedia.sh" />
<separator />
EOF

    # eBooks
    cat <<EOF
<menu id="ebooks" label="eBooks" icon="${OS_ICONS}/places/32/folder-documents.svg" execute="~/.config/openbox/menu_books.sh $HOME/Books" />
EOF
    [[ -x $(command -v calibre) ]] && cat <<EOF
<item label="Calibre eBook Manager" icon="${OS_SHARE}/icons/hicolor/48x48/apps/calibre-gui.png">
<action name="Execute"><command>calibre</command></action> </item>
EOF
    cat <<EOF
<separator />
EOF

    # Office
    cat <<EOF
<item label="LibreOffice Calc" icon="${OS_SHARE}/icons/hicolor/48x48/apps/libreoffice-calc.png">
<action name="Execute"><command>localc</command></action> </item>
<item label="LibreOffice Writer" icon="${OS_SHARE}/icons/hicolor/48x48/apps/libreoffice-writer.png">
<action name="Execute"><command>lowriter</command></action> </item>
<separator />
EOF

    # Virtual Machines
    [[ $MENU_VIRTUALIZATION != no ]] && cat <<EOF
<menu id="virtualization" label="Virtualization" icon="${OS_ICONS}/places/32/folder.svg" execute="~/.config/openbox/menu_virtualization.sh" />
<separator />
EOF

    # Run command
    cat <<EOF
<item label="Run Command" icon="${USER_ICONS}/document-send.png">
<action name="Execute"><command>gmrun</command></action> </item>
<separator />
EOF

    # Applications
    cat <<EOF
<menu id="applications" icon="${OS_ICONS}/places/32/folder.svg" />
<separator />
EOF

    # The Manuals
    cat <<EOF
<menu id="manuals" icon="${USER_ICONS}/Blue_question_mark_icon.svg" />
<separator />
EOF

    # About $OS_NAME
    cat <<EOF
<menu id="about-${OS_NAME}" icon="${OS_LOGO}" />
<separator />
EOF

    # Openbox
    cat <<EOF
<menu id="openbox" icon="${OS_SHARE}/pixmaps/openbox.png" />
<separator />
EOF

    # Desktops
    cat <<EOF
<menu id="client-list-menu" icon="${USER_ICONS}/window-list.png"/>
<separator />
EOF

    # Exit
    cat <<EOF
<menu id="menu-exit" icon="${OS_ICONS}/actions/32/application-exit.svg" label="Exit">
<item label="Log Out" icon="${OS_ICONS}/actions/32/system-log-out.svg">
<action name="Exit"> <prompt>yes</prompt> </action>
</item>
<separator />
<item label="Reboot" icon="${OS_ICONS}/actions/32/system-reboot.svg">
<action name="Execute"> <command>loginctl reboot</command>
<prompt>Are you sure you want to Reboot?</prompt>
</action>
</item>
<item label="Shutdown" icon="${OS_ICONS}/actions/32/system-shutdown.svg">
<action name="Execute"> <command>loginctl poweroff</command>
<prompt>Are you sure you want to Shutdown?</prompt>
</action> </item> </menu>
<separator />
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
