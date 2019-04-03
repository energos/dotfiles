#!/bin/bash

USER_ICONS=$HOME/.local/share/icons
ICONS=/usr/share/icons/Adwaita/48x48

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

# Pipe Menu Header
cat <<EOF
<openbox_pipe_menu>
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

# Pipe Menu Footer
cat <<EOF
</openbox_pipe_menu>
EOF
