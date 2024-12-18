#!/usr/bin/env bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF

[[ -x ~/.local/share/flatpak/exports/bin/cc.arduino.IDE2 ]] && separator=1 && cat <<EOF && crappy=no
<item label="Arduino IDE v2" icon=".local/share/flatpak/exports/share/icons/hicolor/scalable/apps/cc.arduino.IDE2.svg">
  <action name="Execute"><command>.local/share/flatpak/exports/bin/cc.arduino.IDE2</command></action>
</item>
EOF

[[ -x /var/lib/flatpak/exports/bin/cc.arduino.IDE2 ]] && separator=1 && cat <<EOF && crappy=no
<item label="Arduino IDE v2" icon="/var/lib/flatpak/exports/share/icons/hicolor/scalable/apps/cc.arduino.IDE2.svg">
  <action name="Execute"><command>/var/lib/flatpak/exports/bin/cc.arduino.IDE2</command></action>
</item>
EOF

[[ -x $(command -v arduino-ide) ]] && separator=1 && cat <<EOF && crappy=no
<item label="Arduino IDE" icon="${USER_ICONS}/arduino-ide.png">
  <action name="Execute"><command>arduino-ide</command></action>
</item>
EOF

[[ -x $(command -v mcu8051ide) ]] && separator=1 && cat <<EOF && crappy=no
<item label="MCU 8051 IDE" icon="${OS_SHARE}/pixmaps/mcu8051ide.png">
  <action name="Execute"><command>mcu8051ide</command></action>
</item>
EOF

[[ -n $separator ]] && unset separator && cat <<EOF
<separator />
EOF

[[ -x $(command -v kicad) ]] && separator=1 && cat <<EOF && crappy=no
<item label="KiCad" icon="${OS_SHARE}/icons/hicolor/48x48/apps/kicad.png">
  <action name="Execute"><command>kicad</command></action>
</item>
EOF

[[ -x $(command -v distrobox) ]] && [[ -f $HOME/.local/share/applications/eagle.desktop ]] && separator=1 && cat <<EOF && crappy=no
<item label="Eagle PCB" icon="${USER_ICONS}/eagle.png">
  <action name="Execute"><command>gio launch $HOME/.local/share/applications/eagle.desktop</command></action>
</item>
EOF

[[ -n $separator ]] && unset separator && cat <<EOF
<separator />
EOF

[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="${OS_ICONS}/emotes/22/face-wink.svg">
</item>
<separator />
EOF
cat <<EOF
<menu id="datasheets" label="Datasheets" icon="${OS_ICONS}/places/32/folder-documents.svg" execute="~/.config/openbox/menu_books.sh $HOME/Datasheets" />
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
