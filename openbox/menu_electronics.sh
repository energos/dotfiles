#!/bin/bash

USER_ICONS=$HOME/.local/share/icons
ICONS=/usr/share/icons/breeze

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v kicad) ]] && cat <<EOF && crappy=no
<item label="KiCad" icon="/usr/share/icons/hicolor/48x48/apps/kicad.png">
  <action name="Execute"><command>kicad</command></action>
</item>
EOF
[[ -x $(command -v eagle) ]] && cat <<EOF && crappy=no
<item label="Eagle PCB" icon="/usr/share/pixmaps/eagle-7.3.0-icon50.png">
  <action name="Execute"><command>eagle</command></action>
</item>
EOF
[[ -x $(command -v mcu8051ide) ]] && cat <<EOF && crappy=no
<item label="MCU 8051 IDE" icon="/usr/share/pixmaps/mcu8051ide.png">
  <action name="Execute"><command>mcu8051ide</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="${ICONS}/emotes/22/face-wink.svg">
</item>
EOF
cat <<EOF
<menu id="datasheets" label="Datasheets" icon="${USER_ICONS}/book-viewer.png" execute="~/.config/openbox/menu_books.sh $HOME/Datasheets" />
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
