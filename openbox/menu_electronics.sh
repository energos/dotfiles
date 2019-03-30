#!/bin/bash

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
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="/usr/share/icons/Adwaita/48x48/emotes/face-wink.png">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
