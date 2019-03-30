#!/bin/bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v firefox) ]] && cat <<EOF && crappy=no
<item label="Firefox" icon="/usr/share/pixmaps/aurora.png">
  <action name="Execute"><command>firefox</command></action>
</item>
EOF
[[ -x $(command -v falkon) ]] && cat <<EOF && crappy=no
<item label="Falkon" icon="/usr/share/pixmaps/falkon.png">
  <action name="Execute"><command>falkon</command></action>
</item>
EOF
[[ -x $(command -v qutebrowser) ]] && cat <<EOF && crappy=no
<item label="Qutebrowser" icon="/usr/share/icons/hicolor/scalable/apps/qutebrowser.svg">
  <action name="Execute"><command>qutebrowser</command></action>
</item>
EOF
[[ -x $(command -v netsurf-gtk3) ]] && cat <<EOF && crappy=no
<item label="Netsurf" icon="/usr/share/pixmaps/netsurf.xpm">
  <action name="Execute"><command>netsurf-gtk3</command></action>
</item>
EOF
[[ -x $(command -v midori) ]] && cat <<EOF && crappy=no
<item label="Midori" icon="/usr/share/icons/hicolor/22x22/apps/midori.png">
  <action name="Execute"><command>midori</command></action>
</item>
EOF
[[ -x $(command -v dillo) ]] && cat <<EOF && crappy=no
<item label="Dillo" icon="/usr/share/pixmaps/dillo.png">
  <action name="Execute"><command>dillo</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="/usr/share/icons/Adwaita/48x48/emotes/face-wink.png">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
