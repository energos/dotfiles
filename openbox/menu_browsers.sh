#!/usr/bin/env bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v firefox) ]] && cat <<EOF && crappy=no
<item label="Firefox" icon="${OS_SHARE}/pixmaps/firefox.png">
  <action name="Execute"><command>firefox</command></action>
</item>
EOF
[[ -x $(command -v chromium) ]] && cat <<EOF && crappy=no
<item label="Chromium" icon="${OS_SHARE}/icons/hicolor/48x48/apps/chromium-browser.png">
  <action name="Execute"><command>chromium</command></action>
</item>
EOF
[[ -x $(command -v falkon) ]] && cat <<EOF && crappy=no
<item label="Falkon" icon="${OS_SHARE}/icons/hicolor/48x48/apps/falkon.png">
  <action name="Execute"><command>falkon</command></action>
</item>
EOF
[[ -x $(command -v qutebrowser) ]] && cat <<EOF && crappy=no
<item label="Qutebrowser" icon="${OS_SHARE}/icons/hicolor/scalable/apps/qutebrowser.svg">
  <action name="Execute"><command>qutebrowser</command></action>
</item>
EOF
[[ -x $(command -v netsurf-gtk3) ]] && cat <<EOF && crappy=no
<item label="Netsurf" icon="${OS_SHARE}/pixmaps/netsurf.xpm">
  <action name="Execute"><command>netsurf-gtk3</command></action>
</item>
EOF
[[ -x $(command -v midori) ]] && cat <<EOF && crappy=no
<item label="Midori" icon="${OS_SHARE}/icons/hicolor/22x22/apps/midori.png">
  <action name="Execute"><command>midori</command></action>
</item>
EOF
[[ -x $(command -v dillo) ]] && cat <<EOF && crappy=no
<item label="Dillo" icon="${OS_SHARE}/pixmaps/dillo.png">
  <action name="Execute"><command>dillo</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="${ICONS}/emotes/22/face-wink.svg">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
