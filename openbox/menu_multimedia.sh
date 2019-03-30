#!/bin/bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v smplayer) ]] && cat <<EOF && crappy=no
<item label="Media Player" icon="/usr/share/icons/hicolor/64x64/apps/smplayer.png">
  <action name="Execute"><command>smplayer</command></action>
</item>
EOF
[[ -x $(command -v smtube) ]] && cat <<EOF && crappy=no
  <item label="YouTube Browser" icon="/home/energos/.local/share/icons/youtube.png">
    <action name="Execute"><command>smtube</command></action>
  </item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="/usr/share/icons/Adwaita/48x48/emotes/face-wink.png">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
