#!/usr/bin/env bash

ICONS=/usr/share/icons/breeze

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v cmus) ]] && cat <<EOF && crappy=no
<item label="C* Music Player" icon="/usr/share/icons/Adwaita/48x48/mimetypes/audio-x-generic.png">
  <action name="Execute"><command>pqp -h cmus</command></action>
</item>
EOF
[[ -x $(command -v smplayer) ]] && cat <<EOF && crappy=no
<item label="SMPlayer Media Player" icon="/usr/share/icons/hicolor/scalable/apps/smplayer.svg">
  <action name="Execute"><command>smplayer</command></action>
</item>
EOF
[[ -x $(command -v smtube) ]] && cat <<EOF && crappy=no
  <item label="YouTube Browser" icon="/home/energos/.local/share/icons/youtube.png">
    <action name="Execute"><command>smtube</command></action>
  </item>
EOF
[[ -x $(command -v vlc) ]] && cat <<EOF && crappy=no
<item label="VLC Media Player" icon="${ICONS}/apps/48/vlc.svg">
  <action name="Execute"><command>vlc</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="${ICONS}/emotes/22/face-wink.svg">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
