#!/usr/bin/env bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v cmus) ]] && cat <<EOF && crappy=no
<item label="C* Music Player" icon="${USER_ICONS}/Audio-x-generic.svg">
<action name="Execute"><command>pqp -h cmus</command></action>
</item>
EOF
[[ -x $(command -v deadbeef) ]] && cat <<EOF && crappy=no
<item label="DeaDBeeF" icon="${OS_SHARE}/icons/hicolor/48x48/apps/deadbeef.png">
<action name="Execute"><command>pqp deadbeef</command></action>
</item>
EOF
[[ -x $(command -v smplayer) ]] && cat <<EOF && crappy=no
<item label="SMPlayer Media Player" icon="${OS_SHARE}/icons/hicolor/scalable/apps/smplayer.svg">
<action name="Execute"><command>smplayer</command></action>
</item>
EOF
[[ -x $(command -v smtube) ]] && cat <<EOF && crappy=no
<item label="YouTube Browser" icon="${USER_ICONS}/youtube.png">
<action name="Execute"><command>smtube</command></action>
</item>
EOF
[[ -x $(command -v vlc) ]] && cat <<EOF && crappy=no
<item label="VLC Media Player" icon="${OS_ICONS}/apps/48/vlc.svg">
<action name="Execute"><command>vlc</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="${OS_ICONS}/emotes/22/face-wink.svg">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
