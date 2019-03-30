#!/bin/bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v VirtualBox) ]] && cat <<EOF && crappy=no
<item label="VirtualBox" icon="/usr/share/icons/hicolor/48x48/apps/virtualbox.png">
 <action name="Execute"><command>VirtualBox</command></action>
</item>
EOF
[[ -x $(command -v virt-manager) ]] && cat <<EOF && crappy=no
<item label="Virtual Manager" icon="/usr/share/icons/hicolor/48x48/apps/virt-manager.png">
  <action name="Execute"><command>virt-manager</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="/usr/share/icons/Adwaita/48x48/emotes/face-wink.png">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
