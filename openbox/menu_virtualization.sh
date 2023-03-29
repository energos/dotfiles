#!/usr/bin/env bash

crappy=yes

cat <<EOF
<openbox_pipe_menu>
EOF
[[ -x $(command -v vmplayer) ]] && lsmod | grep "vm[mon\|net]" && cat <<EOF && crappy=no
<item label="VMware Workstation Player" icon="${OS_SHARE}/icons/hicolor/48x48/apps/vmware-player.png">
 <action name="Execute"><command>vmplayer</command></action>
</item>
EOF
[[ -x $(command -v VirtualBox) ]] && lsmod | grep vbox && cat <<EOF && crappy=no
<item label="VirtualBox" icon="${OS_ICONS}/apps/48/virtualbox.svg">
 <action name="Execute"><command>VirtualBox</command></action>
</item>
EOF
[[ -x $(command -v virt-manager) ]] && cat <<EOF && crappy=no
<item label="Virtual Manager" icon="${OS_ICONS}/apps/48/virt-manager.svg">
  <action name="Execute"><command>virt-manager</command></action>
</item>
EOF
[[ "$crappy" == "yes" ]] && cat <<EOF
<item label="Get yourself a better computer, kid!" icon="${OS_ICONS}/emotes/22/face-wink.svg">
</item>
EOF
cat <<EOF
</openbox_pipe_menu>
EOF
