#!/bin/bash

DEBUGPORT=${DEBUGPORT:-""}
USER_ICONS=$HOME/.local/share/icons
ICONS=/usr/share/icons/Adwaita/48x48
BOOKS=$*

shortname ()
{
    book=${book%/}
    name=${book##*/}
    name=${name%.*}
    name=${name//_/ }
    [[ -n ${DEBUGPORT} ]] && echo "${0##*/}: \"${name}\"" > "${DEBUGPORT}"
}

cat <<EOF
<openbox_pipe_menu>
EOF

ls -1d "${BOOKS}"/*/ 2> /dev/null | while read -r book
do
    shortname
    cat <<EOF
<menu id="${book}" label="${name}" icon="${ICONS}/places/folder.png" execute="~/.config/openbox/menu_books.sh ${book}" />
EOF
done

ls -1 "${BOOKS}"/*.{pdf,djvu} 2> /dev/null | while read -r book
do
    shortname
    cat <<EOF
<item label="${name}" icon="${USER_ICONS}/pdf.svg">
  <action name="Execute"><command>xdg-open "${book}"</command></action>
</item>
EOF
done

# Get recent files, zathura only
if [[ "$BOOKS" == "${HOME}/Books" ]]; then
    cat <<EOF
<separator />
EOF
    cat <<EOF
<menu id="RecentFiles" label="Recent Files" icon="${ICONS}/actions/document-open-recent.png">
EOF
    awk 'BEGIN { RS=""; FS="\n" } /^[^#]/ { print $11 " " $1 }' "${HOME}"/.local/share/zathura/history | sort -r | head -10 | while read -r book
    do
        book=${book%\]}
        book=${book#*\[}
        shortname
        cat <<EOF
<item label="${name}" icon="${USER_ICONS}/pdf.svg">
  <action name="Execute"><command>xdg-open "${book}"</command></action>
</item>
EOF
    done
    cat<<EOF
</menu>
EOF
fi

cat <<EOF
</openbox_pipe_menu>
EOF

# WARNING: weird characters like '\' and '!' are problematic
# TODO:    https://github.com/koalaman/shellcheck/wiki/SC2012
# TODO:    okular recent files
