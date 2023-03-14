#!/usr/bin/env bash

DEBUGPORT=${DEBUGPORT:-""}
USER_ICONS=$HOME/.local/share/icons
ICONS=/usr/share/icons/breeze
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
<menu id="${book}" label="${name}" icon="${ICONS}/places/32/folder-documents.svg" execute="~/.config/openbox/menu_books.sh ${book}" />
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
<menu id="RecentFiles" label="Recent Files" icon="${ICONS}/places/32/folder-recent.svg">
EOF
    awk 'BEGIN { RS=""; FS="\n" } /^[^#]/ { print $11 " " $1 }' "$HOME/.local/share/zathura/history" | sort -r | head -n 10 | awk -F '[\\[\\]]' '{ print $2 }' | xargs -d '\n' -I % sh -c '{ [[ -f "%" ]] && echo "%"; }' | while read -r book
    do
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
# TODO:    fix crashes with weird characters in zathura history file
# TODO:    https://github.com/koalaman/shellcheck/wiki/SC2012
# TODO:    okular recent files
