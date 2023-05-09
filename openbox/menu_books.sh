#!/usr/bin/env bash

if [[ -f ~/.pqprc ]]; then
    eval $(grep '^DEBUGPORT=' ~/.pqprc)
fi

DEBUGPORT=${DEBUGPORT:-""}
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
<menu id="${book}" label="${name}" icon="${OS_ICONS}/places/32/folder-documents.svg" execute="~/.config/openbox/menu_books.sh ${book}" />
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

# Get recent files, okular and zathura only
if [[ "$BOOKS" == "${HOME}/Books" ]]; then
    cat <<EOF
<separator />
EOF
    cat <<EOF
<menu id="RecentFiles" label="Recent Files" icon="${OS_ICONS}/places/32/folder-recent.svg">
EOF
    case $(xdg-mime query default application/pdf) in
        *zathura*)
            awk 'BEGIN { RS=""; FS="\n" } /^[^#]/ { print $11 " " $1 }' "$HOME/.local/share/zathura/history" | sort -r | awk -F '[\\[\\]]' '$2 ~ /^\// { print $2 }' | head -n 10 | while read -r book
            do
                shortname
                if [[ -f "$book" ]]; then
                    cat <<EOF
<item label="${name}" icon="${USER_ICONS}/pdf.svg">
  <action name="Execute"><command>xdg-open "${book}"</command></action>
</item>
EOF
                fi
            done
            ;;
        *)
            grep ^File ~/.config/okularrc | sed 's/File//g' | sort -n -r | awk -F '=' '{ print $2 }' | envsubst | while read -r book
            do
                shortname
                if [[ -f "$book" ]]; then
                    cat <<EOF
<item label="${name}" icon="${USER_ICONS}/pdf.svg">
  <action name="Execute"><command>xdg-open "${book}"</command></action>
</item>
EOF
                fi
            done
            ;;
    esac
    cat<<EOF
</menu>
EOF
fi

cat <<EOF
</openbox_pipe_menu>
EOF

# WARNING: weird characters like '\' and '!' are problematic
# TODO:    fix crashes with weird characters in zathura history file - xargs? - maybe fixed
# TODO:    https://github.com/koalaman/shellcheck/wiki/SC2012
