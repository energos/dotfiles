#!/bin/bash

DEBUGPORT=${DEBUGPORT:-""}
BOOKS=$1

cat <<EOF
<openbox_pipe_menu>
EOF

ls -1 ${BOOKS}/*.{pdf,djvu} | while read book
do
    fullname=${book##*/}
    shortname=${fullname%%.*}
    name=${shortname//_/ }
    [[ -n $DEBUGPORT ]] && echo "${0##*/}: \"${name}\"" > $DEBUGPORT
    cat <<EOF
<item label="${name}">
  <action name="Execute"><command>zathura "${book}"</command></action>
</item>
EOF
done

cat <<EOF
</openbox_pipe_menu>
EOF
