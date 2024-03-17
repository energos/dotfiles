#!/usr/bin/env bash

book_dir=$HOME/books
book_cache=$HOME/.cache/bks

_open() {
    okular "$1" &
    disown
}

build_db() {
    echo "Generating db cache..."
    calibredb list -f title,authors,tags,formats --with-library ~/Library --sort-by title --ascending --for-machine | jq -r '.[] | .formats[0], .title, .authors, .tags[0]' > $book_cache
    echo "Done!"
}

load_cache() {
    [ ! -f $book_cache ] && build_db
    while read -r file; do
        read title
        read authors
        read tags
        TITLES+=("$title / $authors ($tags)")
        FILES+=("$file")
    done < $book_cache
}

list_bk() {
    for i in "${TITLES[@]}"; do
        echo "$i"
    done
}

die() {
    # echo "=========== OUCH ==========="
    echo -e $1
    exit 1
}

if [[ "$1" == "-sync" ]]; then
    build_db
fi

load_cache

selected=$(list_bk | rofi -dmenu -i -no-custom -location 0 -theme-str 'window {width: 50%;}' -format d -p "Library")
if [ -z "$selected" ]; then
    die "Nothing selected"
fi
selected=$((selected-1))

bfile=${FILES[$selected]}
btitle=${TITLES[$selected]}
# die "Selected: $selected\nTitle: $btitle\nFile: $bfile"
_open "$bfile"
