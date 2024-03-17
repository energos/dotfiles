#!/bin/sh

book_dir=$HOME/books
book_cache=$HOME/.cache/bks
history_file=$HOME/.local/share/zathura/history

declare -A BOOKS

_zathura() {
    zathura "$1" &
    disown
}

last_open() {
    last_file=$(grep '/home/kamui/books/' $history_file | sed 's/^\[//' | sed 's/\]$//' | tail -1)
    _zathura "$last_file"
}

build_db() {
    echo "Generating db cache..."
    calibredb list -f title,formats --with-library ~/books --sort-by title --ascending --for-machine | jq -r '.[] | .formats[0], .title' > $book_cache
    echo "Done!"
}

load_cache() {
    [ ! -f $book_cache ] && build_db
    while read -r file; do
        read title; 
        echo $title $file
        BOOKS["$title"]="$file"
    done < $book_cache
}

list_bk() {
    for i in "${!BOOKS[@]}"; do
        echo "$i"
    done
}

die() {
    echo $1
    exit 1
}

if [[ $1 == "-prev" ]]; then
    last_open
    exit
elif [[ $1 == "-sync" ]]; then
    build_db
fi

load_cache
selected=$(list_bk | rofi -dmenu -i -matching fuzzy -no-custom -location 0 -p "Book > ")

if [ -z $selected ]; then
    die "Nothing selected"
fi

bfile=${BOOKS[$selected]}
_zathura "$bfile"
