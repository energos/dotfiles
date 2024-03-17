# Zathura-Calibre

`bks.sh` is a simple bash script that aims to improve the workflow of quickly fuzzy finding a
book within a calibre database and offers a fast way to open the last read book.
It caches the calibre database metadata internally to display the menu _as fast as possible_.

The script helps bridge the gap between both pieces of software and adds functionality
geared toward window manager users who prefer the fastest way of performing tasks.

## Setup

First, make sure you have the following dependencies installed on your system:

- `calibre`
- `jq`
- `rofi`

To install into `/usr/local/bin`

```
sudo wget -P /usr/local/bin https://raw.githubusercontent.com/kamui-fin/zathura-calibre/master/bks.sh
sudo chmod +x /usr/local/bin/bks.sh
```

Now, you can run `bks.sh` from anywhere you'd like. Two (optional) parameters to note are:

- `-sync`: Updates the cache with the latest metadata of the calibre db. Run after a change is made to the database.
- `-prev`: Opens the most recently read book with zathura

### Keyboard

To use this script to its full potential, two keyboard shortcuts should be configured. Here's an example setup:

1. `Alt+Shift+B` -> `bks.sh -prev`
2. `Alt+Shift+M` -> `bks.sh`
