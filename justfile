# Send a string to the interactive session, Use ANSI escape sequences for complex text
# for example `session-send-key '\x11'` for Ctrl-q, '\e[B' for Down, '\ef' for Alt-f
#
# Will also print the screen after sending the key data
session-send-key keys: && session-get-screen
    @ZELLIJ_SESSION_NAME=chatui zellij action write-chars "`printf '{{keys}}'`"
    @sleep 1

# Restart/start a new interactive session. Will startup a zsh prompt
session-restart:
    @zellij kill-session chatui > /dev/null
    @zellij delete-session chatui > /dev/null
    @sleep 1

# Get the current screen content of the interactive terminal session
session-get-screen:
    @ZELLIJ_SESSION_NAME=chatui zellij action dump-screen /tmp/chatui_screen
    @cat /tmp/chatui_screen
    @rm /tmp/chatui_screen

test:
    export $(cat .env | xargs) && cargo test

quality-check: test
    cargo fmt
    cargo clippy
