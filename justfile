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
    # run all workspaces tests but skip doctests, as they compile very slow and we dont have any
    cargo test --workspace --lib --bins --tests --quiet

lint: test
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Check for disallowed #[allow(dead_code)] attributes without justification
    # Allow if preceded by a comment explaining why (within 2 lines)
    matches=$(rg --type rust -U '#\[allow\(dead_code\)\]' src/ crates/ 2>/dev/null || true)
    if [ -n "$matches" ]; then
        echo "Warning: Found #[allow(dead_code)] attribute(s)."
        echo "Dead code should generally be removed. If keeping it is necessary,"
        echo "ensure there's a comment explaining why within 2 lines above the attribute."
        echo ""
        echo "$matches"
        echo ""
        echo "Consider removing unused code or adding proper justification."
        # Note: This is a warning, not an error, so we continue
    fi
    
    cargo fmt --all
    cargo clippy --workspace -- -Dwarnings
