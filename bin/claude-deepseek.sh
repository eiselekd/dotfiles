#!/bin/bash
# Route Claude Code requests to the local free-claude-code proxy
export ANTHROPIC_BASE_URL="http://localhost:8082"
export ANTHROPIC_AUTH_TOKEN="freecc"

# Shield the real Anthropic API key from the proxy server
unset ANTHROPIC_API_KEY

# Resolve the absolute path of this script, even if it is a symlink
REAL_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(cd "$(dirname "${REAL_SCRIPT}")" && pwd)"
PROXY_DIR="$(cd "${SCRIPT_DIR}/../misc/free-claude-code" && pwd)"

# Ensure the proxy server is running
if ! curl -s http://127.0.0.1:8082/v1/models -H "Authorization: Bearer freecc" > /dev/null; then
    echo "Starting free-claude-code proxy in background..."
    (cd "${PROXY_DIR}" && uv run uvicorn server:app --host 127.0.0.1 --port 8082 --timeout-graceful-shutdown 5 > "${PROXY_DIR}/fcc.log" 2>&1 &)
    sleep 2
fi

# Execute Claude Code with all passed arguments
exec claude "$@"

