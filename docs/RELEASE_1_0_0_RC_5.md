# ExMCP 1.0.0-rc.5

This release removes pre-2.0 duplication while preserving JSON-RPC wire output
and documented public return values for MCP `2024-11-05`, `2025-03-26`,
`2025-06-18`, and `2025-11-25`.

The one intentional behavior change is client-side URL elicitation routing.
An `elicitation/create` request with `mode: "url"` now calls
`handle_url_elicitation/3` when the client handler implements it. A handler that
only implements `handle_elicitation_create/3` keeps working through a warned
fallback and receives the URL payload in the callback's second argument.

No server-to-client JSON-RPC payload changes as part of that fix.
