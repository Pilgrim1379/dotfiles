# ── Catppuccin Mocha ─────────────────────────────────────────
# Text
local text=$'\e[38;2;205;214;244m'       # #cdd6f4
local subtext1=$'\e[38;2;186;194;222m'   # #bac2de
local subtext0=$'\e[38;2;166;173;200m'   # #a6adc8

# Overlays (useful for dimmed/secondary output)
local overlay2=$'\e[38;2;147;153;178m'   # #9399b2
local overlay1=$'\e[38;2;127;132;156m'   # #7f849c
local overlay0=$'\e[38;2;108;112;134m'   # #6c7086

# Accents
local rosewater=$'\e[38;2;245;224;220m'  # #f5e0dc
local flamingo=$'\e[38;2;242;205;205m'   # #f2cdcd
local pink=$'\e[38;2;245;194;231m'       # #f5c2e7
local mauve=$'\e[38;2;203;166;247m'      # #cba4f7
local red=$'\e[38;2;243;139;168m'        # #f38ba8
local maroon=$'\e[38;2;235;160;172m'     # #eba0ac
local peach=$'\e[38;2;250;179;135m'      # #fab387
local yellow=$'\e[38;2;249;226;175m'     # #f9e2af
local green=$'\e[38;2;166;227;161m'      # #a6e3a1
local teal=$'\e[38;2;148;226;213m'       # #94e2d5
local sky=$'\e[38;2;137;220;235m'        # #89dceb
local sapphire=$'\e[38;2;116;199;236m'   # #74c7ec
local blue=$'\e[38;2;137;180;250m'       # #89b4fa
local lavender=$'\e[38;2;180;190;254m'   # #b4befe

# Modifiers
local reset=$'\e[0m'
local bold=$'\e[1m'
local dim=$'\e[2m'
local italic=$'\e[3m'
local underline=$'\e[4m'
# ─────────────────────────────────────────────────────────────

# Notes about the subtler colours:

# subtext0/1 are great for secondary info or "still running…" type messages that shouldn't compete with headers.
# overlay0/1/2 work well for dimming timestamps or paths that are informational but not primary.
# maroon sits between red and peach — useful when you want a warning that isn't as alarming as red.
# flamingo and rosewater are very light pinks — they read as near-white in some terminals, so test before using as body text.
# dim (\e[2m) can be combined with a colour for a faded variant without needing a separate RGB value, e.g. ${dim}${green}.
