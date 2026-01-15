-- Pull in the wezterm API
local wezterm = require 'wezterm'
local mux = wezterm.mux

-- Use config_builder for better error messages (if available)
local config = {}
if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- Theme switching based on system appearance
function scheme_for_appearance(appearance)
    if appearance:find "Dark" then
        return "Catppuccin Mocha"
    else
        return "Catppuccin Latte"
    end
end

config.color_scheme = scheme_for_appearance(wezterm.gui.get_appearance())

-- Needed for better iPython color support
-- REMOVE this entire block
-- config.colors = {
--     indexed = {
--         [16] = "#000000"
--     }
-- }

-- Font config: primary font with fallback to icon font
-- config.font = wezterm.font_with_fallback({
--     "Monaspace Neon NF",
--     { family = "Symbols Nerd Font Mono", scale = 1 } -- Ensures correct rendering of Nerd Font icons
-- })

config.font = wezterm.font_with_fallback({
    "Maple Mono NF",
    { family = "Symbols Nerd Font Mono", scale = 1 } -- Ensures correct rendering of Nerd Font icons
})

config.font_size = 16

-- Disable all ligatures (optional: can be relaxed if you like `==`, `->`, etc. ligatures)
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- Initial window size in cells
config.initial_rows = 54
config.initial_cols = 89

-- Place window on screen top right (exact placement tailored to your resolution)
wezterm.on('gui-startup', function(cmd)
    local tab, pane, window = mux.spawn_window(cmd or {})
    window:gui_window():set_position(
        1736,
        wezterm.gui.screens()['active']['height'] - 2134
    )
end)

-- Optional alternative positions (you’ve commented them out — nice for quick testing)
-- e.g., top left:
-- wezterm.on('gui-startup', function(cmd)
--   local tab, pane, window = mux.spawn_window(cmd or {})
--   window:gui_window():set_position(0, 0)
-- end)

-- Disable bell, auto-reload, and update checks for minimal distraction
config.audible_bell = "Disabled"
config.automatically_reload_config = false
config.check_for_updates = false

-- Default cursor style
config.default_cursor_style = "SteadyBar"

-- Final return
return config
