-- Pull in the wezterm API
local wezterm = require 'wezterm'
local mux = wezterm.mux

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then config = wezterm.config_builder() end

--- This is where you actually apply your config choices
--- Color scheme
function scheme_for_appearance(appearance)
    if appearance:find "Dark" then
        return "Catppuccin Mocha"
    else
        return "Catppuccin Latte"
    end
end

config.color_scheme = scheme_for_appearance(wezterm.gui.get_appearance())

--- Font Config
-- config.font = wezterm.font("MesloLGL Nerd Font")

config.font = wezterm.font("JetBrainsMono NF")

-- config.font = wezterm.font_with_fallback({
--     "JetBrains Mono", {family = "Symbols Nerd Font Mono", scale = 0.75}
-- })

-- font size
config.font_size = 16

-- disable ligatures for all fonts
config.harfbuzz_features = {'calt=0', 'clig=0', 'liga=0'}

--- Window Config
-- initial window size
config.initial_rows = 48
config.initial_cols = 92

-- initial window position - maximized
-- wezterm.on('gui-startup', function(cmd)
--     local tab, pane, window = mux.spawn_window(cmd or {})
--     -- window:gui_window():maximize()
-- end)

-- initial window position - top right
-- wezterm.on('gui-startup', function(cmd)
--     local tab, pane, window = mux.spawn_window(cmd or {})
--     -- window:gui_window():maximize()
--     -- window:gui_window():set_position(1798, 106)
--     window:gui_window():set_position(1798,
--                                      wezterm.gui.screens()['active']['height'] -
--                                          2134)
-- end)

-- initial window position - top left
wezterm.on('gui-startup', function(cmd)
    local tab, pane, window = mux.spawn_window(cmd or {})
    -- window:gui_window():maximize()
    window:gui_window():set_position(0, 0)
end)

-- disable audible beep
config.audible_bell = "Disabled"
-- don't auto reload config
config.automatically_reload_config = false

-- don't check for updates - don't the distraction of being notified of updates
config.check_for_updates = false
-- config.check_for_updates_interval_seconds = 86400

-- and finally, return the configuration to wezterm
return config
-- Nothing To See Here --
