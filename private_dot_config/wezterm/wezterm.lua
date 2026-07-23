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

-- Font config: primary font with fallback to icon font
config.font = wezterm.font_with_fallback({
    "Maple Mono NF",
    { family = "Symbols Nerd Font Mono", scale = 1 } -- Ensures correct rendering of Nerd Font icons
})

config.font_size = 14

-- Disable all ligatures (optional: can be relaxed if you like `==`, `->`, etc. ligatures)
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- Fallback size in cells, used only until the gui-startup layout below runs
-- (and by windows spawned outside gui-startup, e.g. a second `wezterm start`).
config.initial_rows = 54
config.initial_cols = 89

------------------------------------------------------------------------
-- Screen-aware window layout
--
-- Everything is a fraction of the active screen, in the spirit of the
-- Emacs frame.el module: no hard-coded pixel positions.  The window is
-- anchored to the RIGHT edge of the screen.
--
--   * Narrow screens (the built-in MacBook Pro 16" panel): right HALF,
--     which reproduces the old hard-coded position/size.
--   * Wide screens (external monitor): right TWO THIRDS, so neovim gets
--     the extra room.
--
-- Note: wezterm reports screen dimensions in physical pixels here, and
-- `set_position` uses the same coordinate space, so the two compose
-- without any DPI conversion.  If the thresholds ever look wrong, open
-- the debug overlay (Ctrl+Shift+L) and evaluate `wezterm.gui.screens()`
-- to see what your displays actually report.
------------------------------------------------------------------------

local layout = {
    -- Inset around the window, in pixels.
    margin = 8,

    -- Extra space kept clear at the top for the macOS menu bar.  Your old
    -- config placed the window 100 px from the top on the MacBook panel
    -- (2234 - 2134), so that behaviour is preserved here.
    top_offset = 100,

    -- Screens at least this wide count as "big".  The MacBook Pro 16"
    -- panel reports 3456 px wide; a 4K display reports 3840, 5K reports
    -- 5120.  3800 therefore separates the built-in panel from any larger
    -- external monitor.  (If your external display runs a scaled mode
    -- that reports fewer pixels, lower this accordingly.)
    wide_min_width = 3800,

    narrow_fraction = 1 / 2, -- MacBook: right half
    wide_fraction = 2 / 3,   -- big monitor: right two thirds

    -- `set_inner_size` sizes the terminal *content* area; the title bar
    -- is added on top of it.  This allowance keeps the bottom edge on
    -- screen.  56 px is about right for a 2x-scaled macOS title bar;
    -- nudge it if the window overshoots or undershoots vertically.
    titlebar_allowance = 56,
}

-- Compute the desired window rectangle for the active screen.
local function desired_geometry()
    local screen = wezterm.gui.screens().active
    local fraction = (screen.width >= layout.wide_min_width)
        and layout.wide_fraction
        or layout.narrow_fraction

    local width = math.floor(screen.width * fraction) - 2 * layout.margin
    local height = screen.height - layout.top_offset - 2 * layout.margin

    -- Anchor to the right edge of the screen.
    local x = screen.x + screen.width - width - layout.margin
    local y = screen.y + layout.top_offset + layout.margin

    return x, y, width, height
end

-- Apply the layout to a GuiWindow.
local function apply_layout(gui_window)
    local x, y, width, height = desired_geometry()
    gui_window:set_inner_size(width, height - layout.titlebar_allowance)
    gui_window:set_position(x, y)
end

-- Lay out the initial window at startup.
wezterm.on('gui-startup', function(cmd)
    local _, _, window = mux.spawn_window(cmd or {})
    apply_layout(window:gui_window())
end)

-- Re-apply the layout on demand with Cmd+Shift+R -- useful after dragging
-- the window to a different monitor, since the fraction is chosen from
-- whichever screen is active at the time.
config.keys = {
    {
        key = 'r',
        mods = 'CMD|SHIFT',
        action = wezterm.action_callback(function(window, _pane)
            apply_layout(window)
        end),
    },
}

------------------------------------------------------------------------

-- Disable bell, auto-reload, and update checks for minimal distraction
config.audible_bell = "Disabled"
config.automatically_reload_config = false
config.check_for_updates = false

-- Default cursor style
config.default_cursor_style = "SteadyBar"

-- Final return
return config
