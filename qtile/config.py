# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.log_utils import logger
from libqtile.command import lazy, Client
from libqtile import layout, bar, hook, widget

import libqtile.layout.xmonad

import sh

mod = "mod4"
alt = "mod1"
shift = "shift"

keys = [
    # windows style alt-tab/alt-shift-tab
    Key([alt], "Tab", lazy.layout.next()),
    Key([alt, "shift"], "Tab", lazy.layout.previous()),

    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "Return", lazy.layout.swap_main()),
    Key([mod, shift], "l", lazy.layout.grow_main()),
    Key([mod, shift], "h", lazy.layout.shrink_main()),
    Key([mod], "f", lazy.layout.flip()),

    Key([mod], "h", lazy.prev_screen()),
    Key([mod], "s", lazy.next_screen()),

    Key([mod], "t", lazy.window.toggle_floating()),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    # Key([mod], "r", lazy.spawncmd()),


    Key([mod], "w", lazy.window.kill()),
    Key(["control", alt], "l", lazy.spawn("light-locker-command -l")),
    Key(["control"], "Return", lazy.spawn("dmenu_run_history -b -i")),

    #Key([], "Super_L", lazy.spawn("scr toggle /home/tjarvstrand/scr.log")),
    Key([], "XF86Suspend", lazy.spawn("systemctl suspend")),
    Key([], "XF86WLAN", lazy.spawn("toggle-wifi")),
    Key([], "XF86AudioMute", lazy.spawn("/home/tjarvstrand/bin/toggle-mute")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("/home/tjarvstrand/bin/volume dec 10")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("/home/tjarvstrand/bin/volume inc 10")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("light -A 5")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("light -U 5")),
    Key(["control"], "XF86MonBrightnessUp", lazy.spawn("light -A 10")),
    Key(["control"], "XF86MonBrightnessDown", lazy.spawn("light -U 10")),

    Key([], "Print", lazy.spawn("xfce4-screenshooter -f -s /home/tjarvstrand/Dropbox/Pictures/Screenshots")),

]

group_names = [str(i) for i in [1, 2, 3, 4, 5]]
groups = [Group(i) for i in group_names]

for i in groups:
    # mod1 + letter of group = switch to group
    keys.append(Key([mod], i.name, lazy.group[i.name].toscreen()))

    # mod1 + shift + letter of group = switch to & move focused window to group
    keys.append(Key([mod, "shift"], i.name, lazy.window.togroup(i.name)))

layouts = [
    libqtile.layout.xmonad.MonadTall(ratio = 0.66,
                                     margin = 0,
                                     border_width = 1,
                                     single_border_width = 0,
                                     border_normal = "000000",
                                     border_focus = "ff0000",
                                     new_window_replaces_current = True),
    layout.Max()
]

widget_defaults = dict(
    font='monospace',
    fontsize=11,
    padding=0,
    margin_x = 2
)

fontsize = 16

def widgets():
    return [widget.Spacer(length = 10),
            widget.AGroupBox(fontsize = fontsize, foreground = '18BAEB', visible_groups = group_names),
            widget.Sep(margin_x = 5, foreground = '555555'),
            widget.WindowName(padding = 4, width = bar.STRETCH, fontsize = fontsize),
            widget.KeyboardLayout(configured_keyboards = ["se", "custom"], fontsize = fontsize),
            widget.Sep(padding = 8, foreground = '555555'),
            widget.CPUGraph(),
            widget.CheckUpdates(distro = 'Ubuntu',
                                display_format = '{updates} updates',
                                update_interval = 600,
                                execute = 'terminator --command="NO_TMUX=true sudo apt-get update && apt-upgrade && echo Upgrade done; read"',
                                hide_on_no_updates = True,
                                update_after_execute = True,
                                fontsize = fontsize),
            widget.Sep(padding = 8, foreground = '555555'),
            widget.Systray(icon_size = 24),
            widget.Sep(padding = 8, foreground = '555555'),
            widget.Clock(fontsize = fontsize -2, format='%b %d,%H:%M '),
            ]

screens = [
    #Screen(top=bar.Bar(widgets(), 26, background = '111111')),
    Screen(top=bar.Bar(widgets(), 26, background = '111111'))
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating()
auto_fullscreen = True

@hook.subscribe.screen_change
def restart_on_randr(qtile, ev):
    sh.detect_screens()
    qtile.cmd_restart()


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, github issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

def ensure_running(proc_name, run_proc):
  def start_if_required():
    try:
      sh.pidof(proc_name)
    except sh.ErrorReturnCode:
      run_proc()
  return start_if_required

startup_apps = [lambda: sh.wmname(wmname),
                lambda: sh.dropbox("start", "-i", _bg=True),
                lambda: sh.feh("--bg-fill", "/home/tjarvstrand/Pictures/Wallpapers/1.jpeg", _bg=True),
                ensure_running("nm-applet", lambda: sh.nm_applet(_bg=True)),
                # TODO Run these from the xsession or something
                ensure_running("copyq", lambda: sh.copyq(_bg=True)),
                ensure_running("blueman-applet", lambda: sh.blueman_applet(_bg=True)),
                ensure_running("xfce4-power-manager", lambda: sh.xfce4_power_manager(_bg=True)),
                ensure_running("light-locker", lambda: sh.light_locker(_bg=True))
]

@hook.subscribe.startup_complete
def start_apps():
    for start_app in startup_apps:
        start_app()
