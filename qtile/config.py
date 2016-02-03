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
from libqtile.command import lazy
from libqtile import layout, bar, widget

import libqtile.layout.xmonad

import sh

mod = "mod4"
alt = "mod1"

keys = [
    # windows style alt-tab/alt-shift-tab
    Key([alt], "Tab", lazy.layout.next()),
    Key([alt, "shift"], "Tab", lazy.layout.previous()),

    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "Return", lazy.layout.swap_main()),
    Key([mod], "w", lazy.window.kill()),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.spawncmd()),

    Key([mod], "h", lazy.layout.grow()),
    Key([mod], "l", lazy.layout.shrink()),

    Key(["control", alt], "l", lazy.spawn("light-locker-command -l")),

    Key([], "XF86Display", lazy.spawn("scr auto")),
    Key([], "XF86WLAN", lazy.spawn("toggle-wifi"))
]

groups = [Group(i) for i in "12345"]

for i in groups:
    # mod1 + letter of group = switch to group
    keys.append(Key([mod], i.name, lazy.group[i.name].toscreen()))

    # mod1 + shift + letter of group = switch to & move focused window to group
    keys.append(Key([mod, "shift"], i.name, lazy.window.togroup(i.name)))

layouts = [
    libqtile.layout.xmonad.MonadTall(ratio = 0.66,
                                     border_width = 1,
                                     single_border_width = 0),
    layout.Max()
]

widget_defaults = dict(
    font='monospace',
    fontsize=11,
    padding=0,
    margin_x = 2
)

#workaround for bad aptitude query
class CheckUpdates(widget.CheckUpdates):
    def __init__(self, **config):
        widget.CheckUpdates.__init__(self, **config)
        self.cmd = ['aptitude', 'search', '~U']
        self.subtr = 0

#workaround for bad property accessor
class KeyboardLayout(widget.KeyboardLayout):

    def next_keyboard(self):
        current_keyboard = self.keyboard
        if current_keyboard in self.configured_keyboards:
            # iterate the list circularly
            next_keyboard = self.configured_keyboards[
                (self.configured_keyboards.index(current_keyboard) + 1) %
                len(self.configured_keyboards)]
        else:
            next_keyboard = self.configured_keyboards[0]
        self.keyboard = next_keyboard

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.AGroupBox(margin_x = 2, padding_x = 8, border = '777777'),
                widget.Sep(margin_x = 2, foreground = '555555'),
                widget.WindowName(padding = 4, width = bar.CALCULATED),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Prompt(prompt = "run: ", background = '000000'),
                widget.Spacer(),
                widget.KeyboardLayout(configured_keyboards = ["se", "custom"]),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.CPUGraph(),
                CheckUpdates(distro = 'Ubuntu',
                             display_format = '{updates} updates',
                             update_interval = 600,
                             execute = 'terminator --command="sudo apt-get -y --with-new-pkgs upgrade; read"'),
                #widget.CheckUpdates(distro = 'Ubuntu', display_format = '{updates} updates'),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Systray(),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Clock(fontsize = 12, format='%H:%M'),
            ],
            20,
            background = '222222',
            opacity = 0.8
        ),
    ),

    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.WindowName(width = bar.CALCULATED),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Prompt(prompt = "run: ", background = '000000'),
                widget.Spacer(),
                widget.KeyboardLayout(configured_keyboards = ["se", "custom"]),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Battery(),
                widget.CPUGraph(),
                CheckUpdates(distro = 'Ubuntu',
                             display_format = '{updates} updatesa',
                             update_interval = 600,
                             execute = 'terminator --command="sudo apt-get -y --with-new-pkgs upgrade; read"'),
                #widget.CheckUpdates(distro = 'Ubuntu', display_format = '{updates} updates'),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Systray(),
                widget.Sep(padding = 8, foreground = '555555'),
                widget.Clock(fontsize = 12, format='%H:%M'),
            ],
            20,
            background = '222222',
            opacity = 0.8
        ),
    )
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
                ensure_running("nm-applet", lambda: sh.nm_applet(_bg=True)),
                ensure_running("xfce4-volumed", lambda: sh.xfce4_volumed(_bg=True)),
                ensure_running("xfce4-power-manager", lambda: sh.xfce4_power_manager(_bg=True)),
                ensure_running("light-locker", lambda: sh.light_locker(_bg=True)),
                lambda: sh.dropbox("start", _bg=True)]

for start_app in startup_apps:
    start_app()
