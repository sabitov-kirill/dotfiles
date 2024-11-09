# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config: ConfigAPI = config  # type: ignore # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # type: ignore # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig(False)
config.set('content.headers.user_agent',
           'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0', '*')
config.set('content.images', True, '*')
config.set('content.javascript.enabled', True, '*')
config.set('content.local_content_can_access_remote_urls', True, '*')
config.set('input.insert_mode.auto_enter', True)
config.set('input.insert_mode.auto_leave', True)
config.set('input.insert_mode.leave_on_load', True)
config.set('qt.force_platformtheme', 'dark')

c.completion.open_categories = [
    'searchengines', 'quickmarks', 'bookmarks', 'history', 'filesystem'
]

theme_colors = {
    "bg": "#212121",
    "fg": "#CCCCCC",
    "bg_menu": "#1A1A1A",
    "fg_menu": "#CCCCCC",
    "bg_accent": "#93d479",
    "fg_accent": "#212121",
    "bg_selection": "#1A2536",
    "fg_selection": "#6796E6",
    "bg_error": "#F44747",
    "fg_error": "#3D1212",
    "bg_warning": "#3D3019",
    "fg_warning": "#FFCB6B",
    "fg_success": "#C3E88D",
}

c.colors.completion.category.border.top = theme_colors['bg_menu']
c.colors.completion.category.border.bottom = theme_colors['bg_menu']
c.colors.completion.item.selected.fg = theme_colors['bg_menu']
c.colors.downloads.bar.bg = theme_colors['bg_menu']
c.colors.hints.bg = theme_colors['bg_menu']

c.colors.statusbar.normal.fg = theme_colors['fg_menu']
c.colors.statusbar.normal.bg = theme_colors['bg_menu']
c.colors.statusbar.insert.fg = theme_colors['fg_accent']
c.colors.statusbar.insert.bg = theme_colors['bg_accent']
c.colors.statusbar.passthrough.fg = theme_colors['fg_menu']
c.colors.statusbar.passthrough.bg = theme_colors['bg_menu']
c.colors.statusbar.private.fg = theme_colors['fg_menu']
c.colors.statusbar.private.bg = theme_colors['bg_menu']
c.colors.statusbar.command.fg = theme_colors['fg_menu']
c.colors.statusbar.command.bg = theme_colors['bg_menu']
c.colors.statusbar.command.private.fg = theme_colors['fg_menu']
c.colors.statusbar.command.private.bg = theme_colors['bg_menu']
c.colors.statusbar.caret.fg = theme_colors['fg_menu']
c.colors.statusbar.caret.bg = theme_colors['bg_menu']
c.colors.statusbar.caret.selection.fg = theme_colors['fg_selection']
c.colors.statusbar.caret.selection.bg = theme_colors['bg_selection']
c.colors.statusbar.progress.bg = theme_colors['bg_menu']
c.colors.statusbar.url.fg = theme_colors['fg_menu']
c.colors.statusbar.url.error.fg = theme_colors['fg_error']
c.colors.statusbar.url.warn.fg = theme_colors['bg_warning']
c.colors.statusbar.url.hover.fg = theme_colors['fg_selection']
c.colors.statusbar.url.success.http.fg = theme_colors['fg_success']
c.colors.statusbar.url.success.https.fg = theme_colors['fg_success']

c.colors.tabs.even.bg = theme_colors['bg_menu']
c.colors.tabs.odd.bg = theme_colors['bg_menu']
c.colors.tabs.selected.even.bg = theme_colors['bg_accent']
c.colors.tabs.selected.odd.bg = theme_colors['bg_accent']
c.colors.tabs.selected.even.fg = theme_colors['fg_accent']
c.colors.tabs.selected.odd.fg = theme_colors['fg_accent']
c.colors.tabs.pinned.even.bg = theme_colors['bg_menu']
c.colors.tabs.pinned.odd.bg = theme_colors['bg_menu']
c.colors.tabs.pinned.selected.even.bg = theme_colors['bg_accent']
c.colors.tabs.pinned.selected.odd.bg = theme_colors['bg_accent']
c.colors.tabs.pinned.selected.even.fg = theme_colors['fg_accent']
c.colors.tabs.pinned.selected.odd.fg = theme_colors['fg_accent']

c.colors.webpage.darkmode.enabled = False
