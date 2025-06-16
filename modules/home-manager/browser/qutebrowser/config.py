# Don't load settings configured in GUI
config.load_autoconfig(False)

# To enable flat web browsing and to make the Consult buffer sources work,
# Qutebrowser has to be configured to open tabs as windows. It is also
# recommended to hide the tab bar, since there will be no tabs.
# Qutebrowser configured the right way i.e. with Emacs keybindings

# c.tabs.tabs_are_windows = True
c.tabs.show = 'never'

# If you’re having trouble with existing Qutebrowser windows being raised and
# given focus when using one of the qutebrowser-launcher commands to open a new
# window, you might need to add the following line to your config.py:

# Avoid browser being focused when sending commands
# c.new_instance_open_target = 'tab-silent'

# Despite what the name of the setting might suggest, it does not seem to affect
# where new windows/tabs are opened, it only keeps from raising the last used
# window when sending commands using the commandline backend. It might not be
# necessary to set this option when using the IPC backend.

# If you are using the theme synchronization, add this line to your config.py to
# load the exported theme on Qutebrowser startup:

# config.source("emacs_theme.py")

c.statusbar.show = 'never'

c.auto_save.session = True

# Height of completion menu
c.completion.height = '45%'

# Remove scrollbar from completion menu
c.completion.scrollbar.padding = 0
c.completion.scrollbar.width = 0

# Configure title format for windows
c.window.title_format = '{audio}{private}{current_title}'

# Configure external editor
c.editor.command = ['emacsclient', '-c', '{file}']

# For privacy reasons, I’m setting a generic user-agent to minimize fingerprinting.
c.content.headers.user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36'

# requires +qutebrowser-choose-file and +qutebrowser-dired-hook is Emacs
c.fileselect.handler = 'external'
c.fileselect.single_file.command = ['emacsclient', '{}']
c.fileselect.folder.command = ['emacsclient', '{}']
c.fileselect.multiple_files.command = ['emacsclient', '{}']

# Graphite keyboard layout
c.hints.chars = 'strnhaeigy'

# Font
c.fonts.default_family = ["Aporetic Sans"]
c.fonts.default_size = '16pt'
c.fonts.web.family.sans_serif = 'Aporetic Sans'
c.fonts.web.family.serif = 'Aporetic Serif'
c.fonts.web.family.standard = 'Aporetic Sans'
c.fonts.web.size.default = 16
c.fonts.web.size.minimum = 16

c.content.user_stylesheets = ["all-sites.css"]

# All default keybindings will be disabled and redefined one by one.
c.bindings.default['normal'] = {}
c.bindings.default['insert'] = {}

# Insert mode will be disabled completely. All keybindings in normal mode will
# have a modifier key, so it's not needed.
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.input.insert_mode.plugins = False

# All unbound keys will be forwarded.
c.input.forward_unbound_keys = "all"

ESC_BIND = 'clear-keychain ;; search ;; fullscreen --leave'

# Insert mode becomes our "set-mark" mode.
c.bindings.commands['insert'] = {
    '<ctrl-space>': 'mode-leave',
    '<ctrl-g>': 'mode-leave;;fake-key <Left>;;fake-key <Right>',
    '<ctrl-f>': 'fake-key <Shift-Right>',
    '<ctrl-b>': 'fake-key <Shift-Left>',
    '<ctrl-e>': 'fake-key <Shift-End>',
    '<ctrl-a>': 'fake-key <Shift-Home>',
    '<ctrl-p>': 'fake-key <Shift-Up>',
    '<ctrl-n>': 'fake-key <Shift-Down>',
    '<alt-f>': 'fake-key <Ctrl-Shift-Right>',
    '<alt-b>': 'fake-key <Ctrl-Shift-Left>',
    '<Return>': 'mode-leave',
    '<ctrl-w>': 'fake-key <Ctrl-x>;;message-info "cut to clipboard";;mode-leave',
    '<alt-w>': 'fake-key <Ctrl-c>;;message-info "copy to clipboard";;mode-leave',
    '<backspace>': 'fake-key <backspace>;;mode-leave',
    '<alt-x>': 'mode-leave;;cmd-set-text :',
    '<alt-o>': 'mode-leave;;tab-focus last',
    '<Tab>': 'fake-key <f1>',
    '<ctrl-y>': 'insert-text {primary}',
}

import string

for char in list(string.ascii_lowercase):
    c.bindings.commands['insert'].update({char: 'fake-key ' + char + ';;mode-leave'})

for CHAR in list(string.ascii_uppercase):
    c.bindings.commands['insert'].update({CHAR: 'fake-key ' + char + ';;mode-leave'})

for num in list(map(lambda x : str(x), range(0, 10))):
    c.bindings.commands['insert'].update({num: 'fake-key ' + num + ';;mode-leave'})

for symb in [',', '.', '/', '\'', ';', '[', ']', '\\',
             '!', '@','#','$','%','^','&','*','(',')','-','_', '=', '+', '`', '~',
             ':', '\"', '<', '>', '?','{', '}', '|']:
    c.bindings.commands['insert'].update({symb: 'insert-text ' + symb + ' ;;mode-leave'})

c.bindings.commands['normal'] = {
    # Fake keys will be used. This makes everything easier.

    # Editing
    '<ctrl-f>': 'fake-key <Right>',
    '<ctrl-b>': 'fake-key <Left>',

    '<alt-f>': 'fake-key <Ctrl-Right>',
    '<alt-b>': 'fake-key <Ctrl-Left>',

    '<ctrl-a>': 'fake-key <Home>',
    '<ctrl-e>': 'fake-key <End>',

    '<ctrl-n>': 'fake-key <Down>',
    '<ctrl-p>': 'fake-key <Up>',

    '<ctrl-d>': 'fake-key <Delete>',
    '<alt-d>': 'fake-key <Ctrl-Delete>',

    '<ctrl-k>': 'fake-key <Shift-End>;;fake-key <Backspace>',
    '<alt-k>': 'fake-key <Shift-Home>;;fake-key <Backspace>',

    '<ctrl-w>': 'fake-key <Ctrl-x>;;message-info "cut to clipboard"',
    '<alt-w>': 'fake-key <Ctrl-c>;;message-info "copy to clipboard"',

    '<ctrl-y>': 'insert-text {primary}',

    '<alt-backspace>': 'fake-key <Ctrl-Backspace>',

    '<ctrl-/>': 'fake-key <Ctrl-z>',
    '<ctrl-shift-?>': 'fake-key <Ctrl-Shift-z>',
    '<ctrl-_>': 'fake-key <Ctrl-Shift-z>',

    '<ctrl-x>h': 'fake-key <Ctrl-a>',

    # Navigation
    '<ctrl-v>': 'scroll-page 0 0.5',
    '<alt-v>': 'scroll-page 0 -0.5',

    '<ctrl-shift-v>': 'scroll-page 0 1',
    '<alt-shift-v>': 'scroll-page 0 -1',

    '<alt-shift-,>': 'scroll-to-perc 0',
    '<alt-shift-.>': 'scroll-to-perc',

    # Basic
    '<alt-x>': 'cmd-set-text :',
    '<ctrl-x><ctrl-c>': 'quit',

    '<ctrl-g>': ESC_BIND,
    '<ctrl-h>': 'cmd-set-text -s :help',

    '<ctrl-space>': 'mode-enter insert',

    # Opening links
    '<ctrl-l>': 'cmd-set-text -s :open',
    '<ctrl-shift-l>': 'cmd-set-text -s :open {url:pretty}',
    '<ctrl-alt-l>': 'cmd-set-text -s :open {url:pretty}',
    '<alt-l>': 'cmd-set-text -s :open -t',
    '<alt-shift-l>': 'cmd-set-text -s :open -t {url:pretty}',

    '<ctrl-x><ctrl-f>': 'cmd-set-text -s :open',
    '<ctrl-u><ctrl-x><ctrl-f>': 'cmd-set-text -s :open -t',

    # Searching
    '<ctrl-s>': 'cmd-set-text /',
    '<ctrl-r>': 'cmd-set-text ?',

    # Hinting
    '<alt-j>': 'hint all',
    '<ctrl-alt-j>': 'hint all tab',

    '<alt-o>': 'hint links spawn umpv {hint-url}',
    '<alt-.>': 'spawn --detach umpv {url}',
    # '<alt-o>': 'hint links spawn --detach umpv --force-window yes {hint-url}',
    # '<alt-.>': 'spawn --detach umpv --force-window yes {url}',

    # Tabs
    '<alt-e>': 'tab-prev',
    '<alt-a>': 'tab-next',
    '<alt-shift-e>': 'tab-move -',
    '<alt-shift-a>': 'tab-move +',

    '<ctrl-x>k': 'tab-close',
    '<ctrl-x>0': 'tab-close',

    '<ctrl-x>1': 'tab-only;;message-info "cleared all other tabs"',

    '<ctrl-x>b': 'cmd-set-text -s :tab-select;;fake-key <Down><Down><Down>',

    '<ctrl-x>r': 'config-cycle statusbar.hide',

    '<ctrl-x>u': 'undo --window',

    # '<alt-o>': 'tab-focus last',

    # History
    '<alt-n>': 'forward',
    '<alt-p>': 'back',
    '<alt-r>': 'reload',

    # Numbers
    '1': 'fake-key 1',
    '2': 'fake-key 2',
    '3': 'fake-key 3',
    '4': 'fake-key 4',
    '5': 'fake-key 5',
    '6': 'fake-key 6',
    '7': 'fake-key 7',
    '8': 'fake-key 8',
    '9': 'fake-key 9',
    '0': 'fake-key 0',

    # TODO some require qutebrowser.el
    # "h" = "spawn --userscript emacsclient-wrapper '(qutebrowser-launcher)'";
    # "H" = "spawn --userscript emacsclient-wrapper '(qutebrowser-launcher-tab)'";
    # ";l" = "spawn --userscript qute-pass";
    # ";u" = "spawn --userscript qute-pass --username-only";
    # ";p" = "spawn --userscript qute-pass --password-only";
    # ";o" = "spawn --userscript qute-pass --otp-only";
    # ";P" = "spawn --userscript emacsclient-wrapper '(qutebrowser-pass \"{url}\")'";
}

c.bindings.commands['command'] = {
    # Completion
    '<ctrl-n>': 'completion-item-focus next',
    '<ctrl-p>': 'completion-item-focus prev',
    '<ctrl-d>': 'rl-delete-char',
    '<ctrl-y>': 'fake-key -g <Ctrl-V>',
    '<alt-p>': 'command-history-prev',
    '<alt-n>': 'command-history-next',
    '<ctrl-g>': 'mode-leave',

    # Searching
    '<ctrl-s>': 'search-next',
    '<ctrl-r>': 'search-prev',
}

c.bindings.commands['prompt'] = {
    # Completion
    '<ctrl-n>': 'prompt-item-focus next',
    '<ctrl-p>': 'prompt-item-focus prev',
    '<ctrl-g>': 'mode-leave',
}

c.bindings.commands['hint'] = {
    '<ctrl-g>': 'mode-leave',
}

c.bindings.commands['caret'] = {
    # escape hatch
    '<ctrl-g>': 'mode-leave',
    '<ctrl-space>': 'toggle-selection'
}

config.bind('<Tab>', 'fake-key <f1>')
config.bind('<Ctrl-x><Ctrl-l>', 'config-source')
# c.tabs.show = 'never'
# c.statusbar.hide = False
c.url.searchengines["g"] = "https://www.google.com/search?q={}"
c.url.searchengines["ddg"] = "https://www.duckduckgo.com/?q={}"
c.url.searchengines["yt"] = "https://www.youtube.com/results?search_query={}"
c.url.searchengines["np"] = "https://search.nixos.org/packages?query={}"
c.url.searchengines["no"] = "https://search.nixos.org/options?query={}"
c.url.searchengines["melpa"] = "https://melpa.org/#/?q={}"
c.url.searchengines["gh"] = "https://github.com/search?q={}"
c.url.searchengines["DEFAULT"] = "https://www.duckduckgo.com/?q={}"

# * Adblocking
# Valid values:
#   - auto: Use Brave's ABP-style adblocker if available, host blocking otherwise
#   - adblock: Use Brave's ABP-style adblocker
#   - hosts: Use hosts blocking
#   - both: Use both hosts blocking and Brave's ABP-style adblocker
c.content.blocking.method = 'both'
# Don't forget to do `:adblock-update' if you change any of the lists. And
# update them periodically.
c.content.blocking.adblock.lists = [
    "https://easylist.to/easylist/easylist.txt",
    "https://easylist.to/easylist/easyprivacy.txt",
    "https://easylist.to/easylist/fanboy-social.txt",
    "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
    "https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt",
    #"https://gitlab.com/curben/urlhaus-filter/-/raw/master/urlhaus-filter.txt",
    "https://pgl.yoyo.org/adservers/serverlist.php?showintro=0;hostformat=hosts",
    "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/NorwegianExperimentalList%20alternate%20versions/NordicFiltersABP-Inclusion.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt",
    "https://www.i-dont-care-about-cookies.eu/abp/",
    "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt",
    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt",
    "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/quick-fixes.txt",
]

c.content.autoplay = False

# TODO extra config
# config.set('content.cookies.accept', 'no-3rdparty', 'chrome-devtools://*')
# config.set('content.cookies.accept', 'no-3rdparty', 'devtools://*')

# config.set('content.images', True, 'chrome-devtools://*')
# config.set('content.images', True, 'devtools://*')

# config.set('content.javascript.enabled', True, 'chrome-devtools://*')
# config.set('content.javascript.enabled', True, 'devtools://*')
# config.set('content.javascript.enabled', True, 'chrome://*/*')
# config.set('content.javascript.enabled', True, 'qute://*/*')

# config.set('content.notifications.enabled', False, 'https://www.reddit.com')
# config.set('content.notifications.enabled', False, 'https://www.youtube.com')

# config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
# config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/119.0', 'https://accounts.google.com/*')
# config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info} rv:110.0) Gecko/20100101 Firefox/119.0', 'https://*.slack.com/*')
# config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/119.0', 'https://docs.google.com/*')
# config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/119.0', 'https://drive.google.com/*')
