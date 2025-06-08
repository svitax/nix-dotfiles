config.load_autoconfig(False)

# To enable flat web browsing and to make the Consult buffer sources work,
# Qutebrowser has to be configured to open tabs as windows. It is also
# recommended to hide the tab bar, since there will be no tabs.
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

c.window.title_format = '{audio}{private}{current_title}'

c.editor.command = ['emacsclient', '-c', '{file}']

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

# disable insert mode completely
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.input.insert_mode.plugins = False

# Forward unbound keys
c.input.forward_unbound_keys = "all"

ESC_BIND = 'clear-keychain ;; search ;; fullscreen --leave'

import string

c.bindings.default['normal'] = {}
c.bindings.default['insert'] = {}

c.bindings.commands['insert'] = {
    '<ctrl-space>': 'mode-leave',
    '<ctrl-g>': 'mode-leave;;fake-key <Left>;;fake-key <Right>',
    '<ctrl-f>': 'fake-key <Shift-Right>',
    '<ctrl-b>': 'fake-key <Shift-Left>',
    '<ctrl-e>': 'fake-key <Shift-End>',
    '<ctrl-a>': 'fake-key <Shift-Home>',
    '<ctrl-p>': 'fake-key <Shift-Up>',
    '<ctrl-n>': 'fake-key <Shift-Down>',
    '<Return>': 'mode-leave',
    '<ctrl-w>': 'fake-key <Ctrl-x>;;message-info "cut to clipboard";;mode-leave',
    '<alt-w>': 'fake-key <Ctrl-c>;;message-info "copy to clipboard";;mode-leave',
    '<backspace>': 'fake-key <backspace>;;mode-leave',
    '<alt-x>': 'mode-leave;;cmd-set-text :',
    '<alt-o>': 'mode-leave;;tab-focus last',
    '<Tab>': 'fake-key <f1>'
}

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


# Bindings
c.bindings.commands['normal'] = {
    # Navigation
    '<ctrl-space>': 'mode-enter insert',
    '<ctrl-]>': 'fake-key <Ctrl-Shift-Right>',
    '<ctrl-[>': 'fake-key <Ctrl-Shift-Left>',
    '<ctrl-v>': 'scroll-page 0 0.5',
    '<alt-v>': 'scroll-page 0 -0.5',
    '<ctrl-shift-v>': 'scroll-page 0 1',
    '<alt-shift-v>': 'scroll-page 0 -1',

    '<alt-x>': 'cmd-set-text :',
    '<ctrl-x>b': 'cmd-set-text -s :tab-select;;fake-key <Down><Down><Down>',
    '<ctrl-x>k': 'tab-close',
    '<ctrl-x>r': 'config-cycle statusbar.hide',
    '<ctrl-x>1': 'tab-only;;message-info "cleared all other tabs"',
    '<ctrl-x><ctrl-c>': 'quit',

    # searching
    '<ctrl-s>': 'cmd-set-text /',
    '<ctrl-r>': 'cmd-set-text ?',

    # hinting
    '<alt-j>': 'hint',
    # '<alt-o>': 'hint links spawn --detach umpv --force-window yes {hint-url}',
    # '<alt-.>': 'spawn --detach umpv --force-window yes {url}',
    '<alt-o>': 'hint links spawn --detach mpv {hint-url}',
    '<alt-.>': 'spawn --detach mpv {url}',

    # open links
    '<ctrl-l>': 'cmd-set-text -s :open',
    '<alt-l>': 'cmd-set-text -s :open -t',
    '<ctrl-alt-l>': 'cmd-set-text -s :open {url:pretty}',

    # editing
    '<alt-p>': 'back',
    '<alt-n>': 'forward',
    '<alt-r>': 'reload',
    '<ctrl-/>': 'fake-key <Ctrl-z>',
    '<ctrl-shift-?>': 'fake-key <Ctrl-Shift-z>',
    '<ctrl-_>': 'fake-key <Ctrl-Shift-z>',
    '<ctrl-k>': 'fake-key <Shift-End>;;fake-key <Backspace>',
    '<ctrl-f>': 'fake-key <Right>',
    '<ctrl-b>': 'fake-key <Left>',
    # '<alt-o>': 'tab-focus last',
    '<ctrl-a>': 'fake-key <Home>',
    '<ctrl-x>h': 'fake-key <Ctrl-a>',
    '<ctrl-e>': 'fake-key <End>',
    '<ctrl-n>': 'fake-key <Down>',
    '<ctrl-p>': 'fake-key <Up>',
    '<alt-f>': 'fake-key <Ctrl-Right>',
    '<alt-b>': 'fake-key <Ctrl-Left>',
    '<ctrl-d>': 'fake-key <Delete>',
    '<alt-d>': 'fake-key <Ctrl-Delete>',
    '<alt-backspace>': 'fake-key <Ctrl-Backspace>',
    '<ctrl-w>': 'fake-key <Ctrl-x>;;message-info "cut to clipboard"',
    '<alt-w>': 'fake-key <Ctrl-c>;;message-info "copy to clipboard"',
    '<ctrl-y>': 'insert-text {primary}',

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

	# escape hatch
    '<ctrl-h>': 'cmd-set-text -s :help',
    '<ctrl-g>': ESC_BIND,
}

c.bindings.commands['command'] = {
    '<ctrl-s>': 'search-next',
    '<ctrl-r>': 'search-prev',

    '<ctrl-p>': 'completion-item-focus prev',
    '<ctrl-n>': 'completion-item-focus next',

    '<alt-p>': 'command-history-prev',
    '<alt-n>': 'command-history-next',

    # editing
    '<ctrl-y>': 'insert-text',

	# escape hatch
    '<ctrl-g>': 'mode-leave',
}

c.bindings.commands['hint'] = {
    # escape hatch
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
c.url.searchengines["DEFAULT"] = "https://www.duckduckgo.com/?q={}"

# * Adblocking
# Valid values:
#   - auto: Use Brave's ABP-style adblocker if available, host blocking otherwise
#   - adblock: Use Brave's ABP-style adblocker
#   - hosts: Use hosts blocking
#   - both: Use both hosts blocking and Brave's ABP-style adblocker
c.content.blocking.method = 'both'
c.content.blocking.adblock.lists = [
    'https://easylist.to/easylist/easylist.txt',
    'https://easylist.to/easylist/easyprivacy.txt',
    #'https://easylist.to/easylist/fanboy-social.txt', Already included in fanboy-annoyance.txt
    #'https://secure.fanboy.co.nz/fanboy-cookiemonster.txt', Already included in fanboy-annoyance.txt
    'https://secure.fanboy.co.nz/fanboy-annoyance.txt',
    'https://raw.githubusercontent.com/DandelionSprout/adfilt/master/NorwegianExperimentalList%20alternate%20versions/NordicFiltersABP-Inclusion.txt',
    'https://easylist-downloads.adblockplus.org/antiadblockfilters.txt',
    'https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt',
    'https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt',
    'https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances-others.txt',
    'https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances-cookies.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/quick-fixes.txt',
]
