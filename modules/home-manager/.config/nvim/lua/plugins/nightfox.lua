return {
    {
        "svitax/nightfox.nvim",
        branch = "gruvfox",
        opts = {
            options = {
                styles = {
                    keywords = "NONE",
                    functions = "bold",
                    comments = "NONE",
                },
            },
            groups = {
                gruvfox = {
                    ["@parameter"] = { fg = "palette.white" },
                    ["@constant.builtin"] = { style = "bold" },
                    ["@function.builtin"] = { fg = "palette.green" },
                    ["@keyword.function"] = { style = "NONE" },
                    ["@keyword.operator"] = { fg = "palette.red" },

                    Pmenu = { fg = "palette.fg1", bg = "palette.bg2" }, -- Popup menu: normal item.
                    PmenuSel = { bg = "palette.bg3" }, -- Popup menu: selected item.
                    PmenuSbar = { bg = "palette.bg3" }, -- Popup menu: scrollbar.
                    PmenuThumb = { bg = "palette.bg4" }, -- Popup menu: Thumb of the scrollbar.
                    -- NormalFloat = { bg = "palette.bg1" }, -- I only want to change cmp float (documentation)

                    Search = { fg = "NONE", bg = "palette.sel1" },
                    IncSearch = { fg = "NONE", bg = "palette.sel1" },
                    LineNr = { fg = "palette.comment" },

                    -- nvim-cmp start
                    CmpItemAbbrMatch = { fg = "palette.blue" },
                    CmpItemAbbrMatchFuzzy = { link = "CmpItemAbbrMatch" },

                    CmpCompletion = { link = "Pmenu" },
                    CmpCompletionSbar = { link = "PmenuSbar" },
                    CmpCompletionSel = { link = "PmenuSel" },
                    CmpCompletionThumb = { link = "PmenuThumb" },
                    CmpCompletionBorder = { fg = "palette.comment" },

                    CmpItemKindField = { fg = "palette.red" },
                    CmpItemKindProperty = { fg = "palette.red" },
                    CmpItemKindEvent = { fg = "palette.red" },
                    CmpItemKindText = { fg = "palette.green" },
                    CmpItemKindEnum = { fg = "palette.green" },
                    CmpItemKindKeyword = { fg = "palette.green" },
                    CmpItemKindConstant = { fg = "palette.yellow" },
                    CmpItemKindConstructor = { fg = "palette.yellow" },
                    CmpItemKindReference = { fg = "palette.yellow" },
                    CmpItemKindFunction = { fg = "palette.magenta" },
                    CmpItemKindStruct = { fg = "palette.magenta" },
                    CmpItemKindClass = { fg = "palette.magenta" },
                    CmpItemKindModule = { fg = "palette.magenta" },
                    CmpItemKindOperator = { fg = "palette.magenta" },
                    CmpItemKindVariable = { fg = "palette.blue" },
                    CmpItemKindFile = { fg = "palette.blue" },
                    CmpItemKindUnit = { fg = "palette.orange" },
                    CmpItemKindSnippet = { fg = "palette.orange" },
                    CmpItemKindFolder = { fg = "palette.orange" },
                    CmpItemKindMethod = { fg = "palette.blue" },
                    CmpItemKindValue = { fg = "palette.blue" },
                    CmpItemKindEnumMember = { fg = "palette.blue" },
                    CmpItemKindInterface = { fg = "palette.cyan" },
                    CmpItemKindColor = { fg = "palette.cyan" },
                    CmpItemKindTypeParameter = { fg = "palette.cyan" },
                    -- nvim-cmp end

                    -- nvim-dap-ui start
                    DapUIPlayPause = { fg = "palette.green" },
                    DapUIRestart = { fg = "palette.green" },
                    DapUIStepBack = { fg = "palette.cyan" },
                    DapUIStepInto = { fg = "palette.cyan" },
                    DapUIStepOut = { fg = "palette.cyan" },
                    DapUIStepOver = { fg = "palette.cyan" },
                    DapUIStop = { fg = "palette.red" },
                    DapUIUnavailable = { fg = "palette.comment" },
                    -- nvim-dap-ui end

                    -- which-key start
                    WhichKey = { link = "Identifier" },
                    WhichKeyGroup = { link = "Function" },
                    WhichKeyDesc = { link = "Conditional" },
                    WhichKeySeperator = { link = "Comment" },
                    WhichKeySeparator = { link = "Comment" },
                    WhichKeyFloat = { bg = "palette.bg1" },
                    WhichKeyValue = { link = "Comment" },
                    -- which-key end

                    -- statusline start
                    StatusLine = { fg = "palette.fg2", bg = "palette.bg3" }, -- status line of current window
                    StatusLineNC = { fg = "palette.fg2", bg = "palette.bg3" }, -- status lines of not-current windows Note: if this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
                    StatusLineHeader = { fg = "palette.fg1", bg = "palette.bg4" },
                    StatusLineHeaderModified = { fg = "palette.yellow", bg = "palette.bg4", style = "bold" },
                    -- statusline end
                    Hlargs = { link = "@parameter" },

                    NvimSurroundHighlight = { fg = "palette.bg0", bg = "palette.magenta" },

                    NeogitNormal = { bg = "palette.bg1" },
                    NeogitWinSeparator = { bg = "palette.bg1", fg = "palette.comment" },
                    NeogitNotification = { bg = "palette.bg1" },
                    NeogitCursorLine = { bg = "palette.bg3" },
                    NeogitCursorLineNr = { fg = "palette.comment" },
                    NeogitDiffHeader = { bg = "palette.bg4", fg = "palette.blue", style = "underline" },
                    NeogitDiffHeaderHighlight = { bg = "palette.bg4", fg = "palette.orange", style = "bold,underline" },
                    NeogitDiffContext = { bg = "palette.bg1" },
                    NeogitDiffContextHighlight = { bg = "palette.bg2" },
                    NeogitHunkHeader = { fg = "palette.fg2", bg = "palette.bg3", gui = "bold" },
                    NeogitHunkHeaderHighlight = { bg = "palette.magenta", fg = "palette.bg1", gui = "bold" },

                    TelescopeBorder = { fg = "palette.bg4" },
                    TelescopeSelectionCaret = { fg = "palette.yellow" },
                    TelescopeSelection = { link = "Cursorline" },
                    TelescopeMatching = { link = "Search" },
                    TelescopeNormal = { link = "Normal" },
                    TelescopeTitle = { fg = "palette.bg1", bg = "palette.green" },
                    TelescopePromptNormal = { link = "TelescopeNormal" },
                    TelescopePromptBorder = { link = "TelescopeBorder" },

                    -- FloatBorder = { fg = "palette.comment" },
                },
            },
        },
    },
}
