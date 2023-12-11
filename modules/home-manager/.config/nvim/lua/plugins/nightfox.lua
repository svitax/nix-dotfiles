return {
    {
        "svitax/nightfox.nvim",
        branch = "gruvfox",
        opts = {
            options = {
                styles = {
                    keywords = "NONE",
                    functions = "bold",
                    comments = "italic",
                },
            },
            groups = {
                gruvfox = {
                    ["@parameter"] = { fg = "palette.white" },
                    ["@constant.builtin"] = { style = "bold" },
                    ["@function.builtin"] = { fg = "palette.green" },
                    ["@keyword.function"] = { style = "NONE" },

                    ["@keyword.operator"] = { fg = "palette.red" },

                    -- look into cmp config for window and documentation (nvim-cmp/lua/cmp/config/default.lua)
                    -- PmenuSel = { fg = "palette.bg1", bg = "palette.blue" }, -- I only want to change cmp Pmenu sel
                    PmenuThumb = { bg = "palette.bg2" },
                    NormalFloat = { bg = "palette.bg1" }, -- I only want to change cmp float (documentation)

                    CmpItemMenuDefault = { fg = "palette.red", bg = "palette.red" },
                    CmpItemAbbr = { fg = "palette.fg1" },
                    CmpItemAbbrMatch = { fg = "palette.magenta" },
                    CmpItemAbbrMatchFuzzy = { fg = "palette.magenta" },

                    CmpItemKindField = { fg = "palette.bg1", bg = "palette.red" },
                    CmpItemKindProperty = { fg = "palette.bg1", bg = "palette.red" },
                    CmpItemKindEvent = { fg = "palette.bg1", bg = "palette.red" },
                    CmpItemKindText = { fg = "palette.bg1", bg = "palette.green" },
                    CmpItemKindEnum = { fg = "palette.bg1", bg = "palette.green" },
                    CmpItemKindKeyword = { fg = "palette.bg1", bg = "palette.green" },
                    CmpItemKindConstant = { fg = "palette.bg1", bg = "palette.yellow" },
                    CmpItemKindConstructor = { fg = "palette.bg1", bg = "palette.yellow" },
                    CmpItemKindReference = { fg = "palette.bg1", bg = "palette.yellow" },
                    CmpItemKindFunction = { fg = "palette.bg1", bg = "palette.magenta" },
                    CmpItemKindStruct = { fg = "palette.bg1", bg = "palette.magenta" },
                    CmpItemKindClass = { fg = "palette.bg1", bg = "palette.magenta" },
                    CmpItemKindModule = { fg = "palette.bg1", bg = "palette.magenta" },
                    CmpItemKindOperator = { fg = "palette.bg1", bg = "palette.magenta" },
                    CmpItemKindVariable = { fg = "palette.bg1", bg = "palette.blue" },
                    CmpItemKindFile = { fg = "palette.bg1", bg = "palette.blue" },
                    CmpItemKindUnit = { fg = "palette.bg1", bg = "palette.orange" },
                    CmpItemKindSnippet = { fg = "palette.bg1", bg = "palette.orange" },
                    CmpItemKindFolder = { fg = "palette.bg1", bg = "palette.orange" },
                    CmpItemKindMethod = { fg = "palette.bg1", bg = "palette.blue" },
                    CmpItemKindValue = { fg = "palette.bg1", bg = "palette.blue" },
                    CmpItemKindEnumMember = { fg = "palette.bg1", bg = "palette.blue" },
                    CmpItemKindInterface = { fg = "palette.bg1", bg = "palette.cyan" },
                    CmpItemKindColor = { fg = "palette.bg1", bg = "palette.cyan" },
                    CmpItemKindTypeParameter = { fg = "palette.bg1", bg = "palette.cyan" },

                    Search = { fg = "NONE", bg = "palette.sel1" },
                    IncSearch = { fg = "NONE", bg = "palette.sel1" },
                    LineNr = { fg = "palette.comment" },

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
