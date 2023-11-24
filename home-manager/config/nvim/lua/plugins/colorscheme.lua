return {
  {
    -- Highlight hex/rgb/css colors
    "brenoprata10/nvim-highlight-colors",
    opts = { render = "background", enable_named_colors = true },
    event = "BufReadPost",
  },
  { "folke/tokyonight.nvim", opts = {} },
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
          PmenuSel = { fg = "palette.bg1", bg = "palette.blue" }, -- I only want to change cmp Pmenu sel
          PmenuThumb = { bg = "palette.sel0" },
          NormalFloat = { bg = "palette.bg3" }, -- I only want to change cmp float (documentation)

          CmpItemMenuDefault = { fg = "palette.red", bg = "palette.red" },
          CmpItemAbbr = { fg = "palette.fg1" },
          CmpItemAbbrMatch = { fg = "palette.magenta" },
          CmpItemAbbrMatchFuzzy = { fg = "palette.magenta" },
          CmpItemKindField = { fg = "palette.bg0", bg = "palette.red" },
          CmpItemKindProperty = { fg = "palette.bg0", bg = "palette.red" },
          CmpItemKindEvent = { fg = "palette.bg0", bg = "palette.red" },
          CmpItemKindText = { fg = "palette.bg0", bg = "palette.green" },
          CmpItemKindEnum = { fg = "palette.bg0", bg = "palette.green" },
          CmpItemKindKeyword = { fg = "palette.bg0", bg = "palette.green" },
          CmpItemKindConstant = { fg = "palette.bg0", bg = "palette.yellow" },
          CmpItemKindConstructor = { fg = "palette.bg0", bg = "palette.yellow" },
          CmpItemKindReference = { fg = "palette.bg0", bg = "palette.yellow" },
          CmpItemKindFunction = { fg = "palette.bg0", bg = "palette.magenta" },
          CmpItemKindStruct = { fg = "palette.bg0", bg = "palette.magenta" },
          CmpItemKindClass = { fg = "palette.bg0", bg = "palette.magenta" },
          CmpItemKindModule = { fg = "palette.bg0", bg = "palette.magenta" },
          CmpItemKindOperator = { fg = "palette.bg0", bg = "palette.magenta" },
          CmpItemKindVariable = { fg = "palette.bg0", bg = "palette.blue" },
          CmpItemKindFile = { fg = "palette.bg0", bg = "palette.blue" },
          CmpItemKindUnit = { fg = "palette.bg0", bg = "palette.orange" },
          CmpItemKindSnippet = { fg = "palette.bg0", bg = "palette.orange" },
          CmpItemKindFolder = { fg = "palette.bg0", bg = "palette.orange" },
          CmpItemKindMethod = { fg = "palette.bg0", bg = "palette.blue" },
          CmpItemKindValue = { fg = "palette.bg0", bg = "palette.blue" },
          CmpItemKindEnumMember = { fg = "palette.bg0", bg = "palette.blue" },
          CmpItemKindInterface = { fg = "palette.bg0", bg = "palette.cyan" },
          CmpItemKindColor = { fg = "palette.bg0", bg = "palette.cyan" },
          CmpItemKindTypeParameter = { fg = "palette.bg0", bg = "palette.cyan" },

          Search = { fg = "NONE", bg = "palette.sel1" },
          IncSearch = { fg = "NONE", bg = "palette.sel1" },
          LineNr = { fg = "palette.comment" },

          Hlargs = { link = "@parameter" },

          NvimSurroundHighlight = { fg = "palette.bg0", bg = "palette.magenta" },

          -- FloatBorder = { fg = "palette.comment" },
        },
      },
    },
  },
}
