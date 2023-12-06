return {
  {
    "Bekaboo/dropbar.nvim",
    opts = function()
      local bar = require("dropbar.bar")
      local sources = require("dropbar.sources")
      local configs = require("dropbar.configs")
      local icon_kind_opts = configs.opts.icons.kinds

      local filename = {
        get_symbols = function(buff, win, cursor)
          local symbols = sources.path.get_symbols(buff, win, cursor)
          return { symbols[#symbols] }
        end,
      }

      local directory = {
        get_symbols = function(buff, win, cursor)
          return {
            bar.dropbar_symbol_t:new({
              icon = icon_kind_opts.symbols.Folder,
              icon_hl = "DropBarIconKindFolder",
              name = vim.fn.expand("%:p:h:t"),
              name_hl = "DropBarKindFolder",
            }),
          }
        end,
      }
      return {
        bar = { sources = { directory, filename } },
      }
    end,
  },
}
