return {
  {
    "Bekaboo/dropbar.nvim",
    opts = function()
      local bar = require("dropbar.bar")
      local sources = require("dropbar.sources")
      local configs = require("dropbar.configs")
      local icon_kind_opts = configs.opts.icons.kinds

      local filename = {
        get_symbols = function(buf, win, cursor)
          local symbols = sources.path.get_symbols(buf, win, cursor)
          if vim.api.nvim_buf_get_name(buf):find("oil") then
            symbols[#symbols].name = ""
            symbols[#symbols].icon = ""
          end
          return { symbols[#symbols] }
        end,
      }

      local directory = {
        get_symbols = function(buf, win, cursor)
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
        general = {
          enable = function(buf, win)
            return not vim.api.nvim_win_get_config(win).zindex
              and vim.bo[buf].buftype == ""
              and vim.api.nvim_buf_get_name(buf) ~= ""
              and not vim.wo[win].diff
              and vim.filetype ~= "terminal"
              and not vim.api.nvim_buf_get_name(buf):find("Neogit")
              and not vim.api.nvim_buf_get_name(buf):find("Trouble")
              and not vim.api.nvim_buf_get_name(buf):find("COMMIT_EDITMSG")
          end,
        },
      }
    end,
  },
}
