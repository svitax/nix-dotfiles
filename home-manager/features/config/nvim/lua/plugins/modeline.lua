local icons = require("config").icons
local palette = require("nightfox.palette").load("gruvfox")

local components = {
  mode = {
    "mode",
    -- icon = { " " },
    fmt = function(str)
      -- return ""
      local lower = str:lower()
      return lower:sub(1, 1):upper() .. lower:sub(2)
    end,
    color = { gui = "bold" },
  },
  inactive_mode = {
    "mode",
    -- icon = { " " },
    fmt = function(str)
      -- return ""
      local lower = str:lower()
      return lower:sub(1, 1):upper() .. lower:sub(2)
    end,
    color = { gui = "bold", fg = palette.bg1, bg = palette.fg3 },
  },
  filename = {
    "filename",
    fmt = function(filename, _)
      -- if utils.is_buf_unnamed() then
      --   return ""
      -- end
      return filename
    end,
    file_status = false,
    color = { fg = palette.fg2, gui = "bold" },
  },
  search_count = {
    function()
      if vim.v.hlsearch == 0 then
        return ""
      end
      local ok, count = pcall(vim.fn.searchcount, { recompute = true })
      if (not ok) or (count.current == nil) or (count.total == 0) then
        return "0/0"
      end
      if count.incomplete == 1 then
        return "?/?"
      end

      local too_many = (">%d"):format(count.maxcount)
      local total = ((count.total > count.maxcount) and too_many) or count.total
      return ("%s/%s"):format(count.current, total)
    end,
    color = { fg = palette.blue.base },
  },
  diagnostics = {
    "diagnostics",
    symbols = {
      error = icons.diagnostics.Error,
      warn = icons.diagnostics.Warn,
      info = icons.diagnostics.Information,
      hint = icons.diagnostics.Hint,
    },
  },
  filetype = {
    "filetype",
  },
}
return {
  -- Icons
  { "nvim-tree/nvim-web-devicons", lazy = true },
  -- Set lualine as statusline
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    init = function()
      vim.g.lualine_laststatus = vim.o.laststatus
      if vim.fn.argc(-1) > 0 then
        -- set an empty statusline till lualine loads
        vim.o.statusline = " "
      else
        -- hide the statusline on the starter page
        vim.o.laststatus = 0
      end
    end,
    -- See `:help lualine.txt`
    opts = function(_, opts)
      local palette = require("nightfox.palette").load("gruvfox")
      return {
        options = {
          icons_enabled = true,
          theme = {
            normal = {
              a = { fg = palette.bg0, bg = palette.green.base },
              b = { fg = palette.fg2, bg = palette.bg3 },
              c = { fg = palette.fg2, bg = palette.bg3 },
            },
            insert = { a = { fg = palette.bg0, bg = palette.blue.base } },
            command = { a = { fg = palette.bg0, bg = palette.yellow.base } },
            visual = { a = { fg = palette.bg0, bg = palette.magenta.base } },
            replace = { a = { fg = palette.bg0, bg = palette.red.base } },
            terminal = { a = { fg = palette.bg0, bg = palette.orange.base } },
          },
          component_separators = "",
          section_separators = "",
        },
        sections = {
          lualine_a = { components.mode },
          lualine_b = {
            components.search_count,
            -- components.filename,
          },
          lualine_c = {},
          lualine_x = { components.diagnostics },
          lualine_y = { "location", "progress", components.filetype },
          lualine_z = {},
        },
        inactive_sections = {
          lualine_a = { components.inactive_mode },
          lualine_b = {},
          lualine_c = {},
          lualine_x = { components.diagnostics },
          lualine_y = { "location", "progress", components.filetype },
          lualine_z = {},
        },
        extensions = { "lazy" },
      }
    end,
  },
}
