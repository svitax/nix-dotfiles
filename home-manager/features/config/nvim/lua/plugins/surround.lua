return {
  {
    "kylechui/nvim-surround",
    opts = {
      aliases = {
        ["b"] = { ")", "]", "}" },
        ["c"] = "}", -- [c]urly brace
        ["r"] = "]", -- [r]ectangular bracket
        ["e"] = "`", -- t[e]mplate string
        ["q"] = { '"', "'", "`" }, -- [q]uotes
      },
      highlight = { duration = 1000 },
      keymaps = {
        insert = "",
        insert_line = "",
        normal = "",
        normal_cur = "",
        normal_line = "",
        normal_cur_line = "",
        visual = "",
        visual_line = "",
        delete = "",
        change = "",
        change_line = "",
      },
      -- keymaps = { normal = "ms", delete = "md", change = "mr" },
    },
    keys = {
      { "ms", "<Plug>(nvim-surround-normal)", desc = "Surround add" },
      { "ml", "<Plug>(nvim-surround-normal-cur)", desc = "Surround add (current line)" },
      { "md", "<Plug>(nvim-surround-delete)", desc = "Surround delete" },
      { "mr", "<Plug>(nvim-surround-change)", desc = "Surround replace" },
      { mode = "x", "ms", "<Plug>(nvim-surround-visual)", desc = "Surround add" },
    },
  },
}
