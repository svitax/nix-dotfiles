return {
  {
    "luukvbaal/statuscol.nvim",
    event = "BufReadPost",
    branch = "0.10", -- TODO: remove when 0.10 releases
    opts = function()
      local builtin = require("statuscol.builtin")
      -- To display pretty fold icons in `statuscolumn` and show it according to `fillchars`
      local function foldcolumn()
        local chars = vim.opt.fillchars:get()
        local fc = "%#FoldColumn#"
        local clf = "%#CursorLineFold#"
        local hl = vim.fn.line(".") == vim.v.lnum and clf or fc
        local text = " "

        if vim.fn.foldlevel(vim.v.lnum) > vim.fn.foldlevel(vim.v.lnum - 1) then
          if vim.fn.foldclosed(vim.v.lnum) == -1 then
            text = hl .. (chars.foldopen or " ")
          else
            text = hl .. (chars.foldclose or " ")
          end
        elseif vim.fn.foldlevel(vim.v.lnum) == 0 then
          text = hl .. " "
        else
          text = hl .. (chars.foldsep or " ")
        end

        return text
      end
      return {
        relculright = true,
        segments = {
          { sign = { namespace = { "gitsigns" }, maxwidth = 1 }, click = "v:lua.ScSa" },
          { sign = { name = { ".*" }, maxwidth = 1 }, click = "v:lua.ScSa" },
          { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
          { text = { foldcolumn, " " }, click = "v:lua.ScFa" },
        },
      }
    end,
  },
}
