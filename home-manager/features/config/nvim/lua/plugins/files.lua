---Change window-local directory to `dir`
---@param dir string
---@return nil
local function lcd(dir)
  local ok = pcall(vim.cmd.lcd, dir)
  if not ok then
    vim.notify("[oil.nvim] failed to cd to " .. dir, vim.log.levels.WARN)
  end
end

return {
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = function()
      -- TODO: replace vim.loop with vim.uv
      local preview_wins = {} ---@type table<integer, integer>
      local preview_bufs = {} ---@type table<integer, integer>
      local preview_max_fsize = 1000000
      local preview_debounce = 64 -- ms
      local preview_request_last_timestamp = 0

      -- TODO: finish archive func with atool. or create action for multiselection cmdline put
      local function archive()
        local oil = require("oil")
        local mode = vim.api.nvim_get_mode().mode
        local is_visual = mode:match("^[vV]")

        local entries = {}
        if is_visual then
          local _, start_lnum, _, _ = unpack(vim.fn.getpos("v"))
          local _, end_lnum, _, _ = unpack(vim.fn.getcurpos())
          if start_lnum > end_lnum then
            start_lnum, end_lnum = end_lnum, start_lnum
          end
          for i = start_lnum, end_lnum do
            local entry = oil.get_entry_on_line(0, 1)
            if entry then
              table.insert(entries, entry)
            end
          end
        else
          local entry = oil.get_current_dir()
          if entry then
            table.insert(entries, entry)
          end
        end
        if vim.tbl_isempty(entries) then
          vim.notify("Could not find entry under cursor", vim.log.levels.WARN)
          return
        end
        vim.notify(vim.inspect(entries))
      end

      ---Generate lines for preview window when preview is not available
      ---@param msg string
      ---@param height integer
      ---@param width integer
      ---@return string[]
      local function nopreview(msg, height, width)
        local lines = {}
        local fillchar = vim.opt_local.fillchars:get().diff or "-"
        local msglen = #msg + 4
        local padlen_l = math.max(0, math.floor((width - msglen) / 2))
        local padlen_r = math.max(0, width - msglen - padlen_l)
        local line_fill = fillchar:rep(width)
        local half_fill_l = fillchar:rep(padlen_l)
        local half_fill_r = fillchar:rep(padlen_r)
        local line_above = half_fill_l .. string.rep(" ", msglen) .. half_fill_r
        local line_below = line_above
        local line_msg = half_fill_l .. "  " .. msg .. "  " .. half_fill_r
        local half_height_u = math.max(0, math.floor((height - 3) / 2))
        local half_height_d = math.max(0, height - 3 - half_height_u)
        for _ = 1, half_height_u do
          table.insert(lines, line_fill)
        end
        table.insert(lines, line_above)
        table.insert(lines, line_msg)
        table.insert(lines, line_below)
        for _ = 1, half_height_d do
          table.insert(lines, line_fill)
        end
        return lines
      end

      ---End preview for oil window `win`
      ---Close preview window and delete preview buffer
      ---@param oil_win? integer oil window ID
      ---@return nil
      local function end_preview(oil_win)
        oil_win = oil_win or vim.api.nvim_get_current_win()
        local preview_win = preview_wins[oil_win]
        local preview_buf = preview_bufs[oil_win]
        if preview_win and vim.api.nvim_win_is_valid(preview_win) and vim.fn.winbufnr(preview_win) == preview_buf then
          vim.api.nvim_win_close(preview_win, true)
        end
        if preview_buf and vim.api.nvim_win_is_valid(preview_buf) then
          vim.api.nvim_win_close(preview_buf, true)
        end
        preview_wins[oil_win] = nil
        preview_bufs[oil_win] = nil
      end

      ---Preview file under cursor in a split
      ---@return nil
      local function preview()
        local oil = require("oil")

        local entry = oil.get_cursor_entry()
        local fname = entry and entry.name
        local dir = oil.get_current_dir()
        if not dir or not fname then
          return
        end
        local fpath = vim.fs.joinpath(dir, fname)
        local stat = vim.loop.fs_stat(fpath)
        if not stat or (stat.type ~= "file" and stat.type ~= "directory") then
          return
        end
        local oil_win = vim.api.nvim_get_current_win()
        local preview_win = preview_wins[oil_win]
        local preview_buf = preview_bufs[oil_win]
        if
          not preview_win
          or not preview_buf
          or not vim.api.nvim_win_is_valid(preview_win)
          or not vim.api.nvim_buf_is_valid(preview_buf)
        then
          local oil_win_height = vim.api.nvim_win_get_height(oil_win)
          local oil_win_width = vim.api.nvim_win_get_width(oil_win)
          vim.cmd(oil_win_width > 6 * oil_win_height and "vertical new" or "new")
          preview_win = vim.api.nvim_get_current_win()
          preview_buf = vim.api.nvim_get_current_buf()
          preview_wins[oil_win] = preview_win
          preview_bufs[oil_win] = preview_buf
          vim.bo[preview_buf].filetype = "oil_preview"
          vim.bo[preview_buf].buftype = "nofile"
          vim.bo[preview_buf].bufhidden = "wipe"
          vim.bo[preview_buf].swapfile = false
          vim.bo[preview_buf].buflisted = false
          vim.opt_local.spell = false
          vim.opt_local.number = false
          vim.opt_local.relativenumber = false
          vim.opt_local.signcolumn = "no"
          vim.opt_local.foldcolumn = "0"
          vim.opt_local.winbar = ""
          vim.api.nvim_set_current_win(oil_win)
        end
        -- Set keymap for opening the file from preview buffer
        vim.keymap.set("n", "<CR>", function()
          vim.cmd.edit(fpath)
          end_preview(oil_win)
        end, { buffer = preview_buf })
        -- Preview buffer already contains contents of file to preview
        local preview_bufname = vim.fn.bufname(preview_buf)
        local preview_bufnewname = "oil_preview://" .. fpath
        if preview_bufname == preview_bufnewname then
          return
        end
        local preview_win_height = vim.api.nvim_win_get_height(preview_win)
        local preview_win_width = vim.api.nvim_win_get_width(preview_win)
        local add_syntax = false
        local lines = {}
        lines = stat.type == "directory" and vim.fn.systemlist("ls -lhA " .. vim.fn.shellescape(fpath))
          or stat.size == 0 and nopreview("Empty file", preview_win_height, preview_win_width)
          or stat.size > preview_max_fsize and nopreview(
            "File too large to preview",
            preview_win_height,
            preview_win_width
          )
          or not vim.fn.system({ "file", fpath }):match("text") and nopreview(
            "Binary file, no preview available",
            preview_win_height,
            preview_win_width
          )
          or (function()
              add_syntax = true
              return true
            end)()
            and vim
              .iter(io.lines(fpath))
              :map(function(line)
                return (line:gsub("\x0d$", ""))
              end)
              :totable()
        vim.api.nvim_buf_set_lines(preview_buf, 0, -1, false, lines)
        vim.api.nvim_buf_set_name(preview_buf, preview_bufnewname)
        -- If previewing a directory, change cwd to that directory
        -- so that we can `gf` to files in the preview buffer;
        -- else change cwd to the parent directory of the file in preview
        vim.api.nvim_win_call(preview_win, function()
          local target_dir = stat.type == "directory" and fpath or dir
          if not vim.fn.getcwd(0) ~= target_dir then
            lcd(target_dir)
          end
        end)
        vim.api.nvim_buf_call(preview_buf, function()
          vim.treesitter.stop(preview_buf)
        end)
        vim.bo[preview_buf].syntax = ""
        if not add_syntax then
          return
        end
        local ft = vim.filetype.match({
          buf = preview_buf,
          filename = fpath,
        })
        if ft and not pcall(vim.treesitter.start, preview_buf, ft) then
          vim.bo[preview_buf].syntax = ft
        end
      end

      local groupid_preview = vim.api.nvim_create_augroup("OilPreview", {})
      vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled" }, {
        desc = "Update floating preview window when cursor moves or window scrolls.",
        group = groupid_preview,
        pattern = "oil:///*",
        callback = function()
          local oil_win = vim.api.nvim_get_current_win()
          local preview_win = preview_wins[oil_win]
          if not preview_win or not vim.api.nvim_win_is_valid(preview_win) then
            end_preview()
            return
          end
          local current_request_timestamp = vim.loop.now()
          preview_request_last_timestamp = current_request_timestamp
          vim.defer_fn(function()
            if preview_request_last_timestamp == current_request_timestamp then
              preview()
            end
          end, preview_debounce)
        end,
      })
      vim.api.nvim_create_autocmd("BufEnter", {
        desc = "Close preview window when leaving oil buffers.",
        group = groupid_preview,
        callback = function(info)
          if vim.bo[info.buf].filetype ~= "oil" then
            end_preview()
          end
        end,
      })
      vim.api.nvim_create_autocmd("WinClosed", {
        desc = "Close preview window when closing oil windows.",
        group = groupid_preview,
        callback = function(info)
          local win = tonumber(info.match)
          if win and preview_wins[win] then
            end_preview(win)
          end
        end,
      })

      ---Toggle floating preview window
      ---@return nil
      local function toggle_preview()
        local oil_win = vim.api.nvim_get_current_win()
        local preview_win = preview_wins[oil_win]
        if not preview_win or not vim.api.nvim_win_is_valid(preview_win) then
          preview()
          return
        end
        end_preview()
      end

      local preview_mapping = {
        mode = { "n", "x" },
        desc = "Toggle preview",
        callback = toggle_preview,
      }

      return {
        columns = {
          {
            "type",
            icons = { directory = "-", fifo = "p", file = "-", link = "", socket = "s" },
            highlight = "Comment",
          },
          { "permissions", highlight = "Comment" },
          { "size", highlight = "Special" },
          { "mtime", highlight = "Number" },
          { "icon", default_file = "", directory = "", add_padding = false },
        },
        win_options = {
          number = false,
          relativenumber = false,
          signcolumn = "no",
          foldcolumn = "0",
          statuscolumn = "",
        },
        cleanup_delay_ms = 0,
        delete_to_trash = true,
        skip_confirm_for_simple_edits = true,
        prompt_save_on_select_new_entry = true,
        use_default_keymaps = false,
        view_options = {
          is_always_hidden = function(name)
            return name == ".."
          end,
        },
        float = { border = "solid", win_options = { winblend = 0 } },
        preview = { border = "solid", win_options = { winblend = 0 } },
        progress = { border = "solid", win_options = { winblend = 0 } },
        keymaps = {
          ["g?"] = "actions.show_help",
          ["K"] = preview_mapping,
          ["<C-k>"] = preview_mapping,
          ["-"] = "actions.parent",
          ["="] = "actions.select",
          ["+"] = "actions.select",
          ["<CR>"] = "actions.select",
          ["<C-h>"] = "actions.toggle_hidden",
          ["<C-r>"] = "actions.refresh",
          ["gs"] = "actions.change_sort",
          ["gx"] = "actions.open_external",
          ["gy"] = "actions.copy_entry_path",
          ["g."] = "actions.open_cmdline",
          ["gt"] = "actions.toggle_trash",
          ["gz"] = archive,
          ["<C-c>"] = "actions.close",
        },
      }
    end,
    config = function(_, opts)
      local oil = require("oil")
      oil.setup(opts)

      local groupid = vim.api.nvim_create_augroup("OilSyncCwd", {})
      vim.api.nvim_create_autocmd({ "BufEnter", "TextChanged" }, {
        desc = "Set cwd to follow directory shown in oil buffers.",
        group = groupid,
        pattern = "oil:///*",
        callback = function(info)
          if vim.bo[info.buf].filetype == "oil" then
            local cwd = vim.fs.normalize(vim.fn.getcwd(vim.fn.winnr()))
            local oildir = vim.fs.normalize(oil.get_current_dir())
            if cwd ~= oildir and vim.loop.fs_stat(oildir) then
              lcd(oildir)
            end
          end
        end,
      })
      vim.api.nvim_create_autocmd("DirChanged", {
        desc = "Let oil buffers follow cwd.",
        group = groupid,
        callback = function(info)
          if vim.bo[info.buf].filetype == "oil" then
            vim.defer_fn(function()
              local cwd = vim.fs.normalize(vim.fn.getcwd(vim.fn.winnr()))
              local oildir = vim.fs.normalize(oil.get_current_dir() or "")
              if cwd ~= oildir then
                oil.open(cwd)
              end
            end, 100)
          end
        end,
      })

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "oil_preview",
        callback = function(params)
          vim.keymap.set("n", "y", "o", { buffer = params.buf, remap = true, nowait = true })
          vim.keymap.set("n", "n", "c", { buffer = params.buf, remap = true, nowait = true })
        end,
      })

      vim.keymap.set("n", "<leader>e", "<cmd>Oil<cr>", { desc = "File explorer" })
    end,
  },
}
