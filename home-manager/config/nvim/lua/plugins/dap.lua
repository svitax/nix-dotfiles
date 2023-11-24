local adapters = {
  python = function()
    -- require("dap-python").setup(nil, { include_configs = false })
    require("dap-python").setup()
  end,
}
return {

  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "mfussenegger/nvim-dap-python",
      "LiadOz/nvim-dap-repl-highlights",
      "nvim-telescope/telescope-dap.nvim",
    },
    config = function()
      local icons = require("config").icons.dap

      -- Dap icon customization
      -- stylua: ignore start
      vim.fn.sign_define("DapBreakpoint", { text = icons.breakpoint, texthl = "ErrorMsg", linehl = "", numhl = "" })
      vim.fn.sign_define("DapBreakpointCondition", { text = icons.breakpoint_condition, texthl = "ErrorMsg", linehl = "", numhl = "" })
      vim.fn.sign_define("DapLogPoint", { text = icons.log_point, texthl = "ErrorMsg", linehl = "", numhl = "" })
      vim.fn.sign_define("DapStopped", { text = icons.stopped, texthl = "ErrorMsg", linehl = "Substitute", numhl = "Substitute" })
      vim.fn.sign_define("DapBreakpointRejected", { text = icons.breakpoint_rejected, texthl = "WarningMsg", linehl = "", numhl = "" })
      -- stylua: ignore stop

      require("nvim-dap-repl-highlights").setup()
      -- require("nvim-dap-virtual-text").setup()
      require("telescope").load_extension("dap")

      -- Set up the ui
      require("dapui").setup({
        icons = { expanded = icons.expanded, collapsed = icons.collapsed, current_frame = icons.current_frame },
        mappings = {
          expand = { "<CR>", "<2-LeftMouse>" },
          open = "o",
          remove = "d",
          edit = "c",
          repl = "r",
          toggle = "t",
        },
        layouts = {
          { elements = { "breakpoints", "stacks", "scopes" }, size = 38, position = "left" },
          { elements = { "repl", "watches" }, size = 0.24, position = "bottom" },
        },
        controls = {
          enabled = true,
          -- Display controls in this element
          element = "repl",
          icons = {
            pause = icons.pause,
            play = icons.play,
            step_into = icons.step_into,
            step_over = icons.step_over,
            step_out = icons.step_out,
            step_back = icons.step_back,
            run_last = icons.run_last,
            terminate = icons.terminate,
          },
        },
      })

      -- Open ui on start
      require("dap").listeners.after.event_initialized["dapui_config"] = function()
        require("dapui").open()
      end
      require("dap").listeners.before.event_terminated["dapui_config"] = function()
        require("dapui").close()
      end
      require("dap").listeners.before.event_exited["dapui_config"] = function()
        require("dapui").close()
      end

      -- Initialize all debug adapters
      for adapter, config in pairs(adapters) do
        if type(config) == "function" then
          config()
        else
          require("dap").adapters[adapter] = config
        end
      end
    end,
  },
}
