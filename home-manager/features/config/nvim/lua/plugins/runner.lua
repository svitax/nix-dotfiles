local configs = {
  -- Python
  {
    name = "Launch file",
    group = "python",
    type = "python",
    request = "launch",
    program = "${file}",
    presentation = "menuhidden",
  },
  {
    name = "Launch file with arguments",
    group = "python",
    type = "python",
    request = "launch",
    program = "${file}",
    args = function()
      local args_string = vim.fn.input("Arguments: ")
      return vim.split(args_string, " +")
    end,
    presentation = "menuhidden",
  },

  -- Rust
  {
    name = "Run current project",
    group = "rust",
    command = "cargo run",
    presentation = "menuhidden",
  },
}

return {
  {
    "kndndrj/nvim-projector",
    dependencies = { "kndndrj/projector-loader-vscode", "mfussenegger/nvim-dap" },
    opts = function()
      -- Keymappings
      -- need toggle
      -- stylua: ignore start
      vim.keymap.set("n", "<leader>gt", "<cmd>lua require('projector').toggle()<cr>", { desc = "Toggle output" })
      -- vim.keymap.set("n", "<leader>gl", "dap_launch", { desc = "Launch debug target" })
      vim.keymap.set( "n", "<leader>gr", "<cmd>lua require('projector').restart()<cr>", { desc = "Restart task/debugging session" }) -- restart task
      vim.keymap.set( "n", "<leader>gb", "<cmd>lua require('dap').toggle_breakpoint()<cr>", { desc = "Toggle breakpoint" })
      vim.keymap.set( "n", "<leader>gc", "<cmd>lua require('projector').continue()<cr>", { desc = "Continue program execution" }) -- continue task
      -- vim.keymap.set("n", "<leader>gh", "dap_pause", { desc = "Pause program execution" })
      vim.keymap.set("n", "<leader>gi", "<cmd>lua require('dap').step_into()<cr>", { desc = "Step in" })
      vim.keymap.set("n", "<leader>go", "<cmd>lua require('dap').step_out()<cr>", { desc = "Step out" })
      vim.keymap.set("n", "<leader>gw", "<cmd>lua require('dap').step_over()<cr>", { desc = "Step over" })
      vim.keymap.set("n", "<leader>gn", "<cmd>lua require('projector').next()<cr>", { desc = "Step to next" }) -- next task?
      vim.keymap.set("n", "<leader>gv", "<cmd>FzfLua dap_variables<cr>", { desc = "List variables" })
      vim.keymap.set("n", "<leader>gx", "<cmd>lua require('projector').kill()<cr>", { desc = "Kill task/debug session" }) -- kill task
      -- vim.keymap.set("n", "<leader>g<C-c>", "dap_edit_condition", { desc = "Edit breakpoint condition on current line" }) -- set_breakpoint condition
      -- vim.keymap.set("n", "<leader>g<C-l>", "dap_edit_log", { desc = "Edit breakpoint log message on current line" }) -- set breakpoint log message
      -- vim.keymap.set("n", "<leader>gs", "", { desc = "Switch" })
      -- vim.keymap.set("n", "<leader>gst", "dap_switch_thread", { desc = "Switch current thread" })
      -- vim.keymap.set("n", "<leader>gsf", "dap_switch_stack_frame", { desc = "Switch stack frame" })
      -- vim.keymap.set("n", "<leader>ge", "dap_enable_exceptions", { desc = "Enable exception breakpoints" })
      -- vim.keymap.set("n", "<leader>gE", "dap_disable_exceptions", { desc = "Disable exception breakpoints" })
      -- dap-ui
      vim.keymap.set("n", "<leader>ge", "<cmd>lua require('dapui').eval()<cr>", { desc = "Evaluate" })
      -- stylua: ignore end

      return {
        loaders = {
          { module = "tasksjson", options = vim.fn.getcwd() .. "/.vscode/tasks.json" },
          { module = "launchjson", options = vim.fn.getcwd() .. "/.vscode/launch.json" },
          { module = "builtin", options = { path = vim.fn.getcwd() .. "/.vscode/projector.json", configs = configs } },
        },
        display_format = function(_, scope, group, modes, name)
          return scope .. "  " .. group .. "  " .. modes .. "  " .. name
        end,
        automatic_reload = true,
        icons = { enable = true },
      }
    end,
  },
}
