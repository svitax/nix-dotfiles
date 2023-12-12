local M = {}

M.lightbulb = ""

M.ellipsis = "…"

M.folds = {
    -- suffix = "",
    suffix = "",
}
M.diagnostics = {
    Error = "● ",
    Info = "● ",
    Hint = "● ",
    Warn = "● ",
}
M.git = {
    signs = {
        add = "▍",
        change = "▍",
        -- delete = "",
        -- topdelete = "",
        delete = "▁",
        topdelete = "▔",
        changedelete = "▍",
        untracked = "▍",
    },
}
M.dap = {
    breakpoint = "◯",
    breakpoint_condition = "",
    breakpoint_rejected = "",
    log_point = "󰆉",
    stopped = "",
    expanded = "",
    collapsed = "",
    current_frame = "",
    pause = "",
    play = "",
    step_into = "",
    step_over = "",
    step_out = "",
    step_back = "",
    run_last = "↻",
    terminate = "",
}

M.kinds = {
    Array = " ",
    Boolean = "󰨙 ",
    Class = " ",
    Codeium = "󰘦 ",
    Color = " ",
    Control = " ",
    Collapsed = " ",
    Constant = "󰏿 ",
    Constructor = " ",
    Copilot = " ",
    Enum = " ",
    EnumMember = " ",
    Event = " ",
    Field = " ",
    File = "󰈔 ",
    Folder = " ",
    Function = "󰊕 ",
    Interface = " ",
    Key = " ",
    Keyword = " ",
    Method = "󰊕 ",
    Module = " ",
    Namespace = "󰦮 ",
    Null = " ",
    Number = "󰎠 ",
    Object = " ",
    Operator = " ",
    Package = " ",
    Property = " ",
    Reference = " ",
    Snippet = " ",
    String = " ",
    Struct = " ",
    Text = "󰉿 ",
    TypeParameter = " ",
    Unit = " ",
    Value = " ",
    Variable = " ",
}

return M
