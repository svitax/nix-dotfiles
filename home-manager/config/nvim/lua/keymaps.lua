local map = vim.keymap.set

-- Stop highlighting from search
map("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear search highlight" })

-- Quit all
map("n", "<leader>q", "<cmd>qa<cr>", { desc = "Quit" })

-- Move lines/selection up and down and indent appropriately
map("n", "<C-j>", "<cmd>m .+1<cr>==", { desc = "Move line down" })
map("n", "<C-k>", "<cmd>m .-2<cr>==", { desc = "Move line up" })
map("v", "<C-j>", ":m '>+1<cr>gv=gv", { desc = "Move selection down" })
map("v", "<C-k>", ":m '<-2<cr>gv=gv", { desc = "Move selection up" })

-- Keep cursor centered when using n(next) and N(previous)
map({ "n", "v" }, "n", "nzzzv", { desc = "Go to next match (centered)" })
map({ "n", "v" }, "N", "Nzzzv", { desc = "Go to previous match (centered)" })

-- Stays in visual mode while indenting
map({ "v" }, ">", ">gv", { desc = "Indent right" })
map({ "v" }, "<", "<gv", { desc = "Indent left" })

-- Goto
map("", "mm", "%", { remap = true, desc = "Goto matching bracket" })
map("", "ge", "G", { desc = "Goto last line" })
map("", "gh", "0", { desc = "Goto line start" })
map("", "gl", "$", { desc = "Goto line end" })
map("", "gs", "^", { desc = "Goto first non-blank in line" })
map("", "gt", "H", { desc = "Goto window top" })
map("", "gc", "M", { desc = "Goto window center" })
map("", "gb", "L", { desc = "Goto window bottom" })
map("n", "gn", ":bn<cr>", { desc = "Goto next buffer" })
map("n", "gp", ":bp<cr>", { desc = "Goto previous buffer" })
map("n", "ga", "<C-6>", { desc = "Goto alternate file" })

-- Redo
map("n", "U", "<c-r>", { desc = "Redo" })

-- Save
map("n", "<C-s>", ":up!<cr>", { desc = "Save" })

-- i to indent properly on empty lines
map("n", "i", function()
  if #vim.fn.getline(".") == 0 then
    return [["_cc]]
  else
    return "i"
  end
end, { expr = true })

-- smart deletion, dd
-- solves the issue where you want to delete an empty line, but dd will override your last yank
-- will check you are deleting an empty line, if so - use the black hole register
map("n", "dd", function()
  if vim.api.nvim_get_current_line():match("^%s*$") then
    return '"_dd'
  else
    return "dd"
  end
end, { noremap = true, expr = true })
