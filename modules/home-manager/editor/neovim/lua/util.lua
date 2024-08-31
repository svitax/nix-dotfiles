local M = {}

M.lsp = {
    ---@alias lsp.Client.filter {id?: number, bufnr?: number, name?: string, method?: string, filter?:fun(client: lsp.Client):boolean}
    ---@param opts? lsp.Client.filter
    get_clients = function(opts)
        local ret = {} ---@type lsp.Client[]
        if vim.lsp.get_clients then
            ret = vim.lsp.get_clients(opts)
        else
            ---@diagnostic disable-next-line: deprecated
            ret = vim.lsp.get_active_clients(opts)
            if opts and opts.method then
                ---@param client lsp.Client
                ret = vim.tbl_filter(function(client)
                    return client.supports_method(opts.method, { bufnr = opts.bufnr })
                end, ret)
            end
        end
        return opts and opts.filter and vim.tbl_filter(opts.filter, ret) or ret
    end,
    ---@param on_attach fun(client, buffer)
    on_attach = function(on_attach)
        vim.api.nvim_create_autocmd("LspAttach", {
            callback = function(args)
                local buffer = args.buf ---@type number
                local client = vim.lsp.get_client_by_id(args.data.client_id)
                on_attach(client, buffer)
            end,
        })
    end,
}

function M.is_win()
    return vim.loop.os_uname().sysname:find("Windows") ~= nil
end

---@class util.telescope.opts
---@field cwd? string|boolean
---@field show_untracked? boolean

-- This will return a function that calls telescope
-- cwd will default to util.get_root
-- for `files`, git_files or find_files will be chosen depending on .git
---@param builtin string
---@param opts? util.telescope.opts
function M.telescope(builtin, opts)
    local params = { builtin = builtin, opts = opts }
    return function()
        builtin = params.builtin
        opts = params.opts
        opts = vim.tbl_deep_extend("force", { cwd = M.get() }, opts or {}) --[[@as util.telescope.opts]]
        -- opts = vim.tbl_deep_extend("force", {}, opts or {}) --[[@as util.telescope.opts]]
        if builtin == "files" then
            if vim.loop.fs_stat((opts.cwd or vim.loop.cwd()) .. "/.git") then
                opts.show_untracked = true
                builtin = "git_files"
            else
                builtin = "find_files"
            end
        end
        if opts.cwd and opts.cwd ~= vim.loop.cwd() then
            ---@diagnostic disable-next-line: inject-field
            opts.attach_mappings = function(_, map)
                map("i", "<a-c>", function()
                    local action_state = require("telescope.actions.state")
                    local line = action_state.get_current_line()
                    M.telescope(
                        params.builtin,
                        vim.tbl_deep_extend("force", {}, params.opts or {}, { cwd = false, default_text = line })
                    )()
                end)
                return true
            end
        end

        require("telescope.builtin")[builtin](opts)
    end
end

---@type LazyRootSpec[]
M.spec = { "lsp", { ".git", "lua" }, "cwd" }

M.detectors = {}

function M.detectors.cwd()
    return { vim.loop.cwd() }
end

function M.detectors.lsp(buf)
    local bufpath = M.bufpath(buf)
    if not bufpath then
        return {}
    end
    local roots = {} ---@type string[]
    for _, client in pairs(M.lsp.get_clients({ bufnr = buf })) do
        -- only check workspace folders, since we're not interested in clients
        -- running in single file mode
        local workspace = client.config.workspace_folders
        for _, ws in pairs(workspace or {}) do
            roots[#roots + 1] = vim.uri_to_fname(ws.uri)
        end
    end
    return vim.tbl_filter(function(path)
        -- path = Util.norm(path)
        return path and bufpath:find(path, 1, true) == 1
    end, roots)
end

---@param patterns string[]|string
function M.detectors.pattern(buf, patterns)
    patterns = type(patterns) == "string" and { patterns } or patterns
    local path = M.bufpath(buf) or vim.loop.cwd()
    local pattern = vim.fs.find(patterns, { path = path, upward = true })[1]
    return pattern and { vim.fs.dirname(pattern) } or {}
end

function M.bufpath(buf)
    return M.realpath(vim.api.nvim_buf_get_name(assert(buf)))
end

function M.cwd()
    return M.realpath(vim.loop.cwd()) or ""
end

function M.realpath(path)
    if path == "" or path == nil then
        return nil
    end
    path = vim.loop.fs_realpath(path) or path
    -- return Util.norm(path)
    return path
end

---@param spec LazyRootSpec
---@return LazyRootFn
function M.resolve(spec)
    if M.detectors[spec] then
        return M.detectors[spec]
    elseif type(spec) == "function" then
        return spec
    end
    return function(buf)
        return M.detectors.pattern(buf, spec)
    end
end

---@param opts? { buf?: number, spec?: LazyRootSpec[], all?: boolean }
function M.detect(opts)
    opts = opts or {}
    opts.spec = opts.spec or type(vim.g.root_spec) == "table" and vim.g.root_spec or M.spec
    opts.buf = (opts.buf == nil or opts.buf == 0) and vim.api.nvim_get_current_buf() or opts.buf

    local ret = {} ---@type LazyRoot[]
    for _, spec in ipairs(opts.spec) do
        local paths = M.resolve(spec)(opts.buf)
        paths = paths or {}
        paths = type(paths) == "table" and paths or { paths }
        local roots = {} ---@type string[]
        for _, p in ipairs(paths) do
            local pp = M.realpath(p)
            if pp and not vim.tbl_contains(roots, pp) then
                roots[#roots + 1] = pp
            end
        end
        table.sort(roots, function(a, b)
            return #a > #b
        end)
        if #roots > 0 then
            ret[#ret + 1] = { spec = spec, paths = roots }
            if opts.all == false then
                break
            end
        end
    end
    return ret
end

---@type table<number, string>
M.cache = {}

-- returns the root directory based on:
-- * lsp workspace folders
-- * lsp root_dir
-- * root pattern of filename of the current buffer
-- * root pattern of cwd
---@param opts? {normalize?:boolean}
---@return string
function M.get(opts)
    local buf = vim.api.nvim_get_current_buf()
    local ret = M.cache[buf]
    if not ret then
        local roots = M.detect({ all = false })
        ret = roots[1] and roots[1].paths[1] or vim.loop.cwd()
        M.cache[buf] = ret
    end
    if opts and opts.normalize then
        return ret
    end
    return M.is_win() and ret:gsub("/", "\\") or ret
end

---Wrapper of nvim_get_hl(), but does not create a cleared highlight group
---if it doesn't exist
---NOTE: vim.api.nvim_get_hl() has a side effect, it will create a cleared
---highlight group if it doesn't exist, see
---https://github.com/neovim/neovim/issues/24583
---This affects regions highlighted by non-existing highlight groups in a
---winbar, which should falls back to the default 'WinBar' or 'WinBarNC'
---highlight groups but instead falls back to 'Normal' highlight group
---because of this side effect
---So we need to check if the highlight group exists before calling
---vim.api.nvim_get_hl()
---@param ns_id integer
---@param opts table{ name: string?, id: integer?, link: boolean? }
---@return vim.api.keyset.highlight: highlight attributes
function M.gethl(ns_id, opts)
    if not opts.name then
        return vim.api.nvim_get_hl(ns_id, opts)
    end
    return vim.fn.hlexists(opts.name) == 1 and vim.api.nvim_get_hl(ns_id, opts) or {}
end

---@param attr_type 'fg'|'bg'
---@param fbg? string|integer
---@param default? integer
---@return integer|string|nil
function M.normalize_fg_or_bg(attr_type, fbg, default)
    if not fbg then
        return default
    end
    local data_type = type(fbg)
    if data_type == "number" then
        return fbg
    end
    if data_type == "string" then
        if vim.fn.hlexists(fbg) == 1 then
            return vim.api.nvim_get_hl(0, {
                name = fbg,
                link = false,
            })[attr_type]
        end
        if fbg:match("^#%x%x%x%x%x%x$") then
            return fbg
        end
    end
    return default
end

---Normalize highlight attributes
---1. Replace `attr.fg` and `attr.bg` with their corresponding color codes
---   if they are set to highlight group names
---2. If `attr.link` used in combination with other attributes, will first
---   retrieve the attributes of the linked highlight group, then merge
---   with other attributes
---Side effect: change `attr` table
---@param attr vim.api.keyset.highlight highlight attributes
---@return table: normalized highlight attributes
function M.normalize(attr)
    if attr.link then
        local num_keys = #vim.tbl_keys(attr)
        if num_keys <= 1 then
            return attr
        end
        attr.fg = M.normalize_fg_or_bg("fg", attr.fg)
        attr.bg = M.normalize_fg_or_bg("bg", attr.bg)
        attr = vim.tbl_extend("force", M.gethl(0, { name = attr.link, link = false }) or {}, attr)
        attr.link = nil
        return attr
    end
    attr.fg = M.normalize_fg_or_bg("fg", attr.fg)
    attr.bg = M.normalize_fg_or_bg("bg", attr.bg)
    return attr
end

---Wrapper of nvim_set_hl(), normalize highlight attributes before setting
---@param ns_id integer namespace id
---@param name string
---@param attr vim.api.keyset.highlight highlight attributes
---@return nil
function M.set(ns_id, name, attr)
    return vim.api.nvim_set_hl(ns_id, name, M.normalize(attr))
end

local todec = {
    ["0"] = 0,
    ["1"] = 1,
    ["2"] = 2,
    ["3"] = 3,
    ["4"] = 4,
    ["5"] = 5,
    ["6"] = 6,
    ["7"] = 7,
    ["8"] = 8,
    ["9"] = 9,
    ["a"] = 10,
    ["b"] = 11,
    ["c"] = 12,
    ["d"] = 13,
    ["e"] = 14,
    ["f"] = 15,
    ["A"] = 10,
    ["B"] = 11,
    ["C"] = 12,
    ["D"] = 13,
    ["E"] = 14,
    ["F"] = 15,
}

---Convert an integer from hexadecimal to decimal
---@param hex string
---@return integer dec
function M.hex2dec(hex)
    local digit = 1
    local dec = 0
    while digit <= #hex do
        dec = dec + todec[string.sub(hex, digit, digit)] * 16 ^ (#hex - digit)
        digit = digit + 1
    end
    return dec
end

---Convert an integer from decimal to hexadecimal
---@param int integer
---@param n_digits integer? number of digits used for the hex code
---@return string hex
function M.dec2hex(int, n_digits)
    return not n_digits and string.format("%x", int) or string.format("%0" .. n_digits .. "x", int)
end

---Convert a hex color to rgb color
---@param hex string hex code of the color
---@return integer[] rgb
function M.hex2rgb(hex)
    return {
        M.hex2dec(string.sub(hex, 1, 2)),
        M.hex2dec(string.sub(hex, 3, 4)),
        M.hex2dec(string.sub(hex, 5, 6)),
    }
end
---Convert an rgb color to hex color
---@param rgb integer[]
---@return string
function M.rgb2hex(rgb)
    local hex = {
        M.dec2hex(math.floor(rgb[1])),
        M.dec2hex(math.floor(rgb[2])),
        M.dec2hex(math.floor(rgb[3])),
    }
    hex = {
        string.rep("0", 2 - #hex[1]) .. hex[1],
        string.rep("0", 2 - #hex[2]) .. hex[2],
        string.rep("0", 2 - #hex[3]) .. hex[3],
    }
    return table.concat(hex, "")
end

---Blend two colors
---@param c1 string|number|table the first color, in hex, dec, or rgb
---@param c2 string|number|table the second color, in hex, dec, or rgb
---@param alpha number? between 0~1, weight of the first color, default to 0.5
---@return { hex: string, dec: integer, r: integer, g: integer, b: integer }
function M.cblend(c1, c2, alpha)
    alpha = alpha or 0.5
    c1 = type(c1) == "number" and M.dec2hex(c1, 6) or c1
    c2 = type(c2) == "number" and M.dec2hex(c2, 6) or c2
    local rgb1 = type(c1) == "string" and M.hex2rgb(c1:gsub("#", "", 1)) or c1
    local rgb2 = type(c2) == "string" and M.hex2rgb(c2:gsub("#", "", 1)) or c2
    local rgb_blended = {
        alpha * rgb1[1] + (1 - alpha) * rgb2[1],
        alpha * rgb1[2] + (1 - alpha) * rgb2[2],
        alpha * rgb1[3] + (1 - alpha) * rgb2[3],
    }
    local hex = M.rgb2hex(rgb_blended)
    return {
        hex = "#" .. hex,
        dec = M.hex2dec(hex),
        r = math.floor(rgb_blended[1]),
        g = math.floor(rgb_blended[2]),
        b = math.floor(rgb_blended[3]),
    }
end

---Blend two hlgroups
---@param h1 string|table the first hlgroup name or highlight attribute table
---@param h2 string|table the second hlgroup name or highlight attribute table
---@param alpha number? between 0~1, weight of the first color, default to 0.5
---@return table: merged color or highlight attributes
function M.blend(h1, h2, alpha)
  -- stylua: ignore start
  h1 = type(h1) == 'table' and h1 or M.gethl(0, { name = h1, link = false })
  h2 = type(h2) == 'table' and h1 or M.gethl(0, { name = h2, link = false })
  local fg = h1.fg and h2.fg and M.cblend(h1.fg, h2.fg, alpha).dec or h1.fg or h2.fg
  local bg = h1.bg and h2.bg and M.cblend(h1.bg, h2.bg, alpha).dec or h1.bg or h2.bg
  return vim.tbl_deep_extend('force', h1, h2, { fg = fg, bg = bg })
    -- stylua: ignore end
end

return M