return {
    {
        "Wansmer/symbol-usage.nvim",
        event = "LspAttach", -- need run before LspAttach if you use nvim 0.9. On 0.10 use 'LspAttach'
        opts = {
            vt_position = "above",
            request_pending_text = "loading...",
            references = { enabled = true, include_declaration = false },
            definition = { enabled = false },
            implementation = { enabled = false },
            ---@type function(symbol: Symbol): string Symbol{ definition = integer|nil, implementation = integer|nil, references = integer|nil }
            text_format = function(symbol)
                local fragments = {}

                if symbol.references then
                    local usage = symbol.references <= 1 and "reference" or "references"
                    local num = symbol.references == 0 and "no" or symbol.references
                    table.insert(fragments, ("%s %s"):format(num, usage))
                end

                if symbol.definition then
                    local usage = symbol.definition <= 1 and "definition" or "definitions"
                    local num = symbol.definition == 0 and "no" or symbol.definition
                    table.insert(fragments, ("%s %s"):format(num, usage))
                end

                if symbol.implementation then
                    local usage = symbol.implementation <= 1 and "implement" or "implements"
                    local num = symbol.implementation == 0 and "no" or symbol.implementation
                    table.insert(fragments, ("%s %s"):format(num, usage))
                end

                return table.concat(fragments, ", ")
            end,
        },
    },
}
