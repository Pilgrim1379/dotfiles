return {
	-- {
	-- 	"simrat39/rust-tools.nvim",
	-- 	event = { "BufReadPre", "BufNewFile" },
	-- 	config = function()
	-- 		local rt = require("rust-tools")

	-- 		rt.setup({
	-- 			server = {
	-- 				on_attach = function(_, bufnr)
	-- 					-- Hover actions
	-- 					vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
	-- 					-- Code action groups
	-- 					vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
	-- 				end,
	-- 			},
	-- 		})
	-- 		-- capabilities = capabilities
	-- 	end,
	-- 	dependencies = {
	-- 		"neovim/nvim-lspconfig",
	-- 		"nvim-lua/plenary.nvim",
	-- 		"mfussenegger/nvim-dap"
	-- 	}
	-- },
}
