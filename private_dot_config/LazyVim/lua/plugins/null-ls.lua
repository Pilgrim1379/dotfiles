return {
	{
		"jose-elias-alvarez/null-ls.nvim",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = { "mason.nvim" },
		opts = function()
			local nls = require("null-ls")
			local nls_fmt = nls.builtins.formatting
			local nls_diag = nls.builtins.diagnostics
			local nls_act = nls.builtins.code_actions
			return {
				root_dir = require("null-ls.utils").root_pattern(".null-ls-root", ".neoconf.json", "Makefile", ".git"),
				sources = {
					-- [[ Diagnostics ]]
					-- nls_diag.flake8,
					nls_diag.ruff,
					nls_diag.mypy.with {
						extra_args = { "--ignore-missing-imports" }
					},
					nls_diag.shellcheck.with { diagnostics_format = "[#{c}] #{m} (#{s})" },
					nls_diag.chktex,
					-- nls_diag.cppcheck,
					-- nls_diag.proselint,
					-- nls_diag.pylint,
					nls_diag.selene,
					nls_diag.teal,
					-- nls_diag.vale,
					nls_diag.vint,
					nls_diag.write_good.with { filetypes = { 'markdown', 'tex' } },
					nls_diag.credo,

					-- [[ Formatting ]]
					nls_fmt.clang_format,
					-- nls_fmt.cmake_format,
					-- nls_fmt.isort,
					nls_fmt.prettier.with { extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" } },
					nls_fmt.rustfmt,
					nls_fmt.shfmt,
					nls_fmt.stylua,
					nls_fmt.trim_whitespace,
					-- nls_fmt.yapf,
					nls_fmt.elm_format,
					nls_fmt.mix,
					nls_fmt.black.with {
						extra_args = {
							"--fast",
							"--line-length",
							"80",
							"--experimental-string-processing"
						}
					},

					-- [[ Actions ]]
					nls_act.gitsigns,
					nls_act.xo,
					nls_act.refactoring.with { filetypes = { 'javascript', 'typescript', 'lua', 'python', 'c', 'cpp' } },
				},
			}
		end,
	}
}
