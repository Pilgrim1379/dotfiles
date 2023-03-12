;;; init-elixir.el --- Support for the Elixir language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elxir
(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :mode ("\\.exs?\\'")
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options)))))


(provide 'init-elixir)
;;; init-elixir.el ends here
