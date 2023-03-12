;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary: https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
;;; Code:

;; rust-analyzer setup - https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
;; $ git clone https://github.com/rust-analyzer/rust-analyzer.git
;; $ cd rust-analyzer
;; $ cargo xtask install --server # will install rust-analyzer into $HOME/.cargo/bin

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; Rust
(use-package rustic
  ;; :defer 0.5
  ;; :mode ("\\.rs\\'") # enabling this messes with file mode specification.
  :bind (:map rustic-mode-map
              ;; ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))



(provide 'init-rust)
;;; init-rust.el ends here
