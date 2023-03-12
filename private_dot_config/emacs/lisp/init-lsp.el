;;; init-lsp.el --- Support for the Language Servers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TODO: Cache result
(defun linw1995/pdm-get-python-executable (&optional dir)
  (let ((pdm-get-python-cmd "pdm info --python"))
    (string-trim
     (shell-command-to-string
      (if dir
          (concat "cd "
                  dir
                  " && "
                  pdm-get-python-cmd)
        pdm-get-python-cmd)))))

(defun linw1995/pdm-get-packages-path (&optional dir)
  (let ((pdm-get-packages-cmd "pdm info --packages"))
    (concat (string-trim
             (shell-command-to-string
              (if dir
                  (concat "cd "
                          dir
                          " && "
                          pdm-get-packages-cmd)
                pdm-get-packages-cmd)))
            "/lib")))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-completion-provider :capf)
  (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
          ("TAB" . completion-at-point))
  :hook (
          ((python-mode
            go-mode
            js-mode
            typescript-mode
            c-mode
            cpp-mode
            objc-mode
            elixir-mode
            elm-mode
            lua-mode
            rustic-mode
            zig-mode
            crystal-mode
            haskell-mode) . lsp-deferred)
          (python-mode . (lambda ()
               (setq lsp-pyright-python-executable-cmd (linw1995/pdm-get-python-executable))
               (setq lsp-pyright-extra-paths (vector (linw1995/pdm-get-packages-path)))
               (require 'lsp-pyright)))
          (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-lens-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-file-watchers t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-idle-delay 0.500)
  ;; file watcher directories to ignore
  (defvar nqa/ignored-directories '("[/\\\\]\\.elixir_ls\\'" ;; elixir lang server directory
                                    "[/\\\\]_build\\'" ;; elixir build directory
                                    "[/\\\\]node_modules\\'" ;; elixir node_modules directory
                                    "[/\\\\]deps\\'" ;; elixir deps directory
                                    "[/\\\\]\\.logs\\'" ;; elixir deps directory
                                    "[/\\\\]elm-stuff\\'")) ;; elm packages directory
  (dolist (re nqa/ignored-directories)
    (push re lsp-file-watch-ignored-directories))

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored-files)) ; json

(use-package lsp-ui
  ;; :disabled
  :config
  ;; (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  ;; (require 'dap-python)
  (require 'dap-elixir)
  (require 'dap-firefox)
  ;; (require 'dap-chrome)
  ;; (require 'dap-go)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  ;; Rust config
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))


(provide 'init-lsp)
;;; init-lsp.el ends here
