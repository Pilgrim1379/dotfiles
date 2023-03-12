;;; init-eglot.el --- Client for Language Servers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Eglot
(use-package eglot
  ; :init
  ; (setq eglot-stay-out-of '(company))
  :hook ((elm-mode
          python-mode
          elixir-mode
          rust-mode
          lua-mode
          js2-mode
          clojure-mode
          rustic-mode
          zig-mode
          crystal-mode
          clojurescript-mode
          go-mode
          haskell-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(crystal-mode . ("scry"))))



(provide 'init-eglot)
;;; init-eglot.el ends
