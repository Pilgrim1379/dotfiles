;;; init-tree-sitter.el --- Tree sitter support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (setq tree-sitter-debug-highlight-jump-region t)
  (setq tree-sitter-debug-jump-buttons t)

  (use-package tree-sitter-langs))


(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)





(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
