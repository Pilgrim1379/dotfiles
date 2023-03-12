;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Misc config - yet to be placed in separate file

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :hook ((org-mode
          emacs-lisp-mode
          web-mode
          typescript-mode
          js2-mode
          html-mode
          sass-mode
          css-mode) . rainbow-mode))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

(provide 'init-misc)
;;; init-misc.el ends here
