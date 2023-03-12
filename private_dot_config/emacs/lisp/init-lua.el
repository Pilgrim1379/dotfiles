;;; init-lua.el --- Lua language support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))


(provide 'init-lua)
;;; init-lua.el ends here
