;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Elm lang setup ref: https://www.lindsaykwardell.com/blog/setting-up-elm-in-2022/
;;; Code:

;; Elm
(use-package elm-mode
  :mode ("\\.elm\\'")
  :init
  (setq elm-sort-imports-on-save t)
  :hook ((elm-mode . elm-format-on-save-mode)
         (elm-mode . elm-indent-mode))
  :config
  ;; Add node modules
  (use-package add-node-modules-path
    :hook (elm-mode . add-node-modules-path)))


(provide 'init-elm)
;;; init-elm.el ends here
