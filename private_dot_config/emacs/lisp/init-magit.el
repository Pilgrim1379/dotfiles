;;; init-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code

(use-package magit
  :bind
  ("C-c g" . magit-status)
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))


(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

;; In addition to that, I like to see the lines that are being modified in the file while it is being edited.
(use-package git-gutter
  :defer 0.3
  :init
  (global-git-gutter-mode))


(provide 'init-magit)
;;; init-magit.el ends here
