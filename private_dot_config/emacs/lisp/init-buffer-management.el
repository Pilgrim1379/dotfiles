;;; init-buffer-management.el --- buffer management configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

; perspective
(use-package perspective
  :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  ("C-x k" . persp-kill-buffer*)
  :custom
  (persp-initial-frame-name "Main")
  (persp-mode-prefix-key (kbd "C-x x"))  ; pick your own prefix key here
  :init
  (persp-mode))

(provide 'init-buffer-management)
;;; init-buffer-management.el ends here
