;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (setq flymake-diagnostic-at-point-timer-delay 0.1)
  (setq flymake-diagnostic-at-point-error-prefix "☠ ")
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup))

(provide 'init-flymake)
;;; init-flymake.el ends here
