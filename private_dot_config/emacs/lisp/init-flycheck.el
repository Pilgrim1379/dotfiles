;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  ;; :disabled
  :hook ((flycheck-mode . flycheck-set-indication-mode)
         (after-init . global-flycheck-mode))
  :config
  (setq flycheck-indication-mode 'left-fringe))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
