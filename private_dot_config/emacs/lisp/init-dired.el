;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;Fancy icons for directory listing
(use-package all-the-icons-dired
  :disabled
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :straight nil
  :hook (dired-mode . auto-revert-mode)
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        delete-by-moving-to-trash t
        dired-hide-details-hide-symlink-targets nil)

  ;; http://www.emacswiki.org/DiredPlus
  (use-package dired+
    :config
    ;; Details toggling is bound to "(" in `dired-mode' by default
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-wrap-around-flag nil)
    (diredp-toggle-find-file-reuse-dir 1)))



(provide 'init-dired)
;;; init-dired.el ends here