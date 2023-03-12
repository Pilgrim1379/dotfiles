;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; All the icons
(use-package all-the-icons
  :if (display-graphic-p)

  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))


(use-package catppuccin-theme
  :config
  (setq catppuccin-height-title1 1.5)
  (load-theme 'catppuccin t))  ; t treats it as safe)



(provide 'init-themes)
;;; init-themes.el ends here