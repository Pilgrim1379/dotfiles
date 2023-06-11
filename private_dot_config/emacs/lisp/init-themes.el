;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package catppuccin-theme
  :config
  (setq catppuccin-height-title1 1.5)
  (load-theme 'catppuccin t))  ; t treats it as safe)



(provide 'init-themes)
;;; init-themes.el ends here