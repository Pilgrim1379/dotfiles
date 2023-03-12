;;; init-web.el --- Support for the Web templates -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.[lh]?eex\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t))



(provide 'init-web-mode)
;;; init-web-mode.el ends here
