;;; init-fonts.el --- Configure fonts -*- lexical-binding: t -*-
;;; Commentary: hhow emacs should behave without external packages
;;; Code:


 ;;; Font Configuration
(set-face-attribute 'default nil :font "MonaspiceAr NF" :height nqa/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MonaspiceAr NF" :height nqa/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "SF Pro Text" :height nqa/default-variable-font-size :weight 'regular)

;; Nerd Icons
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font"))

(use-package nerd-icons-completion
  :after nerd-icons
  :config
  (nerd-icons-completion-mode))

(use-package treemacs-nerd-icons
  :after nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; All the icons
(use-package all-the-icons
  :if (display-graphic-p)

  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))



(provide 'init-fonts)
;;; init-locales.el ends here