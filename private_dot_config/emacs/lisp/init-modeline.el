;;; init-modeline.el --- Modeline UI and config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package minions
  :config
  (setq minions-mode-line-lighter "[+]")
  ;; (setq minions-direct '(flymake-mode))
  (minions-mode 1))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-persp-name nil)
  ; use unicode as a fallback (instead of ASCII) when not using icons
  (setq doom-modeline-unicode-fallback t)
  ; don't display the buffer encoding
  (setq doom-modeline-buffer-encoding nil)
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/lisp/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; How to detect the project root.
  ;; The default priority of detection is `ffip' > `projectile' > `project'.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'projectile))


(provide 'init-modeline)
;;; init-modeline.el ends here
