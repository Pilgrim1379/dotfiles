;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-debug nil)
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  :custom
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "WORKON_HOME"
     "SSH_AUTH_SOCK"))
  :config
  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(use-package use-package-ensure-system-package
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell


(provide 'init-exec-path)
;;; init-exec-path.el ends here
