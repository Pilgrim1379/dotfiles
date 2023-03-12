;;; init-project.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Returns the parent directory containing a .project.el file, if any,
;; to override the standard project.el detection logic when needed.
;; https://michael.stapelberg.ch/posts/2021-04-02-emacs-project-override/

;; (defun nqa-project-override (dir)
;;   (let ((override (locate-dominating-file dir ".project.el")))
;;     (if override
;;       (cons 'vc override)
;;       nil)))
;;
;; (defun nqa-project-override-x (dir)
;;   (let ((flist (".project.el" "mix.exs" "package.json")))
;;     (override (dolist (n flist) (locate-dominating-file dir n)))
;;     (if override
;;       (cons 'vc override)
;;       nil)))

(use-package project)
  ;; :config
  ;; (cl-defmethod project-root ((project (head local)))
  ;;   "Return root directory of current PROJECT."
  ;;   (cdr project)))
  ;; :straight '(project :type git
  ;;                     :host github
  ;;                     :repo "eemacs-mirror/emacs/blob/master/lisp/emacs-lisp/package.el"))
                      ;; :branch "master"))
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  ;; :config
  ;; (add-hook 'project-find-functions #'nqa-project-override-x))

(use-package project-x
  :init
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))

;; (use-package project-x
;;   :straight (project-x :type git :host github :repo "karthink/project-x")
;;   :after project
;;   :config
;;   (add-hook 'project-find-functions 'project-x-try-local 90)
;;   (add-hook 'kill-emacs-hook 'project-x--window-state-write)
;;   (add-to-list 'project-switch-commands
;;                '(?j "Restore windows" project-x-windows) t)
;;   :bind (("C-x p w" . project-x-window-state-save)
;;          ("C-x p j" . project-x-window-state-load)))




;; inserts
;; abcdefghijklmnopqrstuvwxyz



(provide 'init-project)
;;; init-project.el ends here
