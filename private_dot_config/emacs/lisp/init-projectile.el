;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (projectile-find-file)) ;; or (projectile-commander)
  ;; (magit-status))

(use-package projectile
  :init
  (when (file-directory-p "~/Workspace")
    (setq projectile-project-search-path '("~/Workspace/learning/elixir/"
                                            "~/Workspace/learning/python/"
                                            "~/Workspace/learning/elm/"
                                            "~/Workspace/learning/rust/"
                                            "~/Workspace/learning/ai/"
                                            "~/Workspace/learning/c++/"
                                            "~/Workspace/learning/exercism/"
                                            "~/Workspace/learning/js/")))
  (setq projectile-switch-project-action #'dw/switch-project-action)
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package persp-projectile
  :after projectile)



(provide 'init-projectile)
;;; init-projectile.el ends here
