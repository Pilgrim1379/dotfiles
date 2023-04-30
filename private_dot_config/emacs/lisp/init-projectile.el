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
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/workspace/code/elixir/"
                                            "~/workspace/code/python/"
                                            "~/workspace/code/elm/"
                                            "~/workspace/code/rust/"
                                            "~/workspace/code/ai/"
                                            "~/workspace/code/c++/"
                                            "~/workspace/code/exercism/"
                                            "~/workspace/code/js/")))
  (setq projectile-switch-project-action #'dw/switch-project-action)
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package persp-projectile
  :after projectile)



(provide 'init-projectile)
;;; init-projectile.el ends here
