;;; init-org-mode.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package org
  :defer t
  :config
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("⦿" "○" "●" "○" "●" "○" "●")))

  (setq org-ellipsis " ⏷")

  (add-hook 'org-mode-hook 'org-indent-mode)

  (add-hook 'org-mode-hook 'auto-fill-mode)

  (use-package ob-http)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (http . t)
                                 (python . t)
                                 (ruby . t)))

  (use-package ob-async)

  (setq org-edit-src-content-indentation 0))



(provide 'init-org-mode)
;;; init-org-mode.el ends here
