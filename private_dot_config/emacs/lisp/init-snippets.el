;;; init-snippets.el --- Snippets and code expansion config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq save-abbrevs 'silently)
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(use-package emmet-mode
  :hook ((css-mode sgml-mode html-mode web-mode) . emmet-mode))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

  ;; (use-package yasnippet-snippets
  ;;   :after (yasnippet)))

;; Loren ipsum
(use-package lorem-ipsum
  :defer
  :bind (("C-c C-v l" . lorem-ipsum-insert-list)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v s" . lorem-ipsum-insert-sentences)))



;; Dabbrev
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand)))


(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line



(provide 'init-snippets)
;;; init-snippets.el ends here
