;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq ad-redefinition-action 'accept) ; This will suppress the "... got redifined" warnings

(use-package unfill)

(electric-indent-mode t)

(use-package aggressive-indent
  :hook ((css-mode
          emacs-lisp-mode
          web-mode) . aggressive-indent-mode))

;; Super-save auto-saves your buffers, when certain events happen
;; (use-package super-save
;;   :init
;;   (add-hook 'after-init-hook 'super-save-mode)
;;   :config
;;   (setq super-save-idle-duration 15)
;;   (setq super-save-auto-save-when-idle t)
;;   (setq super-save-remote-files nil)
;;   (setq super-save-exclude '(".gpg")))


;;Real Auto Save File
(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask. Version 2019-11-05"
  (interactive)
  (save-some-buffers t))

(if (version< emacs-version "27")
    (add-hook 'focus-out-hook 'xah-save-all-unsaved)
  (setq after-focus-change-function 'xah-save-all-unsaved))

;; Crux
(use-package crux
  :bind(("C-k" . crux-smart-kill-line)
        ("M-o" . crux-smart-open-line)
        ("S-<return>" . crux-smart-open-line)
        ("M-O" . crux-smart-open-line-above)
        ("C-S-<return>" . crux-smart-open-line-above)
        ("C-c n" . crux-cleanup-buffer-or-region)
        ("C-c e" . crux-eval-and-replace)
        ("C-c D" . crux-delete-file-and-buffer)
        ("C-c d" . crux-duplicate-current-line-or-region)
        ("C-c r" . crux-rename-file-and-buffer)
        ([remap move-beginning-of-line] . crux-move-beginning-of-line)))


;; very large files
(use-package vlf :defer)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))


;; Highlight current column
;; (use-package vline
;;   :disabled
;;   :hook (prog-mode . vline-global-mode))

;; Highlight current column
;; (use-package crosshairs
;;   :disabled
;;   :hook (after-init . crosshairs-mode))

(use-package beacon
  :hook (after-init . beacon-mode)
  :config
  (setq-default beacon-blink-when-focused t)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 40))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; multiple-cursors
; (use-package multiple-cursors
;   :config
;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;   (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


;; Page break lines
(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode))


;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(use-package move-dup
  :config
  (global-set-key [M-up] 'move-dup-move-lines-up)
  (global-set-key [M-down] 'move-dup-move-lines-down)
  (global-set-key [M-S-up] 'move-dup-move-lines-up)
  (global-set-key [M-S-down] 'move-dup-move-lines-down)

  (global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
  (global-set-key (kbd "C-c u") 'move-dup-duplicate-up))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :hook (after-init . whole-line-or-region-global-mode))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package avy
  :bind (("M-s" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package helpful
  :bind (
    ; rebind help keys to use helpful
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
    ; lookup the current symbol at point
         ("C-c C-d" . helpful-at-point)
    ; look up functions (expluding macros)
         ("C-h F" . helpful-function)
    ; look up commands
         ("C-h C" . helpful-command)))

;; hungry-delete
;; Deleting a whitespace character will delete all whitespace until the next non-whitespace character.
(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode))


(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
