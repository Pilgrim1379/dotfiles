;;; init-vanila-emacs.el --- Configure vanila emacs -*- lexical-binding: t -*-
;;; Commentary: hhow emacs should behave without external packages
;;; Code:

;; My sane defaults
(setq-default
    ;; read-process-output-max (* 1024 1024) ;; 1mb moved this to early-init
    ;; blink-cursor-mode 0 ; enable this to disable cursor blinking all together
    blink-cursor-blinks 0 ; blink cursor forever don't stop after 10 blinks
    cursor-type 'bar
    buffers-menu-max-size 30
    case-fold-search t
    column-number-mode t
    ediff-split-window-function 'split-window-horizontally
    ediff-window-setup-function 'ediff-setup-windows-plain
    indent-tabs-mode nil
    tab-width 4
    create-lockfiles nil
    auto-save-default nil ; stop creating those #auto-save# files
    make-backup-files nil
    mouse-yank-at-point t
    save-interprogram-paste-before-kill t
    scroll-preserve-screen-position 'always
    set-mark-command-repeat-pop t
    ring-bell-function 'ignore
    tooltip-delay 1.5
    truncate-lines nil
    visible-bell t
    show-paren-style "mixed"
    fill-column 80                     ; Set width for automatic line breaks
    inhibit-startup-message t
    truncate-partial-width-windows nil)

;; highlight current line
(global-hl-line-mode 1)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; allow typing 'y' or 'n' instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

;; Huge files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)

;; Line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Fill line indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

;;; Handy key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])


(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
(defun nqa/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'nqa/backward-up-sexp) ; C-M-u, C-M-up

;; Recent Files
(add-hook 'after-init-hook 'recentf-mode)
(setq-default recentf-auto-cleanup 'never
              recentf-max-saved-items 500
              recentf-max-menu-items 15
              ;; recentf-save-file (concat user-emacs-directory "var/recentf")
              recentf-exclude (list "COMMIT_EDITMSG"
                                    "~$"
                                    "/scp:"
                                    "/ssh:"
                                    "/sudo:"
                                    "/sudo.*\\'"
                                    "/tmp/"
                                    "/\\.git/.*\\'"
                                    "/git/.*\\'"
                                    "/elpa/.*\\'"
                                    "/tramp.*\\'"
                                    "/var/.*\\'"
                                    "^/var/folders\\.*"
                                    "/node_modules/.*\\'"
                                    "/elpa/.*\\'"
                                    "/straight/.*\\'"))
                                    ;; (concat package-user-dir "/.*-autoloads\\.el\\'"))) ;; not required for stright

;;; Frame Configs

;; Enable mouse support when running in a console
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  ;; (global-set-key [mouse-4] 'scroll-down-line)
  ;; (global-set-key [mouse-5] 'scroll-up-line)
  )

;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq-default
    window-resize-pixelwise t
    frame-resize-pixelwise t)

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable menu bar when running in console
(unless window-system
  (menu-bar-mode -1))

;; Save cursor postion between file operations
(save-place-mode t)



(provide 'init-vanila-emacs)
;;; init-locales.el ends here
