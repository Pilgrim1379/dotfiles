;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
(setq load-prefer-newer t)

;;; I Need These In Place Straight away
; Constants
(defconst *spell-check-support-enabled* nil)  ; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

; Thanks, but no thanks
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;(set-fringe-mode 10)        ; Give some breathing room

; Make frame transparency overridable
; (defvar nqa/frame-transparency '(90 . 90))

; Set frame transparency
; (set-frame-parameter (selected-frame) 'alpha nqa/frame-transparency)
; (add-to-list 'default-frame-alist `(alpha . ,nqa/frame-transparency))

; Restore garabase collection interval after startup
(let ((normal-gc-cons-threshold (* 20 1024 1024)))
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-hook 'emacs-startup-hook
  #'(lambda ()
      (message "%d packages loaded in %s with %d garbage collections."
          (length (hash-table-keys straight--success-cache))
          (format "%.2f seconds"
           (float-time
             (time-subtract after-init-time before-init-time)))
          gcs-done)))


;;; Bootstrap Package Manager
; Update Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory)) ; Keep packages list tucked away in it own file
(require 'init-straight)      ;; Machinery for installing required packages
;; (require 'init-packages)      ;; Machinery for installing required packages


;;; Setup System Path
(require 'init-exec-path) ;; Set up $PATH

;;; Configure Vanila Emacs / Sane Defaults
(require 'init-vanila-emacs)


;;; External Packages That Enhance General Text Editing
; UI
(require 'init-themes)
(require 'init-modeline)

; General editing
(require 'init-editing-utils)
(require 'init-whitespace)


;;; External Coding Utilities
(require 'init-spelling)
(require 'init-ibuffer)
;; (require 'init-buffer-management) ;; perspective
(require 'init-projectile)
;; (require 'init-project)
;; (require 'init-flymake)
(require 'init-flycheck)
(require 'init-uniquify)

;; (require 'init-corfu)
(require 'init-company)

(require 'init-minibuffer) ;; See for orderless
(require 'init-snippets)
; (require 'init-perspective)
(require 'init-vc)
(require 'init-magit)

;; (require 'init-tree-sitter)


;;; Language Servers
;; (require 'init-eglot)
(require 'init-lsp)


;;; Programming Languages
(require 'init-elixir)
(require 'init-elm)
(require 'init-rust)
(require 'init-lua)
(require 'init-python)
(require 'init-markdown)
(require 'init-zig)
(require 'init-sql)
(require 'init-yaml)
(require 'init-go)
(require 'init-slime)
(require 'init-clojure)
(require 'init-zig)
(require 'init-crystal)
(require 'init-scheme)
(require 'init-zig)
(require 'init-julia)
;;
(require 'init-web-mode)
;;
;;
;;
;; ;;; General Keybinginds
(require 'init-osx-keys)
;;
;; ;;; Org Mode
(require 'init-org-mode)




;;; Misc Configurations

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-misc)

; Start emacs by default using the following directory
(setq default-directory (expand-file-name "~/Projects/Code/"))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
