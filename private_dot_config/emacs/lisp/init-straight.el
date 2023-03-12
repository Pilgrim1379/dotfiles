;;; init-straight.el --- Settings and helpers find and install external packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration with use-package
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(setq straight-fix-flycheck t)

;; Saves from having to add :straight t for every use-package declaration
(setq straight-use-package-by-default t)

;; Emacs Native Compilation Feature support
;; https://www.ruiying.online/post/2021-07-02-install-and-use-emacs-28-native-comp-gccemacs/
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      ;; (setq native-comp-async-report-warnings-errors nil)
      (setq comp-deferred-compilation t)
      (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
      (setq package-native-compile t)
      (setq comp-deferred-compilation t))
  (message "Native complation is *not* available"))

;; Test JSON is working ;; to be commented out once satistied
;; (if (functionp 'json-serialize)
;;   (message "Native JSON is available")
;;  (message "Native JSON is *not* available"))

(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


(provide 'init-straight)
;;; init-straight.el ends here
