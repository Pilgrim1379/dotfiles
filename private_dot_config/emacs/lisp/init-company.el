;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation


(use-package pos-tip
  :defer)

;;; Code completion

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Company - Auto completion system
;; smart tab behavior - indent or complete
;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(add-to-list 'completion-styles 'initials t)


(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(advice-add 'company-capf--candidates :around #'just-one-face)

(use-package company
  :init
  (setq company-backends '((
                            ;; company-files
                            company-keywords
                            company-capf
                            company-yasnippet
                            company-dabbrev-code
                            company-etags)))
                            ;; company-dabbrev)))

  (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
          ;; ("RET" . nil)
          ;; ("[return]" . nil)
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next)
          ("M-<". company-select-first)
          ("M->". company-select-last))
        (:map company-mode-map
          ("<tab>". tab-indent-or-complete)
          ("TAB". tab-indent-or-complete))
  :config
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  ;; (setq company-require-match #'company-explicit-action-p)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; General keybindings
;; (global-set-key (kbd "M-C-/") 'company-complete)

;; (use-package company-quickhelp
;;   :after company
;;   :config
;;   (company-quickhelp-mode))

;; (with-eval-after-load 'company
;;   (define-key company-active-map [tab] 'company-complete-selection)
;;   (define-key company-active-map (kbd "TAB") 'company-complete-selection))






(provide 'init-company)
;;; init-company.el ends here
