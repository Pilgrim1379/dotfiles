;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
;; (use-package orderless
;;   :init
;;   (setq completion-styles '(substring orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides nil))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Enable vertico
(use-package vertico
  :after orderless
  :init
  (vertico-mode)
  :config
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  ;; Configure directory extension.
  ;; NOTE: The file `vertico-directory.el' must be installed manually.
  (use-package vertico-directory
    :load-path "straight/build/vertico/extensions/"
    :straight nil)

  ;; :straight (vertico-directory
  ;;               :type git
  ;;               :host github
  ;;               :repo "minad/vertico"
  ;;               :local-repo "vertico-directory"
  ;;               :files ("blob/main/extensions/vertico-directory.el"))

  ;; More convenient directory navigation commands
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package savehist
    :init
    (savehist-mode))

;;
(defun nqa/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu))
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
    consult-theme
    :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
    :preview-key (kbd "M-.")

    ;; Optionally configure a function which returns the project root directory.
    ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (project-roots)
    ;; (setq consult-project-root-function
    ;;       (lambda ()
    ;;         (when-let (project (project-current))
    ;;           (car (project-roots project)))))
    ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'nqa/get-project-root)
    ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
    ;;;; 4. locate-dominating-file
    ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

    ;; Use `consult-completion-in-region' if Vertico is enabled.
    ;; Otherwise use the default `completion--in-region' function.
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))))

(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
(global-set-key [remap goto-line] 'consult-goto-line)


;; Affe
(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))


(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :bind(("C-." . embark-act)         ;; pick some comfortable binding
        ("C-;" . embark-dwim)        ;; good alternative: M-.
        ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult LSP-mode integration
(use-package consult-lsp
  :after (consult lsp)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Consult LSP-mode integration
(use-package consult-flycheck
  :after (consult flycheck))



(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
