;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

;; (setq python-shell-interpreter "python3")

(use-package pip-requirements :defer)

;; Python
(use-package blacken
  :hook (python-mode . blacken-mode)
  :init (setq blacken-line-length 80))

;; Pipenv Environment management
(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :bind (("M-[" . python-nav-backward-block)
         ("M-]" . python-nav-forward-block)))

(use-package py-isort
  :after python
  :hook (before-save . py-isort-before-save))

; (use-package pyvenv
;   :after python
;   :hook ((python-mode . pyvenv-mode)
;          (python-mode . (lambda ()
;                           (if-let ((pyvenv-directory (find-pyvenv-directory (buffer-file-name))))
;                               (pyvenv-activate pyvenv-directory)))))
;   :init
;   ;; (setq pyvenv-default-virtual-env-name "env")
;   (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:"
;                                                               pyvenv-virtual-env-name "]")))
;   :preface
;   (defun find-pyvenv-directory (path)
;     "Checks if a pyvenv directory exists."
;     (cond
;      ((not path) nil)
;      ((file-regular-p path) (find-pyvenv-directory (file-name-directory path)))
;      ((file-directory-p path)
;       (or
;        (seq-find
;         (lambda (path) (file-regular-p (expand-file-name "pyvenv.cfg" path)))
;         (directory-files path t))
;        (let ((parent (file-name-directory (directory-file-name path))))
;          (unless (equal parent path) (find-pyvenv-directory parent))))))))

; (use-package lsp-pyright
    ; :ensure t)
    ;; :hook (python-mode . (lambda ()
    ;;                         (require 'lsp-pyright)
    ;;                         (lsp-deferred))))  ; or lsp-deferred

(use-package lsp-pyright
  :ensure t)
  ; :init (setq lsp-python-ms-auto-install-server t)
  ; :hook (python-mode
  ;        . (lambda ()
  ;            (setq lsp-pyright-python-executable (linw1995/pdm-get-python-executable))
  ;            (setq lsp-pyright-extra-paths (vector (linw1995/pdm-get-packages-path)))
  ;            (require 'lsp-pyright)
  ;            (lsp-deferred))))  ; or lsp


(provide 'init-python)
;;; init-python.el ends here
