;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "WORKON_HOME"
                                    "TMPDIR"
                                    "SSH_AUTH_SOCK"
                                    "GOPATH"
                                    "GOBIN"
                                    "GOROOT"
                                    "GOPRIVATE"
                                    "GOENV_GOPATH_PREFIX"
                                    "GOENV_VERSION"
                                    "RUST_BACKTRACE"))
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-debug nil)

  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;; Set temporary-file-directory to match TMPDIR environment variable
(let ((tmpdir (getenv "TMPDIR")))
  (when (and tmpdir (not (string-blank-p tmpdir)))
    (setq temporary-file-directory tmpdir)))

;; Add bin directory within emacs configuration dir to `exec-path'.
;; (add-to-list 'exec-path (expand-file-name "bin" siren-dir))

(use-package use-package-ensure-system-package
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell



(provide 'init-exec-path)
;;; init-exec-path.el ends here
