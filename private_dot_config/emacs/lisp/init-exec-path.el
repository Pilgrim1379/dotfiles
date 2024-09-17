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

;; Add bin directory within emacs configuration dir to `exec-path'.
;; (add-to-list 'exec-path (expand-file-name "bin" siren-dir))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
