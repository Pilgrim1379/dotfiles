;;; init-slime.el --- Slime support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))


(provide 'init-slime)
;;; init-slime.el ends here
