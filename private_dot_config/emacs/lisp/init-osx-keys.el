;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))


(provide 'init-osx-keys)
;;; init-osx-keys.el ends here