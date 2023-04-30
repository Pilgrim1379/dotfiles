;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-a-mac*
  ;; modifier keys
  (setq mac-command-modifier      'meta
        ns-command-modifier       'meta
        mac-option-modifier       'super
        ns-option-modifier        'super
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none)
  ;; Doom defaults
  ;; (setq mac-command-modifier      'super
  ;;       ns-command-modifier       'super
  ;;       mac-option-modifier       'meta
  ;;       ns-option-modifier        'meta
  ;;       mac-right-option-modifier 'none
  ;;       ns-right-option-modifier  'none)

  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-margin 4)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  
  ;; Enable transparent titlebar
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode))

  ;; Enable use of macOS trash
  (use-package osx-trash
    :custom
    (delete-by-moving-to-trash t)

    :config
    (osx-trash-setup))

  ;; Don't use macOS' Native fullscreen mode.
  (setq ns-use-native-fullscreen nil)

  ;; Override default ctrl+scroll text size keybindings on macOS to not have any
  ;; effect.
  (defun siren-mouse-wheel-text-scale (_event)
    "Custom version of `mouse-wheel-text-scale' which does NOTHING.
  macOS scroll momentum often leads to excessive text size changes
  when using ctrl-based keybindings right after scrolling up/down.
  This is very annoying, and in extreme cases locks up Emacs for
  minutes as it's trying to reach 100,000 font size or something
  crazy."
    (interactive (list last-input-event)))

  (global-set-key (kbd "C-<wheel-down>") 'siren-mouse-wheel-text-scale)
  (global-set-key (kbd "C-<wheel-up>") 'siren-mouse-wheel-text-scale))


(provide 'init-osx-keys)
;;; init-osx-keys.el ends here