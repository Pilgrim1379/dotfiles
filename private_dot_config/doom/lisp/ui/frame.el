;;; frame.el --- Monitor-aware frame layouts for Doom Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; A small, self-contained layout module for graphical Emacs frames.  Layouts
;; are fractions of the current monitor's *workarea*, so they work on the
;; built-in display and external displays without hard-coded screen sizes.
;;
;; On macOS, `set-frame-size' sets a text/client size while
;; `set-frame-position' moves the outer window.  `my/frame--set-outer-rect'
;; measures the resulting outer size and corrects it, accounting for title
;; bars and borders automatically.  No machine-specific calibration values
;; are stored.
;;
;; Fractions must be written as floats (0.5, not 1/2): Emacs Lisp has no
;; ratio syntax, so `1/2' would be read as a *symbol*, not a number.

;;; Code:

;; Without this, several toolkits round pixel size requests to character-cell
;; multiples, which prevents the feedback correction below from converging.
(setq frame-resize-pixelwise t)

(defgroup my/frame nil
  "Monitor-aware frame layouts."
  :group 'frames)

(defcustom my/frame-margin 8
  "Inset, in pixels, around every applied frame layout.

The inset is applied to all four edges of the layout rectangle.  Set this to
zero for edge-to-edge layouts."
  :type 'integer
  :group 'my/frame)

(defcustom my/default-frame-layout 'right-half
  "Layout applied at startup and to subsequently created top-level frames."
  :type '(choice (const full)
                 (const center)
                 (const left-half)
                 (const right-half)
                 (const left-third)
                 (const coding)
                 (symbol :tag "Other layout"))
  :group 'my/frame)

(defcustom my/frame-layouts
  `((full       0.0        0.0  1.0        1.0)
    (center     0.2        0.1  0.6        0.8)
    (left-half  0.0        0.0  0.5        1.0)
    (right-half 0.5        0.0  0.5        1.0)
    (left-third 0.0        0.0  ,(/ 1.0 3) 1.0)
    ;; A wide editor on the right, leaving the left third for another app.
    (coding     ,(/ 1.0 3) 0.0  ,(/ 2.0 3) 1.0))
  "Known frame layouts.

Each entry is (NAME LEFT TOP WIDTH HEIGHT).  The last four values are
fractions of the current monitor workarea, not pixels, and must be numbers
written as floats -- Emacs Lisp reads `3/4' as a symbol, not a ratio.  Add
an entry here to define another layout; for example,
`(reading 0.125 0.1 0.75 0.8)'."
  :type '(repeat (list symbol number number number number))
  :group 'my/frame)

(defconst my/frame--fraction-tolerance 1e-3
  "Slack allowed when checking that fractions sum to at most 1.
Float thirds such as (/ 1.0 3) do not sum to exactly 1.0.")

(defun my/frame--eligible-p (frame)
  "Return non-nil when FRAME is a top-level graphical frame to lay out."
  (and (frame-live-p frame)
       (display-graphic-p frame)
       (not (frame-parent frame))))

(defun my/frame-workarea (&optional frame)
  "Return FRAME's current monitor workarea as (X Y WIDTH HEIGHT).

The workarea excludes space reserved by macOS, such as the menu bar and Dock.
For a multi-monitor setup, it is the workarea of the monitor currently
dominating FRAME."
  (let ((workarea (alist-get 'workarea
                             (frame-monitor-attributes
                              (or frame (selected-frame))))))
    (unless (and (listp workarea) (= (length workarea) 4))
      (user-error "Emacs could not determine this frame's monitor workarea"))
    workarea))

(defun my/frame--layout-spec (layout)
  "Return the fractional rectangle for LAYOUT or signal an informative error."
  (let ((spec (assq layout my/frame-layouts)))
    (unless spec
      (user-error "Unknown frame layout %S; choose one of: %s"
                  layout
                  (mapconcat (lambda (entry) (symbol-name (car entry)))
                             my/frame-layouts ", ")))
    (pcase-let ((`(,_ ,left ,top ,width ,height) spec))
      ;; Catch `1/2'-style symbols explicitly: they are the most likely
      ;; mistake when editing `my/frame-layouts' by hand.
      (unless (and (numberp left) (numberp top)
                   (numberp width) (numberp height))
        (user-error
         "Layout %S contains a non-number; write fractions as floats (0.5, not 1/2)"
         layout))
      (unless (and (<= 0 left) (<= 0 top)
                   (< 0 width) (< 0 height)
                   (<= (+ left width) (+ 1 my/frame--fraction-tolerance))
                   (<= (+ top height) (+ 1 my/frame--fraction-tolerance)))
        (user-error "Invalid fractional rectangle for layout %S" layout))
      (list left top width height))))

(defun my/frame-layout-rect (layout &optional frame)
  "Return LAYOUT's desired outer rectangle for FRAME as (LEFT TOP RIGHT BOTTOM).

The rectangle is calculated from FRAME's monitor workarea and then inset by
`my/frame-margin'.  Fractional edges beyond the workarea (within
`my/frame--fraction-tolerance') are clamped to it."
  (pcase-let* ((`(,mx ,my ,mw ,mh) (my/frame-workarea frame))
               (`(,left ,top ,width ,height) (my/frame--layout-spec layout))
               (right-fraction (min 1.0 (+ left width)))
               (bottom-fraction (min 1.0 (+ top height)))
               (margin (max 0 my/frame-margin))
               (x1 (+ mx (floor (* mw left)) margin))
               (y1 (+ my (floor (* mh top)) margin))
               (x2 (- (+ mx (floor (* mw right-fraction))) margin))
               (y2 (- (+ my (floor (* mh bottom-fraction))) margin)))
    (when (or (<= x2 x1) (<= y2 y1))
      (user-error "Layout %S is too small for a margin of %s pixels"
                  layout margin))
    (list x1 y1 x2 y2)))

(defun my/frame--outer-size (frame)
  "Return FRAME's outer size as a cons cell (WIDTH . HEIGHT)."
  (or (alist-get 'outer-size (frame-geometry frame))
      ;; This fallback is useful on window systems that do not report it.
      (cons (frame-pixel-width frame) (frame-pixel-height frame))))

(defun my/frame--set-outer-rect (frame left top right bottom)
  "Make FRAME's outer rectangle LEFT, TOP, RIGHT, BOTTOM in pixels.

The initial request subtracts the measured difference between FRAME's outer
size and its text size, so macOS title bars, borders, and Emacs internal
borders are measured rather than assumed.  On the Cocoa port, where resizing
is synchronous, a single feedback pass then corrects any residual error.  On
other window systems the resize may complete asynchronously, so re-measuring
immediately would read stale geometry; the correction pass is skipped there
rather than risk applying a stale error twice."
  (when (frame-live-p frame)
    (let* ((wanted-width (- right left))
           (wanted-height (- bottom top))
           (outer-before (my/frame--outer-size frame))
           ;; `set-frame-size' with PIXELWISE sets the *text* size, so the
           ;; decoration delta must be measured against the text size too --
           ;; not `frame-pixel-width', which is the native size and includes
           ;; internal borders and fringes.
           (text-width (frame-text-width frame))
           (text-height (frame-text-height frame))
           (request-width (max 1 (- wanted-width (- (car outer-before)
                                                    text-width))))
           (request-height (max 1 (- wanted-height (- (cdr outer-before)
                                                      text-height)))))
      (set-frame-size frame request-width request-height t)
      (when (eq (window-system frame) 'ns)
        (pcase-let ((`(,actual-width . ,actual-height)
                     (my/frame--outer-size frame)))
          (let ((width-error (- wanted-width actual-width))
                (height-error (- wanted-height actual-height)))
            (unless (and (zerop width-error) (zerop height-error))
              (set-frame-size frame
                              (max 1 (+ request-width width-error))
                              (max 1 (+ request-height height-error))
                              t)))))
      ;; Position last: this uses the outer-frame position and therefore pins
      ;; the requested edges after the final resize.
      (set-frame-position frame left top))))

;;;###autoload
(defun my/apply-frame-layout (&optional layout frame)
  "Apply LAYOUT to FRAME.

LAYOUT defaults to `my/default-frame-layout'; FRAME defaults to the selected
frame.  Interactively, prompt for a layout."
  (interactive
   (list (intern (completing-read "Frame layout: "
                                  (mapcar #'car my/frame-layouts)
                                  nil t nil nil
                                  (symbol-name my/default-frame-layout)))))
  (let ((frame (or frame (selected-frame)))
        (layout (or layout my/default-frame-layout)))
    (unless (my/frame--eligible-p frame)
      (user-error "The selected frame is not a top-level graphical frame"))
    (pcase-let ((`(,left ,top ,right ,bottom)
                 (my/frame-layout-rect layout frame)))
      (my/frame--set-outer-rect frame left top right bottom))))

(defun my/frame-apply-default-layout (&optional frame)
  "Apply `my/default-frame-layout' to FRAME when it is eligible."
  (let ((frame (or frame (selected-frame))))
    (when (my/frame--eligible-p frame)
      (my/apply-frame-layout my/default-frame-layout frame))))

(defun my/frame--apply-default-layout-soon (frame)
  "Apply the default layout to FRAME once the current redisplay settles.

`after-make-frame-functions' can run before FRAME is fully realized
(notably for daemon-created frames), when its geometry and monitor
attributes are not yet reliable.  Deferring with a zero-delay timer lets the
frame finish materializing first.  Eligibility is re-checked inside the
timer because the frame may have been deleted in the meantime."
  (run-with-timer 0 nil
                  (lambda ()
                    (when (my/frame--eligible-p frame)
                      (my/apply-frame-layout my/default-frame-layout frame)))))

;;;###autoload
(defun my/frame-full () (interactive) (my/apply-frame-layout 'full))
;;;###autoload
(defun my/frame-center () (interactive) (my/apply-frame-layout 'center))
;;;###autoload
(defun my/frame-left-half () (interactive) (my/apply-frame-layout 'left-half))
;;;###autoload
(defun my/frame-right-half () (interactive) (my/apply-frame-layout 'right-half))
;;;###autoload
(defun my/frame-left-third () (interactive) (my/apply-frame-layout 'left-third))
;;;###autoload
(defun my/frame-coding () (interactive) (my/apply-frame-layout 'coding))

;;;###autoload
(defun my/frame-install-layout-hooks ()
  "Install idempotent startup and new-frame layout hooks.

`window-setup-hook' handles the initial GUI frame: it runs after
`emacs-startup-hook', once the initial frame's size and monitor attributes
have settled.  `after-make-frame-functions' handles daemon and later frames;
the layout is applied via a zero-delay timer because that hook can fire
before the new frame is fully realized."
  (add-hook 'window-setup-hook #'my/frame-apply-default-layout)
  (add-hook 'after-make-frame-functions #'my/frame--apply-default-layout-soon))

(provide 'my-frame-layouts)
;;; frame.el ends here
