;;; window-pulse.el --- Pulse the active window on selection change  -*- lexical-binding: t -*-

;; Copyright (C) 2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/midsbie/window-pulse
;; Keywords: convenience

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Provides a subtle visual pulse over the entire window area when it becomes
;; the active window.  Remaps the `default' face background so that the pulse
;; covers the full window, including empty space beyond the end of the buffer.
;;
;; Handles both intra-frame window switches (`window-selection-change-functions')
;; and inter-frame focus changes (`after-focus-change-function').  Returning from
;; the minibuffer does not trigger a pulse.

;;; Code:

(require 'color)

(defcustom window-pulse-background 5
  "Background color for the window pulse effect.
If a number, compute the pulse color by shifting the `default' face
background luminance by that many percentage points, in order to make it
lighter for dark backgrounds or darker for light ones.  This adapts
automatically to the current theme.

If a color string (e.g. \"#3a3a5c\"), use it verbatim."
  :type '(choice (number :tag "Luminance shift (percentage points)")
                 (string :tag "Color"))
  :group 'pulse)

(defcustom window-pulse-iterations 16
  "Number of iterations for the window pulse animation."
  :type 'integer
  :group 'pulse)

(defcustom window-pulse-delay 0.02
  "Delay in seconds between pulse animation iterations."
  :type 'number
  :group 'pulse)

(defvar-local window-pulse--cookie nil
  "Face remap cookie for the current pulse animation.")

(defvar-local window-pulse--timer nil
  "Timer for the current pulse animation.")

(defun window-pulse-p ()
  "Return non-nil if the current window should be pulsed."
  (not (or (minibufferp)
           (bound-and-true-p company-candidates)
           (bound-and-true-p corfu--candidates))))

(defun window-pulse--cancel ()
  "Cancel any ongoing pulse animation in the current buffer."
  (when window-pulse--timer
    (cancel-timer window-pulse--timer)
    (setq window-pulse--timer nil))
  (when window-pulse--cookie
    (face-remap-remove-relative window-pulse--cookie)
    (setq window-pulse--cookie nil)))

(defun window-pulse--compute-color ()
  "Compute the pulse start color from `window-pulse-background'.
When a number, shift the `default' face background luminance by that
many percentage points.  When a string, return it as-is."
  (if (stringp window-pulse-background)
      window-pulse-background
    (let* ((bg  (or (face-background 'default nil t) "#000000"))
           (rgb (color-name-to-rgb bg))
           (hsl (apply #'color-rgb-to-hsl rgb))
           (h   (nth 0 hsl))
           (s   (nth 1 hsl))
           (l   (nth 2 hsl))
           (shift (/ (float window-pulse-background) 100.0))
           (new-l (if (< l 0.5)
                      (min 1.0 (+ l shift))
                    (max 0.0 (- l shift)))))
      (apply #'color-rgb-to-hex (color-hsl-to-rgb h s new-l)))))

(defun window-pulse--interpolate-color (from to fraction)
  "Return a color FRACTION of the way between FROM and TO.
FRACTION is a float from 0.0 (returns FROM) to 1.0 (returns TO)."
  (let ((from-rgb (color-name-to-rgb from))
        (to-rgb   (color-name-to-rgb to)))
    (when (and from-rgb to-rgb)
      (apply #'color-rgb-to-hex
             (cl-mapcar (lambda (a b) (+ a (* fraction (- b a))))
                        from-rgb to-rgb)))))

(defun window-pulse ()
  "Pulse the entire visible area of the selected window.
Remaps the `default' face background from the color specified by
`window-pulse-background' to the original background over
`window-pulse-iterations' steps."
  (when (window-pulse-p)
    (window-pulse--cancel)
    (let* ((end-color   (or (face-background 'default nil t) "#000000"))
           (start-color (window-pulse--compute-color))
           (step        0)
           (steps       window-pulse-iterations)
           (buf         (current-buffer)))
      (when start-color
        (setq window-pulse--cookie
              (face-remap-add-relative 'default :background start-color))
        (setq window-pulse--timer
              (run-with-timer
               window-pulse-delay window-pulse-delay
               (lambda ()
                 (if (not (buffer-live-p buf))
                     (cancel-timer window-pulse--timer)
                   (with-current-buffer buf
                     (setq step (1+ step))
                     (if (>= step steps)
                         (window-pulse--cancel)
                       (let ((color (window-pulse--interpolate-color
                                     start-color end-color
                                     (/ (float step) steps))))
                         (when color
                           (face-remap-remove-relative window-pulse--cookie)
                           (setq window-pulse--cookie
                                 (face-remap-add-relative
                                  'default :background color))))))))))))))

(defun window-pulse-on-selection-change (frame)
  "Pulse the selected window when it changes within FRAME.
Does not pulse when returning from the minibuffer."
  (let ((prev (frame-parameter frame 'window-pulse--prev-window))
        (curr (frame-selected-window frame)))
    (set-frame-parameter frame 'window-pulse--prev-window curr)
    (unless (or (eq prev curr)
                (and prev (window-minibuffer-p prev)))
      (with-selected-window curr
        (window-pulse)))))

(defun window-pulse-on-focus-change ()
  "Pulse the selected window when a frame gains focus."
  (dolist (frame (frame-list))
    (when (frame-focus-state frame)
      (let ((prev (frame-parameter frame 'window-pulse--prev-focus))
            (curr t))
        (set-frame-parameter frame 'window-pulse--prev-focus curr)
        (unless prev
          (with-selected-window (frame-selected-window frame)
            (window-pulse))))
      (set-frame-parameter frame 'window-pulse--prev-focus t))
    (set-frame-parameter frame 'window-pulse--prev-focus nil)))

(add-hook 'window-selection-change-functions #'window-pulse-on-selection-change)
(add-function :after after-focus-change-function #'window-pulse-on-focus-change)

(provide 'window-pulse)

;;; window-pulse.el ends here
