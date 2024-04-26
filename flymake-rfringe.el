;;; flymake-rfringe.el --- Flymake diagnostics as relative fringe -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alain Delplanque

;; Author: Dino Chiesa <dpchiesa@hotmail.com>
;;         Alain Delplanque <alaindelplanque@mailoo.org>
;; Maintainer: Alain Delplanque <alaindelplanque@mailoo.org>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://github.com/adelplanque/flymake-rfringe

;; This file is NOT part of GNU Emacs.

;;; Licence:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Original rfringe.el code is licensed differently, under the Ms-PL.  For more
;; details, see: http://www.opensource.org/licenses/ms-pl

;;; Commentary:

;; flymake-rfringe is a small module allowing the display of flymake diagnostics
;; in the form of a small mark in the right fringe.  The marking take place in
;; the visible window in proportion to the diagnostic line within the entire
;; buffer.

;; The style of the marks can be customized with the faces:
;; * `flymake-rfringe-error-face'
;; * `flymake-rfringe-warning-face'
;; * `flymake-rfringe-note-face'

;; flymake-rfringe works by advising functions since flymake does not seem to
;; offer a hook allowing you to be notified of a diagnostic change.

;; Usage:

;; (require 'flymake-rfringe)

;; History:

;; flymake-rfringe started as a fork of Dino Chiesa's rfringe.el to make the
;; module compatible with the version of flymake shipped as standard in Emacs
;; before evolving into a deeper rewrite.

;;; Code:

(require 'compile)
(require 'flymake)
(require 'fringe)

(defgroup flymake-rfringe nil
  "Relative position mark, in the fringe."
  :group 'flymake)

(defface flymake-rfringe-error-face
  '((t :inherit compilation-error))
  "Face for error mark."
  :group 'flymake-rfringe)

(defface flymake-rfringe-warning-face
  '((t :inherit compilation-warning))
  "Face for warning mark."
  :group 'flymake-rfringe)

(defface flymake-rfringe-note-face
  '((t :inherit compilation-info))
  "Face for comment mark."
  :group 'flymake-rfringe)

(defconst flymake-rfringe-type-rank-alist
  '((:error . 2)
    (:warning . 1)
    (:note . 0))
  "A alist which associate a rank to flymake diagnostic type.")

(defconst flymake-rfringe-type-face-alist
  '((:error . flymake-rfringe-error-face)
    (:warning . flymake-rfringe-warning-face)
    (:note . flymake-rfringe-note-face))
  "A alist which associate a face to flymake diagnostic type.")

;; flymake-rfringe displays only one kind of bitmap - a thin dash. Create it here.
(define-fringe-bitmap 'flymake-rfringe-thin-dash [255 0])

(defun flymake-rfringe--compute-position (pos)
  "Computes relative position where to put fringe for absolute position POS."
  (let* ((line-start (line-number-at-pos (window-start)))
         ; pos can be > (point) when flymake diagnostic is outdated
         (line-no (line-number-at-pos (min pos (point-max))))
         (height (window-body-height))
         (line-count (line-number-at-pos (1- (point-max))))
         (rline (min (+ line-start (/ (* (1- line-no) height) line-count))
                     line-count)))
    (save-excursion (goto-char 1)
                    (forward-line (1- rline))
                    (point))))

(defun flymake-rfringe--create-indicator (diag)
  "Create a fringe overlay for a flymake diagnostic DIAG.
The fringe is created in visible area in proportion to the flymake diagnostic
line throughout the buffer."
  (let* ((pos (flymake-diagnostic-beg diag))
         (type (let ((type (flymake-diagnostic-type diag)))
                 (if (member type '(:warning :error)) type :note)))
         (rpos (flymake-rfringe--compute-position pos))
         (face (alist-get type flymake-rfringe-type-face-alist))
         (before-string (propertize "!" 'display
                                    `(right-fringe flymake-rfringe-thin-dash ,face)))
         (ov (make-overlay rpos rpos)))
    (overlay-put ov 'flymake-rfringe t)
    (overlay-put ov 'flymake-rfringe-pos pos)
    (overlay-put ov 'before-string before-string)
    (overlay-put ov 'priority (alist-get type flymake-rfringe-type-rank-alist))
    (overlay-put ov 'fringe-helper t)))

(defun flymake-rfringe--remove-indicators ()
  "Remove all flymake rfringe indicators for the current buffer."
  (mapc (lambda (ov) (if (overlay-get ov 'flymake-rfringe) (delete-overlay ov)))
        (overlays-in 1 (point-max))))

(defun flymake-rfringe--reset-indicators ()
  "Refresh position of all fringe overlays.
Should be called when window change (scroll or resized) to keep overlay in
visible area proportional to the position in buffer."
  (mapc (lambda (ov)
          (when (overlay-get ov 'flymake-rfringe)
            (let ((rpos (flymake-rfringe--compute-position
                         (overlay-get ov 'flymake-rfringe-pos))))
              (move-overlay ov rpos rpos))))
        (overlays-in 1 (point-max))))

(defun flymake-rfringe--update-indicators-on-window-scroll (wnd new-start)
  "A sort-of-hook that gets called as each window is scrolled.
The window is given by WND and the new start position is given by NEW-START.
See `window-scroll-functions' for more info."
  (if wnd (with-current-buffer (window-buffer wnd) (flymake-rfringe--reset-indicators))))

(defun flymake-rfringe--post-syntax-check (&rest r)
  "Update fringe indicators in current buffer.
This function is intended to advice the `flymake--handle-report' function, whose
arguments are R."
  (flymake-rfringe--remove-indicators)
  (mapc (lambda (item) (flymake-rfringe--create-indicator item))
        (flymake-diagnostics)))

;; hooks for managing all indicators
(add-hook 'window-scroll-functions #'flymake-rfringe--update-indicators-on-window-scroll)
(add-hook 'window-configuration-change-hook #'flymake-rfringe--reset-indicators)
(advice-add 'flymake--handle-report :after #'flymake-rfringe--post-syntax-check)

(provide 'flymake-rfringe)
;;; flymake-rfringe.el ends here
