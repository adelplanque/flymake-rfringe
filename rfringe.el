;;; rfringe.el --- display the relative location of the region, in the fringe.
;;
;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : April 2011
;; Version    : 1.0
;; Keywords   : fringe, bitmap
;; X-URL      : http://www.emacswiki.org/emacs/rfringe.el
;; Last-saved : <2011-April-05 11:20:26>

;; Copyright (C) 2011 Dino Chiesa
;;
;; This file is NOT part of GNU Emacs and is licensed differently.
;; rfringe.el is licensed under the Ms-PL.  See the full copy of that
;; license for more details. http://www.opensource.org/licenses/ms-pl
;;

;;; Commentary:
;;
;; This is a module to allow the use of the fringe to indicate locations
;; relative to the bounds of the buffer.  rfringe = "relative fringe".
;;
;; In emacs, displaying fringe indicators is done via text overlays. In
;; that way, bitmaps in the fringe are attached to the lines of text
;; shown in the buffer window.
;;
;; This works nicely when the fringe is used to indicate information
;; that is relevant to the adjacent line; for example, text overflow, or
;; something similar. But, there isn't a simple way for an application
;; or module to use the fringe to display buffer-relative information -
;; for example, the location of compiler error messages.
;;
;; In fact, using the fringe to communicate this kind of information -
;; buffer-relative positions - is probably more intuitive and is
;; certainly more useful for the user. For example, consider the
;; scrollbar. The position and size of the scrollbar indicates to the
;; user the position and size of the current window relative to the
;; entire buffer. This is information that cound not be easily or
;; appropriately conveyed within the visual text. The fringe is
;; perfectly suited for this purpose.
;;
;; Along those lines, a useful integration of fringe with flymake would
;; be to use fringe bitmaps visually indicate the position of all
;; flymake errors and warnings in the buffer, relative to the beginning
;; and end of the buffer. A quick glance at the fringe would give a
;; visual indication of the number of errors or warnings and their
;; approximate positions.
;;
;; Likewise, a diff mode might want to display fringe indicators for the
;; number and approximate relative position of differences.
;;
;; Doing this is not simple, because of the dependency of fringe bitmaps
;; on text overlays that I described above.  To use the fringe to
;; communicate information regarding buffer-relative positions requires
;; a transformation from "buffer position" to "window position".  And
;; this transformation must be re-computed each time a window scrolls or
;; changes size.
;;
;; This module addresses that need, and provides that transformation. It
;; allows you to set an indicator that is buffer-relative in the fringe;
;; the indicator automatically redisplays if the window changes size, or
;; scrolls.
;;
;; Examples:
;;
;; 1. In the simplest case, you can use rfringe to provide a visual
;;    indicator of the top of the region in the buffer, like so:
;;
;;       (rfringe-show-region)
;;
;;    To turn off the indicator, do this:
;;
;;       (rfringe-hide-region)
;;
;;
;; 2. You can also use rfringe to display a set of indicators,
;;    corresponding to a set of locations in the buffer. These might be
;;    the locations of compiler errors, or section beginnings, or
;;    whatever you like.
;;
;;       (setq posns '(79 1000 2000 3000 4000 5000 6000 9000 10000))
;;       (mapc 'rfringe-create-relative-indicator posns)
;;
;;    As you scroll through the buffer, the indicators in the fringe remain fixed.
;;
;;    To remove the indicators, do this:
;;
;;       (rfringe-remove-managed-indicators)
;;
;;    By default, rfringe defines advice to extend flymake to display
;;    indicators this way.  This is not the only intended use of rfringe.el,
;;    but it is a good example.
;;
;;; Code:

(require 'compile)
(require 'flymake)
(require 'fringe)
(eval-when-compile (require 'cl-lib))

(defgroup rfringe nil
  "Relative position mark, in the fringe."
  :group 'flymake)

(defface rfringe-error-face
  '((t :inherit compilation-error))
  "Face for error mark.")

(defface rfringe-warning-face
  '((t :inherit compilation-warning))
  "Face for warning mark.")

(defface rfringe-note-face
  '((t :inherit compilation-info))
  "Face for comment mark.")

(defvar rfringe-region-indicator-ovly nil
  "The overlay used internally for the region; see `rfringe-show-region-indicator'.

Applications should not set this value directly.  It is intended for use
internally by rfringe.el.")

(defconst rfringe-type-rank-alist
  '((:error . 2)
    (:warning . 1)
    (:note . 0))
  "A alist which associate a rank to mark type.")

(defconst rfringe-type-face-alist
  '((:error . rfringe-error-face)
    (:warning . rfringe-warning-face)
    (:note . rfringe-note-face))
  "A alist which associate a face to mark type.")

(make-variable-buffer-local 'rfringe-region-indicator-ovly)

;; rfringe displays only one kind of bitmap - a thin dash. Create it here.
(define-fringe-bitmap 'rfringe-thin-dash [255 0])

(defun rfringe--compute-position (pos)
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

(defun rfringe-hide-region ()
  "Hide any bitmap currently displayed in the fringe indicating the region."
  (interactive)
  (if rfringe-region-indicator-ovly
      (progn
        (delete-overlay rfringe-region-indicator-ovly)
        (setq rfringe-region-indicator-ovly nil))))

(defun rfringe-update-region-indicator (&optional buf)
  "Update any fringe indicator for the region, in the buffer BUF."
  (if (not buf)
      (setq buf (current-buffer)))
  (with-current-buffer buf
    (if rfringe-region-indicator-ovly
        (rfringe-show-region-indicator buf))))

(defun rfringe-create-relative-indicator (pos &optional manage type)
  "Display an indicator in the fringe in the current buffer.

POS is the position in the buffer.  Indicator take place in fringe relative to
the buffer size, via a simple bitmap dash.

Optional TYPE is the `flymake-category' reported, and is used to fit the fringe
face.  By default, the fringe is displayed as `rfringe-note-face'.

If optional MANAGE is non nil, the bitmap will be automatically moved if the
window changes size, or scrolls, and will be deleted with
`rfringe-remove-managed-indicators'.

For example, for a buffer of length 10000, if you pas a POS of 5000, then this
function will display a dash in the fringe, halfway down, regardless of whether
char position 5000 is visible in the window."
  (when (not (member type '(:warning :error))) (setq type :note))
  (let* ((rpos (rfringe--compute-position pos))
         (before-string (propertize "!" 'display
                                    `(right-fringe rfringe-thin-dash
                                                   ,(alist-get type rfringe-type-face-alist))))
         (ov (make-overlay rpos rpos)))
    (overlay-put ov 'rfringe t)
    (overlay-put ov 'rfringe-pos pos)
    (overlay-put ov 'before-string before-string)
    (overlay-put ov 'priority (alist-get type rfringe-type-rank-alist))
    (overlay-put ov 'fringe-helper t)
    (if manage (overlay-put ov 'rfringe-manage t))
    ov))

(defun rfringe-show-region-indicator (buf)
  "Display an indicator in the fringe of the position of the region in the
buffer BUF, via a bitmap dash.

For example, if the region is at the top of the buffer, then a
dash will appear at the top of the fringe, regardless of whether
any part of the region is in fact visible in the window."
  (with-current-buffer buf
    (rfringe-hide-region)
    (if (mark) ;; the mark is set
        (setq rfringe-region-indicator-ovly
              (rfringe-create-relative-indicator (min (point) (mark)) nil)))))

(defun rfringe-remove-managed-indicators ()
  "Remove all rfringe-managed indicators for the current buffer."
  (mapc (lambda (ov)
          (when (and (overlay-get ov 'rfringe) (overlay-get ov 'rfringe-manage))
            (delete-overlay ov)))
        (overlays-in 1 (point-max))))

(defun rfringe-show-region ()
  "Display an indicator in the fringe, for the top of the region."
  (interactive)
  (rfringe-show-region-indicator (current-buffer)))


;; hooks

(defun rfringe--update-region-on-window-scroll (wnd new-start)
  "A sort-of-hook that gets called as each window is scrolled.
The window is given by WND and the new start position is given
by NEW-START.

See `window-scroll-functions' for more info."
  (if wnd
      (rfringe-update-region-indicator (window-buffer wnd))))

(defun rfringe--reset-region-indicator-on-window-config-change ()
  "A sort-of-hook that gets called as a window's \"configuration\" change.

Configuration includes size, width (I guess), and so on. If the user splits or
unsplits the window, then the configuration changes, and this hook gets called.

This one resets the region indicator, if it is visible.

See `window-configuration-change-hook' for more info."
  (if rfringe-region-indicator-ovly
      (rfringe-show-region)))

(defun rfringe--reset-visible-indicators ()
  "A sort-of-hook that gets called as a window's \"configuration\" change.

Configuration includes size, width (I guess), and so on. Also, if the user
splits or unsplits the window, then the configuration changes, and this hook
gets called.

This fn moves all managed indicators.

See`window-configuration-change-hook' for more info."
  (mapc (lambda (ov)
          (when (and (overlay-get ov 'rfringe) (overlay-get ov 'rfringe-manage))
            (let ((rpos (rfringe--compute-position (overlay-get ov 'rfringe-pos))))
              (move-overlay ov rpos rpos))))
        (overlays-in 1 (point-max))))

(defun rfringe--update-managed-indicators-on-window-scroll (wnd new-start)
  "A sort-of-hook that gets called as each window is scrolled.
The window is given by WND and the new start position is given by NEW-START.

See `window-scroll-functions' for more info."
  (if wnd
      (with-current-buffer (window-buffer wnd)
        (rfringe--reset-visible-indicators))))

;; hooks for managing the 'special' region indicator
(add-hook 'window-scroll-functions 'rfringe--update-region-on-window-scroll)
(add-hook 'window-configuration-change-hook
          'rfringe--reset-region-indicator-on-window-config-change)
(add-hook 'activate-mark-hook 'rfringe-update-region-indicator)

;; hooks for managing all managed indicators
(add-hook 'window-scroll-functions 'rfringe--update-managed-indicators-on-window-scroll)
(add-hook 'window-configuration-change-hook 'rfringe--reset-visible-indicators)

(defun flymake-post-syntax-check-rfringe (&rest r)
  "Update fringe indicators in current buffer.
This function is intended to advice `flymake--handle-report', with R arguments."
  (rfringe-remove-managed-indicators)
  (mapc (lambda (item)
          (rfringe-create-relative-indicator (flymake-diagnostic-beg item) t
                                             (flymake-diagnostic-type item)))
        (flymake-diagnostics)))

(advice-add 'flymake--handle-report :after #'flymake-post-syntax-check-rfringe)

(provide 'rfringe)
;;; rfringe.el ends here
