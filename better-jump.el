;;; better-jump.el --- Execute actions at places. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 1st June 2014
;; Package-requires: ((dash "2.6.0") (ov.el "1.0"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'ov)


;;; user settings

;; add more hooks
(defcustom bjump-window-jump-after-action-hook ()
  "Hook run after the action has been executed and all the bjump
  overlays were cleared."
  :type 'hook
  :group 'bjump)

;; TODO: sync these docs into `bjump-jump'.

;;; selectors
;; ...

(defun bjump-selector-char (char)
  "Return a regexp matching CHAR at the beginning of a word."
  (concat "\\<" (char-to-string char)))


;;; window scope
;; "window scope" resolution is useful to narrow down the region of
;; the visible portion of window from where candidates should be
;; picked.  It should return bounds of the region.  These bounds
;; should not "overflow" the visible buffer region.

;; TODO: add more using `thingatpt' (defun, sentence), but guard for
;; "overflowing" the visible buffer area.

(defun bjump-ws-window-bounds ()
  "Get the bounds of the portion of buffer visible in current window."
  (save-excursion
    (cons (progn (move-to-window-line 0) (point))
          (progn (move-to-window-line -1) (line-end-position)))))

(defun bjump-ws-line-bounds ()
  "Get the line bounds at point."
  (cons (line-beginning-position) (line-end-position)))

(defun bjump-ws-paragraph-bounds ()
  "Get the paragraph bounds at point."
  (save-excursion
    (let ((end (progn (forward-paragraph) (point)))
          (beg (progn (backward-paragraph ) (point)) ))
      (cons beg end))))

(defun bjump-ws-ignore ()
  "Return default value of the window scope.

Useful when the selector does not depend on this value."
  (cons 1 1))


;;; frame scope
;; "frame scope" resolution is used to pick windows in which the
;; jumper should operate.  It should return list of windows we
;; consider "workable".  Note especially that you have to return a
;; list even if you only want one window
;; (cf. `bjump-fs-current-window').  The windows are then selected in
;; order of the returned list, so if you want some special ordering,
;; order them here.

(defun bjump-fs-current-window ()
  "Return just the current window."
  (list (car (window-list))))

(defun bjump-fs-current-frame ()
  "Return all windows in current frame, ordered by `next-window'."
  (window-list))

(defun bjump-fs-current-frame-nsw ()
  "Return all windows in current frame, but not the selected window.

Ordered by `next-window'."
  (--remove (equal it (selected-window)) (window-list)))

(defun bjump-fs-visible-frames ()
  "Return all windows in all visible frames.

Ordered by `next-window' and `visible-frame-list'."
  (--mapcat (window-list it) (visible-frame-list)))

(defun bjump-fs-visible-frames-nsw ()
  "Return all windows in all visible frames, but not the selected window.

Ordered by `next-window' and `visible-frame-list'."
  (--remove (equal it (selected-window))
            (--mapcat (window-list it) (visible-frame-list))))



;;; pickers

;; TODO: write some more generic "hint select function" that would
;; also style the window nicely.  Could display the entire hint or
;; just first letter like ace-jump-mode does.  The hint display text
;; could be modified dynamically to reflect changes made by the
;; read-loop.  Should also take prompt and stuff.


;;; actions
;; Action executed at the picked overlay.  This gets the overlay as an
;; argument an can do any action whatsoever.  At the time this
;; function is run, selected window is the window from where the jump
;; function was called.  You can get the window where this overlay is
;; defined by getting the property `bjump-window' from the overlay.

(defun bjump-action-goto-char (ov)
  "Select the frame and window where OV is placed, then go to the beginning of OV."
  (let ((win (ov-val ov 'bjump-window)))
    (select-frame-set-input-focus (window-frame win))
    (select-window win)
    (goto-char (ov-beg ov))))

(defun bjump-action-goto-window (ov)
  "Select the frame and window where OV is placed."
  (let ((win (ov-val ov 'bjump-window)))
    (select-frame-set-input-focus (window-frame win))
    (select-window win)))

(defun bjump-action-goto-frame (ov)
  "Select the frame where OV is placed."
  (let ((win (ov-val ov 'bjump-window)))
    (select-frame-set-input-focus (window-frame win))))


;;; other helpers

(defun bjump-jump (selector window-scope frame-scope picker action &optional hooks)
  "SELECTOR is where to put hints (is regexp or function, function returns ((beg . end)*)).

WINDOW-SCOPE is how to narrow window (executes in \"current window\", return (beg . end)).

FRAME-SCOPE is which windows to pick (return a list of windows)

PICKER is a procedure which picks the match (can be interactive or procedural, recieves the overlay list)

ACTION is what to do with the picked match (takes matched overlay).

HOOKS is a list of actions to run at specific places.  Global
hooks do not make sense because each jump action might need
different hooks, therefore we let the callee provide those."
  (let ((ovs))
    (unwind-protect
        (progn
          (-each (funcall frame-scope)
            (lambda (win)
              (save-window-excursion
                (select-window win)
                (let* ((scope (funcall window-scope))
                       (beg (car scope))
                       (end (cdr scope))
                       new-ovs)
                  ;; this will need to handle situation when two
                  ;; windows are showing the same buffer. ajm uses
                  ;; indirect buffers, but that seems a bit overkill.
                  (cond
                   ((stringp selector)
                    (setq new-ovs (ov-regexp selector beg end)))
                   (t
                    (let ((bounds (funcall selector beg end)))
                      (setq new-ovs (--map (make-overlay (car it) (cdr it)) bounds)))))
                  (setq ovs (-concat (ov-set new-ovs 'bjump-window win) ovs))))))
          (setq ovs (nreverse ovs))
          (--each ovs
            (ov-set it
                    'display (int-to-string it-index)
                    'face 'font-lock-warning-face
                    'evaporate nil
                    'bjump-ov t
                    'bjump-id (int-to-string it-index)))
          (let ((picked-match (funcall picker ovs)))
            (funcall action picked-match)))
      (--each ovs (delete-overlay it))
      (run-hooks (plist-get hooks :after-action)))))


;;; interactive
(defun bjump-word-jump (head-char)
  (interactive "cHead char: ")
  (bjump-jump
   (bjump-selector-char head-char)
   'bjump-ws-window-bounds
   'bjump-fs-current-window
   (lambda (ovs) (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
                   (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   'bjump-action-goto-char))

(defun bjump-word-jump-line (head-char)
  (interactive "cHead char: ")
  (bjump-jump
   (bjump-selector-char head-char)
   'bjump-ws-line-bounds
   'bjump-fs-current-window
   (lambda (ovs) (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
                   (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   'bjump-action-goto-char))

(defun bjump-word-jump-paragraph (head-char)
  (interactive "cHead char: ")
  (bjump-jump
   (bjump-selector-char head-char)
   'bjump-ws-paragraph-bounds
   'bjump-fs-current-window
   (lambda (ovs) (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
                   (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   'bjump-action-goto-char))

(defun bjump-window-jump ()
  (interactive)
  (bjump-jump
   (lambda (_ _) (save-excursion
                   (move-to-window-line 0)
                   (list (cons (point) (point)))))
   'bjump-ws-ignore
   'bjump-fs-visible-frames-nsw
   (lambda (ovs)
     ;; sort the overlays here in some manner
     (--map (overlay-put it 'before-string (overlay-get it 'bjump-id)) ovs)
     (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
       (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   'bjump-action-goto-window
   (list :after-action 'bjump-window-jump-after-action-hook)))

(provide 'better-jump)
;;; better-jump.el ends here
