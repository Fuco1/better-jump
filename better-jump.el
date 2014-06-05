;;; better-jump.el --- Execute actions at places -*- lexical-binding: t -*-

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

(defgroup bjump ()
  "Execute actions at places."
  :group 'editing
  :prefix "bjump-")


;;; User settings
;; Hooks should be used by *end-users only* to modify or customize the
;; behaviour of the "jumpers".  The jumpers themselves should *NOT*
;; rely on the hook mechanisms, instead, they should use the
;; operations prescribed in `bjump-jump'.

;; add more hooks
(defcustom bjump-window-jump-before-action-hook ()
  "Hook run before the jump to another window.

When this hook runs, the selected window is the window from which
we want to jump away."
  :type 'hook
  :group 'bjump)

(defcustom bjump-window-jump-after-action-hook ()
  "Hook run after the jump to another window.

When this hook runs, the selected window is the window to which
we jumped."
  :type 'hook
  :group 'bjump)

(defcustom bjump-window-jump-after-cleanup-hook ()
  "Hook run after the cleanup.

This hook is run even in the case the operation was cancelled, so
there is no guarantee about which window is the selected one."
  :type 'hook
  :group 'bjump)

;; TODO: sync these docs into `bjump-jump'.

;;; Selectors
;; ...

(defun bjump-selector-char (char)
  "Return a regexp matching CHAR at the beginning of a word."
  (concat "\\<" (char-to-string char)))


;;; Window scope
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


;;; Frame scope
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



;;; Pickers

(defface bjump-hint-background
  '((t (:foreground "gray40")))
  "Background face for the entire visible buffer region."
  :group 'bjump)

(defface bjump-hint-foreground
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "Background face used for hints during hint picking."
  :group 'bjump)

(defface bjump-hint-foreground-window-picker
  '((((class color)) (:foreground "red" :height 1000))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "Background face used for hints during hint picking."
  :group 'bjump)

(defcustom bjump-picker-single-letter-list "asdfghjklqwertyuiopzxcvbnm"
  "List of letters to use to pick from the selected values.

These are used in order as specified in the string, so put your
most preferred letters first (for example, the home-row)."
  :type 'string
  :group 'bjump)

;; The picker here should be the "default", emulating ace-jump-mode
;; picker.  But I can imagine tons of other ways to select matches.
;; probably convert this to a generic taking tons of keyword args

;; TODO: abstract into a generic picker
;; - face
;; - initial label generator
;; - label updater after each picked letter
;; - prompt
(defun bjump-picker-single-letter (ovs)
  (let* ((num-choices (length bjump-picker-single-letter-list))
         (num-selected (length ovs))
         (visible-windows (-map 'car (--group-by (ov-val it :bjump-window) ovs)))
         (background-ovs (--map (save-window-excursion
                                  (select-window it)
                                  (let ((bounds (bjump-ws-window-bounds)))
                                    (ov (car bounds) (cdr bounds) 'face 'bjump-hint-background)))
                                visible-windows)))
    (unwind-protect
        (cond
         ((= num-selected 0) nil)
         ((= num-selected 1) (car ovs))
         (t
          (let* ((depth (1+ (floor (log num-selected num-choices))))
                 (ov-labels
                  (apply '-table-flat (lambda (&rest stuff)
                                        (apply 'string (nreverse stuff)))
                         (-repeat depth (string-to-list bjump-picker-single-letter-list))))
                 (r nil)
                 (current-label "")
                 (i 1))
            (setq ovs (--zip-with (ov-set it
                                          'priority 100
                                          'display
                                          (bjump-substr other 0 1 'face 'bjump-hint-foreground)
                                          :bjump-label other)
                                  ovs ov-labels))
            (while (and (not r) (<= i depth))
              (let ((current-str (char-to-string (read-char "Where to jump: "))))
                (setq current-label (concat current-label current-str))
                (let ((sep (--separate (string-prefix-p current-label (ov-val it :bjump-label)) ovs)))
                  (--each (cadr sep) (ov-set it 'display nil))
                  (--each (car sep) (let ((label (ov-val it :bjump-label)))
                                      (ov-set it 'display (bjump-substr
                                                           label i (1+ i)
                                                           'face 'bjump-hint-foreground))))
                  (setq ovs (car sep)))
                (-when-let (ov (--first (equal (ov-val it :bjump-label) current-label) ovs))
                  (setq r ov)))
              (cl-incf i))
            r)))
      (-map 'delete-overlay background-ovs))))


;;; Actions
;; Action executed at the picked overlay.  This gets the overlay as an
;; argument an can do any action whatsoever.  At the time this
;; function is run, selected window is the window from where the jump
;; function was called.  You can get the window where this overlay is
;; defined by getting the property `:bjump-window' from the overlay.

(defun bjump-action-goto-char (ov)
  "Select the frame and window where OV is placed, then go to the beginning of OV."
  (let ((win (ov-val ov :bjump-window)))
    (select-frame-set-input-focus (window-frame win))
    (select-window win)
    (goto-char (ov-beg ov))))

(defun bjump-action-goto-window (ov)
  "Select the frame and window where OV is placed."
  (let ((win (ov-val ov :bjump-window)))
    (select-frame-set-input-focus (window-frame win))
    (select-window win)))

(defun bjump-action-goto-frame (ov)
  "Select the frame where OV is placed."
  (let ((win (ov-val ov :bjump-window)))
    (select-frame-set-input-focus (window-frame win))))

(defun bjump-action-goto-char-and-execute-action (action)
  "Select the frame and window where OV is placed, then go to the beginning of OV."
  (lambda (ov)
    (bjump-action-goto-char ov)
    (funcall action)))


;;; Other helpers

;; TODO: put this into s.el?
(defun bjump-substr (string beg end &rest props)
  "Return substring of STRING from BEG to END, starting from zero.

If END is greater than the length of the string, the portion from
BEG to the end of string is returned.

PROPS is a list of property and value pairs to be applied to the
extracted substring."
  (let ((l (length string)))
    (setq end (min end l))
    (apply 'propertize (substring string beg end) props)))


(defun* bjump-jump (selector
                    &key
                    (window-scope 'bjump-ws-window-bounds)
                    (frame-scope 'bjump-fs-current-window)
                    (picker 'bjump-picker-single-letter)
                    (action 'bjump-action-goto-char)
                    before-action-hook
                    after-action-hook
                    after-cleanup-hook)
  "SELECTOR is where to put hints (is regexp or char or function, function returns ((beg . end)*)).

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
                   ((characterp selector)
                    (setq new-ovs (nreverse (ov-regexp (bjump-selector-char selector) beg end))))
                   ((stringp selector)
                    (setq new-ovs (nreverse (ov-regexp selector beg end))))
                   (t
                    (let ((bounds (funcall selector beg end)))
                      (setq new-ovs (--map (make-overlay (car it) (cdr it)) bounds)))))
                  (setq ovs (-concat ovs (ov-set new-ovs :bjump-window win)))))))
          (--each ovs
            (ov-set it
                    'evaporate nil
                    :bjump-ov t
                    :bjump-id (int-to-string it-index)))
          (-if-let (picked-match (funcall picker ovs))
              (progn
                (run-hooks before-action-hook)
                (funcall action picked-match)
                (run-hooks after-action-hook))
            ;; make these messages better and non-annoying somehow.
            (message "[better jump] No match")))
      (--each ovs (delete-overlay it))
      (run-hooks after-cleanup-hook))))


;;; Interactive
(defun bjump-word-jump (head-char)
  (interactive "cHead char: ")
  (bjump-jump head-char))

(defun bjump-word-jump-line (head-char)
  (interactive "cHead char: ")
  (bjump-jump head-char :window-scope 'bjump-ws-line-bounds))

(defun bjump-word-jump-paragraph (head-char)
  (interactive "cHead char: ")
  (bjump-jump head-char :window-scope 'bjump-ws-paragraph-bounds))

(defun bjump-window-jump ()
  (interactive)
  (bjump-jump
   (lambda (_ _) (save-excursion
                   (move-to-window-line 0)
                   (list (cons (point) (1+ (point))))))
   :window-scope 'bjump-ws-ignore
   :frame-scope 'bjump-fs-visible-frames-nsw
   :action 'bjump-action-goto-window
   :before-action-hook 'bjump-window-jump-before-action-hook
   :after-action-hook 'bjump-window-jump-after-action-hook
   :after-cleanup-hook 'bjump-window-jump-after-cleanup-hook))

(provide 'better-jump)
;;; better-jump.el ends here
