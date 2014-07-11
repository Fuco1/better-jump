;;; better-jump.el --- Execute actions at places -*- lexical-binding: t -*-

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 1st June 2014
;; Package-requires: ((dash "2.7.0") (ov.el "1.0"))
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

;; Read the docstring of `bjump-jump'.

;;; Code:

(require 'dash)
(require 'ov)

(defgroup bjump ()
  "Execute actions at places."
  :group 'editing
  :prefix "bjump-")

(defgroup bjump-faces ()
  "Better-jump faces."
  :group 'bjump
  :prefix "bjump-")

(defgroup bjump-hooks ()
  "Hooks provided for built-in better-jump jumpers."
  :group 'bjump
  :prefix "bjump-")


;;; User settings
;; Hooks should be used by *end-users only* to modify or customize the
;; behaviour of the "jumpers".  The jumpers themselves should *NOT*
;; rely on the hook mechanisms, instead, they should use the
;; operations prescribed in `bjump-jump'.


;; TODO: sync these docs into `bjump-jump'.

;;; Selectors

(defun bjump-selector-word-by-char (char)
  "Return a regexp matching CHAR at the beginning of a word."
  (concat "\\<" (regexp-quote (char-to-string char))))

(defun bjump-selector-window (_ _)
  "Return a single selector for current window.

Useful for window switching/deletion operations."
  (save-excursion
    (move-to-window-line 0)
    (list (ov (point) (1+ (point))
              :bjump-picker-face 'bjump-hint-foreground-window-picker))))


;;; Window scope
;; TODO: add more using `thingatpt' (defun, sentence), but guard for
;; "overflowing" the visible buffer area.

(defun bjump-ws-window-bounds ()
  "Get the bounds of the portion of buffer visible in current window."
  (save-excursion
    (cons (progn (move-to-window-line 0) (point))
          (progn (move-to-window-line -1) (line-end-position)))))

(defun bjump-ws-line-bounds ()
  "Get the line bounds at point."
  (if (bound-and-true-p visual-line-mode)
      (cons
       (save-excursion
         (beginning-of-visual-line)
         (point))
       (save-excursion
         (end-of-visual-line)
         (point)))
    (cons (line-beginning-position) (line-end-position))))

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
  :group 'bjump-faces)

(defface bjump-hint-foreground
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "Background face used for hints during hint picking."
  :group 'bjump-faces)

(defface bjump-hint-foreground-window-picker
  '((((class color)) (:foreground "red" :height 1000))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "Background face used for hints during hint picking."
  :group 'bjump-faces)

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
;; - it should be possible to also take the face to be used from the overlay itself
(defun bjump-picker-single-letter (ovs)
  (let* ((num-choices (length bjump-picker-single-letter-list))
         (num-selected (length ovs))
         (visible-windows (-map 'car (--group-by (ov-val it :bjump-window) ovs)))
         (background-ovs (--map (save-window-excursion
                                  (select-window it)
                                  (let ((bounds (bjump-ws-window-bounds)))
                                    (ov (car bounds) (cdr bounds)
                                        'face 'bjump-hint-background
                                        'priority 10000)))
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
                                          'priority 10001
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

(defun bjump-action-goto-char (ov &optional no-mark)
  "Select the frame and window where OV is placed, then go to the beginning of OV.

If the optional argument NO-MARK is non-nil, do not push the
current position onto the `mark-ring'."
  (let ((win (ov-val ov :bjump-window)))
    (select-frame-set-input-focus (window-frame win))
    (select-window win)
    (unless no-mark (push-mark))
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


;;; Action combinators
;; Functions that take "actions" and combine them with other actions.
;; This is basically the decorator pattern.

(defun bjump-com-goto-char-execute (action)
  "Combine `bjump-action-goto-char' with ACTION.

First, run `bjump-action-goto-char'.  Then execute action.
Action should be a zero-argument function.

Return a function suitable for use in `bjump-jump' :action."
  (lambda (ov)
    (bjump-action-goto-char ov t)
    (funcall action)))

(defun bjump-com-at-char-execute (action)
  "Same as `bjump-com-goto-char-execute' but wrapped with `save-excursion'."
  (lambda (ov)
    (save-excursion
      (bjump-action-goto-char ov t)
      (funcall action))))


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

(defun bjump-point-1 ()
  "Return (cons (1- (point)) (point))."
  (cons (1- (point)) (point)))

(defun bjump-point+1 ()
  "Return (cons (point) (1+ (point)))."
  (cons (point) (1+ (point))))

(defun* bjump-jump (selector
                    &key
                    (window-scope 'bjump-ws-window-bounds)
                    (frame-scope 'bjump-fs-current-window)
                    (picker 'bjump-picker-single-letter)
                    (action 'bjump-action-goto-char)
                    before-action-hook
                    after-action-hook
                    after-cleanup-hook)
  "Execute action at a place.

An \"action\" is an arbitrary piece of elisp code that does
whatever.

A \"place\" or a \"candidate\" is a region inside a buffer upon
which an action is executed.  Note that an action can also just
ignore this information.

A \"jumper\" is a concrete instance of this function with
specified actions, places, scopes and so on.

This function is the main engine behind all the \"jumpers\".  It
is a higher-order function, taking other functions as arguments
which modify its behaviour.  You can think of it as a
\"skeleton\" with plug-able slots where you can put your own
custom code.

SELECTOR specifies the places at which it is possible to execute
an action.  It can be a character, a regular expression or a
function.

If a character, this is converted to a regular expression
matching beginnings of words that start with given character.

If a regular expression, this is used directly to match portions
of the buffer.

If a function, it should take two arguments: beginning and end
positions inside which we search for the places.  It can return
either a list of overlays, where each overlay spans one place, or
a list of cons pairs (beg . end), where each specifies one place.

WINDOW-SCOPE is a function taking zero arguments and returning a
cons pair (beg . end).  This \"region\" is passed to the
selector.

Window scope resolution is useful to narrow down the region of
the visible portion of the window from where candidates should be
picked.  It should return bounds of this region.  These bounds
should not overflow the visible buffer region.

FRAME-SCOPE is a function taking zero arguments and returning a
list of (visible) windows where we should look for candidates.

Note especially that you have to return a list even if you only
want one window (cf. `bjump-fs-current-window').  The windows are
then selected in order of the returned list, so if you want some
special ordering, order them before returning the list.

PICKER is a procedure which picks the place.  It can be an
interactive function but does not need to.  It takes one
argument, the list of overlays representing all possible places.

ACTION is a function taking the overlay representing the place
and producing whatever result, doing anything whatsoever.  At the
time this function is executed, selected window is the window
from where the jump function was called.  You can get the
\"target\" window where the passed overlay was defined by reading
the property `:bjump-window' from the overlay.

The following hooks should be symbols pointing to lists such that
they can be passed into `run-hooks'.

BEFORE-ACTION-HOOK is a hook variable executed before action.

AFTER-ACTION-HOOK is a hook variable executed after action.

AFTER-CLEANUP-HOOK is a hook variable executed just before this
function returns."
  (let (ovs return-value)
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
                    (setq new-ovs (nreverse (ov-regexp (bjump-selector-word-by-char selector) beg end))))
                   ((stringp selector)
                    (setq new-ovs (nreverse (ov-regexp selector beg end))))
                   (t
                    (let ((bounds (funcall selector beg end)))
                      ;; If the first returned bound is overlay, we
                      ;; assume all of them are.  In that case, we
                      ;; just pass them along
                      (if (overlayp (car bounds))
                          (setq new-ovs bounds)
                        (setq new-ovs (--map (make-overlay (car it) (cdr it)) bounds))))))
                  (setq ovs (-concat ovs (ov-set new-ovs :bjump-window win)))))))
          (--each ovs
            (ov-set it
                    'evaporate nil
                    :bjump-ov t
                    :bjump-id (int-to-string it-index)))
          (-if-let (picked-match (funcall picker ovs))
              (progn
                (run-hooks before-action-hook)
                (setq return-value (funcall action picked-match))
                (run-hooks after-action-hook))
            ;; make these messages better and non-annoying somehow.
            (message "[better jump] No match")))
      (--each ovs (delete-overlay it))
      (run-hooks after-cleanup-hook)
      return-value)))


;;; Interactive
;;;###autoload
(defun bjump-char-jump-line (char)
  "Jump to CHAR anywhere on the current line.

This function respects `visual-line-mode'."
  (interactive "cChar: ")
  (bjump-jump (regexp-quote (char-to-string char)) :window-scope 'bjump-ws-line-bounds))

;;;###autoload
(defun bjump-word-jump (head-char)
  "Jump to a word starting with HEAD-CHAR visible in the selected window."
  (interactive "cHead char: ")
  (bjump-jump head-char))

;;;###autoload
(defun bjump-word-jump-line (head-char)
  "Jump to a word starting with HEAD-CHAR anywhere on the current line.

This function respects `visual-line-mode'."
  (interactive "cHead char: ")
  (bjump-jump head-char :window-scope 'bjump-ws-line-bounds))

;;;###autoload
(defun bjump-word-jump-paragraph (head-char)
  "Jump to a word starting with HEAD-CHAR anywhere in the current paragraph."
  (interactive "cHead char: ")
  (bjump-jump head-char :window-scope 'bjump-ws-paragraph-bounds))

(defcustom bjump-window-jump-before-action-hook ()
  "Hook run before the jump to another window.

When this hook runs, the selected window is the window from which
we want to jump away."
  :type 'hook
  :group 'bjump-hooks)

(defcustom bjump-window-jump-after-action-hook ()
  "Hook run after the jump to another window.

When this hook runs, the selected window is the window to which
we jumped."
  :type 'hook
  :group 'bjump-hooks)

(defcustom bjump-window-jump-after-cleanup-hook ()
  "Hook run after the cleanup.

This hook is run even in the case the operation was cancelled, so
there is no guarantee about which window is the selected one."
  :type 'hook
  :group 'bjump-hooks)

;;;###autoload
(defun bjump-window-jump ()
  "Jump to a window in currently visible frames.

If there are only two windows, jump to the other one."
  (interactive)
  (bjump-jump
   'bjump-selector-window
   :window-scope 'bjump-ws-ignore
   :frame-scope 'bjump-fs-visible-frames-nsw
   :action 'bjump-action-goto-window
   :before-action-hook 'bjump-window-jump-before-action-hook
   :after-action-hook 'bjump-window-jump-after-action-hook
   :after-cleanup-hook 'bjump-window-jump-after-cleanup-hook))

;;;###autoload
(defun bjump-window-delete ()
  "Delete a window in currently visible frames.

If there are only two windows, delete the other one.  Otherwise,
interactively pick window to be deleted."
  (interactive)
  (bjump-jump
   'bjump-selector-window
   :window-scope 'bjump-ws-ignore
   :frame-scope 'bjump-fs-visible-frames-nsw
   :action (lambda (ov)
             (let ((win (ov-val ov :bjump-window)))
               (delete-window win)))))

;; TODO these "while stuff do stuff" loops repeat all over the place.
;; Abstract it into some "collector" pattern
;;;###autoload
(defun bjump-help-link-jump ()
  "Follow a help link visible in the selected window."
  (interactive)
  (bjump-jump
   (lambda (beg end)
     (save-excursion
       (let (b buttons)
         (goto-char beg)
         (while (and (setq b (ignore-errors (forward-button 1)))
                     (< b end))
           (push (cons (marker-position b)
                       (1+ (marker-position b)))
                 buttons))
         (nreverse buttons))))
   :action (bjump-com-goto-char-execute 'push-button)))

;;;###autoload
(defun bjump-info-link-jump ()
  "Follow an info link visible in the selected window."
  (interactive)
  (bjump-jump
   (lambda (beg end)
     (save-excursion
       (let (n nodes)
         (goto-char beg)
         (while (and (ignore-errors (Info-next-reference) t)
                     (< (point) end)
                     (if nodes (> (point) (caar nodes)) t))
           (push (bjump-point+1) nodes))
         (nreverse nodes))))
   :action (bjump-com-goto-char-execute 'Info-follow-nearest-node)))

(defcustom bjump-dired-open-command 'dired-find-file
  "Command to execute in `bjump-dired-jump'.

This command is called with point on the file we want to act upon."
  :type 'function
  :group 'bjump)

;;;###autoload
(defun bjump-dired-jump ()
  "Run `bjump-dired-open-command' on a file in a dired buffer."
  (interactive)
  (bjump-jump
   (lambda (beg end)
     (let (r)
       (save-excursion
         (goto-char beg)
         (while (< (point) end)
           (when (dired-get-filename nil t)
             (push (bjump-point-1) r))
           (dired-next-line 1))
         (nreverse r))))
   :action (bjump-com-goto-char-execute bjump-dired-open-command)))

(provide 'better-jump)
;;; better-jump.el ends here
