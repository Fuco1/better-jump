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

;; add more hooks
(defcustom bjump-window-jump-after-action-hook ()
  "Hook run after the action has been executed and all the bjump
  overlays were cleared."
  :type 'hook
  :group 'bjump)

(defun bjump-buffer-window-bounds ()
  "Get the buffer bounds of current window."
  (save-excursion
    (cons (progn
            (move-to-window-line 0)
            (point))
          (progn
            (move-to-window-line -1)
            (line-end-position)))))

;; TODO: write some more generic "hint select function" that would
;; also style the window nicely.  Could display the entire hint or
;; just first letter like ace-jump-mode does.  The hint display text
;; could be modified dynamically to reflect changes made by the
;; read-loop.  Should also take prompt and stuff.

(defun bjump-word-jump (head-char)
  (interactive "cHead char: ")
  (bjump-jump
   (concat "\\<" (char-to-string head-char))
   (lambda (_) (bjump-buffer-window-bounds))
   (lambda () (list (car (window-list))))
   (lambda (ovs) (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
                   (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   (lambda (ov) (goto-char (ov-beg ov)))))

(defun bjump-line-word-jump (head-char)
  (interactive "cHead char: ")
  (bjump-jump
   (concat "\\<" (char-to-string head-char))
   (lambda (_) (cons (line-beginning-position) (line-end-position)))
   (lambda () (list (car (window-list))))
   (lambda (ovs) (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
                   (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   (lambda (ov) (goto-char (ov-beg ov)))))

(defun bjump-window-jump ()
  (interactive)
  (bjump-jump
   (lambda (_ _) (save-excursion
                   (move-to-window-line 0)
                   (list (cons (point) (point)))))
   (lambda (_) (cons 1 1)) ;; this is ignored by the selector
   (lambda () (--remove (equal it (selected-window))
                        (--mapcat (window-list it) (visible-frame-list))))
   (lambda (ovs)
     ;; sort the overlays here in some manner
     (--map (overlay-put it 'before-string (overlay-get it 'bjump-id)) ovs)
     (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
       (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   (lambda (ov)
     (select-window (ov-val ov 'bjump-window)))
   (list :after-action 'bjump-window-jump-after-action-hook)))

(defun bjump-jump (selector window-scope frame-scope picker action &optional hooks)
  "SELECTOR is where to put hints (is regexp or function, function returns ((beg . end)*)).

WINDOW-SCOPE is how to narrow window (takes window, return (beg . end)).

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
                (let* ((scope (funcall window-scope win))
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

(provide 'better-jump)
;;; better-jump.el ends here
