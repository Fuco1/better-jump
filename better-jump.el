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

(defun bjump-buffer-window-bounds ()
  "Get the buffer bounds of current window."
  (save-excursion
    (cons (progn
            (move-to-window-line 0)
            (point))
          (progn
            (move-to-window-line -1)
            (line-end-position)))))

(defun bjump-word-jump (head-char)
  (interactive "cHead char: ")
  (bjump-jump
   (concat "\\<" (char-to-string head-char))
   (lambda (_) (bjump-buffer-window-bounds))
   (lambda () (list (car (window-list))))
   (lambda (ovs) (let ((ido-match (ido-completing-read "Where to jump: " (--map (ov-val it 'bjump-id) ovs))))
                   (nth (--find-index (equal ido-match (ov-val it 'bjump-id)) ovs) ovs)))
   (lambda (ov) (goto-char (ov-beg ov)))))

(defun bjump-jump (selector window-scope frame-scope picker action)
  "SELECTOR is where to put hints (is regexp or function).

WINDOW-SCOPE is how to narrow window (takes window, return (beg . end)).

FRAME-SCOPE is which windows to pick (return a list of windows)

PICKER is a procedure which picks the match (can be interactive or procedural, recieves the overlay list)

ACTION is what to do with the picked match (takes matched overlay)."
  (let ((ovs))
    (unwind-protect
        (progn
          (-each (funcall frame-scope)
            (lambda (win)
              (save-window-excursion
                (select-window win)
                (let* ((scope (funcall window-scope win))
                       (beg (car scope))
                       (end (cdr scope)))
                  (setq ovs (-concat (ov-regexp selector beg end) ovs))))))
          (setq ovs (nreverse ovs))
          (--each ovs
            (ov-set it
                    'display (int-to-string it-index)
                    'face 'font-lock-warning-face
                    'bjump-ov t
                    'bjump-id (int-to-string it-index)))
          (let ((picked-match (funcall picker ovs)))
            (funcall action picked-match)))
      (--each ovs (delete-overlay it)))))

(provide 'better-jump)
;;; better-jump.el ends here
