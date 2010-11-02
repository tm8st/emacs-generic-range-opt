;;; generic-range-opt.el --- generate range operations.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
(defconst generic-range-opt-version "0.1")
;; Keywords: convenience, delete, kill, copy, mark

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received ba  copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 様々な範囲へのdelete, copy, kill, markを定義する為のelです.
;; 現在範囲の指定は始点、終点、マークによって可能です。
;; (defmacro generic-range-opt (name begin end &optional mark)
;; のbegin, end, mark にそれぞれ設定するための関数を渡すとgro-op-nameという名前で定義されるので、
;; それをキーにバインドしてお使いください。

;; Installation:

;; (require 'generic-range-opt)

;; (global-unset-key (kbd "C-d"))
;; (global-set-key (kbd "C-d C-j") 'delete-char)
;; (global-set-key (kbd "C-d C-i") 'gro-delete-follow-word)
;; (global-set-key (kbd "C-d C-f") 'gro-delete-forward-line)
;; (global-set-key (kbd "C-d C-b") 'gro-delete-backward-line)
;; (global-set-key (kbd "C-d C-s") 'gro-delete-search-forward-char)
;; (global-set-key (kbd "C-d C-r") 'gro-delete-search-backward-char)
;; (global-set-key (kbd "C-d C-e") 'gro-delete-follow-sexp)
;; (global-set-key (kbd "C-d C-w") 'gro-delete-follow-string)
;; (global-set-key (kbd "C-d C-d") 'gro-delete-current-line)
;; (global-set-key (kbd "C-d C-n") 'gro-delete-next-line)
;; (global-set-key (kbd "C-d C-p") 'gro-delete-prev-line)
;; (global-set-key (kbd "C-d C-h") 'gro-delete-jaunte-prev)

;; (global-unset-key (kbd "C-8"))
;; (global-set-key (kbd "C-8 C-i") 'gro-mark-follow-word)
;; (global-set-key (kbd "C-8 C-f") 'gro-mark-forward-line)
;; (global-set-key (kbd "C-8 C-b") 'gro-mark-backward-line)
;; (global-set-key (kbd "C-8 C-s") 'gro-mark-search-forward-char)
;; (global-set-key (kbd "C-8 C-r") 'gro-mark-search-backward-char)
;; (global-set-key (kbd "C-8 C-e") 'gro-mark-follow-sexp)
;; (global-set-key (kbd "C-8 C-w") 'gro-mark-follow-string)
;; (global-set-key (kbd "C-8 C-8") 'gro-mark-current-line)
;; (global-set-key (kbd "C-8 C-n") 'gro-mark-next-line)
;; (global-set-key (kbd "C-8 C-p") 'gro-mark-prev-line)
;; (global-set-key (kbd "C-8 C-h") 'gro-mark-jaunte-prev)
;; (global-set-key (kbd "C-8 C-d") 'gro-mark-defun*)

;;; Code:

(eval-when-compile (require 'cl))
(require 'thing-opt)

(defun generic-range-opt-delete-base-function (begin-move-func &optional end-move-func &optional mark-func)
  (interactive)
  (save-excursion
    (if mark-active
	(delete-region (point) (mark))
      (let* ((start-point (point)) (s (progn (when begin-move-func
			(funcall begin-move-func))
		      (point))))
	(let ((e
	       (progn (when end-move-func
			(goto-char start-point)
			(funcall end-move-func))
		      (point))))
	  (when mark-func
	    (goto-char start-point)
	    (funcall mark-func)
	    (setq e (point))
	    (setq s (mark)))
	  (delete-region s e))))))

(defun generic-range-opt-kill-base-function (begin-move-func &optional end-move-func &optional mark-func)
  (interactive)
  (save-excursion
    (if mark-active
	(kill-region (point) (mark))
    (let* ((start-point (point)) (s (progn (when begin-move-func
			(funcall begin-move-func))
		      (point))))
	(let ((e
	       (progn (when end-move-func
			(goto-char start-point)
			(funcall end-move-func))
		      (point))))
	(when mark-func
	  (goto-char start-point)
	  (funcall mark-func)
	  (setq s (mark))
	  (setq e (point)))
	(kill-region s e)
	)))))

(defun generic-range-opt-copy-base-function (begin-move-func &optional end-move-func &optional mark-func)
  (interactive)
  (save-excursion
    (if mark-active
	(copy-region-as-kill (point) (mark))
      (let* ((start-point (point)) (s (progn (when begin-move-func
			(funcall begin-move-func))
		      (point))))
	(let ((e
	       (progn (when end-move-func
			(goto-char start-point)
			(funcall end-move-func))
		      (point))))
	  (when mark-func
	    (goto-char start-point)
	    (funcall mark-func)
	    (setq s (mark))
	    (setq e (point)))
	  (copy-region-as-kill s e)
	  )))))

(defun generic-range-opt-mark-base-function (begin-move-func &optional end-move-func &optional mark-func)
  (interactive)
  (let ((start-point (point))(s (point)) (e (point)))
    (save-excursion
      (setq s (progn (when begin-move-func
		       (funcall begin-move-func))
		     (point)))
      (setq e (progn (when end-move-func
		       (goto-char start-point)
		       (funcall end-move-func))
		     (point)))
      (when mark-func
	(goto-char start-point)
	(funcall mark-func)
	(setq s (mark))
	(setq e (point))))
    (goto-char s)
    (push-mark e nil transient-mark-mode)
    ))

(defmacro generic-range-opt (name begin end &optional mark)
  "generate range operations. mark, delete, kill"
  (fset (intern (concat "gro-mark-" name))
	`(lambda () (interactive) (generic-range-opt-mark-base-function ,begin ,end ,mark)))
  (fset (intern (concat "gro-delete-" name))
	`(lambda () (interactive) (generic-range-opt-delete-base-function ,begin ,end ,mark)))
  (fset (intern (concat "gro-copy-" name))
	`(lambda () (interactive) (generic-range-opt-copy-base-function ,begin ,end ,mark)))
  (fset (intern (concat "gro-kill-" name))
	`(lambda () (interactive) (generic-range-opt-kill-base-function ,begin ,end ,mark)))
  )

;; generate range operation.
(generic-range-opt "forward-word" nil 'forward-word)
(generic-range-opt "backward-word" nil 'backward-word)
(generic-range-opt "forward-line" nil 'end-of-line)
(generic-range-opt "backward-line" nil 'beginning-of-line)
(generic-range-opt "search-forward-char" nil
		   '(lambda () (re-search-forward (char-to-string (read-char "search forward:")))))
(generic-range-opt "search-bacckward-char" nil
		   '(lambda () (re-search-backward (char-to-string (read-char "search backward:")))))
(generic-range-opt "follow-word" nil nil 'mark-word*)
(generic-range-opt "follow-sexp" nil nil 'mark-sexp*)
(generic-range-opt "follow-string" nil nil 'mark-string)
(generic-range-opt "current-line" nil nil 'mark-line)
(generic-range-opt "defun*" nil nil 'mark-defun*)
(generic-range-opt "prev-line" nil nil '(lambda () (previous-line) (mark-line)))
(generic-range-opt "next-line" nil nil '(lambda () (next-line) (mark-line)))
(generic-range-opt "jaunte-prev" nil nil '(lambda () (jaunte) (backward-char)))

(provide 'generic-range-opt)
