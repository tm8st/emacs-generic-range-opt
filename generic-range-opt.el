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

(defun generic-range-opt-base-function (range-opt-runction begin-move-func &optional end-move-func &optional mark-func)
  (interactive)
  (save-excursion
    (if mark-active
				(funcall range-opt-runction (point) (mark))
      (let* ((start-point (point))
						 (s (cond
								 (begin-move-func (funcall begin-move-func) (point))
								 (t start-point)))
						 (e (cond
								 (end-move-func (goto-char start-point) (funcall end-move-func) (point))
								 (t start-point))))
				(when mark-func
					(goto-char start-point)
					(funcall mark-func)
					(setq e (point))
					(setq s (mark)))
				(funcall range-opt-runction s e)))))

(defun generic-range-opt-mark-base-function (begin-move-func &optional end-move-func &optional mark-func)
  (interactive)
  (let* ((start-point (point))
         (prev-mark (mark))
				 (s (cond
						 (begin-move-func (funcall begin-move-func) (point))
						 (t start-point)))
				 (e (cond
						 (end-move-func (goto-char start-point) (funcall end-move-func) (point))
						 (t start-point))))
    (when mark-func
      (goto-char start-point)
      (funcall mark-func)
      (setq e (point))
      (setq s (mark)))
    ;; IF worked mark function, affect to current point and mark.
    (and (numberp (mark))
         (or 
          (/= (mark) prev-mark)
          (/= s start-point)
          (progn 
            (push-mark e t transient-mark-mode)
            (goto-char s)
            )))))
  
(defmacro generic-range-opt (name begin end &optional mark)
  "generate range operations. mark, delete, kill"
  (fset (intern (concat "gro-mark-" name))
	`(lambda () (interactive) (generic-range-opt-mark-base-function ,begin ,end ,mark)))
  (fset (intern (concat "gro-delete-" name))
	`(lambda () (interactive) (generic-range-opt-base-function 'delete-region ,begin ,end ,mark)))
  (fset (intern (concat "gro-copy-" name))
	`(lambda () (interactive) (generic-range-opt-base-function 'copy-region-as-kill ,begin ,end ,mark)))
  (fset (intern (concat "gro-kill-" name))
	`(lambda () (interactive) (generic-range-opt-base-function 'kill-region ,begin ,end ,mark)))
  )

;; generate range operation.
(generic-range-opt "forward-word" nil 'forward-word)
(generic-range-opt "backward-word" nil 'backward-word)
(generic-range-opt "forward-line" nil 'end-of-line)
(generic-range-opt "backward-line" nil 'beginning-of-line)
(generic-range-opt "search-forward-char" nil
		   '(lambda () (re-search-forward (char-to-string (read-char "search forward:")))))
(generic-range-opt "search-backward-char" nil
		   '(lambda () (re-search-backward (char-to-string (read-char "search backward:")))))
(generic-range-opt "follow-word" nil nil 'mark-word*)
(generic-range-opt "follow-sexp" nil nil 'mark-sexp*)
(generic-range-opt "follow-string" nil nil 'mark-string)
(generic-range-opt "current-line" nil nil 'mark-line)
(generic-range-opt "defun*" nil nil 'mark-defun*)
(generic-range-opt "prev-line" nil nil '(lambda () (previous-line) (mark-line)))
(generic-range-opt "next-line" nil nil '(lambda () (next-line) (mark-line)))
(generic-range-opt "goto-line" nil '(lambda () (goto-line (read-number "line number?:")) (end-of-line)))

(generic-range-opt "match-brace" nil nil '(lambda () (gro-match-paren "{" "}" 0)))
(generic-range-opt "match-paren" nil nil '(lambda () (gro-match-paren "(" ")" 0)))
(generic-range-opt "match-brace-1" nil nil '(lambda () (gro-match-paren "{" "}" 1)))
(generic-range-opt "match-paren-1" nil nil '(lambda () (gro-match-paren "(" ")" 1)))
(generic-range-opt "string-1" nil nil '(lambda () (gro-match-paren "\"" "\"" 0)))
(generic-range-opt "char-1" nil nil '(lambda () (gro-match-paren "'" "'" 0)))

(when (require 'jaunte nil t)
  (generic-range-opt "jaunte-prev" nil nil '(lambda () (jaunte) (backward-char)))
  (generic-range-opt "between-jaunte" 'jaunte 'jaunte))

(when (require 'yafastnav nil t)
  (generic-range-opt "yafastnav-prev" nil nil '(lambda () (yafastnav-jump-to-forward) (backward-char)))
  (generic-range-opt "between-yafastnav" 'yafastnav-jump-to-backward 'yafastnav-jump-to-forward))

(defun gro-match-paren (paren close-paren offset)
  ""
  (interactive)
  (let ((b) (e))
    (save-excursion
      (setq b (gro-paren-search-backward-inner paren close-paren 0)))
    (save-excursion
      (setq e (gro-paren-search-forward-inner paren close-paren 0 b)))
    (when (and (numberp b)
               (numberp e))
      (progn
        (goto-char (- (+ e offset) 1))
        (push-mark (+ 1 (- b offset)) t t)
        ))))

(defun gro-paren-search-backward-inner (paren close-paren depth)
  "search backward open paren position, return position or nil."
  (let ((bExit nil)
        (depth 0)
        (ret-pos nil)
        )
    (while (eq bExit nil)
      (if (numberp (re-search-backward (concat "[" paren close-paren "]") nil t))
          (cond ((looking-at paren) (if (<= depth 0)
                                        (progn
                                          (setq ret-pos (point))
                                          (setq bExit t))
                                      (setq depth (- depth 1))))
                ((looking-at close-paren) (setq depth (+ depth 1)))
                (t nil))
        (setq bExit t)))
    ret-pos))

(defun gro-paren-search-forward-inner (paren close-paren depth start)
  "search forward close paren position, return position or nil."
  (if (eq start nil)
      nil
    (progn
      (goto-char (+ 1 start))
      (let ((bExit nil)
            (depth 0)
            (ret-pos nil)
            )
        (while (eq bExit nil)
          (if (numberp (re-search-forward (concat "[" paren close-paren "]") nil t))
              (cond ((looking-back close-paren) (if (<= depth 0)
                                                  (progn 
                                                    (setq ret-pos (point))
                                                    (setq bExit t))
                                                (setq depth (- depth 1))))
                    ((looking-back paren) (setq depth (+ depth 1)))
                    (t nil))
            (setq bExit t)))
        ret-pos))))

(provide 'generic-range-opt)
