;;; maple-iedit-evil.el ---  maple iedit evil configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-iedit

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; maple iedit evil configuration.
;;

;;; Code:
(defmacro maple-iedit-with(&rest body)
  "Run BODY within maple iedit."
  (declare (indent 0) (doc-string 2))
  `(let ((ov (iedit-find-current-occurrence-overlay))) ,@body))

(defun maple-iedit-evil-insert-line ()
  "Replace evil-insert-line within maple-iedit."
  (interactive)
  (maple-iedit-with
    (if ov (progn (goto-char (overlay-start ov))
                  (evil-insert-state))
      (call-interactively 'evil-insert-line)) ))

(defun maple-iedit-evil-append-line ()
  "Replace evil-append-line within maple-iedit."
  (interactive)
  (maple-iedit-with
    (if ov (progn (goto-char (overlay-end ov))
                  (evil-insert-state))
      (call-interactively 'evil-append-line))))

(defun maple-iedit-evil-end-of-line ()
  "Replace evil-end-of-line within maple-iedit."
  (interactive)
  (maple-iedit-with
    (if ov (goto-char (overlay-end ov))
      (call-interactively 'evil-end-of-line))))

(defun maple-iedit-evil-beginning-of-line ()
  "Replace evil-beginning-of-line within maple-iedit."
  (interactive)
  (maple-iedit-with
    (if ov (goto-char (overlay-start ov))
      (call-interactively 'evil-beginning-of-line))))

(defun maple-iedit-evil-paste-replace ()
  "Replace the selection with the yanked text."
  (interactive)
  (maple-iedit-with
    (if ov (progn (iedit-delete-occurrences)
                  (call-interactively 'maple-iedit-evil-paste-after))
      (call-interactively 'evil-paste-before))))

(defun maple-iedit-evil-paste-before (count)
  "Paste the string like evil-paste-before with COUNT."
  (interactive "P")
  (evil-insert-state)
  (let ((text (car kill-ring)))
    (dotimes (_ (or count 1))
      (insert text)))
  (evil-normal-state))

(defun maple-iedit-evil-paste-after (count)
  "Paste the string like evil-paste-after with COUNT."
  (interactive "P")
  (evil-insert-state)
  (goto-char (+ 1 (point)))
  (let ((text (car kill-ring)))
    (dotimes (_ (or count 1))
      (insert text)))
  (evil-normal-state))

(defun maple-iedit-evil-find-char-to ()
  "Replace evil-find-char-to within maple-iedit."
  (interactive)
  (maple-iedit-with
    (if ov (maple-iedit-skip-and-match-next)
      (call-interactively 'evil-find-char-to))))

(defun maple-iedit-evil-find-char-to-backward ()
  "Replace evil-find-char-to-backward within maple-iedit."
  (interactive)
  (maple-iedit-with
    (if ov (maple-iedit-skip-and-match-previous)
      (call-interactively 'evil-find-char-to-backward))))

(defun maple-iedit-hide-unmatch-lines()
  "Show or hide unmatched lines."
  (interactive)
  (iedit-show/hide-context-lines))

(defun maple-iedit-evil-keybinds ()
  "Set up the evil keybindings for `maple-iedit`."
  (evil-define-minor-mode-key 'normal 'maple-iedit-mode
    (kbd "$")        #'maple-iedit-evil-end-of-line
    (kbd "^")        #'maple-iedit-evil-beginning-of-line
    (kbd "n")        #'maple-iedit-next
    (kbd "N")        #'maple-iedit-previous
    (kbd "t")        #'maple-iedit-evil-find-char-to
    (kbd "T")        #'maple-iedit-evil-find-char-to-backward
    (kbd "p")        #'maple-iedit-evil-paste-after
    (kbd "P")        #'maple-iedit-evil-paste-before
    (kbd "I")        #'maple-iedit-evil-insert-line
    (kbd "A")        #'maple-iedit-evil-append-line
    (kbd "U")        #'iedit-upcase-occurrences
    (kbd "D")        #'iedit-delete-occurrences
    (kbd "gg")       #'iedit-goto-first-occurrence
    (kbd "za")       #'maple-iedit-hide-unmatch-lines
    (kbd "G")        #'iedit-goto-last-occurrence
    (kbd "<escape>") #'maple-iedit-abort
    (kbd "C-g")      #'maple-iedit-abort
    (kbd "C-n")      #'maple-iedit-match-next
    (kbd "C-p")      #'maple-iedit-match-previous
    (kbd "C-t")      #'maple-iedit-skip-and-match-next)

  (evil-define-minor-mode-key 'insert 'maple-iedit-mode
    (kbd "<escape>")   #'evil-normal-state))

(provide 'maple-iedit-evil)
;;; maple-iedit-evil.el ends here
