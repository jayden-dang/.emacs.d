;;; maple-iedit.el ---  maple iedit configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Package-Requires: (iedit)
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
;; maple iedit configuration.
;;

;;; Code:
(require 'iedit)
(require 'maple-iedit-evil)
(require 'maple-iedit-expand)

(defgroup maple-iedit nil
  "Multi cursor with iedit."
  :group 'maple)

(defcustom maple-iedit-evil-keybind (featurep 'evil)
  "Whether define evil keybind."
  :type 'boolean
  :group 'maple-iedit)

(defcustom maple-iedit-ignore-case nil
  "Whether ignore case when match."
  :type 'boolean
  :group 'maple-iedit)

(defcustom maple-iedit-follow-here t
  "Whether match regexp follow last match."
  :type 'boolean
  :group 'maple-iedit)

(defun maple-iedit-point()
  "Get current point."
  (if (use-region-p)
      (progn (deactivate-mark t) (cons (region-beginning) (region-end)))
    (let ((_ (iedit-default-occurrence))
          (point (bounds-of-thing-at-point iedit-occurrence-type-local)))
      (unless point (error "Failed to create marker")) point)))

(defun maple-iedit-match(&optional regexp beg end)
  "Match single occurrence REGEXP &OPTIONAL BEG END."
  (let* ((point  (maple-iedit-point))
         (regexp (or regexp (buffer-substring-no-properties (car point) (cdr point))))
         (regexp (regexp-quote regexp)))
    (save-excursion
      (setq iedit-initial-string-local regexp)
      (iedit-start regexp (or beg (car point)) (or end (cdr point))))))

(defun maple-iedit-match-here()
  "Match here occurrence."
  (interactive)
  (cond (iedit-mode
         (let* ((point (maple-iedit-point))
                (beg   (car point))
                (end   (cdr point))
                (ovpoint beg))
           (while (and ovpoint (<= ovpoint end))
             (if (not (iedit-find-overlay-at-point ovpoint 'iedit-occurrence-overlay-name))
                 (setq ovpoint (+ ovpoint 1))
               (save-excursion (goto-char ovpoint) (iedit-toggle-selection))
               (setq ovpoint nil)))
           (when maple-iedit-follow-here
             (setq iedit-initial-string-local (buffer-substring-no-properties beg end)))
           (push (iedit-make-occurrence-overlay beg end) iedit-occurrences-overlays)
           (goto-char beg)))
        (t (maple-iedit-match))))

(defun maple-iedit-match-all()
  "Match all occurrence."
  (interactive)
  (if (and iedit-mode iedit-initial-string-local)
      (maple-iedit-match iedit-initial-string-local (point-min) (point-max))
    (maple-iedit-match nil (point-min) (point-max))))

(defun maple-iedit-match-next(&optional forward)
  "Match next occurrence with FORWARD."
  (interactive)
  (cond (iedit-mode
         (let ((ov (iedit-find-current-occurrence-overlay))
               (case-fold-search maple-iedit-ignore-case)
               beg end)
           (when ov (goto-char (+ (if forward (overlay-start ov) (overlay-end ov))
                                  (if forward -1 1))))
           (unless (re-search-forward iedit-initial-string-local nil t (if forward -1 1))
             (goto-char (if forward (point-max) (point-min)))
             (re-search-forward iedit-initial-string-local nil nil (if forward -1 1)))
           (setq beg (match-beginning 0))
           (setq end (match-end 0))
           (unless (or (iedit-find-overlay-at-point beg 'iedit-occurrence-overlay-name)
                       (iedit-find-overlay-at-point end 'iedit-occurrence-overlay-name))
             (push (iedit-make-occurrence-overlay beg end) iedit-occurrences-overlays))
           (goto-char beg)))
        (t (maple-iedit-match))))

(defun maple-iedit-match-previous()
  "Match previous occurrence."
  (interactive)
  (maple-iedit-match-next t))

(defun maple-iedit-skip-and-match-next()
  "Skip current and match next occurrence."
  (interactive)
  (let* ((ov    (iedit-find-current-occurrence-overlay))
         (point (when ov (+ (overlay-end ov) 1))))
    (iedit-toggle-selection)
    (when point (goto-char point))
    (maple-iedit-match-next)))

(defun maple-iedit-skip-and-match-previous()
  "Skip current and match previous occurrence."
  (interactive)
  (let* ((ov    (iedit-find-current-occurrence-overlay))
         (point (when ov (+ (overlay-start ov) -1))))
    (iedit-toggle-selection)
    (when point (goto-char point))
    (maple-iedit-match-previous)))

(defun maple-iedit-move-beginning-of-line()
  "Move-beginning-of-line within maple-iedit."
  (interactive)
  (maple-iedit-with (if ov (goto-char (overlay-start ov))
                      (call-interactively 'move-beginning-of-line))))

(defun maple-iedit-move-end-of-line()
  "Move-end-of-line within maple-iedit."
  (interactive)
  (maple-iedit-with (if ov (goto-char (overlay-end ov))
                      (call-interactively 'move-end-of-line))))

(defun maple-iedit-next()
  "Goto next occurrence."
  (interactive)
  (let (iedit-forward-success) (call-interactively 'iedit-next-occurrence)))

(defun maple-iedit-previous()
  "Goto previous occurrence."
  (interactive)
  (let (iedit-forward-success) (call-interactively 'iedit-prev-occurrence)))

(defun maple-iedit-abort()
  "Iedit Abort."
  (interactive)
  (iedit-done))

(defvar maple-iedit-mode-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map iedit-mode-keymap)
    (define-key map (kbd "<escape>") #'maple-iedit-abort)
    (define-key map (kbd "C-a") #'maple-iedit-move-beginning-of-line)
    (define-key map (kbd "C-e") #'maple-iedit-move-end-of-line)
    (define-key map (kbd "C-g") #'maple-iedit-abort)
    (define-key map (kbd "C-n") #'maple-iedit-match-next)
    (define-key map (kbd "C-p") #'maple-iedit-match-previous)
    (define-key map (kbd "C-t") #'maple-iedit-skip-and-match-next)
    map) "Keymap of command `maple-iedit-mode'.")

(defun maple-iedit-mode-on()
  "Turn on `maple-iedit-mode`."
  (maple-iedit-mode))

(defun maple-iedit-mode-off()
  "Turn off `maple-iedit-mode`."
  (maple-iedit-mode -1))

(define-minor-mode maple-iedit-mode
  "maple iedit mode"
  :group  'maple-iedit
  :keymap maple-iedit-mode-keymap)

(add-hook 'iedit-mode-hook 'maple-iedit-mode-on)
(add-hook 'iedit-mode-end-hook 'maple-iedit-mode-off)
(when maple-iedit-evil-keybind (maple-iedit-evil-keybinds))

(provide 'maple-iedit)
;;; maple-iedit.el ends here
