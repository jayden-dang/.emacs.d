;;; maple-iedit-expand.el ---  maple iedit expand configuration.	-*- lexical-binding: t -*-

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
;; maple iedit expand configuration.
;;

;;; Code:
(require 'iedit)

(defun maple-iedit-forward-char-expand(count)
  "Expand `forward-char` with COUNT."
  (interactive "P")
  (let ((overlay (iedit-find-current-occurrence-overlay))
        (count   (or count 1))
        overlays)
    (when overlay
      (goto-char (- (overlay-end overlay) 1))
      (dolist (ov iedit-occurrences-overlays)
        (push (iedit-make-occurrence-overlay
               (overlay-start ov)
               (save-excursion
                 (goto-char (overlay-end ov))
                 (min (line-end-position) (+ (overlay-end ov) count))))
              overlays))
      (setq iedit-occurrences-overlays (reverse overlays))
      (forward-char count))))

(defun maple-iedit-forward-char-unexpand(count)
  "Unexpand `forward-char` with COUNT."
  (interactive "P")
  (let ((overlay (iedit-find-current-occurrence-overlay))
        (count   (or count 1)))
    (when overlay
      (goto-char (overlay-start overlay))
      (dolist (ov iedit-occurrences-overlays)
        (iedit-cleanup-occurrences-overlays (overlay-start ov) (+ (overlay-start ov) count) t))
      (forward-char count))))

(defun maple-iedit-backward-char-expand(count)
  "Expand `backward-char` with COUNT."
  (interactive "P")
  (let ((overlay (iedit-find-current-occurrence-overlay))
        (count   (or count 1))
        overlays)
    (when overlay
      (goto-char (overlay-start overlay))
      (dolist (ov iedit-occurrences-overlays)
        (push (iedit-make-occurrence-overlay
               (save-excursion
                 (goto-char (overlay-start ov))
                 (max (line-beginning-position) (- (overlay-start ov) count)))
               (overlay-end ov))
              overlays))
      (setq iedit-occurrences-overlays (reverse overlays))
      (backward-char count))))

(defun maple-iedit-backward-char-unexpand(count)
  "Unexpand `backward-char` with COUNT."
  (interactive "P")
  (let ((overlay (iedit-find-current-occurrence-overlay))
        (count   (or count 1)))
    (when overlay
      (goto-char (- (overlay-end overlay) 1))
      (dolist (ov iedit-occurrences-overlays)
        (iedit-cleanup-occurrences-overlays (- (overlay-end ov) count) (overlay-end ov) t))
      (backward-char count))))

(defun maple-iedit-beginning-of-line-expand()
  "Expand to `beginning-of-line`."
  (interactive)
  (let ((overlay (iedit-find-current-occurrence-overlay))
        overlays)
    (when overlay
      (dolist (ov iedit-occurrences-overlays)
        (push (iedit-make-occurrence-overlay
               (save-excursion
                 (goto-char (overlay-start ov))
                 (line-beginning-position))
               (overlay-end ov))
              overlays))
      (setq iedit-occurrences-overlays (reverse overlays))
      (move-beginning-of-line 1))))

(defun maple-iedit-end-of-line-expand()
  "Expand to `end-of-line`."
  (interactive)
  (let ((overlay (iedit-find-current-occurrence-overlay))
        overlays)
    (when overlay
      (dolist (ov iedit-occurrences-overlays)
        (push (iedit-make-occurrence-overlay
               (overlay-start ov)
               (save-excursion
                 (goto-char (overlay-end ov))
                 (line-end-position)))
              overlays))
      (setq iedit-occurrences-overlays (reverse overlays))
      (move-end-of-line 1))))

(provide 'maple-iedit-expand)
;;; maple-iedit-expand.el ends here
