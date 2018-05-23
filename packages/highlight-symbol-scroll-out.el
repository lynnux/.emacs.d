;;; highlight-symbol-scroll-out.el --- continue highlight symbol when scroll out of view port  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  lynnux

;; Author: lynnux
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; jump to previous highlighted symbol when scroll out of current view port
;; please set `scroll-margin' to 0

;; TODO: support `auto-highlight-symbol'
;;; Code:

(require 'highlight-symbol)
(defvar previous-window-pos nil
  "save the lastest window-start position, used to judge the direction of `re-search-forward'")
(make-variable-buffer-local 'previous-window-pos)

(defvar is-srcoll-out-of-viewport nil
  "flag when scrolled out of view port")
(make-variable-buffer-local 'is-srcoll-out-of-viewport)

(defvar previous-highlighted-symbol nil
  "previous highlighted symbol")
(make-variable-buffer-local 'previous-highlighted-symbol)

(defun highlight-symbol-jump-in-viewport (&optional symbol dir)
  (let ((symbol (or symbol (highlight-symbol-get-symbol)))
	(dir (or dir (if previous-window-pos
			 (if (> (window-start) previous-window-pos)
			     1 -1)
		       1))))	; 1 means search down
    (setq previous-window-pos (window-start))
    (if symbol
        (let* ((case-fold-search nil))
          (let ((target (re-search-forward symbol (if (< 0 dir) (window-end) (window-start))
					   t dir))) ; only search in view port
	    (if target
		(progn 
		  (goto-char target)
		  (highlight-symbol-temp-highlight) ;; fix bug for not highlight when first jump
		  t)
	      nil)))
      nil)))

(defun window-scrolled (window display-start)
  (when (= 0 (current-column))
    ;; we can't call goto-char here! see the tip in `window-scroll-functions'
    (setq is-srcoll-out-of-viewport t)))

(defun goto-no-symbol-position ()
  (while (highlight-symbol-get-symbol)
    (goto-char (+ 1 (point))))
  )

(defcustom highlight-symbol-scroll-out-timer-delay 0.1
  ""
  :type 'number
  :group 'highlight-symbol-scroll-out
)
(defvar highlight-symbol-scroll-out-timer nil)
(defun highlight-symbol-scroll-out-timer-function ()
  (when is-srcoll-out-of-viewport 
    (when previous-highlighted-symbol
      (unless (highlight-symbol-jump-in-viewport previous-highlighted-symbol)
	(goto-no-symbol-position)))
    (setq is-srcoll-out-of-viewport nil)))

(defvar highlight-symbol-scroll-out-mode nil)
(define-minor-mode highlight-symbol-scroll-out-mode
  ""
  :lighter nil
  (if highlight-symbol-scroll-out-mode
      ;; on
      (progn
	(setq highlight-symbol-scroll-out-timer (run-with-idle-timer highlight-symbol-scroll-out-timer-delay t 'highlight-symbol-scroll-out-timer-function))
        (add-hook 'window-scroll-functions 'window-scrolled nil t)
	(defadvice highlight-symbol-temp-highlight (around get-latest-highlight-symbol activate)
	  ad-do-it
	  (let ((symbol (highlight-symbol-get-symbol)))
	    (when (and symbol (not is-srcoll-out-of-viewport))
	      (setq previous-highlighted-symbol symbol))))
	)
    ;; off
    (when highlight-symbol-scroll-out-timer
      (cancel-timer highlight-symbol-scroll-out-timer)
      (setq highlight-symbol-scroll-out-timer nil))
    (remove-hook 'window-scroll-functions 'window-scrolled t)
    (defadvice highlight-symbol-temp-highlight (around get-latest-highlight-symbol disable))
    ))

(define-globalized-minor-mode global-highlight-symbol-scroll-out-mode highlight-symbol-scroll-out-mode
  (lambda () (highlight-symbol-scroll-out-mode)))

(provide 'highlight-symbol-scroll-out)
;;; highlight-symbol-scroll-out.el ends here
