;;; indentinator.el --- Automatically indent code.   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Created: December 22, 2017
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (seq))
;; Keywords: convenience
;; URL: https://github.com/xendk/indentinator

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

;; Indentinator is yet another "auto-indent" mode. Se the README.md
;; for further documentation or read it online at
;; https://github.com/xendk/indentinator#indentinator

;;; Code:

;; Maybe advice save-buffer when indenting is running.

(require 'seq)
(require 'faces)

(defgroup indentinator nil
  "Indentinator configuration."
  :prefix "indentinator-"
  :group 'editing)

(defface indentinator-progress-face
  '((t :inherit font-lock-warning-face))
  "Face used for mode-line indicator when indent is currently running."
  :group 'indentinator)

(defcustom indentinator-initial-idle-time .01
  "Time to wait after buffer modifications before starting re-indentation."
  :type 'number
  :group 'indentinator)

(defcustom indentinator-idle-time .01
  "Idle time between re-indentation actions."
  :type 'number
  :group 'indentinator)

(defvar indentinator-idle-timer nil
  "Idle timer for processing re-indentation.")

(defvar indentinator-debug nil
  "Enables debugging output.")

(defvar-local indentinator-start-marker (make-marker)
  "Marks the start of the current indent run.")

(defvar-local indentinator-current-marker nil
  "The marker of the next indent action.")

(defvar-local indentinator-no-indent-count 0
  "Number of lines not changed.")

(defvar-local indentinator-aborted-markers (list)
  "Markers for `indentinator-current-marker' of aborted indents.")

(defvar-local indentinator-changed-markers (list)
  "Markers for text changes needing re-indenting.")

(defvar-local indentinator-indenting nil
  "Whether an indenting action is currently in progress.")

;;;###autoload
(defun indentinator-toggle-debug ()
  "Toggle debugging."
  (interactive)
  (setq indentinator-debug (not indentinator-debug)))

(define-minor-mode indentinator-mode nil
  :lighter (:eval (if indentinator-idle-timer
                      (propertize " ->" 'face 'indentinator-progress-face)
                    " ->"))
  (if indentinator-mode
      (add-hook 'after-change-functions 'indentinator-after-change-function t t)
    (remove-hook 'after-change-functions 'indentinator-after-change-function t)))

(defmacro indentinator-queue-timer (&optional init)
  "Add idle-timer. Set INIT to t if this is the initial call."
  `(progn
     (unless indentinator-idle-timer
       (setq indentinator-idle-timer
             (run-with-idle-timer
              (if ,init indentinator-initial-idle-time
                (time-add (current-idle-time) indentinator-idle-time))
              nil
              'indentinator-idle-timer-function)))))

(defmacro indentinator-init-indent (start)
  "Initialize re-indenting from marker START."
  `(progn
     (setq indentinator-start-marker ,start)
     (setq indentinator-current-marker (copy-marker indentinator-start-marker))
     (setq indentinator-no-indent-count 0)))

(defmacro indentinator-filter-markers (markers start stop)
  "Filter MARKERS for markers between START and STOP markers, inclusive."
  `(setq ,markers (seq-filter (lambda (marker)
                                (or (<= (marker-position marker) (marker-position ,start))
                                    (>= (marker-position marker) (marker-position ,stop))))
                              ,markers)))

(defmacro indentinator-abort-indent ()
  "Abort current indent, saving starte for resuming."
  ;; Add changed markers and current position to aborted list.
  `(setq indentinator-aborted-markers (cons indentinator-current-marker
                                            (nconc indentinator-changed-markers
                                                   indentinator-aborted-markers))
         indentinator-changed-markers (list)
         indentinator-current-marker nil))

(defun indentinator-after-change-function (start end _len)
  "Handles buffer change between START and END.

BEG and END mark the beginning and end of the change text.  _LEN
is ignored.

Schedules re-indentation of following text."
  (when (and (not indentinator-indenting)
             (not undo-in-progress)
             indentinator-mode)
    ;; Abort current re-indentation, if one is in progress.
    (when indentinator-current-marker (indentinator-abort-indent))

    (when indentinator-debug
      (message "indentinator: after-change %d %d" start end))

    (save-excursion
      ;; Start re-indenting from the start of the changed area.
      (goto-char start)

      ;; Unless the change is only whitespace, then re-indent from the
      ;; next line. This means manually indenting a line wont get
      ;; overridden by indentinator.
      (when (save-excursion
              (skip-chars-forward " \t" end)
              (>= (point) end))
        (forward-line 1))

      (setq indentinator-changed-markers (cons (point-marker)
                                               indentinator-changed-markers))
      ;; Schedule idle timer if there's anything to work on.
      (when (or indentinator-changed-markers indentinator-aborted-markers)
        (indentinator-queue-timer t)))))

(defun indentinator-idle-timer-function ()
  "Idle timer function for re-indenting text."
  (when indentinator-debug
    (message "indentinator: timer called"))
  (when indentinator-idle-timer
    ;; TODO: Is it necessary to cancel the timer? It's not repeating.
    (cancel-timer indentinator-idle-timer)
    (setq indentinator-idle-timer nil))

  ;; Don't indent if currently running another command (like
  ;; query-replace), simply re-schedule.
  (if this-command
      (indentinator-queue-timer)
    (progn
      ;; If not currently indenting, see if there's any changed regions.
      (when (and (not indentinator-current-marker)
                 indentinator-changed-markers)
        ;; Sort in buffer order.
        (setq indentinator-changed-markers
              (seq-sort (lambda (a b)
                          (< (marker-position a) (marker-position b)))
                        indentinator-changed-markers))
        (indentinator-init-indent (car indentinator-changed-markers))
        (setq indentinator-changed-markers (cdr indentinator-changed-markers))
        (when indentinator-debug
          (message "indentinator: grabbing change %d"
                   (marker-position indentinator-current-marker))))
      ;; ...or aborted indents pendning.
      (when (and (not indentinator-current-marker)
                 indentinator-aborted-markers)
        (indentinator-init-indent (car indentinator-aborted-markers))
        (setq indentinator-aborted-markers (cdr indentinator-aborted-markers))
        (when indentinator-debug
          (message "indentinator: grabbing aborted %d"
                   (marker-position indentinator-current-marker))))

      (when indentinator-current-marker
        (let ((previous-current (copy-marker indentinator-current-marker)))

          ;; Indent one line.
          (setq indentinator-no-indent-count
                (if (indentinator-indent-one) 0 (1+ indentinator-no-indent-count)))
          ;; Move marker to next indent point.
          (save-excursion
            (goto-char (marker-position indentinator-current-marker))
            (forward-line)
            (set-marker indentinator-current-marker (point)))
          (if (and
               ;; Only run if we moving the marker and not at end of buffer.
               (not (equal indentinator-current-marker previous-current))
               ;; And there's not been more than 2 unchanged lines since
               ;; last indent.
               (>= 2 indentinator-no-indent-count))
              (if (input-pending-p)
                  ;; Pending input, abort.
                  (indentinator-abort-indent)
                ;; Schedule next indent.
                (indentinator-queue-timer))
            (progn
              ;; Filter out changed and aborted markers we'd accidentally
              ;; processed in the last indent.
              (indentinator-filter-markers indentinator-changed-markers
                                           indentinator-start-marker
                                           indentinator-current-marker)
              (indentinator-filter-markers indentinator-aborted-markers
                                           indentinator-start-marker
                                           indentinator-current-marker)
              (setq indentinator-current-marker nil)
              ;; Schedule re-indent if there's any queued.
              (when (or indentinator-changed-markers indentinator-aborted-markers)
                (indentinator-queue-timer)))))))))

(defun indentinator-indent-one ()
  "Re-indent one line at `indentinator-current-marker'.

Return whether the line changed."
  (save-excursion
    (let ((tick (buffer-chars-modified-tick))
          (indentinator-indenting t))
      (goto-char (marker-position indentinator-current-marker))
      (when indentinator-debug
        (message "indentinator: indent %d" (marker-position indentinator-current-marker)))
      (let ((inhibit-message t))
        (ignore-errors (indent-region (line-beginning-position) (line-end-position))))
      (not (= tick (buffer-chars-modified-tick))))))

(provide 'indentinator)
;;; indentinator.el ends here
