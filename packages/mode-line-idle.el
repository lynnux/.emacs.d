;;; mode-line-idle.el --- Evaluate mode line content when idle -*- lexical-binding:t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-mode-line-idle
;; Version: 0.4
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Simple delayed text evaluation for the mode-line.

;;; Usage:

;; (defvar my-word '(:eval (count-words (point-min) (point-max))))
;; (setq-default mode-line-format
;;   (list "Word Count " '(:eval (mode-line-idle 1.0 my-word "?" :interrupt t))))
;;

;;; Code:

;; ---------------------------------------------------------------------------
;; Generic Utility Functions

(defun mode-line-idle--tree-to-string (tree)
  "Convert TREE recursively to a string.
Behavior matches `mode-line-format', see its doc-string for details."

  ;; Developers note:
  ;; TREE can be one of the following:
  ;; - lists with `car' `:eval'
  ;;   - The `cdr' is evaluated and the result
  ;;     is passed to `mode-line-idle--tree-to-string'
  ;; - Lists with `car' `:propertize'
  ;;   - The `caar' is passed to `mode-line-idle--tree-to-string'.
  ;;   - The `cddr' is passed to as properties to `propertize'.
  ;; - Lists with `car' is an integer,
  ;;   used to pad or truncate (when negative).
  ;; - Other lists: element-wise processed with `mode-line-idle--tree-to-string'
  ;; - A string is passed through (with any associated properties).
  ;; - A nil value is treated as an empty string.
  ;; - A symbol, its value will be passed to `mode-line-idle--tree-to-string'.
  ;; - Any other element is converted into a string using `prin1-to-string'.

  (declare (important-return-value t))
  ;; NOTE: can't be `side-effect-free' because of `eval'.
  (cond
   ((stringp tree)
    ;; Check for strings as falling back to `prin1-to-string' removes any properties
    ;; that may have been set on the string, see bug #2.
    tree)
   ((null tree)
    "")
   ((symbolp tree)
    ;; Support non-string symbols, allows numbers etc to be included.
    (mode-line-idle--tree-to-string (symbol-value tree)))
   ((listp tree)
    (let ((tree-type (car-safe tree)))
      (cond
       ((eq tree-type :eval)
        (mode-line-idle--tree-to-string (eval (cons 'progn (cdr tree)) t)))
       ((eq tree-type :propertize)
        (pcase-let ((`(,item . ,rest) (cdr tree)))
          (apply #'propertize (mode-line-idle--tree-to-string item) rest)))
       ;; Support cons-cell (integer rest), where the integer pads/truncates the string length.
       ((integerp tree-type)
        (let* ((value (mode-line-idle--tree-to-string (cdr tree)))
               (value-len (length value)))
          (cond
           ;; Negative (maybe truncate).
           ((< tree-type 0)
            (let ((value-truncate (- tree-type)))
              (when (> value-len value-truncate)
                (setq value (substring value 0 value-truncate)))))
           ;; Zero or positive (maybe pad).
           (t
            (when (< value-len tree-type)
              (setq value (concat value (make-string (- tree-type value-len) ?\s))))))
          value))
       (t
        (mapconcat #'mode-line-idle--tree-to-string tree "")))))
   (t
    ;; For convenience, support other values being coerced into strings.
    (prin1-to-string tree t))))


;; ---------------------------------------------------------------------------
;; Internal Variables

;; One off timers.
(defvar-local mode-line-idle--timers nil)
;; Cache evaluated expressions.
(defvar-local mode-line-idle--values nil)
;; Prevent timer creation when running the timer callback.
;; Never set as it only exists to be shadowed.
(defvar mode-line-idle--timer-lock nil)


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun mode-line-idle--update-and-redisplay ()
  "Refresh the mode-line after refreshing its contents."
  (declare (important-return-value nil))
  (force-mode-line-update)
  ;; Prevent `mode-line-idle' from starting new idle timers
  ;; since it can cause continuous updates.
  (let ((mode-line-idle--timer-lock t))
    (redisplay t)))

(defun mode-line-idle--timer-callback-impl (item force)
  "Calculate all values for ITEM.
When FORCE is non-nil, don't check for interruption and don't re-display.

Return non-nil when any values were calculated."
  (declare (important-return-value nil))
  (let ((found nil)
        (has-input nil)
        (interrupt-args (list))
        (value-error "<ERROR>"))
    (pcase-dolist (`(,content . ,keywords) (cdr item))
      ;; Arguments which may be set from `keywords'.
      (let ((kw-interrupt nil)
            (kw-literal nil))

        ;; Extract keyword argument pairs.
        (let ((kw-iter keywords))
          (while kw-iter
            (let ((key (car kw-iter)))
              (setq kw-iter (cdr kw-iter))
              (cond
               ((null kw-iter)
                (message "mode-line-idle: keyword %S has no value" key))
               ((eq key :interrupt)
                (setq kw-interrupt (car kw-iter)))
               ((eq key :literal)
                (setq kw-literal (car kw-iter)))
               (t
                (message "mode-line-idle: unknown keyword %S" key)))
              (setq kw-iter (cdr kw-iter)))))

        (when force
          (setq kw-interrupt nil))

        ;; Replace the previous value, if it exists.
        (let ((value nil))
          (cond

           ;; Execute with support for interruption.
           (kw-interrupt
            (unless has-input
              (while-no-input
                ;; Wrap in condition-case so an error in one mode-line
                ;; component does not break others.
                (setq value
                      (condition-case-unless-debug err
                          (mode-line-idle--tree-to-string content)
                        (error
                         (message "mode-line-idle: %s" (error-message-string err))
                         ;; Verbose since mode-line functions should never raise errors.
                         value-error))))
              (unless value
                (setq has-input t)))

            ;; Execution was interrupted, re-run later.
            (unless value
              (let ((default-text (alist-get content mode-line-idle--values)))
                ;; Build a list with cons, add it to `interrupt-args'
                (push
                 (cons (car item) (cons content (cons default-text keywords))) interrupt-args))))

           ;; Default execution.
           (t
            (setq value
                  (condition-case-unless-debug err
                      (mode-line-idle--tree-to-string content)
                    (error
                     (message "mode-line-idle: %s" (error-message-string err))
                     ;; Verbose since mode-line functions should never raise errors.
                     value-error)))))

          ;; May be nil when interrupted.
          (when value

            ;; Prevent `mode-line-format' from interpreting `%'.
            (when kw-literal
              (setq value (string-replace "%" "%%" value)))

            ;; Remove and add `content' at the head of the a-list.
            (setq mode-line-idle--values
                  (cons (cons content value) (assq-delete-all content mode-line-idle--values)))
            (setq found t)))))

    ;; When forcing the update, the caller is responsible for re-displaying.
    ;; This is done because forcing is used for forcibly running all timers,
    ;; so in this case it makes sense to re-display afterwards.
    (unless force
      (when found
        (mode-line-idle--update-and-redisplay))

      ;; Re-create interrupted timers.
      (dolist (args interrupt-args)
        (apply #'mode-line-idle args)))

    found))

(defun mode-line-idle--timer-callback (buf item)
  "Calculate all values in BUF for the timers associated with ITEM."
  (declare (important-return-value nil))
  ;; It's possible the buffer was removed since the timer started.
  ;; In this case there is nothing to do as the timer only runs once
  ;; and the variables are local.
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (mode-line-idle--timer-callback-impl item nil)
      ;; Remove this item.
      (setq mode-line-idle--timers (delq item mode-line-idle--timers)))))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun mode-line-idle (delay-in-seconds content default-text &rest keywords)
  "Delayed evaluation of CONTENT, delayed by DELAY-IN-SECONDS.

CONTENT should be a mode-line construct (e.g., a symbol or `:eval' form).
DEFAULT-TEXT is displayed until the idle timer fires and CONTENT is evaluated.

Argument KEYWORDS is a property list of optional keywords:

- `:interrupt' When non-nil, interrupt evaluation on keyboard input
  (use for long running actions).
- `:literal' When non-nil, replace `%' with `%%',
  to prevent `mode-line-format' from formatting these characters."
  (declare (important-return-value t))

  ;; Check if this is running within `mode-line-idle--timer-callback'.
  (unless mode-line-idle--timer-lock
    ;; Normalize to float before lookup so integer and float delays match.
    (unless (floatp delay-in-seconds)
      (setq delay-in-seconds (float delay-in-seconds)))
    (let ((item (assoc delay-in-seconds mode-line-idle--timers)))
      (unless item
        (setq item (cons delay-in-seconds (list)))
        ;; Since this is a one-off timer, no need to manage, fire and forget.
        (run-with-idle-timer delay-in-seconds nil #'mode-line-idle--timer-callback
                             (current-buffer)
                             item)
        (push item mode-line-idle--timers))

      ;; Add the symbol to the timer list.
      (let ((content-alist (cdr item)))
        ;; Paranoid check we don't add twice.
        (setq content-alist (assq-delete-all content content-alist))
        (push (cons content keywords) content-alist)
        (setcdr item content-alist))))

  ;; Return the cached value.
  (let ((value (alist-get content mode-line-idle--values)))
    (or value default-text)))

;;;###autoload
(defun mode-line-idle-force-update (&optional delay-in-seconds)
  "Calculate pending `mode-line-idle' entries immediately.
When DELAY-IN-SECONDS is nil, all pending values are calculated,
otherwise ignore pending timers over this time.
Return non-nil when the mode-line was updated."
  (declare (important-return-value t))
  (let ((found nil))
    (dolist (item mode-line-idle--timers)
      (when (or (null delay-in-seconds) (<= (car item) delay-in-seconds))
        (when (mode-line-idle--timer-callback-impl item t)
          (setq found t))
        ;; The idle timer isn't removed, nothing left to compute.
        ;; An alternative solution would be to cancel the idle timer but that means storing
        ;; the idle timer to support a fairly rare use-case.
        (setcdr item nil)))
    (when found
      (mode-line-idle--update-and-redisplay))
    found))


(provide 'mode-line-idle)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; mode-line-idle.el ends here
