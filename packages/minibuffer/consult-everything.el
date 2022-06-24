;;; consult-everything.el --- Everything integration with Consult -*- lexical-binding: t; -*-

;; Copyright (C) John Haman 2022
;; Author: John Haman <mail@johnhaman.org>
;; Homepage: https://github.com/jthaman/consult-everything
;; Package-Requires: ((emacs "25.1") (consult "0.15") (orderless "0.6"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides Everything integration with the Emacs program
;; `consult'. Download the Everything commandline tool, es.exe, from
;; https://www.voidtools.com/downloads/#cli and place the binary in your Path.
;; Everything is a useful `locate' alternative on Windows machines.

;;; TODO: do not depend on the orderless package.

;;; Code:

(require 'consult)
(require 'orderless)

(defcustom consult-everything-args
  "es -r"
  "Command line arguments for everything, see `consult-everything'."
  :type 'string)

(defun consult--everything (prompt builder initial)
  "Run everything command.
The function returns the selected file.
The filename at point is added to the future history.
BUILDER is the command builder.
PROMPT is the prompt.
INITIAL is initial input."
  (consult--read
   (consult--async-command builder
     (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
     (consult--async-highlight builder)
     :file-handler nil) ;; do not allow tramp
   :prompt prompt
   :sort nil
   :require-match t
   :initial (consult--async-split-initial initial)
   :add-history (consult--async-split-thingatpt 'filename)
   :category 'file
   :history '(:input consult--everything-history)))

(defvar consult--everything-regexp-type nil)

(defun consult--everything-regexp-type (cmd)
  "Return regexp type supported by es CMD."
  (or consult--everything-regexp-type
      (setq consult--everything-regexp-type
            (if (eq 0 (call-process-shell-command
                       (concat cmd " -regextype emacs -version")))
                'emacs 'basic))))

;; based on `consult--locate-builder'.

(defun consult--everything-builder (input)
  "Build command line given INPUT."
  (pcase-let* ((cmd (split-string-and-unquote consult-everything-args))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic
                                      (and (not (member "-i" cmd))
                                           (not (member "-case" cmd))))))
    (when re
      (list :command
            (append cmd (list (consult--join-regexps re 'orderless)) opts)
            :highlight hl))))

;;;###autoload
(defun consult-everything (&optional initial dir)
  "Search for files matching input regexp given INITIAL input."
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Everything" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--everything (car prompt-dir) #'consult--everything-builder initial))))

(provide 'consult-everything)

;;; consult-everything.el ends here
