(provide 'jumplist)

(defvar jl-insert-marker-funcs
  '(
    "backward-paragraph"
    "backward-sexp"
    "beginning-of-buffer"
    "beginning-of-defun"
    "cscope-find-functions-calling-this-function"
    "cscope-find-global-definition"
    "cscope-find-this-symbol"
    "cscope-select-entry-other-window"
    "dired-advertised-find-file"
    "describe-function"
    "describe-variable"
    "erl-find-source-under-point"
    "etags-select-find-tag-at-point"
    "etags-select-find-tag"
    "etags-select-goto-tag"
    "end-of-buffer"
    "end-of-defun"
    "find-file-at-point"
    "find-tag"
    "forward-paragraph"
    "forward-sexp"
    "highlight-symbol-next"
    "highlight-symbol-prev"
    "ibuffer-visit-buffer"
    ;; "ido-find-file"
    "ido-switch-buffer"
    ;; "iswitchb-buffer"
    "iswitchb-exit-minibuffer"
    "imenu"
    "isearch-backward"
    "isearch-forward"
    ;; "isearch-repeat-backward"
    ;; "isearch-repeat-forward"
    "jl-jump-backward"
    "jl-jump-forward"
    "jump-to-register"
    "mark-whole-buffer"
    "next-buffer"
    "pager-page-down"
    "pager-page-up"
    "previous-buffer"
    "rope-goto-definition"
    "save-buffer"
    "search-backward"
    "search-backward-regexp"
    "search-forward"
    "search-forward-regexp"
    "switch-to-buffer"
    "tabbar-backward-tab"
    "tabbar-forward-tab"
;;    "eval-last-sexp"
    )
  "A list of fuctions. When anyone in it is executed, a marker is set.")

(defvar jl-skip-funcs
  '("self-insert-command"
    "next-line"
    "previous-line"
    "backward-char"
    "forward-char"
    )
  "To improve the performance, this list will be scanned first.
   If the command excecuted is in this list, no mark will be set.")

(defvar jl-skip-buffer
  '("*cscope*"
    "*compilation*"
    )
  "Marker won't be set in these buffers.")
 
(defvar jl-max-marker-nr 50) ;; max number of markers
(defvar jl-marker-nr 0)      ;; nunber of existing markers
(defvar jl-cur-marker-pos 0) ;; current marker position
;;(defvar jl-marker-list (list (point-marker)))
(defvar jl-marker-list nil)

;; To check wheter the string str is in list list
(defun jl-is-in-list(list str)
    (catch 'match
      (while (car list)
	(progn (if (string= (car list) str)
		   (throw 'match 1))
	       (setq list (cdr list))))
      nil))

;; To check whether a marker is in marker list 
(defun jl-marker-is-in-list(marker)
  (let ((list jl-marker-list))
    (catch 'match
      (while (car list)
	(progn (if (equal (car list) marker)
		   (throw 'match 1))
	       (setq list (cdr list))))
      nil)))

(defun jl-insert-marker()
  (let ((marker (point-marker)))
    (if (jl-marker-is-in-list marker)
	;; If marker is already in marker list, just put it to the head.
	;; Except for the following two commands 
	(progn (if (not (or (string= this-command "jl-jump-backward")
			    (string= this-command "jl-jump-forward")))
		   (progn (add-to-list 'jl-marker-list 
				       (pop (nthcdr jl-cur-marker-pos
				    jl-marker-list)))
			  (setq jl-cur-marker-pos 0))))
      ;; If the list is full, pop the last one.
      (progn (if (> (safe-length jl-marker-list) jl-max-marker-nr)
		 (pop (nthcdr (- (safe-length jl-marker-list) 1) jl-marker-list)))
	     
	     (progn (if (not (= jl-cur-marker-pos 0)) ;; move the current marker to the begining
			(progn (add-to-list 'jl-marker-list 
					    (pop (nthcdr jl-cur-marker-pos
							 jl-marker-list)))))
		    (add-to-list 'jl-marker-list marker)
		    (setq jl-cur-marker-pos 0))))))
		      
		      
(defun jl-jump-to-marker(marker)
  (if (not (eq (current-buffer) (marker-buffer marker)))
      (switch-to-buffer (marker-buffer marker)))
  (goto-char (marker-position marker)))
  
(defun jl-check-and-set-pos-range ()
  (if (>= jl-cur-marker-pos (safe-length jl-marker-list))
      (setq jl-marker-list (- (safe-length jl-marker-list) 1))))
  

(defun jl-jump-backward()
  (interactive)

  ;; Make sure the jl-cur-marker-pos is in proper range
  (jl-check-and-set-pos-range)

  (let ((marker (nth jl-cur-marker-pos jl-marker-list)))

    ;; We must handle the condition where buffer containing the marker is deleted, 
    (while (and (equal nil (marker-buffer marker))
		(> (safe-length jl-marker-list) 0))
      (progn (pop (nthcdr jl-cur-marker-pos jl-marker-list))
	     (jl-check-and-set-pos-range)
	     (setq marker (nth jl-cur-marker-pos jl-marker-list))))

     
    (if (or (not (equal (point) (marker-position marker)))
	    (not (equal (current-buffer) (marker-buffer marker))))
	(jl-jump-to-marker marker) ;; jump to the nearest place first

      (progn (if (< jl-cur-marker-pos (- (safe-length jl-marker-list) 1))
		 (setq jl-cur-marker-pos 
		       (+ jl-cur-marker-pos 1)))
	     (setq marker (nth jl-cur-marker-pos jl-marker-list))
	     (jl-jump-to-marker marker)))))
	       
    
  
(defun jl-jump-forward()
  (interactive)  

  ;; Make sure the jl-cur-marker-pos is in proper range
  (jl-check-and-set-pos-range)

  (let ((marker (nth jl-cur-marker-pos jl-marker-list)))

    ;; We must handle the condition where buffer containing the marker is deleted, 
    (while (and (equal nil (marker-buffer marker))
		(> (safe-length jl-marker-list) 0))
      (progn (pop (nthcdr jl-cur-marker-pos jl-marker-list))
	     (if (> (safe-length jl-marker-list) 9)
		 (setq jl-cur-marker-pos (- jl-cur-marker-pos 1)))
	     (setq marker (nth jl-cur-marker-pos jl-marker-list))))

    (if (or (not (equal (point) (marker-position marker)))
	    (not (equal (current-buffer) (marker-buffer marker))))
	(jl-jump-to-marker marker)

      (progn (if (> jl-cur-marker-pos 0)
		 (setq jl-cur-marker-pos 
		     (- jl-cur-marker-pos 1)))
	     (setq marker (nth jl-cur-marker-pos jl-marker-list))
	     (jl-jump-to-marker marker)))))
	
    
    


(defun jl-pre-command-check()
  ;; (message "func: %s" this-command)
  (if (and (symbolp this-command)  ; for emacs 24
       (not (jl-is-in-list jl-skip-funcs this-command))
	   (not (jl-is-in-list jl-skip-buffer (buffer-name))))
      (if (jl-is-in-list jl-insert-marker-funcs this-command)
	  (progn ;(message "jl in list: %s" this-command)
	    (jl-insert-marker)))))

(add-hook 'pre-command-hook 'jl-pre-command-check)


