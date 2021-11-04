;; Time-stamp: <2021-11-04 21:47:11 lynnux>
;; 说明：
;; 自带的lisp包设置等
;; 自带的不用加require，因为xxx-mode基本上都是autoload！
;; C-x f filecache everything recent-visit/changed find-file-at-point

(defun find-file-select (selected)
  (interactive
   (list (read-char "find in [f]ilecache/[e]verything/recent[v]isit/[c]hanged/[a]t point")))
  (cond ((eq selected ?f) 
	 (call-interactively 'file-cache-switch-file))
	((eq selected ?e) 
	 (call-interactively 'helm-locate))
	((eq selected ?c) 
	 (call-interactively 'files-recent-changed))
	((eq selected ?v) 
	 (call-interactively 'files-recent-visited))
	((eq selected ?a) 
	 (call-interactively 'find-file-at-point))
	;; (t (message "other"))
	))
(global-set-key (kbd "C-x f") 'find-file-select)

;; (recentf-mode 1) 用session代替了
(setq history-length 200)
(defun files-recent-type (src)
  (interactive)
  (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
			src))
	 (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file (cdr (assoc-string fname tocpl))))))
(defun files-recent-visited ()
  (interactive)
  (files-recent-type file-name-history))
(defun files-recent-changed () 
  (interactive) 
  ; 需要配合session.el使用
  (files-recent-type (mapcar (lambda (x) (car x)) session-file-alist)))

;;buffer管理，真的太好用了！
(global-set-key (kbd "C-x b") 'bs-show) ;这个更好
(with-eval-after-load 'bs 
  (setq bs-default-configuration "files-and-scratch")
  (define-key bs-mode-map "s"       'bs-show-sorted)
  (define-key bs-mode-map "S"       'bs-save)
  (define-key bs-mode-map "<"       'beginning-of-buffer)
  (define-key bs-mode-map ">"       'end-of-buffer))

;; org mode
(setq org-hide-leading-stars t); 只高亮显示最后一个代表层级的 *
(define-key global-map "\C-ca" 'org-agenda) ;C-c a 进入日程表
(setq org-log-done 'time) ;给已完成事项打上时间戳。可选 note，附加注释
(setq org-startup-folded nil) ; 打开时不折叠
(add-hook 'org-agenda-mode-hook 
	  (lambda ()
	    (setq org-agenda-follow-mode t))
	  )
(autoload 'org-remember-insinuate "org-remember" nil t)
(with-eval-after-load 'remember (org-remember-insinuate)) ;必须加'，否则直接执行
;; org-remember已经被remember代替(还是要结合org使用)
(define-key global-map "\C-cr" 'remember)
;; tabbar配置那里有对org的C-TAB的设置
(defvar website-org-path nil)
(defvar website-org-publish-path nil)
(add-hook 'org-mode-hook
	  (lambda()
	    (setq truncate-lines nil)
					;(define-key org-mode-map  [(control ?\,)] 'ska-point-to-register)
	    ;; 建站专用
	    ;; (require 'org-publish)
	    (when (and website-org-path website-org-publish-path)
	      (setq org-publish-project-alist
					;notes组件
		    `((			;; note ` instead of '
		       "org-notes"
		       :base-directory ,(format "%s" website-org-path) ;设置存放.org文件位置 
		       :base-extension "org" ;仅处理 .org 格式文件
		       :publishing-directory ,(format "%s" website-org-publish-path) ;导出html文件位置
		       :recursive t
		       :publishing-function org-publish-org-to-html
		       :headline-levels 4 ;Just the default for this project.
		       :auto-preamble t
		       :auto-sitemap t	;自动生成 sitemap.org
		       :sitemap-filename "sitemap.org" ;默认名称
		       :sitemap-title "SiteMap"
		       :export-creator-info nil ;禁止在 postamble 显示"Created by Org"
		       :export-author-info nil ;禁止在 postamble 显示 "Author: Your Name"
		       :auto-postamble nil         
		       :table-of-contents nil ;禁止生成文章目录，如果要生成，将 nil 改为 t
		       :section-numbers nil ;禁止在段落标题前使用数字，如果使用，将 nil 改为 t
		       :html-postamble html-last-updated ;自定义 postamble 显示字样
		       :style-include-default nil ;禁用默认 css 样式,使用自定义css
		       )
		      ;;static 组件
		      ("org-static"
		       :base-directory ,(format "%s" website-org-path)
		       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
		       :publishing-directory ,(format "%s" website-org-publish-path)
		       :recursive t
		       :publishing-function org-publish-attachment
		       )
		      ;;publish 组件
		      ("org" :components ("org-notes" "org-static"))
		      ))
	      (defun html-last-updated() 
		(concat "<div id=\"footer\">Last Updated: " (format-time-string "%Y-%m-%d %H:%M") ". contact: lynnux@qq.com</a></div> ")))
	    ))


;; cua mode line
(defun my-cua-mode-setting ()
  (setq cua-remap-control-z nil)	;原来是add-hook，所以设置不成功，eval-after-load会在加载cua文件后立即执行。elisp要加强啊！
  (defface cua-mode-mode-line-face
    '((((type tty pc)) :bold t :foreground "blue" :background "white")
      (t (:background "blue" :foreground "white")))
    "Face used highlight `cua-mode-line-format'.")
  (defvar cua-mode-line-format
    (propertize "CUA"
		;; 'local-map mode-line-minor-mode-keymap
		;; 'help-echo "mouse-3: minor mode menu"
		'face 'cua-mode-mode-line-face)
    "*Mode line format of `cua-mode'.")
  (put 'cua-mode-line-format 'risky-local-variable t)
  (setq minor-mode-alist
	(append
	 `((cua-mode " ") (cua-mode ,cua-mode-line-format)) ;前面一句(cua-mode " ")不是多余的，否则空格也会有蓝色的背景
	 (delq (assq 'cua-mode minor-mode-alist) minor-mode-alist) ))

  ;; shift + click select region，用shift+鼠标选中，需要开启CUA
  (define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
  (define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
  (put 'mouse-set-point 'CUA 'move)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  ;; (setq cua-keep-region-after-copy t) ;选中复制后保持选中状态
  )
;; )
(with-eval-after-load 'cua-base (my-cua-mode-setting))

(defvar view-mode-setted nil)
(defun view-mode-settings ()
  (unless view-mode-setted
    (setq view-mode-setted t)
    (define-key view-mode-map "a" 'move-beginning-of-line)
    (define-key view-mode-map "e" 'move-end-of-line)
    (define-key view-mode-map "m" 'back-to-indentation)
    (define-key view-mode-map "n" 'next-line)
    (define-key view-mode-map "p" 'previous-line)
    (define-key view-mode-map "g" 'lgrep)
    (define-key view-mode-map "1" 'delete-other-windows)
    (define-key view-mode-map "2" 'split-window-vertically)
    (define-key view-mode-map "3" 'split-window-horizontally)
    (define-key view-mode-map "f" 'forward-word)
    (define-key view-mode-map "b" 'backward-word)
    (define-key view-mode-map "v" 'set-mark-command)
    (define-key view-mode-map "t" 'set-mark-command)
    (define-key view-mode-map "o" 'other-window)
    (define-key view-mode-map "G" 'end-of-buffer)
    (define-key view-mode-map "i" 'view-mode)
    (define-key view-mode-map "r" 'move-to-window-line-top-bottom)
    (define-key view-mode-map "c" 'kill-ring-save)
    (define-key view-mode-map "l" 'View-scroll-line-forward)
    (defface view-mode-mode-line-face
      '((((type tty pc)) :bold t :background "red" :foreground "white") (t (:background "red" :foreground "white")))
      "Face used highlight `view-mode-line-format'.")

    (defvar view-mode-line-format
      (propertize "View"
		  ;; 'local-map mode-line-minor-mode-keymap
		  ;; 'help-echo "mouse-3: minor mode menu"
		  'face 'view-mode-mode-line-face)
      "*Mode line format of `view-mode'.")

    (put 'view-mode-line-format 'risky-local-variable t)

    (setq minor-mode-alist
	  (append
	   `((view-mode " ") (view-mode ,view-mode-line-format))
	   (delq (assq 'view-mode minor-mode-alist) minor-mode-alist) ))
    )
  )

;; eval-after-load里的view-mode-setting总是执行，所以改为add-hook方式，估计是因为emacs的默认主页就是view mode的关系吧
(add-hook 'view-mode-hook 'view-mode-settings)
(defun view-exist-file ()
  (when (file-exists-p (buffer-file-name))
    (view-mode)))
(add-hook 'find-file-hook 'view-exist-file)
(keyboard-translate ?\C-i ?\H-i)	;把C-I绑定为开关，terminal貌似不起作用
(global-set-key [?\H-i] 'view-mode)

;;; occur
(global-set-key (kbd "C-c o") 'occur-select)
(add-hook 'occur-mode-hook (lambda () 
			     (local-set-key (kbd "p") 'occur-prev)
			     (local-set-key (kbd "n") 'occur-next)))
;;; from http://www.emacswiki.org/emacs/aok.el
(defun all-occur (rexp)
  "Search all buffers for REXP."
  (interactive "MRegexp: ")
  (multi-occur (buffer-list) rexp))

;; this one {c}/{sh}ould be a completing read that would read from a
;; predefined list of filetype extensions (without requiring a match).
(defun type-occur (extension rexp)
  "EXTENSION denotes a filetype extension to search.
Run occur in all buffers whose names match this type for REXP."
  (interactive "MExtension: \nMRegexp: ")
  (multi-occur-in-matching-buffers (concat ".*\." extension) rexp))

(defun mode-occur (mode rexp)
  "Search all buffers with major mode MODE for REXP."
  (interactive (list (read-command "Mode: ")
                     (read-string "Regexp: ")))
  (message "haha")
  (multi-occur (remove-if (lambda (buf)
                            (set-buffer buf)
                            (not (eq major-mode mode)))
                          (buffer-list))
               rexp))

;;; 参考multi-occur实现，写这个函数很考验lisp功力，调试方法C-X C-E
(defun occur-select (more regx &optional nothing)
  "select what you wan't to see occur"
  (interactive 
   (cons
    (let* ((choice (read-char "Occur in: [a]ll, [t]ype, [m]ode, or just this buffer(any other key)?"))
	   (more  (list (cond ((eq choice ?a) nil)
			      ((eq choice ?t) (read-string "Extension: "))
			      ((eq choice ?m) (read-command "Mode:"))
			      (t ?o)))) ; 即occur
	   )
      (add-to-list 'more choice)
      (nreverse more)) ; nreverse是配合前面的cons
    (occur-read-primary-args))) ;填充regx
  (let* ((choice (cadr more))
	 (morearg (car more)))
    (cond ((eq choice ?a) (all-occur regx))
	  ((eq choice ?t) (type-occur morearg regx))
	  ((eq choice ?m) (mode-occur morearg regx))
	  (t (occur regx))
	  )))


;; ctags/etags，更多设置在plugin_basic里
;; (global-set-key (kbd "C-;") 'complete-tag)
;; (global-set-key (kbd "C-'") 'completion-at-point) ; 24.x增强了
(setq completion-cycle-threshold 3) 	; 不超过3个补全数的话就不显示补全窗口

;;; isearch
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;;; filecache, patched, because I my complition can cycle which ido can't
(defun file-cache-add-this-file ()
  (and buffer-file-name
       (file-exists-p buffer-file-name)
       (file-cache-add-file buffer-file-name)))
(defun file-cache-switch-file ()
  "Interactively open file from file cache'.
First select a file, matched using against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive)
  (let* ((file (completing-read
		"File: "
					 (mapcar
					  (lambda (x)
					    (car x))
					  file-cache-alist)))
         (record (assoc file file-cache-alist)))
    (find-file
     (concat
      (if (= (length record) 2)
          (car (cdr record))
        (completing-read
         (format "Find %s in dir: " file) (cdr record))) file))))

(defun file-cache-read-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
  (interactive "fFile: ")
  (when (file-exists-p file)
    (require 'filecache)
    (file-cache-clear-cache)
    (save-excursion
      (set-buffer (find-file-noselect file))
      (beginning-of-buffer)
      (setq file-cache-alist (read (current-buffer)))
      (kill-buffer (current-buffer))
      )))
(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (require 'filecache)
  (when file-cache-alist
    (with-temp-file (expand-file-name file)
      (prin1 file-cache-alist (current-buffer)))))
(defun lynnux-save-filecache ()
  (file-cache-save-cache-to-file "~/.filecache"))
(file-cache-read-cache-from-file "~/.filecache") ; 须放在下句上面，不然会多个.filecache
(add-hook 'kill-buffer-hook 'file-cache-add-this-file) ;把删除的buffer加入到filecache，我觉得只需要这个特性就可以了
(add-hook 'kill-emacs-hook 'lynnux-save-filecache)
(setq completion-ignore-case t) 	; filecache中是不区分大小写的，而补全需要

(global-set-key (kbd "C-'") 'hippie-expand)
(defun try-zwz-expand-dabbrev-visible (old)
  (save-excursion (try-expand-dabbrev-visible old)))
(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev
	try-zwz-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
;	try-expand-tag
;	try-expand-flexible-abbrev
	))

(defun he-tag-beg ()
  (let ((p
         (save-excursion 
           (backward-word 1)
           (point))))
    p))

(defun try-expand-tag (old)
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))
(defun tags-complete-tag (string predicate what)
  (save-excursion
    (require 'etags)
    (when (or tags-table-list
	      tags-file-name
	      )
      ;; If we need to ask for the tag table, allow that.
      (if (eq what t)
	  (all-completions string (tags-completion-table) predicate)
	(try-completion string (tags-completion-table) predicate)))))

(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (progn
	(he-init-string (he-lisp-symbol-beg) (point))
	(if (not (he-string-member he-search-string he-tried-table))
	    (setq he-tried-table (cons he-search-string he-tried-table)))
	(setq he-expand-list
	      (and (not (equal he-search-string ""))
		   (he-flexible-abbrev-collect he-search-string)))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'word) collection))))
    collection))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
          "\\w*" "\\b"))

;; (setq hippie-expand-try-functions-list
;;       (cons 'try-expand-flexible-abbrev hippie-expand-try-functions-list))


; 老实说gdb一点都不好用，尽量用打印输出来调试
(add-hook 'gdb-mode-hook '(lambda ()
			    (gdb-many-windows)
                            (define-key c-mode-base-map [(f5)] 'gud-go)
                            (define-key c-mode-base-map [(f10)] 'gud-step)
                            (define-key c-mode-base-map [(f11)] 'gud-next)
			    (define-key c-mode-base-map [(f9)] 'gud-break)))

;; autosave 这个会卡
(setq auto-save-default nil
      delete-auto-save-files nil)
;; bakup 
(setq make-backup-files nil)
(setq auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t)))
      backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))

(setq create-lockfiles nil) ;; 禁止创建#.开头的同名文件

;;; inf文件
(add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-windows-mode))
