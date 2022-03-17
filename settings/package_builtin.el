;; Time-stamp: <2022-03-17 11:41:12 lynnux>
;; 说明：
;; 自带的lisp包设置等
;; 自带的不用加require，因为xxx-mode基本上都是autoload！
;; C-x f filecache everything recent-visit/changed find-file-at-point

;; (recentf-mode 1) 用session代替了
(setq history-length 200)
(defun files-recent-type (src)
  (interactive)
  (let* ((tocpl src ;; 全路径好些，可以通过项目名搜索，也解决了文件名相同时的bug
		;; (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
		;; 	src)
		)
	 (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file ;; (cdr (assoc-string fname tocpl))
       fname
       ))))
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
;; (setq org-capture-bookmark nil) ;; 不需要添加到
(setq org-bookmark-names-plist '(:last-capture "org-capture-last-stored"
					       ;;:last-refile "org-refile-last-stored"
					       ;;:last-capture-marker "org-capture-last-stored-marker"
					       ))
(setq org-capture-templates
      `(("i" "Idea" entry (file+headline ,"idea.org" "Index")
	 "* IDEA %?\n %i\n %a")
	("t" "Task" entry (file+headline ,"todo.org" "Task")
	 "* TODO %i%?\n\n于: %U %a")
	))
;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO %i%?\n\n于: %U %a" "F:/kp/org/remember/TODO.org" "Tasks")
;;   	("IDEA" ?i "* IDEA %?\n %i\n %a" "F:/kp/org/remember/Idea.org" "Idea")
;;   	))
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-cc" 'org-capture)
;; 对org-capture涉及的文件去掉只读
(with-eval-after-load 'org-capture
  ;; 需要hook三个函数 find-file-noselect get-file-buffer find-buffer-visiting
  (defadvice org-capture-target-buffer (around my-org-capture-target-buffer activate)
    (setq tmp-disable-view-mode 2);; 2不恢复只读
    ad-do-it
    (setq tmp-disable-view-mode nil)
    )
  )

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

(with-eval-after-load 'view
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
  (define-key view-mode-map "q" 'quit-window)
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
(global-unset-key (kbd "C-x C-q")) ;; 偶尔遇到C-i也不能修改就是这个导致的
(defun view-exist-file ()
  (when (file-exists-p (buffer-file-name))
    (view-mode)))
;; 避免wcy提示失败
(add-hook 'after-init-hook (lambda ()
                             (add-hook 'find-file-hook 'view-exist-file)
                             ))
(keyboard-translate ?\C-i ?\H-i)	;把C-I绑定为开关，terminal貌似不起作用
(global-set-key [?\H-i] 'view-mode)
;; 有些插件如eglot-rename需要临时禁用view-mode，一般用find-file-noselect(fin-file-hook那里做对已经打开的文件无效)，以下是trick
(defun run-with-local-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always runs in the `current-buffer'.

Cancels itself, if this buffer was killed."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))

(defvar disable-startup-load t)
(when disable-startup-load
  (run-with-idle-timer 1 nil (lambda ()
			       (setq disable-startup-load nil))))
(defvar tmp-disable-view-mode nil);; 在需要的函数defadvice里设置，2不恢复只读
(defun check-tmp-disable-view-mode ()
  (when (and (bufferp ad-return-value) (featurep 'wcy-desktop) (not disable-startup-load))
    (with-current-buffer ad-return-value
      (when (eq major-mode 'not-loaded-yet)
	(wcy-desktop-load-file)))) ;; 自动加载，但有个副作用是emacs启动会加载一个，通过上面的1秒timer解决
  (when (and tmp-disable-view-mode (bufferp ad-return-value))
    (with-current-buffer ad-return-value
      (when view-mode
	(view-mode -1) ;; 临时禁用
	(cond
	 ((eq tmp-disable-view-mode 2)())
	 (t
	  ;; 2秒后恢复只读，实际上idle可能超过2秒
	  (run-with-local-idle-timer 2 nil (lambda ()
					     (view-mode 1)
					     ))))
	)
      ))
  )

(defadvice find-file-noselect (around my-find-file-noselect activate)
  ad-do-it
  (check-tmp-disable-view-mode)
  )
;; 参考org-capture-target-buffer和org-find-base-buffer-visiting，找到下面两个的hook点
(defadvice get-file-buffer (around my-get-file-buffer activate)
  ad-do-it
  (check-tmp-disable-view-mode)
  )
(defadvice find-buffer-visiting (around my-find-buffer-visiting activate)
  ad-do-it
  (check-tmp-disable-view-mode)
  )

;;; occur
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
  (multi-occur (remove-if (lambda (buf)
                            (set-buffer buf)
                            (not (eq major-mode mode)))
                          (buffer-list))
               rexp))

(setq completion-ignore-case t)

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
	))

;; 老实说gdb一点都不好用，尽量用打印输出来调试
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

(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
	ediff-split-window-function #'split-window-horizontally ;; 总是左右两个窗口
	ediff-window-setup-function #'ediff-setup-windows-plain) ;; 禁止ediff小窗新开frame
  (defvar doom--ediff-saved-wconf nil)  
  ;; Restore window config after quitting ediff
  
  (defun doom-ediff-save-wconf-h ()
    (setq doom--ediff-saved-wconf (current-window-configuration)))
  (defun doom-ediff-restore-wconf-h ()
    (when (window-configuration-p doom--ediff-saved-wconf)
      (set-window-configuration doom--ediff-saved-wconf)))
  (add-hook 'ediff-before-setup-hook 'doom-ediff-save-wconf-h)
  (add-hook 'ediff-quit-hook 'doom-ediff-restore-wconf-h)
  (add-hook 'ediff-suspend-hook 'doom-ediff-restore-wconf-h)
  )

(global-auto-revert-mode)
