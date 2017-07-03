;; Time-stamp: <2017-07-03 14:32:18 lynnux>
;; 非官方自带packages的设置

(add-to-list 'load-path
	     "~/.emacs.d/packages")

;;; global-linum-mode居然会托慢屏显速度，我一直还以为是emacs的问题！
(require 'nlinum)
(setq nlinum-format "%4d") ; 有点太靠左，设置4字符刚合适
(global-nlinum-mode)

;;; better C-A C-E
(autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
(autoload 'mwim-end-of-line-or-code "mwim" nil t)
(global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code)
(global-set-key (kbd "C-e") 'mwim-end-of-line-or-code)

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/undo/")))) ;; 这个功能爽呆了
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(defun yas ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
  (require 'yasnippet)
  (setq yas-snippet-dirs
	'("~/.emacs.d/packages/yasnippet/yasnippet-snippets-master"
	  "~/.emacs.d/packages/yasnippet/mysnippets" ;; personal snippets
	  ))
  (yas-global-mode 1))
(yas)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-globals-include '((kill-ring 50)
				(session-file-alist 100 t)
				(file-name-history 200)))

;; tabbar, use tabbar-ruler
(add-to-list 'load-path "~/.emacs.d/packages/tabbar") 
(global-set-key (kbd "<C-M-tab>") 'tabbar-backward-group)
(global-set-key (kbd "<C-M-S-tab>") 'tabbar-forward-group)

(global-set-key (kbd "<C-tab>") ;'tabbar-forward-tab
		'my-switch-buffer
		)

(global-set-key (if (string-equal system-type "windows-nt")
		    (kbd "<C-S-tab>")
		  (kbd "<C-S-iso-lefttab>")) ;'tabbar-backward-tab
		(lambda () 
		  (interactive)
		  (my-switch-buffer t))
		)

(defun my-switch-buffer (&optional backward)
  "Switch buffers, but don't record the change until the last one."
  (interactive)
  (save-excursion ; when C-g don't work
    (let* ((blist (copy-sequence (if (featurep 'tabbar-ruler) 
				     (ep-tabbar-buffer-list)
				   (buffer-list))
				 ))
	   (init-buffer (car blist))
	   current
	   (key-forward (kbd "<C-tab>"))
	   (key-backward (if (string-equal system-type "windows-nt")
			     (kbd "<C-S-tab>")
			   (kbd "<C-S-iso-lefttab>")))
	   done
	   (key-stroked (if backward
			    key-backward
			  key-forward))
	   key-previous)
      (setq key-previous key-stroked)
      (if backward
	  (setq blist (nreverse blist))
	(setq blist (append (cdr blist) (list (car blist)))))
      (while (not done)
	(setq current (car blist))
	(setq blist (append (cdr blist) (list current)))
	(switch-to-buffer current t)	; 第二个参数表明不记录
	(when (featurep 'cursor-chg)
	  (curchg-change-cursor-on-overwrite/read-only)) ; 修正cursor光标显示
	(message "C-tab to cycle forward, C-S-tab backward...")
	(setq key-stroked (make-vector 1 (read-event)))
	(unless (equal key-previous key-stroked)
	  (setq blist (nreverse blist))
	  (setq blist (append (cdr blist) (list (car blist)))))
	(setq key-previous key-stroked)
	(cond ((equal key-forward key-stroked)
	       t)
	      ((equal key-backward key-stroked)
	       t)
	      ((equal (kbd "<C-g>") key-stroked) ; don't work
	       (switch-to-buffer init-buffer t))
	      (t 
	       (setq done t)
	       (switch-to-buffer current)
	       (clear-this-command-keys t)
	       (setq unread-command-events (list last-input-event))))
	)
      ;; (when (= last-input-event ?\C-g) ; don't work 会引发warning
      ;; 	(switch-to-buffer init-buffer t))
      )))

;;org的C-tab基本上不用
(add-hook 'org-mode-hook (lambda()
			   (define-key org-mode-map (kbd "<C-tab>") 'my-switch-buffer)))
(setq EmacsPortable-global-tabbar 't)
(require 'tabbar-ruler)
(setq EmacsPortable-excluded-buffers '("*Messages*" "*Completions*" "*ESS*" "*Compile-Log*" "*Ibuffer*" "*SPEEDBAR*" "*etags tmp*" "*reg group-leader*" "*Pymacs*" "*grep*"))
(setq EmacsPortable-included-buffers '("*scratch*" "*shell*"))

(require 'highlight-symbol)
;zenburn
(set-face-background 'highlight-symbol-face "SteelBlue4") ; SteelBlue4
; (set-face-foreground 'highlight-symbol-face "yellow")
;atom-one-dark
; (set-face-background 'highlight-symbol-face "black")
;normal
; (set-face-background 'highlight-symbol-face "yellow")
(setq highlight-symbol-idle-delay 0.1)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;; (highlight-symbol-mode 1)
;; 定义全局mode
(defun highlight-symbol-mode-on ()
  (unless  (or 
	    ;; 排除的mode，在此加
	    (eq major-mode 'occur-mode)
	    (eq major-mode 'occur-edit-mode)
	    (eq major-mode 'erc-mode)
	    )
    (highlight-symbol-mode)) 
  )
(define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode highlight-symbol-mode-on)
(global-highlight-symbol-mode 1)
(require 'highlight-symbol-scroll-out)
(global-highlight-symbol-scroll-out-mode)

(require 'cursor-chg)
(change-cursor-mode )
(setq curchg-default-cursor-color "red3") ; 无法设置cursor的foreground

;;crosshairs不好用，只要vline就行了		
(autoload 'vline-mode "vline" nil t)
(global-set-key [(control ?|)] 'vline-mode)

;(global-hl-line-mode t)

(add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/mydict")
(ac-config-default)
(setq-default ac-sources '(ac-source-semantic
			   ac-source-yasnippet
			   ac-source-abbrev
			   ac-source-dictionary 
			   ac-source-words-in-buffer
			   ac-source-words-in-all-buffer
			   ac-source-imenu
			   ac-source-files-in-current-dir
			   ac-source-filename))
(add-hook 
 'auto-complete-mode-hook 
 (lambda() 
   (define-key ac-completing-map "\C-n" 'ac-next)
   (define-key ac-completing-map "\C-p" 'ac-previous)
   ))
(global-set-key (kbd "<C-return>") 'auto-complete)

;; 一来就加载mode确实挺不爽的，还是用这个了
(require 'wcy-desktop)
(wcy-desktop-init)
(add-hook 'emacs-startup-hook
          (lambda ()
            (ignore-errors
              (wcy-desktop-open-last-opened-files))))

;; clang-format
(autoload 'clang-format-region "clang-format" "" t)
(autoload 'clang-format-buffer "clang-format" "" t)
(defun clang-format-auto ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (call-interactively 'clang-format-region)
    (call-interactively 'clang-format-buffer)))

(add-hook 'c-mode-common-hook 'lynnux-c-mode-hook)
(add-hook 'c++-mode-hook 'lynnux-c++-mode-hook)

(defun lynnux-c-mode-hook ()
  (setq tab-width 4 indent-tabs-mode nil)
  (setq c-hungry-delete-key t)		; 
  (setq c-auto-newline 1)
  (c-set-style "stroustrup")
  (setq cscope-do-not-update-database t)
  (setq cscope-program "gtags-cscope")
  (gtags-settings)
  (define-key c-mode-base-map (kbd "C-h") 'c-electric-backspace) ;修复C-h没有这个效果
  (local-set-key (kbd "C-c C-c") 'comment-eclipse)
  (setq clang-format-style "webkit") ; 只有这个默认tab是4个空格
  (local-set-key [(meta f8)] 'clang-format-auto)
)

(defun lynnux-c++-mode-hook()
  ;;c++ types
  (font-lock-add-keywords 'c++-mode '(("\\<\\(static_assert\\|assert\\|ensure\\)\\>"
				       . font-lock-warning-face)))
  (font-lock-add-keywords 'c++-mode '(("\\pure\\>"
				       . font-lock-keyword-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<u?\\(byte\\|short\\|int\\|long\\)\\>"	
				       . font-lock-type-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<c?\\(float\\|double\\)\\>"	
				       . font-lock-type-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<u?int\\(8\\|16\\|32\\)\\(_t\\)?\\>"
				       . font-lock-type-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<s?size_t\\>"
				       . 'font-lock-type-face)))
  )

(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                 nsis-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                 nsis-mode)) auto-mode-alist))

(autoload 'protobuf-mode "protobuf-mode" "protobuf mode" t)
(setq auto-mode-alist (append '(("\\.proto\\'" .
				 protobuf-mode)) auto-mode-alist))

(eval-after-load "calendar" 
  '(progn
     (require 'cal-china-x)
     (setq mark-holidays-in-calendar t)
     (setq cal-china-x-priority1-holidays cal-china-x-chinese-holidays)
     (setq calendar-holidays cal-china-x-priority1-holidays))) 

(global-set-key (kbd "<f4>") 'next-error)
(global-set-key (kbd "S-<f4>") 'previous-error)

(require 'jumplist)
(global-set-key (kbd "M-n") 'jl-jump-forward)
(global-set-key (kbd "M-p") 'jl-jump-backward)
(add-to-list 'jl-insert-marker-funcs "my-switch-buffer")
(add-to-list 'jl-insert-marker-funcs "ggtags-find-tag-dwim")
(add-to-list 'jl-insert-marker-funcs "ggtags-find-reference")
(add-to-list 'jl-insert-marker-funcs "ggtags-find-file")

(autoload 'iss-mode "iss-mode" "Innosetup Script Mode" t)
(setq auto-mode-alist (append '(("\\.iss$"  . iss-mode)) auto-mode-alist))
(setq iss-compiler-path "D:/Programme/Inno Setup 5/")
(add-hook 'iss-mode-hook 'xsteve-iss-mode-init)
(defun xsteve-iss-mode-init ()
  (interactive)
  (define-key iss-mode-map [f6] 'iss-compile)
  (define-key iss-mode-map [(meta f6)] 'iss-run-installer))

;; (add-hook 'dired-mode-hook (lambda () (require 'w32-browser)))
(eval-after-load "dired"
  '(progn
     (unless (string-equal system-type "windows-nt")
       (require 'w32-browser)
       (define-key dired-mode-map [f3] 'dired-w32-browser)
       (define-key dired-mode-map (kbd "<C-return>") 'dired-w32-browser)
       (define-key dired-mode-map [f4] 'dired-w32explore)
       (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
	 '("Open Associated Application" . dired-w32-browser))
       ;; (define-key diredp-menu-bar-immediate-menu [dired-w32explore]
       ;;   '("Windows Explorer" . dired-w32explore))
       (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
       (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
	 '("Open Associated Applications" . dired-multiple-w32-browser)))))

(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;; paredit
;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code." t)
;; (eval-after-load "paredit" '(progn
;; 			      (require 'eldoc)
;; 			      (eldoc-add-command
;; 			       'paredit-backward-delete
;; 			       'paredit-close-round)
;; 			      (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
;; 			      (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)
;; 			      ))
;; (electric-pair-mode) ; replace autopair

(defun my-elisp-hook()
  (defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")
					; 这功能不知道干什么用，稍微修改让换行时自动indent
  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
	  (save-excursion  ;(newline-and-indent) ; 只需要indent
	    (indent-according-to-mode)))
      (newline arg)
      (indent-according-to-mode)))
  (find-function-setup-keys)  ;直接定位函数变量定义位置的快捷键，C-x F/K/V，注意是大写的
;  (paredit-mode t) ; 习惯非常好用
  (setq eldoc-idle-delay 0)
  (turn-on-eldoc-mode)
;  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (eldoc-add-command 'electrify-return-if-match)
  (show-paren-mode t)
  (local-set-key (kbd "<f12>") 'xref-find-definitions)
  (local-set-key (kbd "M-.") 'xref-find-definitions) ; 这个还是原生的好用
  (local-set-key (kbd "<C-down-mouse-1>") 'xref-find-definitions)
  )
(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-elisp-hook)
(add-hook 'ielm-mode-hook 'my-elisp-hook)

;;; gtags
(defun gtags-settings()
  ;;; gtags 自动更新当文件保存时
  (defun gtags-root-dir ()
    "Returns GTAGS root directory or nil if doesn't exist."
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
	  (buffer-substring (point-min) (1- (point-max)))
	nil)))
  (defun gtags-update ()
    "Make GTAGS incremental update"
    (call-process "global" nil nil nil "-u"))
  (defun gtags-update-hook ()
    (when (gtags-root-dir)		;没有多余的副作用
      (gtags-update)))
  (add-hook 'after-save-hook #'gtags-update-hook)
  (require 'ggtags)
  (ggtags-mode 1)
  (add-hook 'ggtags-global-mode-hook (lambda()
				       (define-key ggtags-global-mode-map "n" 'next-line)
				       (define-key ggtags-global-mode-map "p" 'previous-line)
				       ))
  (setq ggtags-global-abbreviate-filename nil) ; 不缩写路径
  (defadvice ggtags-eldoc-function (around my-ggtags-eldoc-function activate)); eldoc没有开关，只有重写它的函数了
;  (customize-set-variable 'ggtags-highlight-tag nil) ; 禁止下划线 setq对defcustom无效！ 测试是eldoc导致提示process sentinel的
  (local-set-key (kbd "<C-down-mouse-1>") 'ggtags-find-tag-dwim) ; CTRL + 鼠标点击，很好用
  (local-set-key (kbd "<f12>") 'ggtags-find-tag-dwim)
  (local-set-key (kbd "<C-wheel-up>") 'previous-error)
  (local-set-key (kbd "<C-wheel-down>") 'next-error)
  ;(turn-on-eldoc-mode) ; 会卡
  )

;; erlang配置, 主要配置来自http://jixiuf.github.com/erlang/distel.html和
;; https://raw.github.com/jixiuf/emacs_conf/master/site-lisp/joseph/joseph-erlang.el
(add-to-list 'ac-modes 'erlang-mode)
(eval-after-load "erlang" 
  '(progn
     ;; (setq inferior-erlang-machine-options `("-name" ,(concat "emacs@" system-name "")  "+P" "102400")       )
     (require 'erlang-flymake)
     (defun erlang-flymake-get-app-dir() ;重新定义erlang-flymake中的此函数,find out app-root dir
       ;; 有时,代码会放在 src/deep/dir/of/source/这样比较深的目录,erlang-flymake自带的此函数
       ;; 无法处理这种情况
       (let ((erlang-root (locate-dominating-file default-directory "Emakefile")))
	 (if erlang-root
	     (expand-file-name erlang-root)
	   (setq erlang-root (locate-dominating-file default-directory "rebar"))
	   (if erlang-root
	       (expand-file-name erlang-root)
	     (file-name-directory (directory-file-name
				   (file-name-directory (buffer-file-name)))))
	   )))
     (defun my-erlang-flymake-get-include-dirs-function()
       (let* ((app-root (erlang-flymake-get-app-dir))
	      (dir (list (concat app-root "include") ;不支持通配符,
			 (concat  app-root "src/include")
			 (concat  app-root "deps")))
	      (deps (concat  app-root "deps")))
	 (when (file-directory-p deps)
	   (dolist (subdir (directory-files deps))
	     (when (and (file-directory-p (expand-file-name subdir deps))
			(not (string= "." subdir))
			(not (string= ".." subdir)))
	       (add-to-list 'dir (expand-file-name (concat subdir "/include" ) deps))
	       )))
	 dir))
     (setq erlang-flymake-get-include-dirs-function 'my-erlang-flymake-get-include-dirs-function)

     ;;这个没办法,不能单方面设置,只能拷贝系统home下的.erlang.cookie到emacs的home目录里
     ;; (setq derl-cookie "when_home_not_equal")
     (require 'distel)
     (distel-setup)
     ))

;;;; 当打开erl  文件时，自动启动一个shell 以便distel进行补全
(add-hook 'erlang-mode-hook 
	  '(lambda () (unless erl-nodename-cache (distel-load-shell))
	     ;(local-set-key [(control ?\.)] 'erl-find-source-under-point)
	     (local-set-key (kbd "<f12>") 'erl-find-source-under-point)
	     (local-set-key (kbd "M-.") 'etags-select-find-tag-at-point) ; when distel does't work
	     (local-set-key (kbd "C-'")  'erl-complete)
	     ))

(defun distel-load-shell ()
  "Load/reload the erlang shell connection to a distel node"
  (interactive)
  ;; Set default distel node name
  (setq erl-nodename-cache (intern (concat "emacs@" system-name "")))
  ;; (setq derl-cookie (read-home-erlang-cookie)) ;;new added can work
  (setq distel-modeline-node "distel")
  (force-mode-line-update)
  ;; Start up an inferior erlang with node name `distel'
  (let ((file-buffer (current-buffer))
        (file-window (selected-window)))
    ;; (setq inferior-erlang-machine-options '("-sname" "emacs@localhost" "-setcookie" "cookie_for_distel"))
    (setq inferior-erlang-machine-options `("-name" ,(concat "emacs@" system-name "") )) ;; erl -name emacs
    (switch-to-buffer-other-window file-buffer)
    (inferior-erlang)
    (select-window file-window)
    (switch-to-buffer file-buffer)
    (delete-other-windows)))

;; ruby mode
(add-hook 'ruby-mode-hook '(lambda()
			     (require 'rcodetools)
			     (setq ac-omni-completion-sources
				   (list (cons "//." '(ac-source-rcodetools))
					 (cons "::" '(ac-source-rcodetools))))
			     (setq ac-sources (append (list 'ac-source-rcodetools) ac-sources))
			     ))

;; popup yank-pop
(autoload 'popup-kill-ring "popup-kill-ring" nil t)
(global-set-key "\M-y" 'popup-kill-ring)
(global-set-key (kbd "C-`") 'popup-kill-ring)
(eval-after-load "popup-kill-ring" 
  '(progn
     (define-key popup-kill-ring-keymap "\M-n" 'popup-kill-ring-next)
     (define-key popup-kill-ring-keymap (kbd "C-`") 'popup-kill-ring-next)
     (define-key popup-kill-ring-keymap "\M-p" 'popup-kill-ring-previous)
     (setq 
      popup-kill-ring-interactive-insert t
      popup-kill-ring-item-min-width nil
      popup-kill-ring-item-size-max 26
      popup-kill-ring-popup-margin-left 1 
      popup-kill-ring-popup-margin-right 0)     
					;     (define-key popup-kill-ring-keymap "\M-y" 'popup-kill-ring-next)
					; 如果有选中，会删除选中，需开启delete-selecton-mode
     (put 'popup-kill-ring 'delete-selection 'yank)
     ))

;; python
;; (add-hook 'python-mode-hook '(lambda()
;; 			       (require 'pymacs)
;; 			       (pymacs-load "ropemacs" "rope-")
;; 			       (ropemacs-mode)
;; 			       (setq ropemacs-enable-autoimport t)
;; ))

(add-to-list 'load-path "~/.emacs.d/packages/jedi")
(autoload 'jedi:ac-setup "jedi" nil t)
(add-hook 'python-mode-hook '(lambda ()
			       (jedi:ac-setup)
			       (local-set-key (kbd "<C-return>") 'jedi:complete)))
;; (setq jedi:server-command
;;       (list "D:\\Python27\\Python.exe" jedi:server-script))

;; gnu global

(setq minibuffer-complete-cycle t)
(require 'minibuffer-complete-cycle) ;; modified for Emacs24
(custom-set-faces
 '(minibuffer-complete-cycle ((t (:background "blue" :foreground "snow")))))

;;; slime + sbcl，安装配置见~/.emacs.d/packages里的说明
(when (file-exists-p "~/.emacs.d/packages/slime") ; 安装了再进行下一步
  (add-to-list 'load-path "~/.emacs.d/packages/slime")
  (autoload 'slime "slime-autoloads" nil t)
  (eval-after-load "slime-autoloads" '(progn
					(slime-setup '(slime-fancy))
					))
  )

;; ctags
(setq ctags-program "ctags")
(when (string-equal system-type "windows-nt")
  (progn
    (setq ctags-program (concat "\"" (file-truename "~/.emacs.d/bin/ctags.exe") "\""))
    ))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   ;; (message
   (format "%s -f %s -e -R %s" ctags-program 
	   (concat "\"" (expand-file-name "TAGS" dir-name) "\"") ;目录含有空格的话
	   (concat "\"" (expand-file-name "." dir-name) "\"")
	   )))

;; stl文件名无后辍，对vc来说只有vcinstalldir/include这个目录
(defun create-tags-cppstl (dir-name)
  "Create stl tags file."
  (interactive "DDirectory: ")
  (shell-command
   ;; (message
   (format "%s --c++-kinds=+p --fields=+iaS --extra=+q --language-force=c++ -f %s -e -R %s" ctags-program 
	   (concat "\"" (expand-file-name "TAGS" dir-name) "\"") ;目录含有空格的话
	   (concat "\"" (expand-file-name "." dir-name) "\"")
	   )))
;;; 增强M-.
(setq etags-table-search-up-depth 10) ;; 自动向目录上层找TAGS的层次数
(autoload 'etags-select-find-tag-at-point "etags-select" nil t)
(autoload 'etags-select-find-tag "etags-select" nil t)
(global-set-key (kbd "<f12>") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point) ; 除了elisp和ggtags支持的都用这个
(global-set-key (kbd "<C-down-mouse-1>") 'etags-select-find-tag-at-point)

(eval-after-load "etags-select" '
  (progn 
    (require 'etags-table)
    (require 'etags-stack)))

;; lcEngine
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (require 'lcEngine)
;; 	    (local-set-key (kbd "<C-return>") 'ac-complete-lcEngine) ;; 补全
;; 	    (local-set-key (kbd "<f12>") 'lcEngine-goto-definition) ;; 跳转到声明
;; 	    (local-set-key (kbd "<C-f4>") 'lcEngine-syntax-check) ;; 检查错误
	    
;; 	    (ac-lcEngine-setup) ; 感觉用起来太卡了，补全卡，flymake也卡
;; 	    ; 用上来的按键
;; 	    (setq flymake-no-changes-timeout 1.0)
;; 	    (local-set-key (kbd "<f4>") 
;; 			   (lambda ()
;; 			     (interactive)
;; 			     (flymake-goto-next-error)
;; 			     (let ((err (get-char-property (point) 'help-echo)))
;; 			       (when err
;; 				 (message err)))))
;; 	    (local-set-key [(shift f4)]
;; 			   (lambda ()
;; 			     (interactive)
;; 			     (flymake-goto-prev-error)
;; 			     (let ((err (get-char-property (point) 'help-echo)))
;; 			       (when err
;; 				 (message err)))))
;; 	    ))

;; everything, awesome! 就是中文路径有点问题，待修正
(when (string-equal system-type "windows-nt")
  (setq everything-use-ftp t)
  (defun everything-ffap-guesser-wrapper (file)
    (require 'everything)
    (everything-ffap-guesser file)
    )
  (eval-after-load "ffap" '(setf (cdr (last ffap-alist)) '(("\\.*\\'" . everything-ffap-guesser-wrapper)))) ; when C-x f can't find anything, try everything
  (autoload 'everything "everything" nil t)
  ;; 这样就解决乱码问题了，包括搜索输入,emacs is awesome! 进程是话设置`process-coding-system-alist'，文件设置`file-coding-system-alist'
  (eval-after-load "everything" '(add-to-list 'network-coding-system-alist
					      '(21 . (utf-8 . utf-8))
					      )); 默认21端口
  )

;;; shell, main goal is for compile test
(defun smart-compile-run ()
  (interactive)
  (if (equal (buffer-name) "*shell*")
      (switch-to-prev-buffer)
    ;; (let ((run-exe (concat (file-name-sans-extension
    ;; 		     (file-name-nondirectory (buffer-file-name))) ".exe"))))
    (with-current-buffer (shell)
      (end-of-buffer)
      (move-end-of-line nil)
      ;(move-beginning-of-line nil)
      ;(kill-line 1)
      ;(insert-string run-exe)
      ;(move-end-of-line nil)
       )))
(global-set-key (kbd "<f5>") 'smart-compile-run)

;;; TODO 如果编译过其他目录后，另一个目录C-F7时当前目录没有变，必须C-u F7重新配置
;; compile，加入了单独编译某个文件
(setq compilation-auto-jump-to-first-error t compilation-scroll-output t)
(autoload 'smart-compile "smart-compile" nil t)
(autoload 'smart-compile-c-compile "smart-compile" nil t)
(global-set-key [f7] 'smart-compile)
(global-set-key [(shift f7)] 'smart-compile-regenerate)
(global-set-key [(control f7)] 'smart-compile-c-compile)
(defun smart-compile-regenerate()
  (interactive)
  (smart-compile 4))
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (when (equal (buffer-name) "*compilation*") 
    (when (string-match "^finished" msg)
	(progn
	  (delete-windows-on buffer)
	  ;; (tooltip-show "\n Compilation Successful :-) \n ")
	  )
      ;; (tooltip-show "\n Compilation Failed :-( \n ")
      )
    (setq current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame)
    ))
(eval-after-load "compile" '(add-to-list 'compilation-finish-functions
					 'notify-compilation-result))

;; (add-to-list 'load-path "~/.emacs.d/packages/helm")
;; (require 'helm-config)
					;(global-set-key (kbd "C-c h") 'helm-mini)
					;(helm-mode 1)

(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(autoload 'er/expand-region "expand-region" nil t)
(global-set-key "\C-t" 'er/expand-region)
;;; 解决被(setq show-paren-style 'expression)覆盖的问题
(defadvice show-paren-function (around not-show-when-expand-region activate)
  (if (and (or (eq major-mode 'lisp-interaction-mode) (eq major-mode 'emacs-lisp-mode))
	   (memq last-command '(er/expand-region er/contract-region)))
      (progn
	(setq show-paren-style 'parenthesis)
	ad-do-it
	(setq show-paren-style 'expression)
	)
    ad-do-it))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; 类似sublime的多光标功能(以M键更像是visual code)
(setq mc/list-file "~/.emacs.d/packages/multiple-cursors/my-cmds.el")
(add-to-list 'load-path
	     "~/.emacs.d/packages/multiple-cursors")
(autoload 'mc/add-cursor-on-click "multiple-cursors" nil t)
(autoload 'mc/unmark-next-like-this "multiple-cursors" nil t)
(autoload 'mc/unmark-previous-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-previous-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)
(autoload 'mc/edit-lines "multiple-cursors" nil t)
(global-unset-key (kbd "M-<down-mouse-1>"))  ; CTRL + 鼠标单击给ggtag用了
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-M-<mouse-1>") 'mc/unmark-next-like-this) ; 取消光标以下的mark
(global-set-key (kbd "M-S-<mouse-1>") 'mc/unmark-previous-like-this) ;取消光标以上的mark
(global-set-key (kbd "M-<wheel-up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<wheel-down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-t") 'mc/edit-lines)  ;居然不支持同行的range
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; 屏幕内快速跳过，默认是跳到字母开头的单词位置，C-u C-j改为跳回原来的位置，C-u C-u C-j是跳到行
(autoload  'ace-jump-mode  "ace-jump-mode"  "Emacs quick move minor mode"  t)
(define-key global-map (kbd "C-o") 'ace-jump-mode)
(eval-after-load 'ace-jump-mode '(setq ace-jump-mode-submode-list
				  '(ace-jump-word-mode
				    ace-jump-mode-pop-mark
				    ace-jump-char-mode
				    ace-jump-line-mode)))

;;; autohotkey文件编辑
(autoload 'xahk-mode "xahk-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;;; rust mode
(add-to-list 'load-path "~/.emacs.d/packages/rust")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;; racer补全
(autoload 'racer-mode "racer" nil t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(defun my/racer-mode-hook ()
  (require 'ac-racer)
  (ac-racer-setup)
  (make-local-variable 'ac-auto-start) ; 自动弹出会卡
  (setq ac-auto-start nil)
  (when (featurep 'smartparens-rust)
      ;; 必须加载了smartparens-rust，不然没效果。其实不要smartparens-rust就好了
    (eval-after-load 'smartparens-rust
      '(progn
	(sp-local-pair 'rust-mode "'" nil :actions nil)
	(sp-local-pair 'rust-mode "<" nil :actions nil) ;  第4个参数不写">"也可以
	))))
(add-hook 'racer-mode-hook 'my/racer-mode-hook)
(setq racer-cmd "racer") ;; 自己添加racer到PATH环境变量
;;; (setq racer-rust-src-path "") 自己设置RUST_SRC_PATH环境变量指向rust源码的src目录

(add-to-list 'load-path "~/.emacs.d/packages/neotree")
(autoload 'neotree-toggle "neotree" nil t)
(global-set-key (kbd "<C-f1>") 'neotree-toggle)
(global-set-key (kbd "C-x d") 'neotree-toggle) ; dired基本不用
(setq neo-window-width 32
      neo-create-file-auto-open t
      neo-show-updir-line t
      neo-mode-line-type 'neotree
      neo-smart-open t
      neo-show-hidden-files t
      neo-auto-indent-point t
      neo-vc-integration nil)

;; 有点类似自动截段长行的效果，默认绑定到m-q，elisp-mode无效
(autoload 'unfill-toggle "unfill" nil t)
(global-set-key [remap fill-paragraph] #'unfill-toggle)

(require 'hungry-delete)
(global-hungry-delete-mode)
(setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace

(add-to-list 'load-path "~/.emacs.d/packages/smartparens")
(require 'smartparens-config)
(sp-use-paredit-bindings)
;; 补充paredit的M-(，其它模式由于pair不止(所以不可用
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "(" nil :bind "M-(") ; 这个其实是包装成sp-wrap了
(set-default 'sp-autoskip-closing-pair 'always)
;; Don't kill the entire symbol on C-k
(set-default 'sp-hybrid-kill-entire-symbol nil)
(smartparens-global-strict-mode)
(show-smartparens-global-mode) ;; Show parenthesis

(with-eval-after-load 'smartparens
  (dolist (key '( [remap delete-char]
                  [remap delete-forward-char]))
    (define-key smartparens-strict-mode-map key
      ;; menu-item是一个symbol，而且很有趣的是，F1-K能实时知道是调用哪个函数
      '(menu-item "maybe-sp-delete-char" nil
                  :filter (lambda (&optional _)
                            (unless (looking-at-p "[[:space:]\n]")
                              #'sp-delete-char)))))

  (dolist (key '([remap backward-delete-char-untabify]
                 [remap backward-delete-char]
                 [remap delete-backward-char]))
    (define-key smartparens-strict-mode-map key
      '(menu-item "maybe-sp-backward-delete-char" nil
                  :filter (lambda (&optional _)
                            (unless (looking-back "[[:space:]\n]" 1)
                              #'sp-backward-delete-char)))))
  
  ;; C-W支持
  (dolist (key '( [remap kill-region]))
    (define-key smartparens-strict-mode-map key
      '(menu-item "maybe-sp-kill-region" nil
                  :filter (lambda (&optional _)
                            (when (use-region-p) ;; 有选中时才用sp的
                              #'sp-kill-region)))))
  )


;; !themes要放到最后，内置theme查看 M-x customize-themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; 类似vim的tagbar，比之前那个sr-speedbar不知道好用多少倍!
;; 不过这个没有neotree好，会多弹出一个frame，就不默认开启了，看代码时很有用
(autoload 'imenu-list-smart-toggle "imenu-list" nil t)
(global-set-key [(control f4)] 'imenu-list-smart-toggle)
