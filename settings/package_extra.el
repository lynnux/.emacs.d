;; Time-stamp: <2017-08-01 09:15:09 lynnux>
;; 非官方自带packages的设置
;; benchmark: 使用profiler-start和profiler-report来查看会影响emacs性能，如造成卡顿的命令等
;; 一般都是eldoc会卡，如ggtag和racer mode都是因为调用了其它进程造成卡的

(add-to-list 'load-path
	     "~/.emacs.d/packages")

;; !themes要放到最后，内置theme查看 M-x customize-themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

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
  (yas-global-mode 1)

  ;; 这个其实还挺好用的，用~xxx代替要替换的，或者`xxx'，多行要选中单行不用选中
  (autoload 'aya-create "auto-yasnippet" nil t)
  (autoload 'aya-expand "auto-yasnippet" nil t)
  (global-set-key (kbd "C-c y") #'aya-create)
  (global-set-key (kbd "C-c e") #'aya-expand)
  )
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
			   (define-key org-mode-map (kbd "<C-tab>") nil)))
(setq EmacsPortable-global-tabbar 't)
(require 'tabbar-ruler)
(setq EmacsPortable-excluded-buffers '("*Messages*" "*Completions*" "*ESS*" "*Compile-Log*" "*Ibuffer*" "*SPEEDBAR*" "*etags tmp*" "*reg group-leader*" "*Pymacs*" "*grep*"))
(setq EmacsPortable-included-buffers '("*scratch*" "*shell*"))

(require 'highlight-symbol)
;; zenburn
(set-face-background 'highlight-symbol-face "SteelBlue4") ; SteelBlue4
;; (set-face-foreground 'highlight-symbol-face "yellow")
;;atom-one-dark
;; (set-face-background 'highlight-symbol-face "black")
;;normal
;; (set-face-background 'highlight-symbol-face "yellow")
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

;; (global-hl-line-mode t)

(if t
    ;; auto complete
    (progn
      (add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
      (require 'auto-complete-config)
      (add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/mydict")
      (ac-config-default)
      (setq-default ac-sources '(ac-source-abbrev
				 ac-source-dictionary
				 ac-source-words-in-same-mode-buffers
				 ac-source-semantic
				 ac-source-yasnippet
				 ac-source-words-in-buffer
				 ac-source-words-in-all-buffer
				 ac-source-imenu
				 ac-source-files-in-current-dir
				 ac-source-filename))
      (setq
       ac-use-fuzzy t ; 貌似自动弹出的menu的是没有fuzzy的，但手动补全是可以的
       ;; ac-auto-show-menu 0.1
       ac-use-quick-help nil
       ;; ac-ignore-case t
       ac-use-comphist t
       )
      (add-hook 
       'auto-complete-mode-hook 
       (lambda() 
	 (define-key ac-completing-map "\C-n" 'ac-next)
	 (define-key ac-completing-map "\C-p" 'ac-previous)
	 ))
      (global-set-key (kbd "<C-return>") 'auto-complete)
      (global-set-key (kbd "<M-return>") 'auto-complete)
      )
  (progn
    ;; company mode，这个支持comment中文，但不支持补全history
    (add-to-list 'load-path "~/.emacs.d/packages/company-mode")
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "M-n") 'company-next-page)
    (define-key company-active-map (kbd "M-p") 'company-previous-page)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection) ; 类似return
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    (define-key company-active-map (kbd "C-h") nil) ; 取消绑定，按f1代替。c-w直接看源码
    (setq company-idle-delay 0.2   ; 设置为nil就不自动启用补全了，参考racer设置
	  company-minimum-prefix-length 2
	  company-require-match nil
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil)
    (global-set-key (kbd "<C-return>") 'company-indent-or-complete-common)
    (global-set-key (kbd "<M-return>") 'company-indent-or-complete-common)
    )
  )


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
  (gtags-settings)
  ;; (define-key c-mode-base-map (kbd "C-h") 'c-electric-backspace) ;修复C-h没有这个效果
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

(require 'jumplist)
(global-set-key (kbd "M-n") 'jl-jump-forward)
(global-set-key (kbd "M-p") 'jl-jump-backward)
(add-to-list 'jl-insert-marker-funcs "my-switch-buffer")
(add-to-list 'jl-insert-marker-funcs "ggtags-find-tag-dwim")
(add-to-list 'jl-insert-marker-funcs "ggtags-find-reference")
(add-to-list 'jl-insert-marker-funcs "ggtags-find-file")
(add-to-list 'jl-insert-marker-funcs "racer-find-definition")
(add-to-list 'jl-insert-marker-funcs "swiper")
(add-to-list 'jl-insert-marker-funcs "helm-occur")
(add-to-list 'jl-insert-marker-funcs "helm-imenu-in-all-buffers")

(autoload 'iss-mode "iss-mode" "Innosetup Script Mode" t)
(setq auto-mode-alist (append '(("\\.iss$"  . iss-mode)) auto-mode-alist))
(setq iss-compiler-path "D:/Programme/Inno Setup 5/")
(add-hook 'iss-mode-hook 'xsteve-iss-mode-init)
(defun xsteve-iss-mode-init ()
  (interactive)
  (define-key iss-mode-map [f6] 'iss-compile)
  (define-key iss-mode-map [(meta f6)] 'iss-run-installer))

;; (add-hook 'dired-mode-hook (lambda () (require 'w32-browser)))
(with-eval-after-load 'dired
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
      '("Open Associated Applications" . dired-multiple-w32-browser))))

(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

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
  (setq eldoc-idle-delay 0)
  (turn-on-eldoc-mode)
  ;;  (local-set-key (kbd "RET") 'electrify-return-if-match)
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
					;(turn-on-eldoc-mode) ; 会卡
  )

;; ruby mode
(add-hook 'ruby-mode-hook '(lambda()
			     (require 'rcodetools)
			     (when (featurep 'auto-complete)
			       (setq ac-omni-completion-sources
				     (list (cons "//." '(ac-source-rcodetools))
					   (cons "::" '(ac-source-rcodetools))))
			       (setq ac-sources (append (list 'ac-source-rcodetools) ac-sources)))
			     ))

;; python
(add-to-list 'load-path "~/.emacs.d/packages/jedi")
(when (featurep 'auto-complete)
  (autoload 'jedi:ac-setup "jedi" nil t)
  (add-hook 'python-mode-hook '(lambda ()
				 (jedi:ac-setup)
				 (local-set-key (kbd "<C-return>") 'jedi:complete))))
;; (setq jedi:server-command
;;       (list "D:\\Python27\\Python.exe" jedi:server-script))

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

;;; shell, main goal is for compile test
(defvar smart-compile-run-last-buffer nil)
(defun smart-compile-run ()
  (interactive)
  (if (equal (buffer-name) "*shell*")
      (progn
	(if smart-compile-run-last-buffer
	    (switch-to-buffer smart-compile-run-last-buffer)
	  (switch-to-prev-buffer))
	(delete-other-windows))
    ;; (let ((run-exe (concat (file-name-sans-extension
    ;; 		     (file-name-nondirectory (buffer-file-name))) ".exe"))))
    (progn
      (setq smart-compile-run-last-buffer (buffer-name))
      (with-current-buffer (shell)
	(end-of-buffer)
	(move-end-of-line nil)
					;(move-beginning-of-line nil)
					;(kill-line 1)
					;(insert-string run-exe)
					;(move-end-of-line nil)
	))))
(global-set-key (kbd "<f5>") 'smart-compile-run)

;;; TODO 如果编译过其他目录后，另一个目录C-F7时当前目录没有变，必须C-u F7重新配置
;; compile，加入了单独编译某个文件
(setq compilation-auto-jump-to-first-error t
      compilation-scroll-output t)
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
(with-eval-after-load 'compile (add-to-list 'compilation-finish-functions
					    'notify-compilation-result))

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
(with-eval-after-load 'ace-jump-mode (setq ace-jump-mode-submode-list
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
  (when (featurep 'auto-complete)
    (require 'ac-racer)
    (ac-racer-setup)
    (make-local-variable 'ac-auto-start) ; 自动弹出会卡
    (setq ac-auto-start nil)
    )
  (when (featurep 'company)
    (make-local-variable 'company-idle-delay)
    (setq company-idle-delay nil) 	; 不自动补全
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t))
  (setq-local eldoc-documentation-function #'ignore) ; eldoc严重影响输入！
  (when (featurep 'smartparens-rust)
    ;; 必须加载了smartparens-rust，不然没效果。其实不要smartparens-rust就好了
    (with-eval-after-load 'smartparens-rust
      (sp-local-pair 'rust-mode "'" nil :actions nil)
      (sp-local-pair 'rust-mode "<" nil :actions nil) ;  第4个参数不写">"也可以
      )))
(add-hook 'racer-mode-hook 'my/racer-mode-hook)
(setq racer-cmd "racer") ;; 自己添加racer到PATH环境变量
;;; (setq racer-rust-src-path "") 自己设置RUST_SRC_PATH环境变量指向rust源码的src目录

(add-to-list 'load-path "~/.emacs.d/packages/neotree")
(autoload 'neotree-toggle "neotree" nil t)
(global-set-key (kbd "<C-f1>") 'neotree-toggle)
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

;; 使支持hungry-delete
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
;; 换行自动indent，from https://github.com/Fuco1/smartparens/issues/80
(sp-local-pair '(c++-mode rust-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


;; 类似vim的tagbar，比之前那个sr-speedbar不知道好用多少倍!
;; 不过这个没有neotree好，会多弹出一个frame，就不默认开启了，看代码时很有用
(autoload 'imenu-list-smart-toggle "imenu-list" nil t)
(global-set-key [(control f4)] 'imenu-list-smart-toggle)
(global-set-key [(control f2)] 'imenu-list-smart-toggle)

(if t
    ;; 用helm可以抛弃好多包啊，有imenu-anywhere，popup-kill-ring，ripgrep，minibuffer-complete-cycle，etags-select那三个，everything(helm-locate)
    ;; 参考helm作者的配置https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
    (progn
      (add-to-list 'load-path "~/.emacs.d/packages/helm/emacs-async-master")
      (require 'async-autoloads)
      (add-to-list 'load-path "~/.emacs.d/packages/helm/helm-master")
      (add-to-list 'load-path "~/.emacs.d/packages/helm")
      (require 'helm-config)
      ;;(global-set-key (kbd "C-c h") 'helm-mini)
      (global-set-key (kbd "M-x") 'undefined)
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "C-s") 'helm-occur) ;; 不用helm swoop了，这个支持在c-x c-b里使用。开启follow mode
      (global-set-key (kbd "M-y") 'helm-show-kill-ring) ; 比popup-kill-ring好的是多了搜索
      (global-set-key (kbd "C-`") 'helm-show-kill-ring)
      (global-set-key [f2] 'helm-do-grep-ag) ; 使用ripgrep即rg搜索，这个有坑啊，居然读gitignore! 文档rg --help

      (global-set-key (kbd "C-x C-f") 'helm-find-files) ; 这个操作多文件非常方便！ C-c ?仔细学学！
      (global-set-key (kbd "C-x b") 'helm-buffers-list)
      (global-set-key (kbd "C-x C-b") 'helm-buffers-list) ; 比原来那个好啊
      (global-set-key (kbd "C-c C-r") 'helm-resume)	  ;继续刚才的session
      (global-set-key (kbd "<f6>") 'helm-resume)

      ;; (define-key global-map [remap find-tag]              'helm-etags-select) ;; 
      ;; (define-key global-map [remap xref-find-definitions] 'helm-etags-select) ;; 不知道为什么会屏蔽local key
      (global-set-key (kbd "<f12>") 'helm-etags-select)
      (global-set-key (kbd "M-.") 'helm-etags-select) ; 除了elisp和ggtags支持的都用这个
      (global-set-key (kbd "<C-down-mouse-1>") 'helm-etags-select)

      ;; helm-do-grep-ag 这个好像有bug啊，在helm-swoop就搜索不到
      (setq
       ;; smart case跟emacs类似，默认读gitignore实在不习惯
       helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number --no-ignore %s %s %s"
       ;; helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'") ;; 貌似没什么用
       helm-move-to-line-cycle-in-source t ; 使到顶尾时可以循环，缺点是如果有两个列表，下面那个没法过去了
       helm-echo-input-in-header-line t ; 这个挺awesome的，不使用minibuffer，在中间眼睛移动更小
       helm-split-window-in-side-p t ; 不然的话，如果有两个窗口，它就会使用另一个窗口。另一个是横的还好，竖的就不习惯了
       helm-ff-file-name-history-use-recentf t
       helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
       helm-buffers-fuzzy-matching t    ; 这种细化的fuzzy最喜欢了
       helm-recentf-fuzzy-match    t
       helm-follow-mode-persistent t
       helm-allow-mouse t
       )
      
      (with-eval-after-load 'helm
	(helm-mode 1)

	;; (defadvice start-file-process-shell-command (before my-start-file-process-shell-command activate)
	;;   (message (ad-get-arg 2)))
	
	;; 光标移动时也自动定位到所在位置
	(push "Occur" helm-source-names-using-follow) ; 需要helm-follow-mode-persistent为t
	
	;; 参考swiper设置颜色，这个一改瞬间感觉不一样
	(custom-set-faces
	 '(helm-selection ((t (:inherit isearch-lazy-highlight-face :underline t :background "#3F3F3F")))) ; underline好看，:background nil去不掉背景色，就改成zenburn同色了
	 '(helm-selection-line ((t (:inherit isearch-lazy-highlight-face :underline t :background "#3F3F3F"))))
	 ;;helm-match-item 
	 )
	(define-key helm-map (kbd "<f1>") 'nil)
	(define-key helm-map (kbd "C-1") 'keyboard-escape-quit)

	(define-key helm-map (kbd "C-h") 'nil)
	(define-key helm-map (kbd "C-t") 'nil) ; c-t是翻转样式
	(define-key helm-map (kbd "C-t") 'helm-toggle-visible-mark)
	(define-key helm-map (kbd "C-v") 'nil)
	(define-key helm-map (kbd "<f4>") 'helm-next-line)
	(define-key helm-map (kbd "<S-f4>") 'helm-previous-line)
	;; (define-key helm-map (kbd "C-s") 'helm-next-line) ;; 这个还是留给helm-occur或者helm-ff-run-grep
	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
	(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
	(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
	;; (define-key helm-map (kbd "TAB") 'helm-next-line)
	;; (define-key helm-map (kbd "<backtab>") 'helm-previous-line)
	;;(define-key helm-map (kbd "C-w") 'ivy-yank-word) ; 居然不默认

	;; 还是这样舒服，要使用原来功能请C-z。helm mode太多了，用到哪些再来改
	(define-key helm-map (kbd "C-s") 'helm-next-line) ; 原无
	(define-key helm-map (kbd "C-r") 'helm-previous-line) ; 原history
	(define-key helm-find-files-map (kbd "C-s") 'helm-next-line) ;原无
	(define-key helm-find-files-map (kbd "C-r") 'helm-previous-line) ;原history
	(define-key helm-buffer-map (kbd "C-s") 'helm-next-line) ;原occur
	(define-key helm-buffer-map (kbd "C-r") 'helm-previous-line) ; 原history

	;; helm-locate即everything里打开所在位置
	(define-key helm-generic-files-map (kbd "C-x C-d")
	  (lambda ()
	    (interactive)
	    (with-helm-alive-p
	      (helm-exit-and-execute-action (lambda (file)
					      (require 'w32-browser)
					      (w32explore file)
					      )))))
	
	(when helm-echo-input-in-header-line
	  (add-hook 'helm-minibuffer-set-up-hook
		    'helm-hide-minibuffer-maybe))        
	)

      (defadvice completing-read (before my-completing-read activate)
	(helm-mode 1))
      (global-set-key (kbd "M-m") 'helm-imenu-in-all-buffers)
      )
  (progn
    ;; ivy确实好用，就是公司win7机子有时c-x c-f会有延迟
    (add-to-list 'load-path "~/.emacs.d/packages/swiper")
    (autoload 'swiper "swiper" nil t)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (autoload 'ivy-resume "ivy" nil t)
    (autoload 'ivy-mode "ivy" nil t)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (autoload 'counsel-M-x "counsel" nil t)
    (autoload 'counsel-find-file "counsel" nil t)
    (autoload 'counsel-describe-function "counsel" nil t)
    (autoload 'counsel-describe-variable "counsel" nil t)
    (autoload 'counsel-find-library "counsel" nil t)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (with-eval-after-load 'ivy
      (ivy-mode 1)
      (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line)
      (define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line)
      (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-next-line)
      (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-previous-line)
      (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word) ; 居然不默认
      )
    (defadvice completing-read (before my-completing-read activate)
      (ivy-mode 1))
    ))


;; 自动indent
(autoload 'aggressive-indent-mode "aggressive-indent" nil t)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(with-eval-after-load 'aggressive-indent
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line))))))

;; hydra使用autoload的方式 https://github.com/abo-abo/hydra/issues/149
(defun hydra-hideshow/body()
  (interactive)
  (require 'hydra)
  (funcall (defhydra hydra-hideshow ()
	     "
_h_: hide block _s_: show block
_H_: hide all   _S_: show all 
_i_: hide comment _q_uit
"
	     ("h" hs-toggle-hiding-all nil :color blue)
	     ("s" hs-show-block nil :color blue)
	     ("H" hs-toggle-hiding nil :color blue)
	     ("S" hs-show-all nil :color blue)
	     ("i" hs-hide-initial-comment-block nil :color blue)
	     ("q" nil "nil" :color blue))))
(global-set-key (kbd "C-c h") 'hydra-hideshow/body) ; bug:最后一个第3参数必须带名字，否则上面最后一行不显示

(defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  ;;;###autoload
(defun hs-toggle-hiding-all () "Toggle hideshow all."
       (interactive)
       (setq-local my-hs-hide (not my-hs-hide))
       (if my-hs-hide
	   (hs-hide-all)
	 (hs-show-all)))

(defun copy-buffer-name (choice &optional use_win_path)
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?a)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (if use_win_path
	  (let ((win-path (replace-regexp-in-string "/" "\\\\" new-kill-string)))
	    (message "%s copied" win-path)
	    (kill-new win-path))
	(message "%s copied" new-kill-string)
	(kill-new new-kill-string)))))
(defun hydra-copybf4/body()
  (interactive)
  (require 'hydra)
  (funcall (defhydra hydra-copybf4 ()
	     "
Copy Buffer Name: _f_ull, _d_irectoy, n_a_me ?
"
	     ("f" (copy-buffer-name ?f) nil :color blue)
	     ("d" (copy-buffer-name ?d) nil :color blue)
	     ("a" (copy-buffer-name ?a) nil :color blue)
	     ("q" nil "" :color blue))))
(global-set-key (kbd "C-4") 'hydra-copybf4/body)
(defun hydra-copybf3/body() ()
       (interactive)
       (require 'hydra)
       (funcall (defhydra hydra-copybf3 ()
		  "
Copy Buffer Name: _f_ull, _d_irectoy, n_a_me ?
"
		  ("f" (copy-buffer-name ?f t) nil :color blue)
		  ("d" (copy-buffer-name ?d t) nil :color blue)
		  ("a" (copy-buffer-name ?a t) nil :color blue)
		  ("q" nil "" :color blue))))
(global-set-key (kbd "C-3") 'hydra-copybf3/body)

;; 为了延迟加载hideshowvis也是够拼了
(defvar hideshowvis-inited nil "")
(defun hideshowvis-setting()
  (unless hideshowvis-inited
    (setq hideshowvis-inited t)
    ;; 配合hideshow，更好地区分被折叠的代码
    (require 'hideshowvis)
    (custom-set-variables '(hideshowvis-ignore-same-line nil)) ; hideshowvis-enable会卡，必须用这个
    (hideshowvis-symbols) ;; 显示+号和折叠行数
    ;; 抄这里的配置http://www.cnblogs.com/aaron2015/p/4882652.html，没想到最适合zenburn主题
    (custom-set-faces
     '(hs-fringe-face ((t (:foreground "#afeeee" :box (:line-width 2 :color "grey75" :style released-button))))) ; 那个+号
     '(hs-face ((t (:background "#444" :box t)))) ; 替换原来的...，很清楚地显示是被折叠了，并且显示被折叠了多少行
     '(hideshowvis-hidable-face ((t (:foreground "#2f4f4f")))) ; 这样设置那个难看的-就看不清了
     )
    (add-hook 'prog-mode-hook 'hideshowvis-enable) ;; 其实不需要这个也可以，这个可以让点击+号就展开，方便鼠标操作
    (hideshowvis-enable) ;; fix当前buffer不能点击+
    ))
(defadvice hs-hide-all (before my-hs-hide-all activate)
  (hideshowvis-setting))
(defadvice hs-hide-block (before my-hs-hide-block activate)
  (hideshowvis-setting))

(load-theme 'zenburn t)
