;; Time-stamp: <2021-11-05 10:11:37 lynnux>
;; 非官方自带packages的设置
;; benchmark: 使用profiler-start和profiler-report来查看会影响emacs性能，如造成卡顿的命令等
;; 一般都是eldoc会卡，如ggtag和racer mode都是因为调用了其它进程造成卡的

(add-to-list 'load-path
	     "~/.emacs.d/packages")

;; !themes要放到最后，内置theme查看 M-x customize-themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(autoload 'defhydra "hydra" nil t)
(global-set-key (kbd "C-x f") 'hydra-find-file-select)
(global-set-key (kbd "C-c o") 'hydra-occur-select)
(global-set-key (kbd "C-c h") 'hydra-hideshow-select) ; bug:最后一个第3参数必须带名字，否则上面最后一行不显示

(defun hydra-find-file-select ()
  (interactive)
  (unless (functionp 'hydra-find-file/body)
    (defhydra hydra-find-file ()
      "
_f_: file cache     _e_: helm locate
_c_: file changed   _v_: file visited
_a_: file at point  _q_uit
"
      ("f" file-cache-switch-file nil :color blue)
      ("e" helm-locate nil :color blue)
      ("c" files-recent-changed nil :color blue)
      ("v" files-recent-visited nil :color blue)
      ("a" find-file-at-point nil :color blue)
      ("q" nil "nil" :color blue))
    )
  (funcall 'hydra-find-file/body)
  )

(defun hydra-occur-select ()
  (interactive)
  (unless (functionp 'hydra-occur/body)
    (defhydra hydra-occur ()
      "
_a_: all   _t_: type
_m_: mode  _RET_: this buffer
_q_uit
"
      ("a" all-occur nil :color blue)
      ("t" type-occur nil :color blue)
      ("m" mode-occur nil :color blue)
      ("RET" occur nil :color blue)
      ("q" nil "nil" :color blue))
    )
  (funcall 'hydra-occur/body)
  )

;; hydra使用autoload的方式 https://github.com/abo-abo/hydra/issues/149
(defun hydra-hideshow-select()
  (interactive)
  (hs-minor-mode)
  (unless (functionp 'hydra-hideshow/body)
    (defhydra hydra-hideshow ()
      "
_h_: hide all toggle     _s_: show block
_H_: hide block toggle   _S_: show all 
_c_: hide comment        _q_uit
"
      ("h" hs-toggle-hiding-all nil :color blue)
      ("s" hs-show-block nil :color blue)
      ("H" hs-toggle-hiding nil :color blue)
      ("S" hs-show-all nil :color blue)
      ("c" hs-hide-initial-comment-block nil :color blue)
      ("q" nil "nil" :color blue)))
  (funcall 'hydra-hideshow/body))

(defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  ;;;###autoload
(defun hs-toggle-hiding-all () "Toggle hideshow all."
       (interactive)
       (setq-local my-hs-hide (not my-hs-hide))
       (if my-hs-hide
	   (hs-hide-all)
	 (hs-show-all)))

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
	'("~/.emacs.d/packages/yasnippet/yasnippet-snippets-master/snippets"
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

;; 参考easy-kill重写了C-TAB
(defvar myswitch-buffer-list nil)
(defvar myswitch-buffer-current nil)
(defun myswitch-activate-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-tab>") 'myswitch_next_buffer)
    (define-key map (if (string-equal system-type "windows-nt")
			(kbd "<C-S-tab>")
		      (kbd "<C-S-iso-lefttab>"))
      'myswitch_prev_buffer)
    (set-transient-map ;; 关键函数！
     map
     t ;; 继续keymap
     (lambda ()
       ;; 退出时真正切换
       (switch-to-buffer (nth myswitch-buffer-current myswitch-buffer-list)); 不带t，最终切换过去
       )
     )))
(defun myswitch_next_buffer()
  (interactive)
  (incf myswitch-buffer-current)
  (if (>= myswitch-buffer-current (length myswitch-buffer-list))
      (setq myswitch-buffer-current 0))
  (switch-to-buffer (nth myswitch-buffer-current myswitch-buffer-list) t) ; 第二个参数表明不真正切换
  )
(defun myswitch_prev_buffer()
  (interactive)
  (decf myswitch-buffer-current)
  (if (< myswitch-buffer-current 0)
      (setq myswitch-buffer-current (1- (length myswitch-buffer-list))))
  (switch-to-buffer (nth myswitch-buffer-current myswitch-buffer-list) t) ; 第二个参数表明不真正切换
  )
(defun myswitch_next_buffer_start(&optional backward)
  (interactive)
  (setq myswitch-buffer-list (copy-sequence (if (featurep 'tabbar-ruler) 
						(ep-tabbar-buffer-list)
					      (buffer-list))
					    ))
  (message "C-tab to cycle forward, C-S-tab backward...")
  (setq myswitch-buffer-current 0)
  (if backward
      (myswitch_prev_buffer)
    (myswitch_next_buffer))
  (myswitch-activate-keymap)
  )
(global-set-key (kbd "<C-tab>")
		'myswitch_next_buffer_start
		)
(global-set-key (if (string-equal system-type "windows-nt")
		    (kbd "<C-S-tab>")
		  (kbd "<C-S-iso-lefttab>"))
		(lambda () 
		  (interactive)
		  (myswitch_next_buffer_start t))
		)

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
	    (eq major-mode 'fundamental-mode)
	    (eq major-mode 'helm-major-mode)
	    )
    (highlight-symbol-mode)) 
  )
(define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode highlight-symbol-mode-on)
(global-highlight-symbol-mode 1)
;; (require 'highlight-symbol-scroll-out)
;; (global-highlight-symbol-scroll-out-mode)
;; 高亮选中项
(defadvice highlight-symbol-get-symbol (around my-highlight-symbol-get-symbol activate)
  (if (use-region-p)
      (setq ad-return-value (buffer-substring (region-beginning) (region-end)))
    ad-do-it
    )
  )

(require 'cursor-chg)
(change-cursor-mode )
(setq curchg-default-cursor-color "red3") ; 无法设置cursor的foreground

;;crosshairs不好用，只要vline就行了		
(autoload 'vline-mode "vline" nil t)
(global-set-key [(control ?|)] 'vline-mode)

;; (global-hl-line-mode t)

(if nil
    ;; auto complete 很可惜已停止维护了，暂时保留吧
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
    (dotimes (i 10)
      (define-key company-active-map (read-kbd-macro (format "C-%d" i)) 'company-complete-number))
    (setq ;company-idle-delay 0.5 ; 为0的话太卡了，输入就会卡住，默认就行了
     company-minimum-prefix-length 2
     company-require-match nil
     company-dabbrev-ignore-case nil
     company-dabbrev-downcase nil
     company-show-numbers t)
    ;; 下载TabNine.exe拷贝到~\.TabNine\2.2.2\x86_64-pc-windows-gnu
    (with-eval-after-load 'dash
      (add-to-list 'load-path "~/.emacs.d/packages/company-mode/company-tabnine")
      (require 'company-tabnine)
      (add-to-list 'company-backends #'company-tabnine)
      )
    (global-set-key (kbd "<C-return>") 'company-indent-or-complete-common)
    (global-set-key (kbd "<M-return>") 'company-indent-or-complete-common)
    ;; (require 'company-posframe) ;; 挺好，但感觉对启动有影响
    ;; (company-posframe-mode 1)
    ;; (setq company-posframe-quickhelp-delay 0.1)
    (require 'company-ctags)
    (company-ctags-auto-setup)
    )
  )


;; 一来就加载mode确实挺不爽的，还是用这个了
(require 'wcy-desktop)
(wcy-desktop-init)
(add-hook 'emacs-startup-hook
          (lambda ()
            (ignore-errors
              (wcy-desktop-open-last-opened-files))))
(defadvice wcy-desktop-load-file (after my-wcy-desktop-load-file activate)
  (setq buffer-undo-list nil)
  );; 解决undo-tree冲突

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
  ;;(setq c-auto-newline 1)
  (c-set-style "stroustrup")
  (gtags-settings)
  ;; (define-key c-mode-base-map (kbd "C-h") 'c-electric-backspace) ;修复C-h没有这个效果
  (local-set-key (kbd "C-c C-c") 'comment-eclipse)
  (setq clang-format-style "webkit") ; 只有这个默认tab是4个空格
  ;; (local-set-key [(meta f8)] 'clang-format-auto)
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
(add-to-list 'jl-insert-marker-funcs "xref-find-definitions")
(global-set-key [(control ?\,)] 'my-save-pos) ; 手动触发记录位置
(defun my-save-pos()
  (interactive)
  )
(add-to-list 'jl-insert-marker-funcs "my-save-pos")


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
  ;; (add-hook 'after-save-hook #'gtags-update-hook)
  (require 'ggtags)
  (ggtags-mode 1)
  (add-hook 'ggtags-global-mode-hook (lambda()
				       (define-key ggtags-global-mode-map "n" 'next-line)
				       (define-key ggtags-global-mode-map "p" 'previous-line)
				       ))
  (setq ggtags-global-abbreviate-filename nil) ; 不缩写路径
  (defadvice ggtags-eldoc-function (around my-ggtags-eldoc-function activate)); eldoc没有开关，只有重写它的函数了
					;  (customize-set-variable 'ggtags-highlight-tag nil) ; 禁止下划线 setq对defcustom无效！ 测试是eldoc导致提示process sentinel的
  ;; (local-set-key (kbd "<f12>") 'ggtags-find-tag-dwim)
  ;; (local-set-key (kbd "C-.") 'ggtags-find-tag-dwim)
  ;; (local-set-key (kbd "<C-down-mouse-1>") 'ggtags-find-tag-dwim) ; CTRL + 鼠标点击，很好用
  ;;(turn-on-eldoc-mode) ; 会卡
  )

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

;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
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
;; (global-set-key (kbd "<f5>") 'smart-compile-run) ; 用projetile的f5 x s代替

;;; TODO 如果编译过其他目录后，另一个目录C-F7时当前目录没有变，必须C-u F7重新配置
;; compile，加入了单独编译某个文件
(setq compilation-auto-jump-to-first-error nil ; 自动跳到错误，这个在只有warning时相当烦！
      compilation-scroll-output t)
(autoload 'smart-compile "smart-compile" nil t)
(autoload 'smart-compile-c-compile "smart-compile" nil t)
(global-set-key [f7] 'smart-compile)
(global-set-key [(shift f7)] 'smart-compile-regenerate)
(global-set-key [(control f7)] 'smart-compile-c-compile)
(defun smart-compile-regenerate()
  (interactive)
  (smart-compile 4))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (if (string-match "finished" string)
	     ;; 找不到如何获取代码buffer的方法，只有在compilation里变色了
	     (progn (face-remap-add-relative
		     'mode-line-inactive '((:foreground "ivory" :background "SeaGreen") mode-line))
		    (face-remap-add-relative
		     'mode-line '((:foreground "ivory" :background "SeaGreen") mode-line))
		    t)
	   (progn (face-remap-add-relative
		   'mode-line-inactive '((:foreground "ivory" :background "DarkOrange2") mode-line))
		  (face-remap-add-relative
		   'mode-line '((:foreground "ivory" :background "DarkOrange2") mode-line))
		  nil))
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (delete-windows-on buffer)
    ;; (run-with-timer 1 nil
    ;; 		    (lambda (buf)
    ;; 		      (bury-buffer buf)
    ;; 		      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
    ;; 		    buffer)  ;; 这个左右两个窗口并不会退出窗口
    (setq current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame) ; 最后这两句好像没作用，保留吧
    ))
(with-eval-after-load 'compile (add-to-list 'compilation-finish-functions
					    'bury-compile-buffer-if-successful))

;; expand-region被easy-kill的easy-mark替换了，但要保留会被调用
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
;; (autoload 'er/expand-region "expand-region" nil t)
;; (autoload 'er/contract-region "expand-region" nil t)
;; (global-set-key "\C-t" 'er/expand-region)
;; (global-set-key (kbd "C-S-t") 'er/contract-region)

;; see https://github.com/magnars/expand-region.el/issues/229
(with-eval-after-load "expand-region"
  (global-set-key (kbd "C-q") #'(lambda (arg)
				  (interactive "P")
				  (setq transient-mark-mode t)
				  (set-mark-command arg))))
;;; 解决被(setq show-paren-style 'expression)覆盖的问题
(defadvice show-paren-function (around not-show-when-expand-region activate)
  (if (and (or (eq major-mode 'lisp-interaction-mode) (eq major-mode 'emacs-lisp-mode))
	   (memq last-command '(er/expand-region er/contract-region easy-mark easy-kill-er-expand easy-kill-er-unexpand)))
      (progn
	(setq show-paren-style 'parenthesis)
	ad-do-it
	(setq show-paren-style 'expression)
	)
    ad-do-it))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(modify-coding-system-alist 'file "\\.lua\\'" 'utf-8) ; 不要带BOM，BOM是ms特有的

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
(global-unset-key (kbd "<M-down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-M-<mouse-1>") 'mc/unmark-next-like-this) ; 取消光标以下的mark
(global-set-key (kbd "M-S-<mouse-1>") 'mc/unmark-previous-like-this) ;取消光标以上的mark
(global-set-key (kbd "M-<wheel-up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<wheel-down>") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-S-t") 'mc/edit-lines)  ;居然不支持同行的range
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "<f8>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(with-eval-after-load 'multiple-cursors
  (define-key mc/keymap (kbd "C-v") nil)
  (define-key mc/keymap (kbd "RET") 'multiple-cursors-mode))

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
(modify-coding-system-alist 'file "\\.rs\\'" 'utf-8-with-signature) ; 带中文必须这个编码，干脆就默认

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
(with-eval-after-load 'neotree (define-key neotree-mode-map (kbd "C-l") 'neotree-select-up-node))

;; 有点类似自动截段长行的效果，默认绑定到m-q，elisp-mode无效
(autoload 'unfill-toggle "unfill" nil t)
(global-set-key [remap fill-paragraph] #'unfill-toggle)

(require 'hungry-delete)
(global-hungry-delete-mode)
(setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace

(add-to-list 'load-path "~/.emacs.d/packages/smartparens")
(require 'smartparens-config)
;; (sp-use-paredit-bindings)
(sp-use-smartparens-bindings)
(define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "M-a") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "M-e") 'sp-forward-sexp)
;; 补充paredit的M-(，其它模式由于pair不止(所以不可用
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "(" nil :bind "M-(") ; 这个其实是包装成sp-wrap了
(set-default 'sp-autoskip-closing-pair 'always)
;; Don't kill the entire symbol on C-k
(set-default 'sp-hybrid-kill-entire-symbol nil)
;; 参考doom设置
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)
(setq sp-max-prefix-length 25)
(setq sp-max-pair-length 4)
(smartparens-global-strict-mode)
;;(show-smartparens-global-mode) ;; Show parenthesis 好像没什么作用了?
;;(defadvice sp-show--pair-echo-match (around my-sp-show--pair-echo-match activate)) ; 屏蔽 Matches:消息

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

(if t
    ;; 用helm可以抛弃好多包啊，有imenu-anywhere，popup-kill-ring，ripgrep，minibuffer-complete-cycle，etags-select那三个，everything(helm-locate)
    ;; 参考helm作者的配置https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
    (progn
      (icomplete-mode 1)		; 这个跟swiper冲突，helm没问题
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
      (autoload 'ansi-color-apply-sequence "ansi-color" nil t) ; F2时要被helm-lib使用

      (global-set-key (kbd "C-x C-f") 'helm-find-files) ; 这个操作多文件非常方便！ C-c ?仔细学学！
      (global-set-key (kbd "C-x C-b") 'helm-buffers-list) ; 比原来那个好啊
      (global-set-key (kbd "C-c C-r") 'helm-resume)	  ;继续刚才的session
      (global-set-key (kbd "<f6>") 'helm-resume)

      ;; (define-key global-map [remap find-tag]              'helm-etags-select) ;; 
      ;; (define-key global-map [remap xref-find-definitions] 'helm-etags-select) ;; 不知道为什么会屏蔽local key
      (global-set-key [(control f2)] (lambda () (interactive)
				       (require 'vc)
				       (helm-fd-1 (or (vc-find-root "." ".git") (helm-current-directory))))) ; 用fd查找文件，有git的话从git根目录查找
      (autoload 'helm-fd-1 "helm-fd" nil t) ; F2时要被helm-lib使用

      ;; helm-do-grep-ag 这个好像有bug啊，在helm-swoop就搜索不到
      (setq
       ;; smart case跟emacs类似，默认读gitignore实在不习惯
       helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number --no-ignore %s %s %s"
       ;; helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'") ;; 貌似没什么用
       helm-move-to-line-cycle-in-source t ; 使到顶尾时可以循环，缺点是如果有两个列表，下面那个用C-o或者M->切换过去
       helm-echo-input-in-header-line t ; 这个挺awesome的，不使用minibuffer，在中间眼睛移动更小
       helm-split-window-in-side-p t ; 不然的话，如果有两个窗口，它就会使用另一个窗口。另一个是横的还好，竖的就不习惯了
       helm-ff-file-name-history-use-recentf t
       helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
       helm-buffers-fuzzy-matching t    ; 这种细化的fuzzy最喜欢了
       helm-recentf-fuzzy-match    t
       helm-follow-mode-persistent t
       helm-allow-mouse t
       helm-grep-input-idle-delay 0.02 	; 后来加的默认0.6，让搜索有延迟
       )
      
      (with-eval-after-load 'helm
	(helm-mode 1)

	;; (defadvice start-file-process-shell-command (before my-start-file-process-shell-command activate)
	;;   (message (ad-get-arg 2)))
	
	;; 光标移动时也自动定位到所在位置
	(push "Occur" helm-source-names-using-follow) ; 需要helm-follow-mode-persistent为t
	(push "RG" helm-source-names-using-follow)

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
	;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
	;; tab自动补全光标所在的symbol，TODO：本来想只对空输入有效，但是判断空有问题
	(define-key helm-map (kbd "<tab>") (lambda ()(interactive)
					     (let ((src (helm-get-current-source)))
					       (if (member (assoc-default 'name src)
							   (list "RG" "Occur"))
						   (let ((symbol-under-cursor (with-helm-current-buffer
									       (thing-at-point 'symbol))))
						     (if symbol-under-cursor
							 (helm-set-pattern symbol-under-cursor)
						       )
						     )
						 (call-interactively 'helm-execute-persistent-action)
						 )
					       )
					     ))
	
	(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
	(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
	;; (define-key helm-map (kbd "TAB") 'helm-next-line)
	;; (define-key helm-map (kbd "<backtab>") 'helm-previous-line)
	;;(define-key helm-map (kbd "C-w") 'ivy-yank-word) ; 居然不默认

	;; 还是这样舒服，要使用原来功能请C-z。helm mode太多了，用到哪些再来改
	(define-key helm-map (kbd "C-s") 'helm-next-line) ; 原来的是在当前行里查找
	(define-key helm-find-files-map (kbd "C-s") 'helm-next-line) ; 原来的是在当前行里查找
	(define-key helm-buffer-map (kbd "C-s") 'helm-next-line) ;原occur

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
      ;; 这功能不好写，将就用总比没有好吧
      (defun helm-grep-search-parent-directory ()
	(interactive)
	(helm-run-after-exit (lambda ()
			       (let* ((parent (file-name-directory (directory-file-name default-directory)))
				      (default-directory parent))
				 (helm-grep-ag (expand-file-name parent) nil)))))
      (defun helm-show-search()
	(interactive)
	(yank)
	)
      
      (with-eval-after-load 'helm-grep
	(define-key helm-grep-map (kbd "DEL") 'nil) ; helm-delete-backward-no-update有延迟
	(define-key helm-grep-map (kbd "C-l") 'helm-grep-search-parent-directory)
	;; (define-key helm-grep-map (kbd "<tab>") 'helm-show-search) ; TODO: 空输入的时候，自动补全光标下的单词
	)
      (with-eval-after-load 'helm-find
	(define-key helm-find-map (kbd "DEL") 'nil) ; helm-delete-backward-no-update有延迟
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
    (global-set-key [f2] 'counsel-rg) 	; Great! 貌似是解决了rg卡死的问题了，不频繁启动rg的方式https://github.com/abo-abo/swiper/pull/2552
    (autoload 'counsel-rg "counsel" nil t)
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
      (define-key ivy-minibuffer-map (kbd "C-v") 'nil)
      (setq ivy-more-chars-alist '((counsel-rg . 1) (t . 3)))
      )
    ;; (with-eval-after-load 'counsel
    ;;   (append counsel-rg-base-command "--no-ignore")) ; 好像不用加，不会搜索ignore的
    (defadvice completing-read (before my-completing-read activate)
      (ivy-mode 1))
    (add-to-list 'sp-ignore-modes-list 'minibuffer-mode) ; C-K不自然在minibuffer里禁用smartparens
    ))


;; 自动indent
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
(with-eval-after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line))))))


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

;; go lang
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))
;; (with-eval-after-load 'go-mode (add-hook 'before-save-hook 'gofmt-before-save))

;; magit
(add-to-list 'load-path "~/.emacs.d/packages/magit/magit-master/lisp")
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(autoload 'magit "magit" nil t)
(global-set-key (kbd "C-c C-c") 'magit)

;;; zig mode
(autoload 'zig-mode "zig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(modify-coding-system-alist 'file "\\.zig\\'" 'utf-8-with-signature) ; 强制zig文件视为utf8，否则有中文显示问题
(setq zig-format-on-save nil)
(with-eval-after-load 'zig-mode (add-hook 'zig-mode-hook (lambda () (local-set-key [(meta f8)] 'zig-format-buffer))))

;; dumb-jump，使用rg查找定义！需要定义project root，添加任意这些文件都可以：.dumbjump .projectile .git .hg .fslckout .bzr _darcs .svn Makefile PkgInfo -pkg.el.
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(autoload 'dumb-jump-xref-activate "dumb-jump" nil t)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; 不用xref，用helm，但这个C-C C-F切换follow mode有问题，暂时不管了
(global-set-key (kbd "<f12>") 'xref-find-definitions) 
(global-set-key (kbd "C-.") 'xref-find-definitions) ; 除了ggtags支持的都用这个
(global-set-key (kbd "<C-down-mouse-1>") 'xref-find-definitions)
(with-eval-after-load 'dumb-jump
  (defadvice dumb-jump-get-project-root (before my-dumb-jump-get-project-root activate)
    ;; arount设置有问题
    (setq dumb-jump-default-project default-directory) ; 默认设置为当前目录
    )
  )

;; easy-kill，添加类似vim里yi/a的东西！
(add-to-list 'load-path "~/.emacs.d/packages/easy-kill")
(global-set-key [remap kill-ring-save] 'easy-kill)
(autoload 'easy-kill "easy-kill" nil t)
(autoload 'easy-mark "easy-kill" nil t)
(global-set-key "\C-t" 'easy-mark) ; 替换expand-region
(with-eval-after-load 'easy-kill
  (require 'easy-kill-er)
  (require 'extra-things)
  (require 'easy-kill-extras)
  (setq easy-kill-try-things '(line)) ; 只复制line
  (setq easy-mark-try-things '(word sexp)) ; word优先，特别是有横杠什么都时候
  ;; (define-key easy-kill-base-map (kbd "C-r") 'easy-kill-er-expand) ; 不要再定义了，避免mark时不能复制
  (define-key easy-kill-base-map (kbd "C-t") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "C-S-t") 'easy-kill-er-unexpand)
  (define-key easy-kill-base-map (kbd "n") 'easy-kill-expand)
  (define-key easy-kill-base-map (kbd "p") 'easy-kill-shrink)
  (define-key easy-kill-base-map (kbd "C-d") 'easy-kill-region)
  (define-key easy-kill-base-map (kbd "C-h") 'easy-kill-region)
  (autoload 'er--expand-region-1 "expand-region" nil t)
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?a buffer ""))
  ;; (add-to-list 'easy-kill-alist '(?< buffer-before-point "")) ;; 拿给copy<>感觉更常用
  ;; (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?W  WORD " ") t)
  (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?q  quoted-string "") t)
  (add-to-list 'easy-kill-alist '(?Q  quoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?>  angles-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?<  angles-pair "\n") t)
  )

;; projectile搜索配合rg
(add-to-list 'load-path "~/.emacs.d/packages/projectile")
(defun invoke_projectile ()
  (interactive)
  (require 'projectile)
  (projectile-mode +1)
  (set-transient-map projectile-command-map) ; 继续后面的key，缺点是首次的?不能用
  )
(global-set-key (kbd "C-;") 'invoke_projectile)
(global-set-key (kbd "<f5>") 'invoke_projectile) ; 原功能可以用f5 x s

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-;") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "<f5>") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "<f5> s-") nil) ; Undefine prefix binding https://emacs.stackexchange.com/questions/3706/undefine-prefix-binding
  (define-key projectile-mode-map (kbd "<f5> s") #'projectile-ripgrep) ; 对C-; s同样生效
  )

;; rg，这个还挺好用的，带修改搜索的功能(需要buffer可写)，更多功能看菜单
(global-set-key (kbd "C-S-f") 'rg-dwim)
(autoload 'rg-dwim "rg" nil t)

;; which-key确实好用
(require 'which-key)
(which-key-mode)

;; 这是需要最后加载
(load-theme 'zenburn t)
