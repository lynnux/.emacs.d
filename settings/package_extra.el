;; Time-stamp: <2022-04-30 16:21:03 lynnux>
;; 非官方自带packages的设置
;; benchmark: 使用profiler-start和profiler-report来查看会影响emacs性能，如造成卡顿的命令等
;; 拖慢gui测试：C-x 3开两个窗口，打开不同的buffer，C-s搜索可能出现比较多的词，测试出doom modeline和tabbar ruler比较慢

;; use-package的好处之一是defer可以设置延迟几秒加载！光yas一项就提升了启动速度
;; :load-path不是延迟设置
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/packages/use-package")
  (require 'use-package))

(add-to-list 'load-path
	         "~/.emacs.d/packages")

;; F1 v查看变量 sanityinc/require-times，正常一页就显示完了，目前11个包
;; 有个几年前的封装没有用，直接用的原版的 https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
(use-package init-benchmarking
  :disabled
  :config
  (add-hook 'after-init-hook (lambda ()
                               ;; 启动后就不再需要了，会把helm等算进去
                               (advice-remove 'require 'sanityinc/require-times-wrapper)
                               ))
  )

;; !themes要放到最后，内置theme查看 M-x customize-themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defvar use-my-face nil)

;; 消除mode line上的minor提示字符
(use-package diminish
  :init
  (with-eval-after-load 'eldoc
    (diminish 'eldoc-mode))
  :commands(diminish))

(autoload 'defhydra "hydra" nil t)

(defhydra hydra-bookmark ()
  "
_m_: set     _b_: jump
_l_: list
_q_uit
"
  ("m" bookmark-set nil :color blue)
  ("b" bookmark-jump nil :color blue) ;; 这个只有session才有的，recentf没有
  ("l" bookmark-bmenu-list nil :color blue)
  ("q" nil "nil" :color blue))
(global-set-key (kbd "C-c b") 'hydra-bookmark/body)

;; helm M-x也会导致dired被加载，加载tree-sittr也会，只能在scratch里执行(featurep 'dired+)看
(use-package dired
  ;;:disabled
  :load-path "~/.emacs.d/packages/dired"
  :init
  ;; 用dired+自带的
  ;; (use-package diredfl
  ;;   :hook(dired-mode . diredfl-mode)) ;; 对自定义time处理有bug
  (put 'dired-find-alternate-file 'disabled nil) ;; 避免使用该函数时提示
  (global-set-key [remap dired] 'dired-jump) ;; 直接打开buffer所在目录，无须确认目录
  :commands(dired dired-jump)
  :config

  (when (string-equal system-type "windows-nt")
    (setq ls-lisp-use-insert-directory-program t) ;; 默认用lisp实现的ls
    ;; 真正实现是在files.el里的insert-directory
    (defadvice dired-insert-directory (around my-dired-insert-directory activate)
      (let ((old coding-system-for-read))
	    (setq coding-system-for-read 'utf-8) ;; git里的ls是输出是utf-8
	    ad-do-it
	    (setq coding-system-for-read old)
	    ))
    (define-key dired-mode-map (kbd "C-x C-d") 'dired-w32explore)
    (define-key dired-mode-map (kbd "<C-return>") 'dired-w32-browser) ;; 使用explorer打开
    (define-key dired-mode-map (kbd "<C-enter>") 'dired-w32-browser) ;; 使用explorer打开
    )

  (setq dired-listing-switches "-alh --group-directories-first --time-style \"+%Y/%m/%d %H:%M\"") ;; 除了name外其它排序都是目录排最前
  ;; allow dired to delete or copy dir
  (setq dired-recursive-copies (quote always)) ; “always” means no asking
  (setq dired-recursive-deletes (quote top)) ; “top” means ask once
  (setq dired-dwim-target t) ;; 开两个dired的话会自动识别other dired为target
  ;; 不新开buf打开文件和目录，还有个效果是打开文件后自动关闭了dired buffer
  (define-key dired-mode-map [remap dired-find-file] 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-l") (lambda () (interactive) (find-alternate-file ".."))) ;; C-l上级目录
  (define-key dired-mode-map (kbd "l") (lambda () (interactive) (find-alternate-file ".."))) ;; l上级目录
  (define-key dired-mode-map [remap quit-window] 'volatile-kill-buffer)  ;; 关闭dired不然tab有问题
  (define-key dired-mode-map (kbd "C-o") 'avy-goto-word-1)
  ;; dired里面再次C-x d可以设置路径
  (define-key dired-mode-map (kbd "C-x d") (lambda ()(interactive) (call-interactively 'dired)))
  (define-key dired-mode-map " " 'View-scroll-page-forward)
  (define-key dired-mode-map [?\S-\ ] 'View-scroll-page-backward)
  (define-key dired-mode-map "w" 'View-scroll-page-backward)
  (define-key dired-mode-map "W" 'dired-copy-filename-as-kill)
  (define-key dired-mode-map "1" 'delete-other-windows)

  ;; dired-quick-sort
  ;;  (setq dired-quick-sort-suppress-setup-warning t)
  (require 'dired-quick-sort)
  (dired-quick-sort-setup)
  (define-key dired-mode-map "s" 'hydra-dired-quick-sort/body) ;; 不用默认的s

  ;; dired-hacks功能很多
  (use-package dired-filter
    :config
    (defhydra dired-filter-map-select ()
      "
  _._: by extension         _n_: by name
  _f_: by file              _d_: by directory
  _m_: by mode              _e_: by predicate
  _r_: by regexp            _s_: by symlink
  _g_: by garbage           _h_: by dot files
  _o_: by omit              _p_: pop
  _x_: by executable        _|_: or
  _TAB_: transpose          _!_: negate
  _*_: decompose            _S_: save filters
  _/_: pop all              _A_: add saved filters
  _D_: delete saved filters _L_: load saved filters
  _q_uit
  "
      ("TAB" dired-filter-transpose nil :color blue)
      ("!" dired-filter-negate nil :color blue) ;; 配合rename是真牛B啊！
      ("*" dired-filter-decompose nil :color blue)
      ("." dired-filter-by-extension nil :color blue)
      ("/" dired-filter-pop-all nil :color blue)
      ("A" dired-filter-add-saved-filters nil :color blue) ;; 显示不了？
      ("D" dired-filter-delete-saved-filters nil :color blue)
      ("L" dired-filter-load-saved-filters nil :color blue) ;; 不懂
      ("S" dired-filter-save-filters nil :color blue)
      ("d" dired-filter-by-directory nil :color blue)
	  ("e" dired-filter-by-predicate nil :color blue)
	  ("f" dired-filter-by-file nil :color blue)
	  ("g" dired-filter-by-garbage nil :color blue)
	  ("h" dired-filter-by-dot-files nil :color blue)
	  ("m" dired-filter-by-mode nil :color blue)
	  ("n" dired-filter-by-name nil :color blue)
	  ("o" dired-filter-by-omit nil :color blue)
	  ("p" dired-filter-pop nil :color blue)
	  ("r" dired-filter-by-regexp nil :color blue)
	  ("s" dired-filter-by-symlink nil :color blue)
	  ("x" dired-filter-by-executable nil :color blue)
	  ("|" dired-filter-or nil :color blue)
	  ("q" nil "nil" :color blue))
	
	(define-key dired-mode-map "f" 'dired-filter-map-select/body)
	)
  ;; 类似exploer的操作了，不过这个可以同时拷贝不同目录的文件放到ring里
  ;; 但粘贴时也要一个一个粘贴
  (use-package dired-ranger
	:config
	(define-key dired-mode-map "z" 'dired-do-compress-to)
	(define-key dired-mode-map "c" 'dired-ranger-copy) ;; cz交换
	(define-key dired-mode-map "y" 'dired-ranger-paste) ;; 原命令显示文件类型没什么大用
	(define-key dired-mode-map "v" 'dired-ranger-move) ;; view file用o代替
	(defun dired-ranger-clear()
	  (interactive)
	  (let ((count (ring-size dired-ranger-copy-ring))
		    (s 0))
	    (while (< s count )
	      (ring-remove dired-ranger-copy-ring 0)
	      (setq s (1+ s)))))
	;; 显示ring里的文件，可惜没有去重复啊
	(defun dired-ranger-show-ring()
	  (let ((l (ring-elements dired-ranger-copy-ring))
		    (s ""))
	    (dolist (ll l)
	      (dolist (path (cdr ll))
		    (setq s (concat s (file-name-nondirectory path) "\n"))
		    )
	      )
	    (message s)
	    ))
	(defadvice dired-ranger-paste (after my-dired-ranger-paste activate)
	  (dired-ranger-show-ring))
	(defadvice dired-ranger-copy (after my-dired-ranger-copy activate)
	  (dired-ranger-show-ring))
	)
  )


;; 保存cursor位置
(use-package saveplace
  ;; 这里不加defer了，wcy加载时要run它的hook
  :config
  (setq save-place-file (expand-file-name ".saveplace" user-emacs-directory))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode t)
  (defadvice save-place-find-file-hook (after my-save-place-find-file-hook activate)
    (with-current-buffer (window-buffer)
      (recenter))
    ;; 将位置居中，默认是goto-char可能是最下面
    )
  )

(if t
    (use-package session
      ;; session只保存了修改文件的point
      ;; 搜索"Open...Recently Changed"可以确定session-file-alist只保存了修改过的文件列表
      ;; C-x C-/可以跳到最近的修改处
      :init
      ;; 不要禁用saveplace hook，不要保存places功能(session只保存了修改文件的point，用saveplace)
      ;; menus需要保留，不然file-name-history即recent files列表不对
      (setq session-initialize (list 'session 'keys 'menus))
      ;; test (string-match session-name-disable-regexp "COMMIT_EDITMSG")
      (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\|COMMIT_EDITMSG\\)")
      (setq session-save-file-coding-system 'utf-8)
      (setq session-globals-include '((kill-ring 50)
				                      (session-file-alist 100 t)
				                      (file-name-history 300)))
      
      :config
      (add-hook 'after-init-hook 'session-initialize)
      ;; 使用一段时间后，可以自行查看~/.emacs.d/.session里占用太多，然后添加到排除列表里
      (add-to-list 'session-globals-exclude 'rg-history)
      (add-to-list 'session-globals-exclude 'helm-grep-ag-history)
      (add-to-list 'session-globals-exclude 'helm-grep-history)
      (add-to-list 'session-globals-exclude 'helm-occur-history)
      (add-to-list 'session-globals-exclude 'ggtags-global-search-history)
      (add-to-list 'session-globals-exclude 'log-edit-comment-ring)
      (add-to-list 'session-globals-exclude 'helm-source-complex-command-history)
      (add-to-list 'session-globals-exclude 'kmacro-ring)
      )
  (progn
    ;; Save minibuffer history. 不仅仅是minibuffer!
    (use-package savehist
      :defer 0.4
      :config
      (setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
      ;; The maximum length of a minibuffer history list. Once reached, the oldest
      ;; entries get deleted.
      (setq history-length 10000)
      ;; Keep duplicates in the history.
      (setq history-delete-duplicates nil)
      (setq savehist-autosave-interval nil); save on kill only
      ;; Save search entries as well.
      (setq savehist-additional-variables '(search-ring regexp-search-ring))
      (setq savehist-save-minibuffer-history t)
      (savehist-mode t))

    (use-package recentf
      :init
      (setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
      (setq recentf-max-saved-items 500)
      ;; Disable recentf-cleanup on Emacs start, because it can cause problems with
      ;; remote files.
      ;; recentf-auto-cleanup 'never
      (setq recentf-exclude
            '(".cache"
              ".cask"
              "bookmarks"
              "cache"
              "recentf"
              "undo-tree-hist"
              "url"
              "COMMIT_EDITMSG\\'"
              "/ssh:"
              "/sudo:"
              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\|zip\\|xz\\)$"
              "^/tmp/"
              (lambda (file) (file-in-directory-p file package-user-dir))))
      :config
      :hook
      (after-init . recentf-mode)
      )
    (use-package goto-chg
      :commands(goto-last-change)
      :init
      (define-key ctl-x-map [(control ?\/)] 'goto-last-change)
      ;;   (global-set-key [(control ?,)] 'goto-last-change-reverse)
      )
    ))

(global-set-key (kbd "C-x f") 'hydra-find-file-select)
(global-set-key (kbd "C-x C-r") 'files-recent-visited)
(global-set-key (kbd "C-c o") 'hydra-occur-select)
(global-set-key (kbd "C-c h") 'hydra-hideshow-select) ; bug:最后一个第3参数必须带名字，否则上面最后一行不显示

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
  (files-recent-type (if (featurep 'recentf)
                         recentf-list   ; 这个只在exit emacs时更新file-name-history
                       file-name-history)))
;; 目前只发现session有changed file列表
(defun files-recent-changed () 
  (interactive) 
  ;; 需要配合session.el使用
  (files-recent-type (mapcar (lambda (x) (car x)) session-file-alist)))

(defun hydra-find-file-select ()
  (interactive)
  (unless (functionp 'hydra-find-file/body)
    (defhydra hydra-find-file ()
      "
_c_: file changed  _v_: file visited
_a_: file at point _e_: helm locate
_r_: file visited  _p_: file in project
_q_uit
"
      ("e" helm-locate nil :color blue)
      ("c" files-recent-changed nil :color blue) ;; 这个只有session才有的，recentf没有
      ("v" files-recent-visited nil :color blue)
      ("a" find-file-at-point nil :color blue)
      ("r" files-recent-visited nil :color blue)
      ("p" projectile-find-file nil :color blue)
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

;; display-line-numbers是C实现的，最快！
(use-package display-line-numbers
  :commands
  (display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start 3)
  :hook
  (find-file . display-line-numbers-mode)
  )

;;; better C-A C-E
(autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
(autoload 'mwim-end-of-line-or-code "mwim" nil t)
(global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code)
(global-set-key (kbd "C-e") 'mwim-end-of-line-or-code)

;; undo-fu小巧才15K
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  (global-set-key (kbd "M-/") 'undo-fu-only-redo)
  (global-set-key (kbd "C-x u") 'undo-fu-only-redo) ;; 这个其实是undo，习惯undo tree这个快捷键了
  )

(use-package undo-fu-session
  :disabled
  :after(undo-fu)
  :config
  (global-undo-fu-session-mode))

;; 经常C-x C-s按错，还是用这个吧
;;super save好像eldoc都会触发保存，有点烦
;; (auto-save-visited-mode 1)
(use-package super-save
  ;; :disabled
  :defer 0.5
  :init
  ;; 默认切换窗口时保存，这里确保idle时也保存
  (setq super-save-auto-save-when-idle t) 
  :config
  (super-save-mode +1))

(use-package yasnippet
  :defer 1
  :init
  (add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
  :diminish(yas-minor-mode)
  :config
  ;; copy from yasnippet-snippets.el，fix for eglot
  (defun yasnippet-snippets--no-indent ()
    "Set `yas-indent-line' to nil."
    (set (make-local-variable 'yas-indent-line) nil)) 
  (defun yasnippet-snippets--fixed-indent ()
    "Set `yas-indent-line' to `fixed'."
    (set (make-local-variable 'yas-indent-line) 'fixed))
  (setq yas-snippet-dirs
	    '("~/.emacs.d/packages/yasnippet/mysnippets" ;; 自定义要在前，保存才会默认保存这里
	      "~/.emacs.d/packages/yasnippet/yasnippet-snippets-master/snippets"
	      ))
  (yas-global-mode 1)

  ;; 这个其实还挺好用的，用~xxx代替要替换的，或者`xxx'，多行要选中单行不用选中
  (autoload 'aya-create "auto-yasnippet" nil t)
  (autoload 'aya-expand "auto-yasnippet" nil t)
  (global-set-key (kbd "C-c y") #'aya-create)
  (global-set-key (kbd "C-c e") #'aya-expand)
  )

;; from tabbar-ruler
(setq EmacsPortable-included-buffers '("*scratch*" "*shell*" "*eww*"))
(defun ep-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
		             ((string-match "^TAGS\\(<[0-9]+>\\)?$" (format "%s" (buffer-name b))) nil)
                     ((string-match "^magit.*:.*" (format "%s" (buffer-name b))) nil)
                     ((buffer-file-name b) b)
		             ((member (buffer-name b) EmacsPortable-included-buffers) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((char-equal ?* (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(if (display-graphic-p)
    ;; 之前用的tabbar-ruler在c-x 3多窗口会导致helm弹出延迟
    
    ;; 27.1自带tab-bar-mode和tab-line-mode，试了下tab-line跟tabbar-ruler是一样的效果
    (when (functionp 'global-tab-line-mode)
      (use-package tab-line
        :defer 0.5
        :init
        (setq tab-line-tabs-function 'ep-tabbar-buffer-list)
        :config
        (global-tab-line-mode 1)  
        )
      
      )
  (progn
    (global-set-key (kbd "<C-tab>") 'helm-buffers-list)
    (global-set-key (kbd "<C-M-S-tab>") 'helm-buffers-list)
    ))

;; highlight-symbol下岗啦，font lock速度慢，现在都是overlay。ahs范围小，还跟输入
(use-package symbol-overlay
  :defer 1.1
  :init
  :diminish
  :config
  (setq symbol-overlay-idle-time 0.1)
  (when use-my-face
    (if (display-graphic-p)
        (set-face-attribute 'symbol-overlay-default-face nil :background "#666") ;; zenburn里取色(开启rainbow-mode)
      (set-face-attribute 'symbol-overlay-default-face nil :background "#666" :foreground "Black")
      ))
  
  (global-set-key [f3] 'symbol-overlay-jump-next)
  (global-set-key [(shift f3)] 'symbol-overlay-jump-prev)
  (global-set-key [(meta f3)] 'symbol-overlay-query-replace) ;; symbol-overlay-rename
  (define-globalized-minor-mode global-highlight-symbol-mode symbol-overlay-mode symbol-overlay-mode)
  (global-highlight-symbol-mode 1)
  (defhydra symbol-overlay-select ()
    "
_p_: put      _t_: toggle
_r_: rename   _s_: save name
_h_: help     _c_: count
_R_: remove all _m_: jump mark
_f_: jump first _l_: jump last
_d_: jump definition _%_: query-replace
_q_uit
"
    ("p" symbol-overlay-put nil :color blue)
    ("t" symbol-overlay-toggle-in-scope nil :color blue) ;; 配合rename是真牛B啊！
    ("r" symbol-overlay-rename nil :color blue)
    ("s" symbol-overlay-save-symbol nil :color blue)
    ("h" symbol-overlay-map-help nil :color blue)
    ("c" symbol-overlay-count nil :color blue) ;; 显示不了？
    ("R" symbol-overlay-remove-all nil :color blue)
    ("m" symbol-overlay-echo-mark nil :color blue) ;; 不懂
    ("f" symbol-overlay-jump-first nil :color blue)
    ("l" symbol-overlay-jump-last nil :color blue)
    ("d" symbol-overlay-jump-to-definition nil :color blue)
    ("%" symbol-overlay-query-replace nil :color blue)
    ("q" nil "nil" :color blue))
  (global-set-key [(control f3)] 'symbol-overlay-select/body)
  )

(use-package cursor-chg
  :defer 0.5
  :config
  (change-cursor-mode)
  (setq curchg-default-cursor-color "red3") ; 无法设置cursor的foreground
  )

;;crosshairs不好用，只要vline就行了		
(autoload 'vline-mode "vline" nil t)
(global-set-key [(control ?|)] 'vline-mode)

(when (display-graphic-p)
  (use-package hl-line
    :defer 0.6
    :config
    (global-hl-line-mode t)
    (when use-my-face
      (if (display-graphic-p)
	      (set-face-attribute 'hl-line nil :background "#2B2B2B") ;; zenburn选中的默认颜色
        ;; (set-face-attribute 'hl-line nil :background "#E0CF9F" :foreground "Black")
        ))
    )  
  )

(progn
  ;; company mode，这个支持comment中文，但不支持补全history
  (use-package company
    :defer 1
    :init
    (add-to-list 'load-path "~/.emacs.d/packages/company-mode")
    :diminish
    :config
    (global-company-mode)
    (when use-my-face
      (set-face-attribute 'company-tooltip-selection nil :background "#666"))
    
    ;; 句尾TAB就很烦了。。
    ;;(setq tab-always-indent 'complete) ;; 当已经格式好后就是补全，配合indent-for-tab-command使用
    ;;(define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
    (define-key company-mode-map [remap completion-at-point] 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "M-n") 'company-next-page)
    (define-key company-active-map (kbd "M-p") 'company-previous-page)
    ;; 这就是我想要的啊！跟bash里的tab类似
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle) 
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    (define-key company-active-map (kbd "C-h") nil) ; 取消绑定，按f1代替。c-w直接看源码
    (dotimes (i 10)
	  (define-key company-active-map (read-kbd-macro (format "C-%d" i)) 'company-complete-number))
    (setq-default company-dabbrev-other-buffers 'all
                  company-tooltip-align-annotations t)
    (setq ;company-idle-delay 0.5 ; 为0的话太卡了，输入就会卡住，默认就行了
     company-minimum-prefix-length 2
     company-require-match nil
     company-dabbrev-ignore-case nil
     company-dabbrev-downcase nil
     company-show-numbers t)
    ;; 下载TabNine.exe拷贝到~\.TabNine\2.2.2\x86_64-pc-windows-gnu
    ;; (with-eval-after-load 'dash
    ;;   (add-to-list 'load-path "~/.emacs.d/packages/company-mode/company-tabnine")
    ;;   (require 'company-tabnine)
    ;;   (add-to-list 'company-backends #'company-tabnine)
    ;;   )
    
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
(use-package wcy-desktop
  ;;:defer 0.5
  :config
  (wcy-desktop-init)
  (add-hook 'emacs-startup-hook
            (lambda ()
	          (ignore-errors
		        (wcy-desktop-open-last-opened-files))))
  (defadvice wcy-desktop-load-file (after my-wcy-desktop-load-file activate)
    (setq buffer-undo-list nil) ;; 解决undo-tree冲突
    ;; 修正buffer打开时的point
    (when (featurep 'saveplace)
      (save-place-find-file-hook))
    (when (featurep 'undo-fu-session)
      (undo-fu-session-recover-safe))
    ))

(use-package google-c-style
            :commands(google-set-c-style))

(defun my-c-mode-hook-set()
  (google-set-c-style)
  (setq c-basic-offset 4) ;; tab4个空格习惯了
  (abbrev-mode -1) ;; 有yas就够了
  (define-key c-mode-base-map (kbd "C-c C-c") 'magit)
  (define-key c-mode-base-map "\C-d" nil) ;; 干扰其他parens处理了
  (define-key c-mode-base-map "\177" nil) ;; backspack
  )

(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                 nsis-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                 nsis-mode)) auto-mode-alist))

(autoload 'protobuf-mode "protobuf-mode" "protobuf mode" t)
(setq auto-mode-alist (append '(("\\.proto\\'" .
				                 protobuf-mode)) auto-mode-alist))

(use-package jumplist
  :defer 0.7
  :config
  (global-set-key (kbd "M-n") 'jl-jump-forward)
  (global-set-key (kbd "M-p") 'jl-jump-backward)
  (add-to-list 'jl-insert-marker-funcs "my-switch-buffer")
  (add-to-list 'jl-insert-marker-funcs "ggtags-find-tag-dwim")
  (add-to-list 'jl-insert-marker-funcs "ggtags-find-reference")
  (add-to-list 'jl-insert-marker-funcs "ggtags-find-file")
  (add-to-list 'jl-insert-marker-funcs "helm-occur")
  (add-to-list 'jl-insert-marker-funcs "helm-imenu-in-all-buffers")
  (add-to-list 'jl-insert-marker-funcs "xref-find-definitions")
  (add-to-list 'jl-insert-marker-funcs "session-jump-to-last-change")
  (global-set-key [(control ?\,)] 'my-save-pos) ; 手动触发记录位置
  (defun my-save-pos()
    (interactive)
    )
  (add-to-list 'jl-insert-marker-funcs "my-save-pos")
  )

(use-package iss-mode
  :init
  (setq iss-compiler-path "D:/Programme/Inno Setup 5/")
  (setq auto-mode-alist (append '(("\\.iss$"  . iss-mode)) auto-mode-alist))
  :commands(iss-mode)
  :config
  (define-key iss-mode-map [f6] 'iss-compile)
  (define-key iss-mode-map [(meta f6)] 'iss-run-installer)
  )

(use-package w32-browser
  :commands (w32explore dired-mouse-w32-browser dired-w32-browser dired-multiple-w32-browser dired-w32explore)
  :init
  ;; windows中打开并选中buffer对应的文件，C-X 6 是2C mode的前辍
  (when (string-equal system-type "windows-nt")
    (global-set-key (kbd "C-x C-d")
		            (lambda () (interactive)
		              ;; (shell-command (format "explorer.exe /n,/select, \"%s\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name (current-buffer)))))
		              (w32explore (buffer-file-name (current-buffer)))
		              )))
  )

(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		        ("\\.cmake\\'" . cmake-mode))
	          auto-mode-alist))

(defun my-elisp-hook()
  (make-local-variable 'eldoc-idle-delay)
  (setq eldoc-idle-delay 0.5) ;; 设置为0会导致鼠标点击时不显示
  (turn-on-eldoc-mode)
  )
(find-function-setup-keys)  ;直接定位函数变量定义位置的快捷键，C-x F/K/V，注意是大写的
(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-elisp-hook)

;;; 当lsp不能用时，用ggtag
(use-package ggtags
  :commands(ggtags-mode)
  :config
  ;;; gtags 自动更新当文件保存时
  (defun gtags-root-dir ()
    "Returns GTAGS root directory or nil if doesn't exist."
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
	      (buffer-substring (point-min) (1- (point-max)))
	    nil)))
  (defun gtags-update1 ()
    "Make GTAGS incremental update"
    (call-process "global" nil nil nil "-u"))
  (defun gtags-update ()
    (interactive)
    (when (gtags-root-dir)		;没有多余的副作用
      (gtags-update1)))
  (define-key ggtags-global-mode-map "n" 'next-line)
  (define-key ggtags-global-mode-map "p" 'previous-line)
  
  ;; ggtags给xref添加了backend的，但是提示找不到global
  (define-key ggtags-mode-map [remap xref-find-definitions] 'ggtags-find-tag-dwim)
  (setq ggtags-global-abbreviate-filename nil) ; 不缩写路径
  ;;(defadvice ggtags-eldoc-function (around my-ggtags-eldoc-function activate)); eldoc没有开关，只有重写它的函数了
  ;; (customize-set-variable 'ggtags-highlight-tag nil) ; 禁止下划线 setq对defcustom无效！ 测试是eldoc导致提示process sentinel的
  ;;(turn-on-eldoc-mode) ; 会卡
  )
(defvar global-ggtags nil)
(defun ggtag-all-buffer (enable)
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (eq major-mode 'c++-mode)
		        (eq major-mode 'c-mode)
		        (eq major-mode 'objc-mode))
	    (ggtags-mode enable)))))
(defun ggtag-toggle()
  (interactive)
  (if global-ggtags
      (progn
	    (ggtag-all-buffer -1)
	    (remove-hook 'c-mode-common-hook 'ggtags-mode)
	    (setq global-ggtags nil)
	    )
    (progn
      (ggtag-all-buffer 1)
      (add-hook 'c-mode-common-hook 'ggtags-mode)
      (setq global-ggtags t)
      )))


;;; TODO 如果编译过其他目录后，另一个目录C-F7时当前目录没有变，必须C-u F7重新配置
;; compile，加入了单独编译某个文件
(setq compilation-auto-jump-to-first-error nil ; 自动跳到错误，这个在只有warning时相当烦！
      compilation-scroll-output t)

;; expand-region被 easy-kill的easy-mark替换了，但要保留会被调用 
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(use-package expand-region
  :commands(er/expand-region))
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
;; (defadvice show-paren-function (around not-show-when-expand-region activate)
;;   (if (and (or (eq major-mode 'lisp-interaction-mode) (eq major-mode 'emacs-lisp-mode))
;; 	   (memq last-command '(er/expand-region er/contract-region easy-mark easy-kill-er-expand easy-kill-er-unexpand)))
;;       (progn
;; 	(setq show-paren-style 'parenthesis)
;; 	ad-do-it
;; 	(setq show-paren-style 'expression)
;; 	)
;;     ad-do-it))

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
(use-package multiple-cursors
  :load-path "~/.emacs.d/packages/multiple-cursors"
  :init
  ;; mc询问你的命令都保存在这里面了
  (setq mc/list-file "~/.emacs.d/packages/multiple-cursors/my-cmds.el")
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
  :commands(multiple-cursors-mode
            mc/add-cursor-on-click
            mc/unmark-next-like-this
            mc/unmark-previous-like-this
            mc/mark-previous-like-this
            mc/mark-next-like-this
            mc/edit-lines
            )
  :config
  (define-key mc/keymap (kbd "C-v") nil)
  (define-key mc/keymap (kbd "RET") 'multiple-cursors-mode)
  )

;; avy可以配得跟ace jump完全一样，就没必要保留ace jump了
(use-package avy
  :commands(avy-goto-char-timer avy-goto-word-1 avy-goto-line avy-resume)
  :init
  (define-key global-map (kbd "C-o") 'avy-goto-word-1)
  (global-set-key [remap goto-line] 'avy-goto-line)
  (setq avy-background t) ;; 开启跟ace一样了
  (setq-default avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  )

;;; autohotkey文件编辑
(autoload 'xahk-mode "xahk-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;;; rust mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(modify-coding-system-alist 'file "\\.rs\\'" 'utf-8-with-signature) ; 带中文必须这个编码，干脆就默认
(with-eval-after-load 'rust-mode
  (when (featurep 'auto-complete)
    (make-local-variable 'ac-auto-start) ; 自动弹出会卡
    (setq ac-auto-start nil)
    )
  (when (featurep 'company)
    ;; (make-local-variable 'company-idle-delay)
    ;; (setq company-idle-delay nil) 	; 不自动补全
    ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-0annotations t))
  (setq-local eldoc-documentation-function #'ignore) ; eldoc严重影响输入！
  )

(use-package treemacs
  :commands(treemacs treemacs-add-and-display-current-project treemacs-current-visibility)
  :init
  (add-to-list 'load-path "~/.emacs.d/packages/treemacs/")
  (add-to-list 'load-path "~/.emacs.d/packages/treemacs/src/elisp")
  (add-to-list 'load-path "~/.emacs.d/packages/treemacs/src/extra")
  (require 'treemacs-autoloads)
  ;; 习惯打开时浏览目录，只能调用treemacs-add-and-display-current-project了
  (global-set-key (kbd "<C-f1>") 
                  (lambda ()(interactive)
                    (let ((buf (current-buffer)))
                      (pcase (treemacs-current-visibility)
                        ('visible (treemacs)) ;; 已展示就隐藏
                        ('exists  (call-interactively 'treemacs-add-and-display-current-project))
                        ('none    (call-interactively 'treemacs-add-and-display-current-project)))
                      ;; (switch-to-buffer buf) ;; focus切换回buffer里
                      )))
  :config
  
  (with-eval-after-load 'treemacs-mode
    (define-key treemacs-mode-map "l" 'treemacs-goto-parent-node)
    (define-key treemacs-mode-map "D" 'treemacs-remove-project-from-workspace)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action) ;; 默认鼠标双击

    (define-key treemacs-mode-map (kbd "C-x C-d")
      ;; 抄自treemacs-visit-node-in-external-application
      (lambda () (interactive
                  (-if-let (path (treemacs--prop-at-point :path))
                      (w32explore path)
                    (treemacs-pulse-on-failure "Nothing to open here."))
                  )))
    
    (when (display-graphic-p)
      ;; 改变高亮行背景色
      (add-hook 'treemacs-mode-hook
                (lambda ()
                  (face-remap-add-relative 'hl-line '(:background "#666")))))
    )
  
  ;; 避免treemacs persistence文件变成只读
  (with-eval-after-load 'treemacs-persistence
    (defadvice treemacs--persist (around my-treemacs--persist activate)
      (setq tmp-disable-view-mode 2);; 2不恢复只读
      ad-do-it
      (setq tmp-disable-view-mode nil)
      )
    )
  (treemacs-follow-mode t)      ; 切换buffer自动定位，特牛，目录再深都能定位到
  (treemacs-fringe-indicator-mode -1) ; 有高亮行就不需要fringe了(本身也被disable)
  (treemacs-filewatch-mode t)          ; 监视系统文件变化
  (setq treemacs-recenter-after-file-follow t ;好像没效果啊
        treemacs-recenter-after-tag-follow t
        treemacs-recenter-distance 0.5
        ;; treemacs-no-png-images t
        )
  )
(use-package treemacs-projectile
  :after (treemacs projectile)
  )

;; 这个给dired添加图标，不错。
;; 不过当doom theme设置treemacs为doom-colors时有时不显示，故而用all-the-icons-dired
(use-package treemacs-icons-dired
  :disabled
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  )

;; magit更新时也刷新treemacs
(use-package treemacs-magit
  :after (treemacs magit)
  )
(use-package cfrs
  :commands(cfrs-read))




;; 有点类似自动截段长行的效果，默认绑定到m-q，elisp-mode无效
(autoload 'unfill-toggle "unfill" nil t)
(global-set-key [remap fill-paragraph] #'unfill-toggle)


(use-package imenu-list 
  :commands(imenu-list-smart-toggle)
  :init
  (setq imenu-list-idle-update-delay 0.5)
  (global-set-key [(control f4)] 'imenu-list-smart-toggle)
  :hook(imenu-list-major-mode . (lambda ()
				                  (when (display-graphic-p)
				                    ;; 指示当前是在哪个函数里     
				                    (face-remap-add-relative 'hl-line '(:background "#666"))))))

(defun chinese-char-p (char)
  (if (string-match "\\cC\\{1\\}" (string char))
      t
    nil))
(defun chinese-word-chinese-string-p (string)
  "Return t if STRING is a Chinese string."
  (cl-find-if 'chinese-char-p (string-to-list string))
  )

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

  (global-set-key [f2] 'helm-do-grep-ag) ; 使用ripgrep即rg搜索，文档rg --help
  (global-set-key [S-f2] (lambda() (interactive)
                                        ; 只搜索当前目录，加入-g/*
                           (let ((helm-grep-ag-command "rg --color=always --colors match:style:nobold --smart-case --no-heading --line-number --no-ignore -g/* %s %s %s"))
                             (call-interactively 'helm-do-grep-ag))
                           ))
  (setq
   ;; smart case跟emacs类似，不读gitignore，--colors match:style:nobold是用doom themes时必须的，否则rg无高亮效果
   helm-grep-ag-command "rg --color=always --colors match:style:nobold --smart-case --no-heading --line-number --no-ignore %s %s %s"
   helm-grep-ag-pipe-cmd-switches '("--colors match:style:nobold") ;; 多个patterns时通过pipe
   helm-move-to-line-cycle-in-source t ; 使到顶尾时可以循环，缺点是如果有两个列表，下面那个用C-o或者M->切换过去
   helm-echo-input-in-header-line t ; 这个挺awesome的，不使用minibuffer，在中间眼睛移动更小
   helm-split-window-in-side-p t ; 不然的话，如果有两个窗口，它就会使用另一个窗口。另一个是横的还好，竖的就不习惯了
   helm-ff-file-name-history-use-recentf t
   helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match    t
   helm-follow-mode-persistent t
   helm-allow-mouse t
   ;;helm-grep-input-idle-delay 0.02 	; 默认0.1，让搜索有延迟
   helm-browse-url-default-browser-alist nil ; 新版本提示没有browse-url-galeon-program，致helm-find-files不能用，直接屏蔽算了，用不到
   )
  
  ;; 对于 中文 启用--pre rgpre
  (defadvice helm-grep-ag-prepare-cmd-line (around my-helm-grep-ag-prepare-cmd-line activate)
    (if (chinese-word-chinese-string-p (ad-get-arg 0))
        (let ((helm-grep-ag-command (concat helm-grep-ag-command " --pre rgpre")))
          ad-do-it)
      ad-do-it)
    )
  
  (with-eval-after-load 'helm
	;; 调试helm(setq helm-debug t)，然后要调试命令后再run helm-debug-open-last-log(helm-debug会被设为nil)
	;; describe-current-coding-system 查看当前系统编码
	;; 设置rg进程编码不起作用，因为win上启动rg用的是cmdproxy，设置cmdproxy才有效果！
	
	;; rg输出是utf-8，这个将第1个文件名转换为gbk，关键函数encode-coding-string
	;; 这个方法不太好，emacs是自动识别buffer内容的，所以才能显示中文内容，只单独处理路径虽然界面没问题，但实际有点小问题
	;; (defadvice helm-grep-split-line (around my-helm-grep-split-line activate)
	;;   ad-do-it
	;;   (let ((fname (car-safe ad-return-value)))
	;;     (when fname
	;;       (setq ad-return-value (cons (encode-coding-string fname locale-coding-system) (cdr ad-return-value) ))
	;;       ))
	;;   )
	;; 临时设置cmdproxy编码
	(with-eval-after-load 'helm-grep
	  (defadvice helm-grep-ag-init (around my-helm-grep-ag-init activate)
	    (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
	      (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
	      ad-do-it
	      (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)
	      )))
	
	(helm-mode 1)
	(when (fboundp 'diminish)
	  (add-hook 'helm-mode-hook (lambda ()(diminish 'helm-mode)))
	  )

	;; (defadvice start-file-process-shell-command (before my-start-file-process-shell-command activate)
	;;   (message (ad-get-arg 2)))
	
	;; 光标移动时也自动定位到所在位置
	(push "Occur" helm-source-names-using-follow) ; 需要helm-follow-mode-persistent为t
	(push "RG" helm-source-names-using-follow)

    (when (display-graphic-p)
	  ;; 参考swiper设置颜色，这个一改瞬间感觉不一样
      ;; 当前主题背景色(face-attribute 'default :background)
	  (custom-set-faces
       ;; 对于doom theme, helm-selection要特别留意，去掉多余的东西如:inherit bold等，不然rg选中行没关键词高亮
	   '(helm-selection ((t (:inherit unspecified :underline t :background nil :distant-foreground nil :foreground nil)))) ; underline好看，去掉背景色
	   '(helm-selection-line ((t (:underline t :background nil))))
	   ;;helm-match-item 
	   ))

	(define-key helm-map (kbd "<f1>") 'nil)
	(define-key helm-map (kbd "C-1") 'keyboard-escape-quit)

	(define-key helm-map (kbd "C-h") 'nil)
	(define-key helm-map (kbd "C-t") 'nil) ; c-t是翻转样式
	(define-key helm-map (kbd "C-t") 'helm-toggle-visible-mark)
	(define-key helm-map (kbd "C-v") 'nil)
	(define-key helm-map (kbd "<f4>") 'helm-next-line)
	(define-key helm-map (kbd "<S-f4>") 'helm-previous-line)
	;; (define-key helm-map (kbd "C-s") 'helm-next-line) ;; 这个还是留给helm-occur或者helm-ff-run-grep
	(define-key helm-map (kbd "<tab>") (lambda ()(interactive)
					                     (let* ((src (helm-get-current-source))
						                        (name (assoc-default 'name src)))
					                       (if (or (member name (list "RG" "Occur"))
						                           (string= name "Helm occur")
						                           (string= name "RG")) ;; TODO 
						                       (progn (call-interactively 'next-history-element)
							                          (move-end-of-line 1)())
						                     ;; 其它模式还是helm-execute-persistent-action，比如文件补全
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
  (global-set-key (kbd "M-m") 'helm-imenu)
  )


(use-package smart-hungry-delete
  :defer 0.8
  :config
  (smart-hungry-delete-add-default-hooks)
  (global-set-key [remap delete-forward-char] 'smart-hungry-delete-forward-char)
  (global-set-key [remap delete-char] 'smart-hungry-delete-forward-char)
  (global-set-key [remap delete-backward-char] 'smart-hungry-delete-backward-char)
  (global-set-key [remap backward-delete-char-untabify] 'smart-hungry-delete-backward-char)
  )

;; 自动indent，indentinator有肉眼可见的闪动，但执行良好，aggressive-indent经常不起作用
(use-package indentinator
  :defer 1
  :diminish
  :config
  (defun check-mode()
	(unless (or (derived-mode-p 'c-mode 'c++-mode) ;; c/cpp保存时调用clang-format
		        (eq major-mode 'rust-mode)) ;;(eq major-mode 'python-mode)
	  (indentinator-mode)))
  (define-globalized-minor-mode global-indentinator-mode indentinator-mode check-mode)
  (global-indentinator-mode 1)
  )

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

;; magit
(use-package magit
  :init
  (add-to-list 'load-path "~/.emacs.d/packages/magit/magit-master/lisp")
  (add-to-list 'load-path "~/.emacs.d/packages/magit")
  (modify-coding-system-alist 'file "\\.git/COMMIT_EDITMSG\\'" 'utf-8)
  (setq magit-version "3.3.0")
  :commands (magit)
  :bind(("C-c C-c". magit))
  :config
  )
(defun git-add-file ()
  "Adds (with force) the file from the current buffer to the git repo"
  (interactive)
  (shell-command (concat "git add -f "
			             (shell-quote-argument buffer-file-name))))
(defun git-set-proxy ()
  "Adds (with force) the file from the current buffer to the git repo"
  (interactive)
  (shell-command (read-string "http: " "git config --global http.proxy \"socks5://127.0.0.1:10808\""))
  (shell-command (read-string "https: " "git config --global https.proxy \"socks5://127.0.0.1:10808\"")))
(defun git-unset-proxy ()
  "Adds (with force) the file from the current buffer to the git repo"
  (interactive)
  (shell-command "git config --global --unset http.proxy")
  (shell-command "git config --global --unset https.proxy"))

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
(global-set-key (kbd "C-.") 'xref-find-definitions)
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
;; 有选中或者mark时用expand-region，否则用easy-mark
(global-set-key "\C-t" (lambda ()(interactive)
			             (if (region-active-p)
			                 (call-interactively 'er/expand-region)
			               (call-interactively 'easy-mark)
			               ))) ; 替换expand-region

(with-eval-after-load 'easy-kill
  (defadvice easy-kill (after my-easy-kill activate)
    (unless (or (use-region-p) (not (called-interactively-p 'any)))
      (let ((string (buffer-substring (line-beginning-position)
				                      (line-beginning-position 2))))
	    (when (> (length string) 0)
	      (put-text-property 0 (length string)
			                 'yank-handler '(yank-line) string))
	    (kill-new string nil)
	    )
      ))
  
  (require 'easy-kill-er)
  (require 'extra-things)
  (require 'easy-kill-extras)
  (setq easy-kill-try-things '(my-line)) ; 只复制line
  ;; 参考easy-kill-on-buffer-file-name
  (defvar my-line-ov nil)
  (defun easy-kill-on-my-line (n)
    "copy line添加yank line功能"
    (if (easy-kill-get mark)
	    (easy-kill-echo "Not supported in `easy-mark'")
      ;; 不用判断是否有region，有的话根本不会进来
      (let ((string (buffer-substring (line-beginning-position)
				                      (line-beginning-position 2))))
	    ;; 因为easy-kill-adjust-candidate传递的是string，就没有overlay效果了，需要自己加
	    (setq my-line-ov (make-overlay (line-beginning-position) (line-beginning-position 2)))
	    (overlay-put my-line-ov 'priority 999) ;; 在hl line之上
	    (overlay-put my-line-ov 'face 'easy-kill-selection)
	    (when (> (length string) 0)
	      (put-text-property 0 (length string)
			                 'yank-handler '(yank-line) string))
	    (easy-kill-adjust-candidate 'my-line string) 
	    )
      ))
  
  (defun kill-my-line-ov()
    (when my-line-ov
      (delete-overlay my-line-ov)
      (setq my-line-ov nil)))
  ;; easy kill退出时也清除我们的overlay
  (defadvice easy-kill-destroy-candidate (after my-easy-kill-destroy-candidate activate)
    (kill-my-line-ov)
    )
  ;; 当overlay改变时，如按w也清除我们的overlay
  ;; (defun move-overlay-around (orig-fun n begin end &rest args)
  ;;   (when (and (eq n easy-kill-candidate) (/= begin end)) ;; easy kill没有overlay的begin end都是(point)
  ;;     (kill-my-line-ov))
  ;;   (apply orig-fun n begin end args)
  ;;   )
  ;; (advice-add 'move-overlay :around #'move-overlay-around)
  ;; (advice-remove 'move-overlay #'move-overlay-around)
  ;; 担心move-overlay影响效率使用下面方法
  (defadvice easy-kill-adjust-candidate (after my-easy-kill-adjust-candidate activate)
    ;; 参考easy-kill-candidate
    (with-current-buffer (easy-kill-get buffer)
      (pcase (easy-kill-get bounds)
        (`(,_x . ,_x) ();; 这就是字符串形式
	     )
        (`(,beg . ,end) (kill-my-line-ov)
	     ))))
  
  ;; 当光标在屏幕下一半，minibuffer显示有换行的拷贝内容，会导致C-l效果，需要去掉换行
  ;; 测试带汉字也会。。还是添加overlay表示复制了吧
  (defun easy-kill-echo-around (orig-fun format-string &rest args)
    ;; (apply orig-fun format-string
    ;; 	   (let ((no-line (ignore-errors
    ;; 			    (when (listp args)
    ;; 			      (list (string-join (split-string (substring-no-properties (car args)) "[\n]+"))))
    ;; 			    )))
    ;; 	     (if no-line
    ;; 		 no-line
    ;; 	       args)))
    )
  (advice-add 'easy-kill-echo :around #'easy-kill-echo-around)
  ;;(advice-remove 'easy-kill-echo  #'easy-kill-echo-around)
  (add-to-list 'easy-kill-alist '(?= my-line ""))

  (setq easy-mark-try-things '(word sexp)) ; word优先，特别是有横杠什么都时候
  ;; (define-key easy-kill-base-map (kbd "C-r") 'easy-kill-er-expand) ; 不要再定义了，避免mark时不能复制
  (define-key easy-kill-base-map (kbd "C-t") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "C-S-t") 'easy-kill-er-unexpand)
  (define-key easy-kill-base-map (kbd "n") 'easy-kill-expand)
  (define-key easy-kill-base-map (kbd "p") 'easy-kill-shrink)
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

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-;") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-; s-") nil) ; Undefine prefix binding https://emacs.stackexchange.com/questions/3706/undefine-prefix-binding
  (define-key projectile-mode-map (kbd "C-; s") #'projectile-ripgrep) ; 对C-; s同样生效
  )
(autoload 'projectile-compile-project "projectile" nil t)
(global-set-key [f7] (lambda ()(interactive)
			           (progn
			             (call-interactively 'projectile-compile-project)
			             (setq compilation-read-command nil) ;; 不再提示
			             )))
(global-set-key [(shift f7)]
		        (lambda ()(interactive)
		          (progn
			        (setq compilation-read-command t)
			        (call-interactively 'projectile-compile-project)
			        (setq compilation-read-command nil) ;; 不再提示
			        )))

;; rg，这个还挺好用的，带修改搜索的功能(需要buffer可写)，更多功能看菜单
(global-set-key (kbd "C-S-f") 'rg-dwim)
(setq rg-ignore-case 'force) ;; 不知道为什么regex搜索的时候会区分大小，只能强制了
(autoload 'rg-dwim "rg" nil t)
(with-eval-after-load 'rg-result
  ;; 解决rg输出为utf-8，导致emacs不能正常处理中文路径的问题
  ;; 临时设置cmdproxy编码
  (defadvice rg-run (around my-run activate)
    (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
	  (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
	  ad-do-it
	  (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)
	  ))
  (defadvice rg-recompile (around my-rg-recompile activate)
    (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
	  (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
	  ad-do-it
	  (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)
	  ))
  ;; 中文
  (defadvice rg-build-command (around my-rg-build-command activate)
    (if (chinese-word-chinese-string-p (ad-get-arg 0))
        (let ((rg-command-line-flags (list "--pre rgpre")))
          ad-do-it
          )
      ad-do-it))
  )

;;
(defadvice wgrep-commit-file (around my-wgrep-commit-file activate)
  (setq tmp-disable-view-mode t)
  ad-do-it
  (setq tmp-disable-view-mode nil)
  )

;; which-key，用于一些自带mode-map的比较好，用原生的帮助显示太乱了
(use-package which-key
  :disabled
  :init
  (with-eval-after-load 'bookmark
    (define-key bookmark-bmenu-mode-map "?" (lambda ()(interactive)(which-key-show-keymap 'bookmark-bmenu-mode-map))))
  (with-eval-after-load 'dired
    (unless (featurep 'dired+) ;; dired+ has `diredp-dired-plus-help`
      (define-key dired-mode-map "?" (lambda ()(interactive) (which-key-show-keymap 'dired-mode-map)))))
  :commands(which-key-show-keymap))

;; lsp，c++装个llvm(包含clangd)，python装pyright，rust装rust-analyzer
(add-to-list 'load-path "~/.emacs.d/packages/lsp")
(if t
    ;; eglot
    (use-package eglot
      :load-path "~/.emacs.d/packages/lsp"
      :init
      (defun lsp-ensure() (eglot-ensure))
      :commands (eglot eglot-ensure eglot-rename)
      :config
      ;; flymake还是要开的，错误不处理的话，补全就不能用了。用跟cmake一样的vs版本可以解决很多错误
      ;; (add-to-list 'eglot-stay-out-of 'flymake)
      (setq eglot-autoshutdown t)      ;; 不关退出emacs会卡死
      (push :documentHighlightProvider ;; 关闭光标下sybmol加粗高亮
            eglot-ignored-server-capabilities) 
      ;; 临时禁止view-mode，使重命名可用
      (defadvice eglot--apply-workspace-edit (around my-eglot--apply-workspace-edit activate)
	    (setq tmp-disable-view-mode t)
	    ad-do-it
	    (setq tmp-disable-view-mode nil)
	    )
      ;; clang-format不需要了，默认情况下会sort includes line，导致编译不过，但clangd的却不会，但是要自定义格式需要创建.clang-format文件
      (define-key eglot-mode-map [(meta f8)] 'eglot-format)
      )

  ;; 相比eglot，退出emacs会卡一下
  (progn
    (add-to-list 'load-path "~/.emacs.d/packages/lsp/lsp-mode-master")
    (add-to-list 'load-path "~/.emacs.d/packages/lsp/lsp-mode-master/clients")
    (use-package lsp-mode
      :init
      ;; lens和modeline没效果？好像要配合lsp ui用
      (setq lsp-lens-enable nil
            lsp-modeline-code-actions-enable nil
            lsp-modeline-diagnostics-enable nil
            lsp-modeline-workspace-status-enable nil
            lsp-headerline-breadcrumb-enable nil ;; 遮挡tabbar了
            lsp-enable-symbol-highlighting nil ;; 高亮光标下的词，除了能限定作用域没什么大用
            lsp-enable-folding nil
            lsp-semantic-tokens-enable nil
            lsp-enable-links nil
            lsp-enable-text-document-color nil
            )
      (use-package lsp-pyright
        :load-path "~/.emacs.d/packages/lsp/lsp-pyright-master"
        :defer t
        )
      :commands (lsp lsp-deferred)
      :config
      ;; 使重命名可用
      (defadvice lsp--apply-workspace-edit (around my-lsp--apply-workspace-edit activate)
	    (setq tmp-disable-view-mode t)
	    ad-do-it
	    (setq tmp-disable-view-mode nil)
	    )
      (require 'lsp-diagnostics)
      (define-key lsp-mode-map [(meta f8)] (lambda () (interactive)
                                             (if (use-region-p)
                                                 (call-interactively 'lsp-format-region)
                                               (call-interactively 'lsp-format-buffer))
			                                 )))
    (defun lsp-ensure() (lsp-deferred))

    ;; dap-mode 依赖treemacs,bui,lsp-treemacs,posframe
    (use-package dap-mode
      :load-path "~/.emacs.d/packages/lsp/dap-mode-master"
      :after(lsp-mode)
      :init
      (use-package bui
        :load-path "~/.emacs.d/packages/lsp/bui.el-master"
        :defer t
        )
      (use-package lsp-treemacs
        :load-path "~/.emacs.d/packages/lsp/lsp-treemacs-master"
        :defer t)
      (require 'dap-autoloads) ;; 通过dap-autoloads.txt里的命令自己生成的，没有包管理器不好办啊。注册了命令lsp会自动显示breakpoint(需要fringe)
      ;; server log窗口太大了，减小它 https://github.com/emacs-lsp/dap-mode/issues/428
      (add-to-list 'display-buffer-alist '(" server log\\*\\'" display-buffer-at-bottom (window-height . 0.2)))
      ;; controls目前有bug
      (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))
      :config
      ;; f2设置断点跟rg冲突了，所以用vs那套按钮(lsp启动时也会启动dap mode)
      (define-key dap-mode-map (kbd "<f5>") (lambda ()(interactive)
                                              (let ((cs (dap--cur-session)))
                                                (if cs
                                                    (if (dap--session-running cs)
                                                        (call-interactively 'dap-continue)
                                                      (call-interactively 'dap-debug-restart))
                                                  (call-interactively 'dap-debug))
                                                )))
      (define-key dap-mode-map (kbd "<f12>") 'dap-hydra)
      (define-key dap-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
      (define-key dap-mode-map (kbd "<f11>") 'dap-step-in)
      (define-key dap-mode-map (kbd "<f10>") 'dap-next)

      ;; 解决hl line不及时更新问题
      (add-hook 'dap-stack-frame-changed-hook (lambda (debug-session)
                                                (when global-hl-line-mode
                                                  (global-hl-line-highlight))
                                                ))
      (use-package dap-python
        :after (dap-mode python))

      ;; 需要调用dap-debug-edit-template，或者dap-hydra里d e，来编辑运行参数，类似vscode那样设置
      (use-package dap-cpptools
        :after (dap-mode cc-mode))
      
      (use-package dap-hydra
        :commands(dap-hydra)
        :init
        (add-hook 'dap-stopped-hook
                  (lambda (arg)
                    (call-interactively #'dap-hydra)))
        )
      ;; TODO: Locals里的icon显示不正常
      )
    )
  )

(use-package quickrun
  :commands(quickrun quickrun-shell helm-quickrun)
  :init
  (global-set-key (kbd "<f5>") 'quickrun)
  (setq quickrun-option-shebang nil)    ;; windows没有这东西
  :config
  ;; 添加额外消息，不然执行了都不知道
  (defun my/quickrun-after-run-hook ()
    (save-excursion
      (read-only-mode -1)
      (goto-char (point-max))
      (newline)
      (insert (format "Command %s at %s"
                      (propertize "finished" 'face 'compilation-info)
                      (current-time-string)))
      (read-only-mode 1)))
  (add-hook 'quickrun-after-run-hook 'my/quickrun-after-run-hook)
  )

;; 不能任意hook，不然右键无法打开文件，因为eglot找不到对应的server会报错

;; pyright好像还需要node，低版本的还报错，装个支持的win7的最后版本就可以了(node-v13.14.0-x64.msi)
(add-hook 'python-mode-hook (lambda ()
			                  (when (featurep 'lsp-pyright)
                                (require 'lsp-pyright)
                                )
			                  (lsp-ensure)
			                  ))

(defun enable-format-on-save()
  (add-hook 'before-save-hook (lambda()
					            (when (featurep 'eglot)
					              (eglot-format))
                                (when (featurep 'lsp-mode)
                                  (lsp-format-buffer))
					            ) nil 'local)
  )
;; cc-mode包含java等
(add-hook 'c-mode-common-hook 
	      (lambda ()
	        (when (derived-mode-p 'c-mode 'c++-mode)
	          ;; 保存时自动format，因为下面的google style跟clang-format的google有点不一致
              (enable-format-on-save)
              (my-c-mode-hook-set)
              (lsp-ensure)
	          )))
(add-hook 'rust-mode-hook (lambda ()
			                (enable-format-on-save)
			                (lsp-ensure)
			                ))

;; tfs，还有Team Explorer Everywhere但没用起来，直接用vs自带的根本不用配置(前提在vs项目里用过)
;; 请在init里设置tfs/tf-exe
(defun hydra-tfs-select1 ()
  (interactive)
  (unless (functionp 'hydra-tfs-select/body)
    (require 'tfs)
    (defhydra hydra-tfs-select ()
      "
_o_: checkout _i_: checkin
_r_: rename   _g_: get
_a_: add      _d_: delete
_u_: undo     _p_: properties
_h_: history  _s_: status
_q_uit
"
      ("o" tfs/checkout nil :color blue)
      ("i" tfs/checkin nil :color blue)
      ("r" tfs/rename nil :color blue)
      ("g" tfs/get nil :color blue)
      ("a" tfs/add nil :color blue)
      ("d" tfs/delete nil :color blue)
      ("u" tfs/undo nil :color blue)
      ("p" tfs/properties nil :color blue)
      ("h" tfs/history nil :color blue)
      ("s" tfs/status nil :color blue)
      ("q" nil "nil" :color blue))
    )
  (funcall 'hydra-tfs-select/body)
  )
(global-set-key  "\C-ct" 'hydra-tfs-select1)
;; 解决checkout后revert buffer光标位置不对的问题
(with-eval-after-load 'tfs
  (when (featurep 'saveplace)
    (defadvice tfs/checkout (before my-tfs/checkout activate)
      (save-place-to-alist))
    ))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :commands
  (rainbow-delimiters-mode)
  :hook
  (prog-mode . rainbow-delimiters-mode
	         ;; (lambda ()
	         ;; 	 (unless  (or 
	         ;; 		   (eq major-mode 'emacs-lisp-mode)
	         ;; 		   (eq major-mode 'lisp-interaction-mode)
	         ;; 		   )
	         ;; 	   (rainbow-delimiters-mode)))
	         )
  :config
  (when use-my-face
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#B3B3B3")
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#80ee80")
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#ffaa77")
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#66bbff")
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#afafaf")
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#ffa500")
    (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#da6bda")
    (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#ff5e5e")
    (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#dddd77")
    (set-face-attribute 'rainbow-delimiters-base-error-face nil :foreground "#ff2020")
    )
  )

;; 自动转换文本中的RGB颜色
(use-package rainbow-mode
  :commands
  (rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

;; 以dired形式展示fd搜索的文件
(use-package fd-dired
  :commands(fd-dired))

;; tree sitter
(add-to-list 'load-path "~/.emacs.d/packages/tree-sitter")
(add-to-list 'load-path "~/.emacs.d/packages/tree-sitter/core")
(add-to-list 'load-path "~/.emacs.d/packages/tree-sitter/lisp")
(add-to-list 'load-path "~/.emacs.d/packages/tree-sitter/langs")
(setq tree-sitter-langs--testing t) ;; 静止联网check bin
;; tsc里的(require 'dired-aux) 导致dired被加载了
(use-package tree-sitter-hl
  :commands(tree-sitter-hl-mode)
  ;; 来自tree-sitter-major-mode-language-alist
  :hook ((sh-mode
	      c-mode
	      csharp-mode
	      c++-mode 
	      css-mode 
	      elm-mode 
	      emacs-lisp-mode ;; 需要自己编译
	      go-mode 
	      hcl-mode
	      html-mode 
	      mhtml-mode
	      java-mode 
	      javascript-mode
	      js-mode 
	      js2-mode
	      js3-mode
	      json-mode
	      jsonc-mode
	      julia-mode 
	      ocaml-mode 
	      php-mode 
	      python-mode
	      pygn-mode 
	      rjsx-mode 
	      ruby-mode 
	      rust-mode
	      rustic-mode
	      scala-mode
	      swift-mode
	      tuareg-mode
	      typescript-mode) . (lambda ()
		  (tree-sitter-hl-mode)
		  (grammatical-edit-mode 1)
		  ))
  :config
  ;; elisp没有高亮
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (use-package tree-sitter-langs)
  )

;; grammatical-edit bug太多了，pair用这个就够了
(use-package elec-pair
  :config
  (electric-pair-mode 1)
  )

(use-package grammatical-edit
  :commands(grammatical-edit-mode)
  :config
  ;; (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
  ;; (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
  ;; (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
  ;; (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
  ;; (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
  ;; (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
  ;; (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

  ;; (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
  ;; (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)

  ;; (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  ;; (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

  ;; 会影响kill-ring，作者还不改，暂时不用了
  ;; (define-key grammatical-edit-mode-map [remap delete-backward-char] 'grammatical-edit-backward-delete)
  ;; (define-key grammatical-edit-mode-map [remap backward-delete-char-untabify] 'grammatical-edit-backward-delete)
  ;; (define-key grammatical-edit-mode-map [remap delete-forward-char] 'grammatical-edit-forward-delete)
  ;; (define-key grammatical-edit-mode-map [remap delete-char] 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

  ;; 也可以C-q 选中在直接(，[等
  (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
  (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
  (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
  (define-key grammatical-edit-mode-map (kbd "M-s") 'grammatical-edit-unwrap)

  (define-key grammatical-edit-mode-map (kbd "M-<return>") 'grammatical-edit-jump-out-pair-and-newline)

  (when nil
    ;; 支持hungry delete
    (dolist (key '( [remap delete-char]
		            [remap delete-forward-char]))
      (define-key grammatical-edit-mode-map key
        ;; menu-item是一个symbol，而且很有趣的是，F1-K能实时知道是调用哪个函数
        '(menu-item "maybe-grammatical-edit-forward-delete" nil
		            :filter (lambda (&optional _)
			                  (unless (looking-at-p "[[:space:]\n]")
			                    #'grammatical-edit-forward-delete)))))

    (dolist (key '([remap backward-delete-char-untabify]
		           [remap backward-delete-char]
		           [remap delete-backward-char]))
      (define-key grammatical-edit-mode-map key
        '(menu-item "maybe-grammatical-edit-backward-delete" nil
		            :filter (lambda (&optional _)
			                  (unless (looking-back "[[:space:]\n]" 1)
			                    #'grammatical-edit-backward-delete)))))
    )
  )

(with-eval-after-load 'python
  (define-key python-mode-map "\177" nil) ;; 不需要python自带的DEL键处理
  )

;; 由emacs module实现ctrl tab切换窗口
(ignore-errors (module-load (expand-file-name "~/.emacs.d/bin/pop_select.dll"))) ; 需要全路径加载(如果没把~/.emacs.d/bin加入环境变量的话)
(when (fboundp 'pop-select/select)
  (defun my-pop-select(&optional backward)
    (interactive)
    (let* ((myswitch-buffer-list (copy-sequence (ep-tabbar-buffer-list)
					                            )
                                 )  (vec_name [])
                                    sel
                                    )
      (cl-dolist (buf myswitch-buffer-list)
        (setq vec_name (vconcat vec_name (list (buffer-name buf)))))
      ;; 返回序号
      (setq sel (pop-select/select vec_name (if backward
                                                (1- (length vec_name))
                                              1
                                              )))
      (let ((buf (switch-to-buffer (nth sel myswitch-buffer-list))))
        (when (and (bufferp buf) (featurep 'wcy-desktop))
	      (with-current-buffer buf
	        (when (eq major-mode 'not-loaded-yet)
	          (wcy-desktop-load-file))))
        )
      )
    )
  (global-set-key (kbd "<C-tab>") 'my-pop-select)
  (global-set-key (if (string-equal system-type "windows-nt")
		              (kbd "<C-S-tab>")
	                (kbd "<C-S-iso-lefttab>"))
                  (lambda ()(interactive)
                    (my-pop-select t)))
  )

;; jump后自动把屏幕居中
(use-package scroll-on-jump
  :config
  (setq scroll-on-jump-duration 0.0)
  (setq scroll-on-jump-smooth nil)
  (with-eval-after-load 'session
    (scroll-on-jump-advice-add session-jump-to-last-change) ; 有效
    )
  (scroll-on-jump-advice-add jl-jump-backward) ; 有效
  (scroll-on-jump-advice-add jl-jump-forward) ; 有效
  
  ;; 调用了set-window-start的，要用scroll-on-jump-with-scroll-..
  )

;; 对于scroll-on-jump没效果的，可以手动开启centered-cursor-mode
(use-package centered-cursor-mode
  :commands(centered-cursor-mode))

(use-package yaml-mode
  :commands(yaml-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))  
  )


;; eaf，依赖node的npm，python需要epc，不用能virtualenv，
;; https://gitee.com/emacs-eaf/emacs-application-framework
;; install-eaf.py --use-mirror --install app
;; browser video-player pdf-viewer mindmap jupyter
(defvar eaf-path (cond ((file-exists-p "~/emacs-application-framework-master") "~/emacs-application-framework-master")
                       ((file-exists-p "~/emacs-application-framework") "~/emacs-application-framework")
                       (t nil)))
(when eaf-path
  (use-package eaf
    :init
    (add-to-list 'load-path eaf-path)
    :commands(eaf-open
              eaf-get-theme-foreground-color ;; brower
              ;; eaf-app-binding-alist ;; mindmap
              ) 
    :custom
    (eaf-browser-continue-where-left-off t)
    (eaf-browser-enable-adblocker t)
    (browse-url-browser-function 'eaf-open-browser)
    :config
    (defalias 'browse-web #'eaf-open-browser)
    ;; 补充eaf-open
    ;; 测试就成功显示了一次，*eaf*啥也不显示
    (when (file-exists-p (concat eaf-path "/app/markdown-previewer"))
      (use-package eaf-markdown-previewer))
    ;; 补充eaf-open
    (when (file-exists-p (concat eaf-path "/app/pdf-viewer"))
      (use-package eaf-pdf-viewer
        :config
        (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
        (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
        ))
    )
  (when (file-exists-p (concat eaf-path "/app/browser"))
    (use-package eaf-browser
      :init
      (add-to-list 'load-path (concat eaf-path "/app/browser"))
      ;; TODO: 浏览器搜索殷勤，proxy设置
      ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
      :commands(eaf-open-browser
                eaf-open-browser-with-history
                eaf-open-browser-other-window
                eaf-search-it ;; 打开浏览器搜索
                )))
  )

(use-package nyan-mode
  :load-path "~/.emacs.d/themes/nyan-mode-master"
  :defer 1.5
  :config
  (nyan-mode)
  )

(use-package diff-hl
  :load-path "~/.emacs.d/themes/diff-hl-master"
  :defer 1.4
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; 默认修改时会去掉高亮，这个让它保留
  (use-package diff-hl-flydiff
    :config
    (diff-hl-flydiff-mode)
    )

  (use-package diff-hl-margin
    :disabled ;; 修复了show-hunk问题就不需要这个了，不过doom都用的margin模式暂时保留
    :config
    ;; (diff-hl-margin-mode) 这个会遍历buffer-list导致启动卡一下，所以我们手动hook
    (progn
      (add-hook 'diff-hl-mode-on-hook 'diff-hl-margin-local-mode)
      (add-hook 'diff-hl-mode-off-hook 'diff-hl-margin-local-mode-off)
      (add-hook 'diff-hl-dired-mode-on-hook 'diff-hl-margin-local-mode)
      (add-hook 'diff-hl-dired-mode-off-hook 'diff-hl-margin-local-mode-off))
    )
  
  (use-package diff-hl-dired
    :config
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    )

  ;; 点击fringe或margin时，弹出菜单可以转到下一个chunk，很方便！
  (use-package diff-hl-show-hunk
    :config
    (global-diff-hl-show-hunk-mouse-mode)
    ;; 解决相关按钮被view mode覆盖的问题，如q。这里用set-transient-map完全覆盖了原来的按键！
    (defvar set-transient-map-exit-func nil)
    (defun my-diff-hl-inline-popup-hide()
      (interactive)
      (diff-hl-inline-popup-hide)
      (when (functionp set-transient-map-exit-func)
        (funcall set-transient-map-exit-func))
      )
    (defvar diff-hl-inline-popup-transient-mode-map1
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-g") #'my-diff-hl-inline-popup-hide)
        (define-key map [escape] #'my-diff-hl-inline-popup-hide)
        (define-key map (kbd "q") #'my-diff-hl-inline-popup-hide)
        map)
      )
    (set-keymap-parent diff-hl-inline-popup-transient-mode-map1
                       diff-hl-show-hunk-map)
    (setq diff-hl-show-hunk-function (lambda (buffer &optional _ignored-line)
                                       (funcall 'diff-hl-show-hunk-inline-popup buffer _ignored-line)
                                       (setq set-transient-map-exit-func (set-transient-map diff-hl-inline-popup-transient-mode-map1 t 'diff-hl-inline-popup-hide))
                                       )))
  )


(ignore-errors (module-load
                (expand-file-name "~/.emacs.d/bin/emacs_beacon.dll")
                ))
(when (fboundp 'emacs-beacon/set-window-parameters)
  ;; 51afef
  (emacs-beacon/set-window-parameters 300 20 #x51 #xaf #xef 50)
  (use-package beacon
    :defer 1.5
    :init
    (setq beacon-blink-when-focused t)
    (setq beacon-blink-delay 0.01)
    (setq beacon-blink-duration 0.2)
    :config
    (beacon-mode 1)
    
    (defun beacon-blink() (interactive)
           (when  (fboundp 'emacs-beacon/beacon) 
             (let ((p (window-absolute-pixel-position)))
               (emacs-beacon/beacon (car p) ; x
                                    (cdr p) ; y
                                    (truncate (* beacon-blink-duration 1000)) ; timer
                                    (truncate (* beacon-blink-delay 1000)) ; delay
                                    )))))
  )

;; 好的theme特点:
;; treemacs里git非源码里区别明显(doom-one)，
;; eldoc参数当前哪个参数很明显
;; tabbar被修改的*文件有明显显示(spacemacs)
;; 当前buffer modeline背景色(doom-dark+)
;; helm occur关键字高亮明显，不明显就换了吧那个暂时还不知道如何定制
(if (display-graphic-p)
    (progn

      (add-to-list 'load-path "~/.emacs.d/themes")

      ;; 需要手动安装all-the-icons.el-master/fonts里的ttf
      (use-package all-the-icons
        :load-path "~/.emacs.d/themes/all-the-icons.el-master"
        :defer t
        
        )
      ;; treemacs-icons-dired那个当设置doom-colors时有时不显示
      (use-package all-the-icons-dired
        :hook(dired-mode . all-the-icons-dired-mode))
      
      ;; 在modeline提示bell，这个功能太实用了，因为bell被禁止发声了
      (autoload 'doom-themes-visual-bell-config "extensions/doom-themes-ext-visual-bell" "" nil nil)
      (doom-themes-visual-bell-config)
      
      ;; 这是需要最后加载
      ;; doom搜集themes系列
      ;; https://github.com/doomemacs/themes
      (use-package doom-themes
        :load-path "~/.emacs.d/themes/themes-master"
        :config
        (setq doom-themes-enable-bold nil
              doom-themes-enable-italic nil)
        (autoload 'doom-themes-org-config "extensions/doom-themes-ext-org" "" nil nil)

        (defun get-theme(x)
          (intern (replace-regexp-in-string ".*/" "" (string-replace "-theme.el" "" x)))
          )
        ;; 随机加载theme，不对的话建议重启emacs，不然上次的theme可能会干扰本次theme
        (defun random-load-doom-theme(tl)
          (interactive)
          (let* ((th (nth (mod (random t) (length tl)) tl))
                 )
            (message "load-doom-theme: %s" (symbol-name th))

            ;; before load
            (cond ((eq th 'spacemacs-dark)
                   ;; 背景色合适，但颜色绿色太多了，部分颜色要改
                   ;; https://github.com/nashamri/spacemacs-theme#override-themes-colors
                   (custom-set-variables '(spacemacs-theme-custom-colors
                                           '(
                                             (base . "#bbc2cf") ;文本 tangotango
                                             (comment . "#888a85") ; 注释 tangotango
                                             (border . "#292b2e")  ; border太丑了
                                             ))))
                  ((eq th 'doom-one)
                   ;; (setq doom-one-brighter-comments t)
                   )
                  )
            
            (load-theme th t)

            ;; after load
            (doom-themes-org-config)
            (cond ((eq th 'spacemacs-dark)
                   (set-face-attribute 'mode-line-inactive nil :foreground "#888a85")
                   )
                  ((not (string-prefix-p "doom" (symbol-name th)))
                   (set-face-attribute 'doom-themes-visual-bell nil :background "#ff6c6b")
                   
                   )
                  (t
                   ;; 参考的spacemacs
                   (set-face-attribute 'show-paren-match nil :underline t :weight 'bold))
                  )

            (when t
              (setq doom-themes-treemacs-enable-variable-pitch t) 
              ;; treemacs的字体会变小，但风格跟doom的统一了
              ;; doom-clors在打开dired时可能没有图标
              (setq doom-themes-treemacs-theme "doom-colors") ; "doom-colors" doom-atom，
              (autoload 'doom-themes-treemacs-config "extensions/doom-themes-ext-treemacs" "" nil nil)
              (with-eval-after-load 'treemacs
                (doom-themes-treemacs-config))
              )
            )
          )
        ;; (random-load-doom-theme (mapcar 'get-theme (directory-files "~/.emacs.d/themes/themes-master/themes" t "^[a-zA-Z0-9].*.el$")))
        (random-load-doom-theme (list
                                 'doom-one
                                 ;; 'spacemacs-dark
                                 ;; 'modus-vivendi
                                 ))
        )
      
      (when nil
        ;; region有点看不清，单独设置
        (set-face-attribute 'region nil :background "#4C7073")
        )
      )
  (progn
    (set-face-attribute 'region nil :background "#4C7073" :foreground "Black"))
  )

;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
