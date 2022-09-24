﻿;; 非官方自带packages的设置 -*- lexical-binding: t -*-
;; benchmark: 使用profiler-start和profiler-report来查看会影响emacs性能，如造成卡顿的命令等
;; 拖慢gui测试：C-x 3开两个窗口，打开不同的buffer，C-s搜索可能出现比较多的词，测试出doom modeline和tabbar ruler比较慢
;; 参考https://gitee.com/advanceflow/elisp/blob/main/%E9%99%84%E5%BD%95H-%E6%A0%87%E5%87%86%E9%92%A9%E5%AD%90.org 查看各种hook，能去掉就去掉
;; use-package的好处之一是defer可以设置延迟几秒加载！光yas一项就提升了启动速度
;; :load-path不是延迟设置

;; 使用老版本提示
;; 28.0.50，1.查看eln-cache，会有几个文件的TMP一直生成，找到删除对应elc就可以了，不然每次emacs启动都会去编译 2.删除自带的python不然lsp有问题
;; 在使用了org-roam的功能后退出emacs会崩溃，最后发现应该是native-comp的问题，有个gnus-art的文件比较大，但是跟上面一样只有tmp生成，找到删除gnus-art.elc就可以了
;; 以后的崩溃问题都可以参考这个处理，一般是eln-cache里有tmp没编译好造成emacs退出时崩溃
 
(defun update-all-packages()
  (interactive)
  "测试发现对启动速度还是有影响的，这里手动执行更新就可以了，tree-sistter那个用根目录的setup.py下载bin和解压"
  (ensure-latest "~/.emacs.d/themes/diff-hl-master.zip")
  (ensure-latest "~/.emacs.d/themes/themes-master.zip")
  (ensure-latest "~/.emacs.d/packages/citre/citre-master.zip")
  (ensure-latest "~/.emacs.d/packages/corfu/corfu-main.zip")
  (ensure-latest "~/.emacs.d/packages/dired/dired-hacks-master.zip")
  (ensure-latest "~/.emacs.d/packages/expand-region/expand-region.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/easy-kill/easy-kill-extras.el-master.zip" t)
  (ensure-latest "~/.emacs.d/packages/multiple-cursors/multiple-cursors.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/vertico-main.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/embark-master.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/consult-main.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/compat.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/magit/magit-master.zip")
  (ensure-latest "~/.emacs.d/packages/lsp/lsp-bridge-master.zip")
  (ensure-latest "~/.emacs.d/packages/org/emacs-maple-preview-master.zip")
  (ensure-latest "~/.emacs.d/packages/org/org-roam.zip")
  (ensure-latest "~/.emacs.d/packages/org/emacsql-master.zip")
  (ensure-latest "~/.emacs.d/packages/projectile/rg.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/tools/elfeed-master.zip")
  (ensure-latest "~/.emacs.d/packages/use-package/use-package-master.zip")
  (ensure-latest "~/.emacs.d/packages/yasnippet/yasnippet-snippets-master.zip")
  (ensure-latest "~/.emacs.d/packages/tree-sitter/evil-textobj-tree-sitter-master.zip")
  )

;; 用于use-package避免自动设置:laod-path
(defun my-eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun get-file-time (file)
  "获取文件的修改时间"
  (interactive "P")
  (time-convert (file-attribute-modification-time (file-attributes file)) 'integer))

(defun straight--build-compile (dir)
  "核心就是调用byte-recompile-directory，但只有跨进程才不报错"
  (let* (
         (emacs (concat invocation-directory invocation-name))
         (program (format "(let ((default-directory %S))
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory %S 0 'force))"
                          dir dir))
         (args (list "-Q" "-L" dir "--batch" "--eval" program)))
    (with-current-buffer (get-buffer-create "*straight-byte-compilation*")
      (insert (format "\n$ %s %s %s \\\n %S\n" emacs
                      (string-join (cl-subseq args 0 3) " ")
                      (string-join (cl-subseq args 3 5) " ")
                      program)))
    (apply #'call-process
           `(,emacs nil "*straight-byte-compilation*" nil ,@args))))
(defun ensure-latest (zip &optional no-compile)
  "自动更新zip包，zip里的目录组织有格式要求，直接用git下载的zip就可以了"
  (interactive "P")
  (let* ((check-file (expand-file-name (concat ".cache/" (file-name-nondirectory zip)) user-emacs-directory))
         (expand-zip (expand-file-name zip))
         (extdir (file-name-directory expand-zip))
         (target-dir (concat extdir (file-name-base expand-zip))))
    (when (file-newer-than-file-p expand-zip check-file)
      (delete-directory target-dir t nil) ;先删除目录
      (call-process-shell-command (concat "unzip " expand-zip " -d " extdir))
      (unless no-compile
        (message (format "Building %s ..." target-dir))
        (straight--build-compile target-dir))
      ;; 创建空的时间戳文件
      (unless (file-exists-p (expand-file-name ".cache" user-emacs-directory))
        (make-directory (expand-file-name ".cache" user-emacs-directory) t))
      (call-process-shell-command (concat "touch " check-file " -r " expand-zip))
      )))

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/packages/use-package/use-package-master")
  (condition-case nil
      (require 'use-package)
    (error (update-all-packages))) ;; 首次检查是否解压，之后还是手动更新包吧
  )

(add-to-list 'load-path
	     "~/.emacs.d/packages")

;; (ensure-latest "~/.emacs.d/settings/test.zip")
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
  :commands(diminish))

(use-package eldoc
  :defer t
  :diminish(eldoc-mode)
  :init
  (setq eldoc-echo-area-use-multiline-p nil) ;; 不要多行显示
  :config
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore) ;; 在pre-command-hook里影响性能
  )

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
  (use-package dired-recent
    :load-path "~/.emacs.d/packages/dired/dired-hacks-master"
    :commands(dired-recent-mode dired-recent-open)
    :init
    (setq dired-recent-mode-map nil);; 禁止它注册C-x C-d
    )
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
  (setq dired-dwim-target t  ;; 开两个dired的话会自动识别other dired为target
        dired-kill-when-opening-new-dired-buffer t ; 28.1新加的
        )
  (define-key dired-mode-map (kbd "C-l") (lambda () (interactive) (find-alternate-file ".."))) ;; C-l上级目录
  (define-key dired-mode-map (kbd "l") (lambda () (interactive) (find-alternate-file ".."))) ;; l上级目录
  (define-key dired-mode-map (kbd "C-o") 'avy-goto-word-1)
  ;; dired里面再次C-x d可以设置路径
  (define-key dired-mode-map (kbd "C-x d") (lambda ()(interactive) (call-interactively 'dired)))
  (define-key dired-mode-map " " 'View-scroll-page-forward)
  (define-key dired-mode-map [?\S-\ ] 'View-scroll-page-backward)
  (define-key dired-mode-map "w" 'View-scroll-page-backward)
  (define-key dired-mode-map "W" 'dired-copy-filename-as-kill)
  (define-key dired-mode-map "1" 'delete-other-windows)
  (define-key dired-mode-map "\C-t" 'mark-word) ; 还有t, U等mark快捷键

  ;; consult-find -> embark-export to dired-mode工作流无敌！这里改成跟wgrep一样的快捷键
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
  
  (defun delay-dired-relate-init()
    "因为tree-sittr加载会导致dired加载，这里把dired相关mode延迟到dired-mode-hook"
    (remove-hook 'dired-mode-hook 'delay-dired-relate-init)
    
    (dired-recent-mode 1)
    (dired-recent-path-save) ;; 调用一次修复延迟加载导致的问题
    
    (use-package dired-hist
      :config
      (define-key dired-mode-map (kbd "M-p") #'dired-hist-go-back)
      (define-key dired-mode-map (kbd "M-n") #'dired-hist-go-forward)
      (dired-hist-mode 1)
      (dired-hist--update) ;; 调用一次修复延迟加载导致的问题
      )
    ;; dired-quick-sort
    ;;  (setq dired-quick-sort-suppress-setup-warning t)
    (use-package dired-quick-sort
      :config
      (dired-quick-sort-setup)
      (define-key dired-mode-map "s" 'hydra-dired-quick-sort/body) ;; 不用默认的s
      (dired-quick-sort-set-switches) ;; 调用一次修复延迟加载导致的问题
      )
    
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
    
    (use-package diredful
      :init
      (setq diredful-init-file "~/.emacs.d/packages/dired/diredful-conf.el")
      :config
      (diredful-mode 1)
      )
    )
  (add-hook 'dired-mode-hook 'delay-dired-relate-init)
  )

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map "w" 'scroll-down-command)
  (define-key help-mode-map (kbd "M-p") 'help-go-back)
  (define-key help-mode-map (kbd "M-n") 'help-go-forward)
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

;; 还是放弃session的那个file-name-history吧，现在都用这个了
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

;; session的C-x C-/貌似好用一点？
(use-package session
  ;; session只保存了修改文件的point
  ;; 搜索"Open...Recently Changed"可以确定session-file-alist只保存了修改过的文件列表
  ;; C-x C-/可以跳到最近的修改处
  :init
  ;; 不要禁用saveplace hook，不要保存places功能(session只保存了修改文件的point，用saveplace)
  ;; menus需要保留，不然file-name-history即recent files列表不对
  (setq session-initialize (list 'session 'keys)) ; 如果要重新打开文件也可以jump to last，那么就需要'session
  ;; test (string-match session-name-disable-regexp "COMMIT_EDITMSG")
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\|COMMIT_EDITMSG\\)")
  ;; (setq session-set-file-name-exclude-regexp ) ; 过滤 file-name-history
  (setq session-save-file-coding-system 'utf-8)
  (setq session-globals-include '(
                                  ;;(kill-ring 50) ;; 用ditto就行了
				  (session-file-alist 100 t)
				  (file-name-history 300))
        session-locals-include nil)
  
  :config
  (add-hook 'after-init-hook 'session-initialize)
  ;; 使用一段时间后，可以自行查看~/.emacs.d/.session里占用太多，然后添加到排除列表里
  (add-to-list 'session-globals-exclude 'rg-history)
  (add-to-list 'session-globals-exclude 'helm-grep-ag-history)
  (add-to-list 'session-globals-exclude 'helm-grep-history)
  (add-to-list 'session-globals-exclude 'helm-occur-history)
  (add-to-list 'session-globals-exclude 'log-edit-comment-ring)
  (add-to-list 'session-globals-exclude 'helm-source-complex-command-history)
  (add-to-list 'session-globals-exclude 'kmacro-ring)
  (add-to-list 'session-globals-exclude 'file-name-history) ;; recentf记录了
  (add-to-list 'session-globals-exclude 'vertico-repeat-history)
  (add-to-list 'session-globals-exclude 'org-roam-node-history)
  (add-to-list 'session-globals-exclude 'consult--buffer-history)
  (add-to-list 'session-globals-exclude 'buffer-name-history) ;; 这个wcy已经记录了
  (add-to-list 'session-globals-exclude 'consult-xref--history)
  (add-to-list 'session-globals-exclude 'kill-ring)
  )

;; 目前只给dired-quick-sort用，因为session不能保存非consp的变量
(use-package savehist
  :after(dired)
  :init
  (setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
  ;;(setq history-delete-duplicates nil)
  (setq savehist-autosave-interval nil); 只emacs退出时保存，不要timer
  (setq savehist-save-minibuffer-history nil) ;; 我们只给dired的sort用
  :config
  (defun my-init-savehist()
    (savehist-mode +1)
    (remove-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)
    (remove-hook 'dired-mode-hook #'my-init-savehist)
    )
  (add-to-list 'dired-mode-hook 'my-init-savehist)
)


(global-set-key (kbd "C-x f") 'hydra-find-file-select)
(global-set-key (kbd "C-x C-r") 'files-recent-visited)
(global-set-key (kbd "C-c o") 'hydra-occur-select)
(global-set-key (kbd "C-c m") 'hydra-hideshow-select) ; bug:最后一个第3参数必须带名字，否则上面最后一行不显示  

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
    (defun prj-find-file()
      (interactive)
      (call-interactively 'project-find-file))
    (defhydra hydra-find-file ()
      "
_c_: file changed  _v_: file visited
_a_: file at point _e_: consult-everything
_r_: file visited  _p_: file in project
_f_: consult-find  _d_: dired recent
_q_uit
"
      ("f" consult-find nil :color blue)
      ("e" consult-everything nil :color blue)
      ("c" files-recent-changed nil :color blue) ;; 这个只有session才有的，recentf没有
      ("v" files-recent-visited nil :color blue)      ("a" find-file-at-point nil :color blue)
      ("r" files-recent-visited nil :color blue)
      ("p" prj-find-file nil :color blue)
      ("d" dired-recent-open nil :color blue)
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
_q_uit"
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
(use-package mwim
  :commands(mwim-beginning-of-code-or-line
            mwim-beginning-of-line-or-code
            mwim-end-of-code-or-line
            mwim-end-of-line-or-code
            )
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
  )

;; undo-fu小巧才15K
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  (global-set-key (kbd "M-/") 'undo-fu-only-redo)
  (global-set-key (kbd "C-x u") 'undo-fu-only-redo) ;; 这个其实是undo，习惯undo tree这个快捷键了
  )

;; 经常C-x C-s按错，还是用这个吧
(setq auto-save-visited-interval 1
      save-silently t)
(auto-save-visited-mode 1)
(when auto-save-visited-mode
  ;; 参考super save，对于某些命令立即调用保存，避免save buffer yes no提示
  (defvar super-save-triggers '(magit my-project-magit project-compile quickrun
                                      save-buffers-kill-terminal volatile-kill-buffer))
  ;; 调用后立即执行的。eglot只有保存后才会更新code actions
  (defvar super-save-triggers-after '(eglot-code-actions))
  (defun super-save-command-advice (&rest _args) 
    (super-save-command))
  (defun super-save-advise-trigger-commands ()
    (mapc
     (lambda (command)
       (advice-add command :before #'super-save-command-advice))
     super-save-triggers)
    (mapc
     (lambda (command)
       (advice-add command :after #'super-save-command-advice))
     super-save-triggers-after)
    )
  (defun super-save-command ()
    (when auto-save-visited-mode ;; 暂时没有remove advice
      ;; 调用auto-save-visited-mode的timer省一些逻辑
      (apply (timer--function auto-save--timer) (timer--args auto-save--timer)) 
      ))
  (super-save-advise-trigger-commands)
  )

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
(setq EmacsPortable-included-buffers '("*scratch*" "*shell*" "*rg*"
                                       "*eww*" "*xref*" "*org-roam*" "*elfeed-entry*" "*elfeed-search*"))
(defun ep-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
		     ((string-match "^TAGS\\(<.*>\\)?$" (format "%s" (buffer-name b))) nil)
                     ;;((string-match "^magit.*:.*" (format "%s" (buffer-name b))) nil)
                     ((string-match "^magit-.*:.*" (format "%s" (buffer-name b))) nil);; 排除magit-process
                     ((buffer-file-name b) b)
                     ((string-match "^\*gud-.*" (format "%s" (buffer-name b))) b) ;; gud buffer
                     ((string-match "^\*Embark .*" (format "%s" (buffer-name b))) b)
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
        (setq tab-line-tabs-function 'ep-tabbar-buffer-list
              tab-line-close-button-show t
              tab-line-new-button-show nil)
        :config
        (global-tab-line-mode 1)
        (add-to-list 'tab-line-exclude-modes 'speedbar-mode)
        )
      
      )
  (progn
    (global-set-key (kbd "<C-tab>") 'helm-buffers-list)
    (global-set-key (kbd "<C-M-S-tab>") 'helm-buffers-list)
    ))

;; undo-fu作者写的有保障，不添加post-command-hook，高亮只是屏幕可见区域
(use-package idle-highlight-mode
  :defer 1.1
  :init
  (setq idle-highlight-idle-time 0.35 ;; 别太小，同一位置它会一直调用高亮显示
        idle-highlight-exclude-point t ;; 可以设置不高亮光标下
        ;;idle-highlight-ignore-modes (list 'minibuffer-mode)
        idle-highlight-exceptions-syntax nil ;; 默认光标在单词末尾是不高亮的，有点不习惯
        idle-highlight-exceptions-face nil ;; 高亮全部
        )
  ;; 没什么功能，还好embark有实现
  (global-set-key [f3] 'embark-next-symbol)
  (global-set-key [(shift f3)] 'embark-previous-symbol)
  (global-set-key [(control f3)] 'embark-toggle-highlight)
  :config
  ;; 输入时高亮效果不是那么好，这里设置鼠标点击时高亮点击处
  (defadvice idle-highlight--highlight (around my-idle-highlight--highlight activate)
    (if (memq last-command '(mouse-set-point 
                             embark-next-symbol embark-previous-symbol 
                             up-slightly down-slightly
                             next-line previous-line forward-char backward-char)) ;; this-command是nil
        (let ((idle-highlight-exclude-point nil))
          ad-do-it)
      ad-do-it))
  ;; 避免在cursor位置不变时仍然高亮
  (defvar-local idle-highlight-last-point nil)
  (defadvice idle-highlight--time-callback-or-disable (around my-idle-highlight--time-callback-or-disable activate)
    (unless (or (eq (point) idle-highlight-last-point) 
                (bound-and-true-p multiple-cursors-mode))
      ad-do-it
      (setq idle-highlight-last-point (point))
      ))
  (with-eval-after-load 'multiple-cursors-core
    (add-hook 'multiple-cursors-mode-enabled-hook 'idle-highlight--unhighlight)
    (add-hook 'multiple-cursors-mode-disabled-hook (lambda()
                                                     (setq idle-highlight-last-point nil);; 确实刷新
                                                     (idle-highlight--time-callback-or-disable)
                                                     ))
    )
  (global-idle-highlight-mode)
  (custom-set-faces
   '(idle-highlight ((t (:inherit isearch)))) ;; 参见https://gitee.com/advanceflow/elisp/blob/main/40-Emacs%E6%98%BE%E7%A4%BA.org#40128-%E5%9F%BA%E6%9C%AC%E9%9D%A2
   )
  )



(progn
  (defvar last-readonly-state t) ;; 设为t让*scratch*可以正常显示
  (defun my-cursor-chg()
    (when (not (eq last-readonly-state  buffer-read-only))
      (setq last-readonly-state buffer-read-only)
      (let ((cursor-type (if last-readonly-state
                             'box
                           'bar)))
        (modify-frame-parameters (selected-frame) (list (cons 'cursor-type cursor-type))))
      )
    )
  (add-hook 'view-mode-hook 'my-cursor-chg)
  (defvar disable-cursor-chg nil)
  (defvar cursor-chg-timer nil)
  (defun cursor-chg-function()
    (when (and
           (not disable-cursor-chg)
           ;; (not (minibufferp)) ;; minibuffer有bug没有变，但C-s回来又变了
           (not (eq (current-buffer) eldoc--doc-buffer)) ;; 排除eldoc
           )
      ;; (when buffer-read-only
      ;;   (message (buffer-name));; 补全时dabbrev会扫描其它buffer导致调用buffer-list-update-hook
      ;;   )
      (my-cursor-chg)))
  
  (add-hook 'buffer-list-update-hook (lambda()
                                       (when cursor-chg-timer
                                         (cancel-timer cursor-chg-timer))
                                       (setq cursor-chg-timer
                                             (run-with-idle-timer 0.1 nil 'cursor-chg-function))
                                       ))
  )

;;crosshairs不好用，只要vline就行了		
(autoload 'vline-mode "vline" nil t)
(global-set-key [(control ?|)] 'vline-mode)

(when (display-graphic-p)
  (use-package hl-line
    :disabled
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

;; corfu的原理是添加completion-at-point-functions，很标准的做法
;; company机制不清楚。eglot+ctags用corfu好配一点
(use-package corfu
  :defer 1
  :load-path "~/.emacs.d/packages/corfu/corfu-main"
  :commands(global-corfu-mode corfu-mode)
  :init
  
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 1
        corfu-preview-current nil ; 避免直接上屏，有时候输入完了要马上C-n/C-p，这时只需要按个C-g就可以了，而不需要再删除上屏的
        corfu-auto-delay 0.5      ;; 避免输完后马上C-n/C-p也补全
        corfu-quit-at-boundary nil ;; 可以用M-空格来分词
        corfu-quit-no-match t ;; 没有match时退出，不然有个No match影响操作
        )
  (add-to-list 'load-path "~/.emacs.d/packages/corfu/corfu-main/extensions")
  (add-hook 'emacs-lisp-mode-hook 'corfu-mode)
  :config
  (defadvice corfu--update (around my-corfu--update activate)
    (let ((inhibit-message t)) ;; 我们hack的dabbrev运行时会提示错误，实际功能正常
      ad-do-it))
  
  ;; 历史输入排前！
  (use-package corfu-history
    :init
    ;; session自己就记录了corfu-history了，savehist需要配置
    :config
    (corfu-history-mode 1)
    )
  (use-package corfu-quick
    :commands(corfu-quick-insert corfu-quick-complete)
    :init
    (setq corfu-quick1 "arstne"
          corfu-quick2 "ioh")
    (define-key corfu-map (kbd "C-o") 'corfu-quick-insert) ; corfu-quick-complete 效果是一样的，分不清
    )
  (use-package corfu-info
    :commands(corfu-info-documentation corfu-info-location)
    :init
    ;; 对于elisp-completion-at-point后端，这两个是可以用的，但是提示是Dabbrev的话就无法用了
    (define-key corfu-map (kbd "<f1>") 'corfu-info-documentation)
    (define-key corfu-map "\M-l" 'corfu-info-location)
    )
  
  (global-set-key (kbd "<C-return>") 'completion-at-point)
  (define-key corfu-map (kbd "M-n") 'corfu-scroll-up)
  (define-key corfu-map (kbd "M-p") 'corfu-scroll-down)
  
  ;; 将补全移动到minibuffer进行，这样就可以用embark了！
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)
  
  (use-package dabbrev
    :commands(dabbrev--reset-global-variables)
    :init
    ;; from https://eshelyaron.com/esy.html
    ;; 直接用dabbrev-capf有问题，cape的dabbrev也有问题(如它忽略了dabbrev-abbrev-char-regexp导致中文设置不生效，另外补全项好像没有dabbrev-completion多？)
    (defvar has-dabbrev-capf nil)
    (defun my-dabbrev-capf ()
      "Workaround for issue with `dabbrev-capf'."
      (let ((inhibit-message t)
            (disable-cursor-chg t)
            ) ;; 屏蔽dabbrev和corfu的消息
        (dabbrev--reset-global-variables)  
        (setq dabbrev-case-fold-search nil)
        (if has-dabbrev-capf
            (dabbrev-capf)
          (cl-letf (((symbol-function #'completion-in-region)
                     (lambda (beg end table &rest args)
                       (list beg end table)
                       )))
            (dabbrev-completion)) ;; hack dabbrev-completion to return list
          )
        ))
    (add-to-list 'completion-at-point-functions 'my-dabbrev-capf)
    :config
    (setq has-dabbrev-capf (functionp 'dabbrev-capf))
    )
  
  (defun esy/file-capf ()
    "File completion at point function."
    (let ((bs (bounds-of-thing-at-point 'filename)))
      (when bs
        (let* ((start (car bs))
               (end   (cdr bs)))
          `(,start ,end completion--file-name-table . (:exclusive no))))))
  (add-to-list 'completion-at-point-functions 'esy/file-capf t)
  
  (when (functionp 'eglot-ensure)
    ;; eglot的capf没有:exclusive标识，所以是独占的，这里补充tag补全
    ;; 参考https://github.com/seagle0128/.emacs.d/blob/8f1a2fc483da8cb430f3bd53e6a5f7ce392c3c4f/lisp/init-ctags.el
    (defun lsp-other-capf-function ()
      (let ((lsp-result (eglot-completion-at-point)))
        (if (and lsp-result
                 (try-completion
                  (buffer-substring (nth 0 lsp-result)
                                    (nth 1 lsp-result))
                  (nth 2 lsp-result)))
            lsp-result
          ;; TODO: 添加其它后端
          (tags-completion-at-point-function))))
    (defun enable-eglot-other-backend()
      "替换eglot的补全"
      (setq-local completion-at-point-functions (cl-nsubst #'lsp-other-capf-function
			                                   'eglot-completion-at-point
			                                   completion-at-point-functions))
      )
    (add-hook 'eglot-managed-mode-hook 'enable-eglot-other-backend))
  )
       
 

;; 一来就加载mode确实挺不爽的，还是用这个了
(use-package wcy-desktop
  ;;:defer 0.5
  :config
  (defadvice wcy-desktop-load-file (after my-wcy-desktop-load-file activate)
    (setq buffer-undo-list nil) ;; 解决undo-tree冲突
    ;; 修正buffer打开时的point
    (when (featurep 'saveplace)
      (save-place-find-file-hook))
    ;; win10莫名其妙鼠标指针变圆圈，不变回来了，这个trick可以让它变回来
    (run-with-idle-timer 0.1 nil (lambda ()
                                 (let ((f (selected-frame)))
                                   (set-mouse-position f (car (cdr (mouse-position))) (cdr (cdr (mouse-position))))
                                   t)
                                 )) 
    )
  (add-hook 'emacs-startup-hook
            (lambda ()
	      (ignore-errors
                (wcy-desktop-init)
		(wcy-desktop-open-last-opened-files))))
  )

(use-package google-c-style
  :commands( google-set-c-style))

(use-package cc-mode
  :defer t
  :config
  ;; 自带的key基本没什么用
  (setq c-mode-base-map (make-sparse-keymap)
        c-mode-map (make-sparse-keymap)
        c++-mode-map (make-sparse-keymap)
        objc-mode-map (make-sparse-keymap)
        java-mode-map (make-sparse-keymap)
        idl-mode-map (make-sparse-keymap)
        pike-mode-map (make-sparse-keymap)
        awk-mode-map (make-sparse-keymap)
        ))

(defun my-c-mode-hook-set()
  (google-set-c-style)
  (setq c-basic-offset 4) ;; tab4个空格习惯了
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
  (remove-hook 'pre-command-hook 'jl-pre-command-check) ;; 影响效率，用advice替代
  (global-set-key (kbd "M-n") 'jl-jump-forward)
  (global-set-key (kbd "M-p") 'jl-jump-backward)
  (defun my-push-mark-wrapper (&rest args)
    (when (symbolp this-command)
      (jl-insert-marker))
    )
  (defvar jump-commands '(
                          jl-jump-backward 
                          jl-jump-forward
                          beginning-of-buffer
                          end-of-buffer
                          jump-to-register
                          mark-whole-buffer
                          next-buffer
                          previous-buffer
                          switch-to-buffer
                          describe-function
                          describe-variable
                          find-file-at-point
                          xref-find-definitions
                          session-jump-to-last-change
                          org-roam-preview-visit
                          counsel-rg consult-ripgrep consult-line
                          avy-goto-word-1
                          my-consult-ripgrep embark-act consult-imenu-multi keyboard-escape-quit
                          lsp-bridge-find-define lsp-bridge-find-def
                          embark-next-symbol embark-previous-symbol
                          my-pop-select
                          ))
  (cl-dolist (jc jump-commands)
    (advice-add jc :before #'my-push-mark-wrapper)
    )  
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

(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

(defun my-elisp-hook()
  (make-local-variable 'eldoc-idle-delay)
  (setq eldoc-idle-delay 0.5) ;; 设置为0会导致鼠标点击时不显示
  (turn-on-eldoc-mode)
  (setq-local tab-width 8) ;; 官方源码很多TAB，又是按8
  )
(find-function-setup-keys)  ;直接定位函数变量定义位置的快捷键，C-x F/K/V，注意是大写的
(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-elisp-hook)


;;; TODO 如果编译过其他目录后，另一个目录C-F7时当前目录没有变，必须C-u F7重新配置
;; compile，加入了单独编译某个文件
(setq compilation-auto-jump-to-first-error nil ; 自动跳到错误，这个在只有warning时相当烦！
      compilation-scroll-output t)

;; expand-region被 easy-kill的easy-mark替换了，但要保留会被调用 
(use-package expand-region
  :load-path "~/.emacs.d/packages/expand-region/expand-region.el-master"
  :commands(er/expand-region er/contract-region)
  :init
  (global-set-key (kbd "C-S-t") 'er/contract-region)
  :config
  ;; see https://github.com/magnars/expand-region.el/issues/229
  ;; (global-set-key (kbd "C-q") #'(lambda (arg)
  ;;   			                  (interactive "P")
  ;;   			                  (setq transient-mark-mode t)
  ;;   			                  (set-mark-command arg)))
  )

(use-package smart-region
  :commands(smart-region) 
  :init
  ;; C-q不动再按C-q触发expand region，移动到其它行，同一列触发multiple-cursors，不同列是rectangle-mark
  ;; 同列触发mc这个要常用，不过有bug有汉字的话判定为rectangle-mark
  (global-set-key (kbd "C-q") 'smart-region)
  :config
  (when (functionp 'which-key--show-keymap)
    (defadvice smart-region (after my-smart-region activate)
      (when rectangle-mark-mode
        (which-key--show-keymap "keymap" ctl-x-r-map nil nil 'no-paging)
        (set-transient-map ctl-x-r-map (lambda()rectangle-mark-mode) 'which-key--hide-popup)
        )
      ))
  )

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
  :load-path "~/.emacs.d/packages/multiple-cursors/multiple-cursors.el-master"
  :init
  ;; mc询问你的命令都保存在这里面了
  (setq mc/list-file "~/.emacs.d/packages/multiple-cursors/my-cmds.el"
        mc/always-run-for-all t ;; 禁止提示，需要run once的在my-cmds.el里加
        )
  (global-unset-key (kbd "<M-down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-M-<mouse-1>") 'mc/unmark-next-like-this) ; 取消光标以下的mark
  (global-set-key (kbd "M-S-<mouse-1>") 'mc/unmark-previous-like-this) ;取消光标以上的mark
  (global-set-key (kbd "M-<wheel-up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-<wheel-down>") 'mc/mark-next-like-this)
  ;; (global-set-key (kbd "C-S-t") 'mc/edit-lines)  ;居然不支持同行的range
  (global-set-key (kbd "<f8>") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<f8>") 'mc/mark-all-dwim) ;; 最智能！无须选中自动选中当前symbol，也支持region，多行region是选里面的！先是选中defun里的，再按是所有！
  (global-set-key (kbd "S-<f8>") 'mc/skip-to-next-like-this) ;; 跳过当前选中
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim) ;; dwim的更智能
  ;; Tips: C-'可以隐藏没有选中的项，modeline有提示当前有多少mc项
  :commands(multiple-cursors-mode
            mc/add-cursor-on-click
            mc/unmark-next-like-this
            mc/unmark-previous-like-this
            mc/mark-previous-like-this
            mc/mark-next-like-this
            mc/edit-lines
            mc/mark-all-dwim
            mc/skip-to-next-like-this
            mc/mark-all-like-this-dwim
            )
  :config
  (define-key mc/keymap (kbd "C-v") nil)
  (define-key mc/keymap (kbd "RET") 'multiple-cursors-mode) ;; 退出，C-J输入换行
  ;; 改变指针颜色以示区别 
  (defvar last-cursor-color nil)
  (add-hook 'multiple-cursors-mode-enabled-hook (lambda()
                                                  (unless last-cursor-color
                                                    (setq last-cursor-color (frame-parameter nil 'cursor-color)))
                                                  (set-cursor-color "yellow")))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda()
                                                   (when last-cursor-color
                                                     (set-cursor-color last-cursor-color))
                                                   ))
  )

;; avy可以配得跟ace jump完全一样，就没必要保留ace jump了
(use-package avy
  :commands(avy-goto-char-timer avy-goto-word-1 avy-goto-line avy-resume)
  :init
  (define-key global-map (kbd "C-o") 'avy-goto-word-1)
  (global-set-key [remap goto-line] 'avy-goto-line)
  (setq avy-background t) ;; 开启跟ace一样了
  (setq-default avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  :config
  ;; 神器啊！avy还可以直接对目标进行一些操作。先C-o输入单词开头字母，然后按命令key，再输入目标的标识就可以
  ;; 这里实现了不跳到目标就执行相关操作，avy-dispatch-alist
  ;; 相关代码抄自 https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  
  (when (fboundp 'embark-act)
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark
          (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay ; 自带用的X，x会kill并跳过去
          (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
          ;; (alist-get ?y avy-dispatch-alist) 'avy-action-yank
          (alist-get ?w avy-dispatch-alist) 'avy-action-copy ; 原n是不能用了
          (alist-get ?c avy-dispatch-alist) 'avy-action-copy-whole-line
          (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
          ;; (alist-get ?t avy-dispatch-alist) 'avy-action-teleport ; 目标剪切到当前处
          (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line  ;; 目标整行剪切到此处
          )
    ))

;; 删除当前位置到某个位置，要学会常用啊，比选中再删除快多了
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;; autohotkey文件编辑
(autoload 'xahk-mode "xahk-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;;; rust mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(modify-coding-system-alist 'file "\\.rs\\'" 'utf-8-with-signature) ; 带中文必须这个编码，干脆就默认

(use-package sr-speedbar
  :commands(sr-speedbar-toggle)
  :init
  (setq sr-speedbar-right-side nil
        sr-speedbar-auto-refresh t
        dframe-update-speed 0.2   ;; 自动刷新时间
        speedbar-show-unknown-files t ;; 我去rust文件都不认识？
        speedbar-obj-do-check nil ;; 默认检查如c文件的obj文件是否更新
        speedbar-vc-do-check nil ;; 好像不支持git吧
        speedbar-directory-unshown-regexp "^\(\.\.*$\)\'" ;; 显示unkown dir
        ;; speedbar-hide-button-brackets-flag t
        speedbar-use-images t ;; 图标太丑了，或许可以用all-the-icon?
        ;; 关键函数 speedbar-make-tag-line  speedbar-image-dump 可以查看所有图标
        )
  (global-set-key (kbd "<C-f1>") 'sr-speedbar-toggle)
  :config
  (unless sr-speedbar-auto-refresh
    ;; 重新打开时更新目录
    (defadvice sr-speedbar-toggle (before my-sr-speedbar-toggle activate)
      (unless (sr-speedbar-exist-p)
        (message default-directory)
        (sr-speedbar-refresh)
        t)
      ))
  (define-key speedbar-mode-map (kbd "q") 'sr-speedbar-close)
  (define-key speedbar-mode-map (kbd "l") 'speedbar-up-directory)
  (define-key speedbar-mode-map (kbd "w") 'speedbar-scroll-down)
  (define-key speedbar-mode-map (kbd "SPC") 'speedbar-scroll-up)
  (define-key speedbar-file-key-map (kbd "<") 'beginning-of-buffer)
  (define-key speedbar-file-key-map (kbd ">") 'end-of-buffer)
  (define-key speedbar-file-key-map (kbd "SPC") 'speedbar-scroll-up)
  (define-key speedbar-file-key-map (kbd "S-SPC") 'speedbar-scroll-down)
  (define-key speedbar-file-key-map (kbd "RET") 'speedbar-toggle-line-expansion) ;; 原来是直接进入目录，只需要展开就行了
  )
  


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

;; 自动搜索光标下的单词，非helm的方案都需要这个
(defun enable-minibuffer-auto-search-at-point()
  ;; 参考https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  ;; 还有个更简单的https://emacs-china.org/t/xxx-thing-at-point/18047，但是不太注意细节
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines keep-lines ivy-read
      swiper swiper-backward swiper-all
      swiper-isearch swiper-isearch-backward
      lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
      counsel-grep-or-swiper counsel-grep-or-swiper-backward
      counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt
      my-project-search my-counsel-rg ;; call-interactively 'counsel-rg的函数需要加进来
      consult-line consult-ripgrep
      my-consult-ripgrep my-consult-ripgrep-only-current-dir my-consult-ripgrep-or-line
      ))

  (defvar my-ivy-fly-back-commands
    '(self-insert-command
      ivy-forward-char ivy-delete-char delete-forward-char kill-word kill-sexp
      end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
      yank ivy-yank-word ivy-yank-char ivy-yank-symbol counsel-yank-pop))

  (defvar-local my-ivy-fly--travel nil)
  (defun my-ivy-fly-back-to-present ()
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command my-ivy-fly-back-commands)
               (equal (this-command-keys-vector) (kbd "M-n")))
           (unless my-ivy-fly--travel
             (delete-region (point) (point-max))
             (when (memq this-command '(ivy-forward-char
                                        ivy-delete-char delete-forward-char
                                        kill-word kill-sexp
                                        end-of-line mwim-end-of-line
                                        mwim-end-of-code-or-line
                                        mwim-end-of-line-or-code))
               ;; 如果是C-e之类，会重新插入搜索内容，但不再是灰的啦
               (when (functionp 'ivy-cleanup-string)
                 (insert (ivy-cleanup-string ivy-text)))
               (when (featurep 'vertico)
                 (insert (substring-no-properties (or (car-safe vertico--input) ""))))
               (when (memq this-command '(ivy-delete-char
                                          delete-forward-char
                                          kill-word kill-sexp))
                 (beginning-of-line)))
             (setq my-ivy-fly--travel t)))))

  (defvar disable-for-vertico-repeat nil)
  (defun my-ivy-fly-time-travel ()
    (unless disable-for-vertico-repeat
      (when (memq this-command my-ivy-fly-commands)
        (insert (propertize
                 (save-excursion
		   (set-buffer (window-buffer (minibuffer-selected-window)))
                   ;; 参考https://emacs-china.org/t/xxx-thing-at-point/18047，可以搜索region
		   (or (seq-some (lambda (thing) (thing-at-point thing t))
				 '(region symbol)) ;; url sexp
		       "")
                   )
                 'face 'shadow))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)
        (beginning-of-line)))
    )

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))
  )

(progn
  (add-to-list 'load-path "~/.emacs.d/packages/minibuffer")
  ;; 主要参考https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el
  (use-package vertico
    :load-path "~/.emacs.d/packages/minibuffer/vertico-main"
    :init
    (setq enable-recursive-minibuffers t ; 允许minibuffer里再执行命令
          vertico-cycle t ;; vertico不同于其它的是当前行之前的结果被列在了最后
          vertico-resize nil ;; 让它初始化就固定大小
          vertico-count 20   ;; 高度多少行
          )
    :defer 0.3
    :config
    (vertico-mode 1)
    (enable-minibuffer-auto-search-at-point) ;; consult有个:initial也可以设置，不过搜索其它的话要先删除
    ;; extension说明
    ;; vertico-buffer.el     用buffer窗口代替minibuffer，但是多个窗口不确定它会出现在什么地方
    ;; vertico-directory.el  测试没成功
    ;; vertico-flat.el       跟原生那样的补全，无用
    ;; vertico-grid.el       自动多列显示！这个很牛。但中文乱码排列也会乱。行太长会自动隐藏，所以需要大显示屏
    ;; vertico-indexed.el    给结果列表前面加上数字索引
    ;; vertico-mouse.el      鼠标支持，好像就支持点击，scroll都不行
    ;; vertico-multiform.el  设置自动执行某种如grid视图
    ;; vertico-quick.el      快速插入或者快速jump，后者实现类似avy
    ;; vertico-repeat.el     helm resume功能，但只记录搜索词，最后执行哪行不知道。关键还跟上面自动搜索光标下的设置冲突
    ;; vertico-reverse.el    reverse结果列表

    (define-key vertico-map (kbd "C-s") 'vertico-next) ; 不支持在结果里搜索
    ;; (define-key vertico-map (kbd "C-r") 'vertico-previous) ;; C-r可以复制
    (defun is-consult-ripgrep()
      (eq 'consult-grep
          (completion-metadata-get
           (completion-metadata (minibuffer-contents)
                                minibuffer-completion-table
                                minibuffer-completion-predicate)
           'category)))
    (defun is-consult-line()
      (eq 'consult-location ;; 其它有几个也是这个，影响不大
          (completion-metadata-get
           (completion-metadata (minibuffer-contents)
                                minibuffer-completion-table
                                minibuffer-completion-predicate)
           'category)))
    (defun my/vertico-C-l ()
      "vertico find-file和consult-ripgrep都是共用的，让C-l在consult-ripgrep执行搜索父目录"
      (interactive)
      ;; 判断当前是否执行consult-grep，参考(vertico-directory--completing-file-p)
      (if (is-consult-ripgrep)
          (progn
            ;; 参考enable-minibuffer-auto-search-at-point获取当前输入
            (let ((text (substring-no-properties (or (car-safe vertico--input) ""))
                        )
                  (dir (file-name-directory (directory-file-name default-directory))))
              ;; (delete-minibuffer-contents) ;; 参考vertico-directory-up
              ;; (insert text) ;; 只改内容，preview和RET都不正常，还是要重新搜索下
              ;; minad大佬的解决办法跟我的一样 https://github.com/minad/consult/issues/596
              (run-at-time 0 nil (lambda ()
                                   (let ((this-command 'my-consult-ripgrep) ;; 以consult-buffer形式查看
                                         (disable-for-vertico-repeat t))
                                     (my-consult-ripgrep dir text))
                                   ))
              (abort-recursive-edit) ;; 用vertio-exit C-g就不能回到原来位置
              ))
        (call-interactively 'vertico-directory-delete-word)))
    (defun my/vertico-tab()
      (interactive)
      (if (or (is-consult-line) (is-consult-ripgrep))
          (progn
            (let ((this-command 'end-of-line)) ;; 由于enable-minibuffer-auto-search-at-point的设置，只需要移动到末尾就自动填写搜索词了
              (my-ivy-fly-back-to-present) ;; 单纯调用call-interactively C-s还是灰色的
              ))
        (call-interactively 'vertico-insert)))
    ;; 另一种设置的方法 https://github.com/minad/consult/wiki#add-category-specific-minibuffer-keybindings
    (define-key vertico-map (kbd "C-l") 'my/vertico-C-l) ;; 转上级目录，或者搜索上级目录
    (define-key vertico-map (kbd "<tab>") 'my/vertico-tab) ;; rg时tab是插入搜索词
    (define-key vertico-map (kbd "C-j") 'vertico-exit-input) ; 避免选中项，比如新建文件，但列表有命中项时。默认绑定M-r
    (define-key vertico-map (kbd "M-o") 'vertico-next-group) ;; 下个组,C-o给avy了
    (define-key vertico-map (kbd "M-O") 'vertico-previous-group) ;; 上个组

    ;; 习惯只要underline，不随主题改变
    (custom-set-faces
     '(vertico-current ((t (:inherit unspecified :underline t :background nil :distant-foreground nil :foreground nil)))) 
     '(consult-preview-line ((t (:underline t :background nil))))
     )

    ;; 只会恢复关键词
    ;; consult-line需要配合(setq consult-line-start-from-top nil)，这样首行就是当前位置
    ;; consult-ripgrep暂时没有好方法
    (use-package vertico-repeat
      :load-path "~/.emacs.d/packages/minibuffer/vertico-main/extensions"
      :commands(vertico-repeat vertico-repeat-save)
      :init
      (global-set-key [f6] #'vertico-repeat) ; C-u F6还可以选择
      (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
      :config
      ;; 避免跟enable-minibuffer-auto-search-at-point冲突
      (defadvice vertico-repeat (around my-vertico-repeat activate)
        (let ((disable-for-vertico-repeat t))
          ad-do-it
          )))
    (use-package vertico-directory
      :commands(vertico-directory-delete-word)
      )
    (use-package vertico-quick
      :commands(vertico-quick-exit)
      :init
      (setq vertico-quick1 "arstne"
            vertico-quick2 "ioh")
      ;; 类似avy，我一直想在helm中实现的
      (define-key vertico-map "\C-o" #'vertico-quick-exit))
    (use-package vertico-grid
      :commands(vertico-multiform-grid vertico-grid-mode)
      :init
      (define-key vertico-map "\M-G" #'vertico-multiform-grid))
    (use-package vertico-reverse
      :commands(vertico-multiform-reverse vertico-reverse-mode)
      :init
      (define-key vertico-map "\M-R" #'vertico-multiform-reverse))
    (use-package vertico-multiform
      :init
      ;; 定制什么命令使用何种布局，太爽了！  buffer默认height要高一些，其它好像并没有什么不同
      ;; (defadvice vertico-multiform--setup (before my-vertico-multiform--setup activate)
      ;;   (message (concat "last command:" (symbol-name this-command)))
      ;;   )
      (setq vertico-multiform-commands
            '(
              (consult-line buffer) ; buffer 可以显示更多搜索出来的内容
              (my-consult-ripgrep buffer)
              (my-project-search buffer)
              (project-find-regexp buffer) ;; project内置搜索，xarg发送files给rg可以搜索被ignore目录里的force add文件
              (dired-do-find-regexp buffer) ;; dired里的A，可以只搜索mark了的文件
              (my-consult-ripgrep-only-current-dir buffer)
              (consult-ripgrep buffer) 
              (execute-extended-command grid) ; M-x
              (yas-insert-snippet grid)))
      ;; (setq vertico-multiform-categories
      ;;       '((file buffer grid)
      ;;         (imenu (:not indexed mouse))
      ;;         (symbol (vertico-sort-function . vertico-sort-alpha))))
      (define-key vertico-map "\M-V" #'vertico-multiform-vertical)
      :config
      (vertico-multiform-mode))
    
    (use-package vertico-buffer
      :commands(vertico-buffer-mode)
      :init
      ;; (setq vertico-buffer-display-action '(display-buffer-in-side-window))
      )
    (use-package vertico-mouse
      :config
      (vertico-mouse-mode))
    )
  (with-eval-after-load 'vertico
    (use-package orderless
      ;; 启动用无序匹配，本质是一个自动正则生成器，结果给completion-styles用
      :config
      ;; https://github.com/minad/consult/wiki#minads-orderless-configuration
      ;; 以这些开头或者结尾都是可以的
      (defvar +orderless-dispatch-alist
        '((?% . char-fold-to-regexp) ;; 默认就是支持regex，这里%可以转换欧州那些字母为英文
          (?! . orderless-without-literal) ;; !不包含文本
          (?`. orderless-initialism) ;; 每个字母都是一个词的开头，如`or匹配oaaa rbbb
          (?= . orderless-literal) ;; 纯文本包含(regexp-quote实现)
          (?~ . orderless-flex)) ;; fuzzy，不过是有顺序的
        )
      (defun +orderless--suffix-regexp ()
        (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
            (format "[%c-%c]*$"
                    consult--tofu-char
                    (+ consult--tofu-char consult--tofu-range -1))
          "$"))
      (defun +orderless-dispatch (word _index _total)
        (cond
         ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
         ((string-suffix-p "$" word)
          `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
         ;; File extensions
         ((and (or minibuffer-completing-file-name
                   (derived-mode-p 'eshell-mode))
               (string-match-p "\\`\\.." word))
          `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
         ;; Ignore single !
         ((equal "!" word) `(orderless-literal . ""))
         ;; Prefix and suffix
         ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
              (cons (cdr x) (substring word 1))
            (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
              (cons (cdr x) (substring word 0 -1)))))))
      ;; +orderless-with-initialism直接进completion-styles-alist了
      (orderless-define-completion-style +orderless-with-initialism
        (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
      (defun my/orderless-dispatch-flex-first (_pattern index _total)
        "https://github.com/minad/corfu/wiki#advanced-example-configuration-with-orderless"
        (and (eq index 0) 'orderless-flex))
      ;; eglot本身就是flex的，只需要设置elisp就行了，也可以用空格分词(需要设置 corfu-quit-at-boundary)
      (add-hook 'emacs-lisp-mode-hook (lambda ()
                                        (make-local-variable 'orderless-style-dispatchers)
                                        (setq orderless-style-dispatchers '(+orderless-dispatch my/orderless-dispatch-flex-first))))
      (setq completion-styles '(orderless basic)
            completion-category-defaults nil
            completion-category-overrides '((file (styles flex)) ;; helm是flex
                                            (command (styles +orderless-with-initialism)) ; 相当于ivy的^吧？
                                            (variable (styles +orderless-with-initialism))
                                            (symbol (styles +orderless-with-initialism)))
            orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
            orderless-style-dispatchers '(+orderless-dispatch) ; 按+orderless-dispatch-alist执行相应的筛选
            )

      ;; https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind
      ;; 让rg也用上上面的特殊符号等，这个很爽！
      (defun consult--orderless-regexp-compiler (input type &rest _config)
        (setq input (orderless-pattern-compiler input))
        (cons
         (mapcar (lambda (r) (consult--convert-regexp r type)) input)
         (lambda (str) (orderless--highlight input str))))
      (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)
      )
    
    ;; 美化
    (use-package marginalia
      :commands(marginalia-mode)
      :bind (("M-A" . marginalia-cycle)
             :map minibuffer-local-map
             ("M-A" . marginalia-cycle))
      :init
      (marginalia-mode)
      :config
      )
    (use-package embark
      :load-path "~/.emacs.d/packages/minibuffer/embark-master"
      :commands(embark-export
                embark-act
                embark-act-with-completing-read 
                embark-next-symbol 
                embark-previous-symbol 
                embark-toggle-highlight)
      :bind
      (("M-." . embark-act)  ;; 按这个后，还可以按其它prefix key如C-x调用C-x开头的键，起了which-key的作用！
       ;; ("M-." . embark-dwim) ;; 除非你知道每个object默认的操作，否则还是embark-act吧
       ("<f1> B" . embark-bindings) ;; 列举当前可用的键及其命令
       )
      :init
      (setq
       embark-mixed-indicator-delay 0   ;; 按钮提示菜单延迟，熟练后可以设置长点
       ;; embark-quit-after-action nil     ;; 默认就退出minibuffer了
       )
      (with-eval-after-load 'vertico
        (define-key vertico-map (kbd "C-.") 'embark-act)
        (define-key vertico-map (kbd "C-c C-o") 'embark-export)
        (define-key vertico-map (kbd "C-c C-c") 'embark-act)
        )
      :config
      (setq prefix-help-command #'embark-prefix-help-command) ;; C-h可以输入命令，有时候显示不全或许记不住命令行
      (define-key embark-file-map (kbd "C-x C-d") (lambda (file)
                                                    (interactive "f")
                                                    (browse-file-in-explorer file)))
      )
    )
  ;; 预览功能很快！好像不是真的加载
  (use-package consult
    :load-path "~/.emacs.d/packages/minibuffer/consult-main"
    :commands(consult-ripgrep
              consult-buffer ;; buffer+recent+bookmark
              consult-line
              consult-buffer-other-window
              consult-buffer-other-frame
              consult-goto-line ;; 带预览
              consult-completion-in-region ;; 将补全移动到minibuffer进行
              consult-project-buffer;; buffer+file
              consult-locate ;; everything!
              consult-find ;; minad说fd不太成熟，就用find吧
              )
    :init
    
    (setq
     consult-line-start-from-top nil ;; nil前面行会排后面，但t初始行是最前面那个
     consult-line-point-placement 'match-beginning ; jump后跳到匹配词的开头
     consult-async-min-input 1 ;; 确保单个汉字也可以搜索
     ;; consult-fontify-max-size 1024 ;; 不设置的话大文件第1次用consult-line会卡几秒，设置consult-fontify-preserve就不需要设置这个了
     consult-fontify-preserve nil ;; 直接禁用fontify，副作用是搜索到的没有color，没什么影响
     consult-async-split-style nil ;; 默认async是'perl会有个#在开头，而consult-eglot过滤的话还要删除那个#按f空格才可以
     consult-locate-args (encode-coding-string "es.exe -n 30 -p -r" 'gbk)
     )
    ;; consult的异步没有通过cmd proxy，这点很棒！
    (add-to-list 'process-coding-system-alist '("[rR][gG]" . (utf-8 . gbk-dos))) ;; rg支持中文
    (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
    (add-to-list 'process-coding-system-alist '("[fF][iI][nN][dD]" . (utf-8 . gbk-dos))) ;; find支持中文
    
    ;; https://github.com/phikal/compat.el
    (use-package compat
      :defer t
      :load-path "~/.emacs.d/packages/minibuffer/compat.el-master"
      :init
      (unless (functionp 'ensure-list)
        (defun ensure-list (object)
          ;; for 28.0.5
          (if (listp object)
              object
            (list object)))
        ))
    
    (defun my-consult-ripgrep(&optional dir initial)
      (interactive "P") ;; C-u F2可以选择dir，否则就是当前目录
      (require 'consult)
      ;; 不忽略ignore
      (let  ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore")))
        (consult-ripgrep (or dir default-directory) initial))
      )
    (defun my-consult-ripgrep-only-current-dir(&optional dir)
      (interactive "P") ;; C-u F2可以选择dir，否则就是当前目录
      (require 'consult)
      ;; 不忽略ignore
      (let  ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore -g!*/")))
        (consult-ripgrep (or dir default-directory)))
      )
    (global-set-key [f2] 'my-consult-ripgrep)
    (global-set-key [S-f2] 'my-consult-ripgrep-only-current-dir)
    
    ;; 按f/b/m/p加上空格可以只显示相应files/buffer/bookmark/project！(project默认hidden)
    (global-set-key (kbd "C-x C-b") 'consult-buffer)
    (global-set-key (kbd "C-s") 'consult-line)  ;; consult-line -> embark-export to occur-mode 按e进行编辑，是实时更新buffer的
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)
    (global-set-key [remap bookmark-jump] 'consult-bookmark)
    ;; 默认有点问题，解决办法来自 https://github.com/minad/consult/issues/317#issuecomment-980797343，
    ;; 处理w32-quote-process-args后，上面consult--regexp-compiler设置为orderless也有效了！
    (global-set-key [(control f2)] (lambda ()
                                     (interactive)
                                     (let ((w32-quote-process-args ?\\) ;; or (w32-quote-process-args ?*)
                                           )
                                       (call-interactively 'consult-find))))
    
    (defun my-project-imenu()
      (interactive)
      (if (bound-and-true-p eglot--managed-mode)
          (call-interactively 'consult-eglot-symbols)
        (call-interactively 'consult-imenu-multi)))
    
    (use-package consult-imenu
      :commands(consult-imenu consult-imenu-multi)
      :init
      ;; 所有project打开的buffer中查找，太爽了！因为函数名/变量等没有多少，所以没有效率问题
      ;; (global-set-key [(control ?\,)] 'consult-imenu-multi)
      (global-set-key (kbd "M-m") 'consult-imenu)
      (keyboard-translate ?\C-m ?\H-m)
      (global-set-key [?\H-m] 'consult-imenu)
      )
    (use-package consult-org
      :commands(consult-org-agenda) ; 这个很卡啊，还是不替换C-c a了(C-c a再按t也比较卡，应该是org mode问题)
      )
    (use-package consult-xref
      :after(xref) ;; 有时候显示结果为0，但只要一输入或者移动就有结果了
      :commands(consult-xref)
      :init
      (setq xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref)
      )
    ;; 还有个consult-lsp如果用lsp mode的话
    (use-package consult-eglot
      :commands(consult-eglot-symbols)
      :init
      (global-set-key [(control ?\,)] 'my-project-imenu)
      ;; 好像没办法过滤，只有用vertico的f+空格过滤function，其它见`consult-eglot-narrow'
      )
    (use-package consult-project-extra
      :commands(consult-project-extra-find))
    ;; 正是我需要的，给marginalia添加yas应该不简单，这个能显示按短字符，还能看预览插入效果！
    (use-package consult-yasnippet
      :commands(consult-yasnippet
                consult-yasnippet-visit-snippet-file ;; 打开编辑snippet文件
                ))
    ;; 目前仅能用简单的orderless，不支持上面的~=等，将就用吧
    (use-package consult-everything
      :commands(consult-everything)
      :init
      (setq consult-everything-args "es -p -n 30 -r") ;; -i是区分大小写
      ;; 它默认用consult--regexp-compiler，跟我们的设置冲突
      (defun consult--with-orderless (&rest args)
        (minibuffer-with-setup-hook
            (lambda ()
              (setq-local consult--regexp-compiler #'consult--default-regexp-compiler))
          (apply args)))
      (advice-add #'consult-everything :around #'consult--with-orderless)
      :config
      )
    ;; 会绑定consult-file-externally到embark里
    (use-package embark-consult
      :after (embark)
      :demand t
      :hook
      (embark-collect-mode . consult-preview-at-point-mode))
    ;; 随时选择路径
    (use-package consult-dir
      :commands(consult-dir)
      :init
      (with-eval-after-load 'vertico
        (define-key vertico-map "\M-D" 'consult-dir))
      ;; (define-key vertico-map (kbd "C-x C-d") 'consult-dir)
      :config
      (defadvice consult-dir--recentf-dirs (around my-consult-dir--recentf-dirs activate)
        (setq ad-return-value dired-recent-directories)
        )
      )
    :config
    ;; 禁止某些preview
    (consult-customize
     consult-recent-file
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     ;; :preview-key '(:debounce 0.2 any) ;; 只是延迟
     :preview-key (kbd "M-.")
     )
    ;; 含中文字符搜索时添加--pre rgpre
    (defadvice consult--ripgrep-builder (around my-consult--ripgrep-builder activate)
      (if (chinese-word-chinese-string-p (ad-get-arg 0))
          (let ((consult-ripgrep-args (concat consult-ripgrep-args " --pre rgpre")))
            ad-do-it
            )
        ad-do-it))
    (define-key occur-mode-map (kbd "C-c C-p") 'occur-edit-mode)
    )
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
  (setq magit-version "3.3.0"
        magit-commit-show-diff nil ;; commit时不用显示diff，在stage时一般就检查了
        magit-status-sections-hook
        '(
          ;; magit-insert-status-headers
          ;; magit-insert-merge-log
          ;; magit-insert-rebase-sequence
          ;; magit-insert-am-sequence
          ;; magit-insert-sequencer-sequence
          ;; magit-insert-bisect-output
          ;; magit-insert-bisect-rest
          ;; magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          ;; magit-insert-unpushed-to-pushremote
          ;; magit-insert-unpushed-to-upstream-or-recent
          ;; magit-insert-unpulled-from-pushremote
          ;; magit-insert-unpulled-from-upstream
          )
        )
  (with-eval-after-load 'magit-git
    ;; https://github.com/magit/magit/issues/2982#issuecomment-1081204026
    (defun magit-rev-format (format &optional rev args)
      (let ((str (magit-git-string "log" "-1" "--no-patch"
                                   (concat "--format=" format) args
                                   (if rev (concat rev "^{commit}") "HEAD") "--")))
        (unless (string-equal str "")
          str)))
    )
  :commands (magit magit-status)        ;; magit-status
  :config
  (define-key magit-status-mode-map "L" 'magit-section-up) ;; diff差异太多，按L返回所属文件
  (define-key magit-status-mode-map (kbd "<C-tab>") nil) ;; 使切换buffer
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
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

(use-package dumb-jump
  :commands(dumb-jump-xref-activate)
  :init
  ;; dumb-jump，使用rg查找定义！需要定义project root，添加任意这些文件都可以：.dumbjump .projectile .git .hg .fslckout .bzr _darcs .svn Makefile PkgInfo -pkg.el.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; 不用xref，用helm，但这个C-C C-F切换follow mode有问题，暂时不管了
  :config
  ;; (defadvice dumb-jump-get-project-root (before my-dumb-jump-get-project-root activate)
  ;;   ;; arount设置有问题
  ;;   (setq dumb-jump-default-project default-directory) ; 默认设置为当前目录
  ;;   )
  )

(use-package which-key
  :init
  (setq
   which-key-popup-type 'side-window ;；用minibuffer当easy-mark时会移动屏幕，需要easy-kill-init-candidate里屏蔽narrow-to-region，并且非set-transient-map一闪而过
   which-key-use-C-h-commands nil ;; 避免C-h时调用成which-key的
   )
  :commands(which-key--show-keymap which-key--hide-popup)
  :config
  )

;; easy-kill，添加类似vim里yi/a的东西！
(use-package easy-kill
  :load-path "~/.emacs.d/packages/easy-kill"
  :commands(easy-kill easy-mark)
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  ;; 有选中或者mark时用expand-region，否则用easy-mark
  ;; 替换expand-region
  (global-set-key "\C-t" (lambda ()(interactive)
			   (if (region-active-p)
			       (call-interactively 'er/expand-region)
			     (call-interactively 'easy-mark)
			     )))
  :config
  (add-to-list 'load-path "~/.emacs.d/packages/easy-kill/easy-kill-extras.el-master")
  (require 'easy-kill-er)
  (require 'extra-things)
  (require 'easy-kill-extras)
  ;; (setq easy-kill-try-things '(my-line)) ; 只复制line
  (setq easy-kill-try-things '(line-with-yank-handler)) ; 只复制line
  (defun easy-kill-on-line-with-yank-handler(n)
    "必须以easy-kill-on-开头，easy-kill-adjust-candidate可以设置位置并正确添加overlay，或者直接传string"
    (let ((beg (easy-kill-get start)) (end (easy-kill-get end)))
      (save-excursion 
        (pcase n 
          (`+ (goto-char end) ;; easy-kill-expand
              (setq end (line-beginning-position 2)))
          (`- (goto-char beg) ;; easy-kill-shrink
              (previous-line)
              (setq beg (line-beginning-position)))
          (1 (setq beg (line-beginning-position)
                   end (line-beginning-position 2)))))
      (easy-kill-adjust-candidate 'line-with-yank-handler beg end))
    ;; (easy-kill-adjust-candidate 'line-with-yank-handler (line-beginning-position) (line-beginning-position 2))
    )
  (defadvice easy-kill-candidate (after my-easy-kill-candidate activate)
    "获取所选文字最关键的函数，这里判断是beg end位置方式，再判断是否是自定义thing，就给返回字符追yank-handler"
    (with-current-buffer (easy-kill-get buffer)
      (pcase (easy-kill-get bounds)
        (`(,_x . ,_x) ();; 这就是字符串形式
         )
        (`(,beg . ,end) ;; begin end位置模式 
         (when (eq (easy-kill-get thing) 'line-with-yank-handler)
           (let ((string ad-return-value))
             (when (> (length string) 0)
	       (put-text-property 0 (length string)
			          'yank-handler '(yank-line) string))
             (setq ad-return-value string)))))))
  
  (defun easy-kill-on-forward-word(n)
    (let ((beg (easy-kill-get start)) (end (easy-kill-get end)))
      (save-excursion 
        (goto-char end)
        (forward-word)
        (setq end (point)))
      (easy-kill-adjust-candidate 'line-with-yank-handler beg end)))
  (defun easy-kill-on-backward-word(n)
    (let ((beg (easy-kill-get start)) (end (easy-kill-get end)))
      (save-excursion 
        (goto-char beg)
        (backward-word)
        (setq beg (point)))
      (easy-kill-adjust-candidate 'line-with-yank-handler beg end)))
  
  ;; 当光标在屏幕下一半，minibuffer显示有换行的拷贝内容，会导致C-l效果，需要去掉换行
  ;; 测试带汉字也会。。所以屏蔽echo
  (defun easy-kill-echo-around (orig-fun format-string &rest args)
    )
  (advice-add 'easy-kill-echo :around #'easy-kill-echo-around)
  (add-to-list 'easy-kill-alist '(?= line-with-yank-handler ""))

  (setq easy-mark-try-things '(symbol sexp)) ; 要加sexp，不然在)处expand会有bug
  ;; (define-key easy-kill-base-map (kbd "C-r") 'easy-kill-er-expand) ; 不要再定义了，避免mark时不能复制
  (define-key easy-kill-base-map (kbd "C-t") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "C-S-t") 'easy-kill-er-unexpand)
  (define-key easy-kill-base-map (kbd "n") 'easy-kill-expand) ;; 有bug，只有mark时有用
  (define-key easy-kill-base-map (kbd "p") 'easy-kill-shrink)
  (define-key easy-kill-base-map (kbd "g") 'easy-kill-abort)
  (define-key easy-kill-base-map (kbd "q") 'easy-kill-abort)
  (define-key easy-kill-base-map (kbd "x") 'exchange-point-and-mark)
  (autoload 'er--expand-region-1 "expand-region" nil t)
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (assq-delete-all ?b easy-kill-alist)  ;; 删除内置的，否则which-key提示不正确
  ;;(add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?b backward-word ""))
  ;; (add-to-list 'easy-kill-alist '(?a buffer-file-name ""))
  (add-to-list 'easy-kill-alist '(?a backward-line-edge ""))
  (assq-delete-all ?e easy-kill-alist)  ;; 删除内置的，否则which-key提示不正确
  (add-to-list 'easy-kill-alist '(?e forward-line-edge ""))
  (assq-delete-all ?f easy-kill-alist)  ;; 删除内置的，否则which-key提示不正确
  (add-to-list 'easy-kill-alist '(?f forward-word ""))
  ;;(add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?W  WORD " ") t)
  (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
  (assq-delete-all ?s easy-kill-alist)  ;; 删除内置的，否则which-key提示不正确
  (add-to-list 'easy-kill-alist '(?s  quoted-string "") t) ;; 选择string，支持上面\'\"\`三种
  ;;(add-to-list 'easy-kill-alist '(?s symbol "")) ;; 初始就是symbol了
  (add-to-list 'easy-kill-alist '(?Q  quoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\( parentheses-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\) parentheses-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\[ brackets-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\] brackets-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?{  curlies-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?}  curlies-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?<  angles-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?>  angles-pair "\n") t)
  
  (when (functionp 'which-key--show-keymap)
    ;; 简化which-key提示
    (with-eval-after-load 'which-key
      ;; 去掉easy-kill-前辍
      ;; (push '((nil . "easy-kill-digit-argument") . (nil . "")) which-key-replacement-alist)
      (push '((nil . "easy-kill-") . (nil . "")) which-key-replacement-alist)
      
      ;; 额外居然都显示easy-kill-thing，这里替换它们的显示
      ;; easy-kill-help 可以显示所有功能
      ;; (push '(("s" . "easy-kill-thing") . (nil . "symbol")) which-key-replacement-alist)
      (cl-dolist (one easy-kill-alist)
        (push `((,(regexp-quote (char-to-string (car one))) . "easy-kill-thing") . (nil . ,(symbol-name (nth 1 one)))) which-key-replacement-alist)
        )
      )
    (defvar my-easy-kill-map nil)
    (unless my-easy-kill-map
      (let ((easy-kill-base-map easy-kill-base-map))
        ;; remove number keys
        (cl-loop for i from 0 to 9
                 do (define-key easy-kill-base-map (number-to-string i) nil))
        (setq my-easy-kill-map (easy-kill-map))
        ))
    (if t ;; 有两种显示方式1.按?显示 2.一直显示。2会影响性能不用
        (define-key easy-kill-base-map "?"
          (lambda () (interactive)
            (which-key--show-keymap "keymap" my-easy-kill-map nil nil 'no-paging)
            (set-transient-map my-easy-kill-map nil 'which-key--hide-popup)
            ))
      (progn
        (defadvice easy-kill-activate-keymap (before my-easy-kill-activate-keymap activate)
          (unless my-easy-kill-map
            (let ((easy-kill-base-map easy-kill-base-map))
              ;; remove number keys
              (cl-loop for i from 0 to 9
                       do (define-key easy-kill-base-map (number-to-string i) nil))
              (setq my-easy-kill-map (easy-kill-map))
              ))
          ;; 有popper窗口时不弹出
          (if (and (functionp 'popper-toggle-latest) (boundp 'popper-open-popup-alist))
              (unless popper-open-popup-alist 
                (which-key--show-keymap "keymap" my-easy-kill-map nil nil 'no-paging))
            (which-key--show-keymap "keymap" my-easy-kill-map nil nil 'no-paging)))
        (defadvice set-transient-map (before my-set-transient-map activate)
          (let ((map (ad-get-arg 0)))
            ;; 判断是否是easy-kill的keymap
            (when (eq (lookup-key map "?") 'easy-kill-help)
              (ad-set-arg 2 'which-key--hide-popup) ;; easy-kill按n时overlay还是消除不掉，暂时不管了
              )))
        )
      )
    )
  )

;; project内置查找会用到，支持ripgrep了！
(use-package xref
  :defer t
  :init
  (setq xref-search-program 'ripgrep
        xref-auto-jump-to-first-definition 'show ;; 自动跳转到第一个
        xref-auto-jump-to-first-xref 'show)
  (global-set-key (kbd "C-.") 'xref-find-definitions)
  (global-set-key (kbd "<C-down-mouse-1>") 'xref-find-definitions)
  :config
  (define-key xref--xref-buffer-mode-map (kbd "C-n") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "C-p") 'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "M-n") 'xref-next-group)
  (define-key xref--xref-buffer-mode-map (kbd "M-p") 'xref-prev-group)
  (define-key xref--xref-buffer-mode-map [remap keyboard-quit] 'xref-quit-and-pop-marker-stack)
  (define-key xref--xref-buffer-mode-map "q" 'xref-quit-and-pop-marker-stack)
  (define-key xref--xref-buffer-mode-map "w" 'scroll-down-command)
  
  (defadvice xref-matches-in-files (around my-xref-matches-in-files activate)
    (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
      ;; 注意project search是xargs实现的，会向rg先发送文件列表，因此对输入编码有要求
      (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . utf-8))
      ;; 检查是否含中文
      (if (chinese-word-chinese-string-p (ad-get-arg 0))
          (let ((xref-search-program-alist (list (cons 'ripgrep (concat (cdr (assoc 'ripgrep xref-search-program-alist)) " --pre rgpre")))))
            ad-do-it
            )
        ad-do-it)
      (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)))
  
  ;; 修复xref只调用一个backend的问题
  (defun my/xref-find-backends()
    (let (backends)
      ;; 好像没有办法获取全局变量，但run hook是可以遍历local和全局的
      (run-hook-wrapped 'xref-backend-functions
                        (lambda (f)
                          (when (functionp f)
                            (setq backend (funcall f))
                            (when backend
                              (cl-pushnew backend backends)))
                          nil
                          ))
      (reverse backends))
    )
  (defun zjy/xref--create-fetcher (input kind arg)
    "参考原版`xref--create-fetcher'改写的"
    (let* ((orig-buffer (current-buffer))
           (orig-position (point))
           (backends (my/xref-find-backends))
           (method (intern (format "xref-backend-%s" kind))))
      (lambda ()
        (save-excursion
          (when (buffer-live-p orig-buffer)
            (set-buffer orig-buffer)
            (ignore-errors (goto-char orig-position)))
          (let (xrefs)
            (cl-dolist (backend backends)
              (ignore-errors
                (setq xrefs (funcall method backend arg))
                (if xrefs
                    (cl-return)
                  (message (format "Xref backend: %s failed to find definition." (symbol-name backend))))))
            (unless xrefs
              (xref--not-found-error kind input))
            xrefs)))))
  (advice-add #'xref--create-fetcher :override #'zjy/xref--create-fetcher)
  )


;; TODO: ctags生成好像还含有外部引用？另外--exclude需要自己加上
;; 测试问题：xref空白处会卡死，补全时也会卡死emacs(尤其是el文件写注释的时候，会创建process并提示失败)
;; 所以目前仅用它来创建TAGS文件
(use-package citre-ctags
  ;; :disabled ;; 输入汉子导致emacs卡死(它自己设置了capf)？Message查看提示process什么失败，还以为是corfu导致的呢。。
  ;; :diminish(citre-mode)
  :load-path "~/.emacs.d/packages/citre/citre-master"
  :commands(citre-create-tags-file citre-update-this-tags-file)
  :init
  
  (setq citre-default-create-tags-file-location 'project-cache
        citre-use-project-root-when-creating-tags t
        citre-tags-file-per-project-cache-dir "" ;; 强制tag放到根目录，需配合后面设置
        ;; citre-prompt-language-for-ctags-command t 设置这个就没有确认对话框了 
        citre-edit-cmd-buf-default-cmd "ctags
-o
%TAGSFILE%
;; programming languages to be scanned
--languages=C,C++
--kinds-all=*
--fields=*
--extras=*
-Re
;; add dirs/files to scan here, one line per dir/file
;; add exclude by: --exclude=target
"
        )
  (use-package citre-util
    :commands(citre-tags-file-path)
    )
  (use-package etags
    :defer t
    :commands(find-tag--default ;; 修复eglot的xref at-point
              tags-lazy-completion-table ;; 空白处M-.
              tags-completion-at-point-function ;; etags的capf
              )
    :init
    ;; xref有bug，`xref-backend-functions'只支持一个backend(无论local hook或者全局)
    ;; https://github.com/seagle0128/.emacs.d/blob/3eabad00e75605ad1277fb37ebc1bf0619e44180/lisp/init-ctags.el#L62
    ;; (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
    ;;   (let ((fetcher (apply fn args))
    ;;         (etag-fetcher
    ;;          (let ((xref-backend-functions '(etags--xref-backend t)))
    ;;            (ignore xref-backend-functions)
    ;;            (apply fn args))))
    ;;     ;; 这个需要开启-*- lexical-binding: t -*-，写在开头就可以了
    ;;     (lambda ()
    ;;       (or (with-demoted-errors "%s, fallback to etag"
    ;;             (funcall fetcher))
    ;;           (funcall etag-fetcher)
    ;;           ))))
    ;; 最坑的是eglot定义了个xref-backend-identifier-at-point，却只是用来在找不到时提示找不到"LSP identifier at point."
    ;; 这样传给etags去查找的就是"LSP identifier at point."
    (with-eval-after-load 'eglot
      ;; 重新定义eglot的xref-backend-identifier-at-point，避免上面xref--create-fetcher传给etags搜索词错误
      (cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
        (find-tag--default))
      ;; 在空白处运行M-. eglot提示没实现，那就直接换成etags的了。此功能用consult-eglot也可以(C-,)
      (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
        (tags-lazy-completion-table))
      )
    ;; 避免每次都提示查找TAG文件
    (defconst ask-when-to-tag nil) ;; xref-find-definitions advise没成功。
    (defun check_tags()
      ;; 参考`citre-update-this-tags-file'
      (if-let* ((tagsfile (citre-tags-file-path)))
          (visit-tags-table tagsfile t)
        (when (and ask-when-to-tag (y-or-n-p "Can't find tags file for this buffer.  Create one? "))
          (citre-create-tags-file)
          (setq tagsfile (citre-tags-file-path))
          (when tagsfile
            (visit-tags-table tagsfile t))
          ))
      )
    (add-hook 'prog-mode-hook 'check_tags)
    (setq tags-add-tables nil  ;; 打开其它工程时保留tag提示，不保留，否则会混起
          tags-revert-without-query t;; 当TAGS更新后不提示是否revert TAGS buffer
          )
    (defun tag-find-definition(identifier)
      "有时如duilib的头文件里eglot就不行，用tag还行"
      (interactive (list (xref--read-identifier "Find definitions of: ")))
      (let ((xref-backend-functions '(etags--xref-backend)))
        (xref--find-definitions identifier nil)))
    :config
    ;; 跟citre生成的TAGS兼容(头两行带有更新TAGS的命令)
    (defadvice etags-verify-tags-table (around my-etags-verify-tags-table activate)
      ;; 原来的判断是开头0xC字符
      (setq ad-return-value t)
      ))
  :config
  ;; 强制tag名
  (defadvice citre--path-to-cache-tags-file-name (around my-citre--path-to-cache-tags-file-name activate)
    (setq ad-return-value "TAGS")
    )
  (define-key citre-edit-cmd-buf-map (kbd "C-c C-l") 'citre-edit-cmd-buf-add-lang)
  )

(defun my-project-search()
  (interactive)
  (cond ((functionp 'helm-do-grep-ag)
         ;; 比project-find-regex好的是可以随时更改搜索词
         (let ((default-directory (project-root (project-current t)))
               ;; 项目搜索要启动.gitignore了。不过ignore目录的forced added文件仍然会被忽略
               (helm-grep-ag-command "rg --color=always --colors match:style:nobold --smart-case --no-heading --line-number %s %s %s"))
           (call-interactively 'helm-do-grep-ag)
           ))
        
        ((functionp 'counsel-rg) (call-interactively 'counsel-rg))
        ((functionp 'consult-ripgrep) (call-interactively 'consult-ripgrep))
        ))
(defun my-project-find-file()
  (interactive)
  (if (functionp 'consult-project-extra-find)
      ;; 加速弹出project选择框，不然首次总感觉有点慢
      (let ((default-directory (project-root (project-current t))))
        (call-interactively 'consult-project-extra-find))
    (call-interactively 'project-find-file))
  )
(defun my-project-buffer()
  (interactive)
  (if (functionp 'consult-project-buffer)
      (call-interactively 'consult-project-buffer)
    (call-interactively 'project-switch-to-buffer)))

;; 没有cache查找文件也超快！
(use-package project
  :defer t
  :commands(project-compile project-find-regexp project-root)
  :init
  (defun invoke_project ()
    (interactive)
    (when (functionp' which-key--show-keymap)
      (which-key--show-keymap "keymap" project-prefix-map nil nil 'no-paging)
      )
    (set-transient-map project-prefix-map nil 'which-key--hide-popup)
    )
  (global-set-key (kbd "C-;") 'invoke_project)
  
  ;; eglot+ctags补全是不错的方案(clangd flymake很多错误时补全失灵)
  (defun my/generate-tags()
    (interactive)
    ;; projectile那个tags太简单了，不能指定语言很慢
    (when (functionp 'citre-update-this-tags-file)
      (call-interactively 'citre-update-this-tags-file))
    )
  (defun my-project-magit()
    (interactive)
    (let ((pr (project-current t)))
      (magit-status (project-root pr))))
  ;; p切换project时显示的命令
  (setq project-switch-commands
        `((?f "File" my-project-find-file)
          (?s "Search" my-project-search)
          (?d "Dired" project-dired)
          (?m "Magit" magit-status)
          (?b "Buffer" my-project-buffer) ; 这个当recent buffer使用了
          (?v "VC-Dir" project-vc-dir)
          (?e "Eshell" project-eshell)
          )
        project-switch-use-entire-map t ; switch prj时也可以按其它键
        )
  ;; 这个还可以避免projectile search的bug(比如搜索 projectile-indexing-method，projectile.el就被忽略了)
  (define-key project-prefix-map "f" 'my-project-find-file)
  (define-key project-prefix-map "s" 'my-project-search)
  (define-key project-prefix-map "S" 'project-shell)
  (define-key project-prefix-map "m" 'my-project-magit) ; v保留，那个更快更精简
  (define-key project-prefix-map "t" 'my/generate-tags)
  (define-key project-prefix-map "b" 'my-project-buffer)
  
  :config
  ;; 子目录有.project就以子目录为prj root
  (defun my-project-find-functions (dir)
    (let ((override (locate-dominating-file dir ".project"
                                            )))
      (if override
          (if (version< emacs-version "29")
              (cons 'vc override)
            (list 'vc 'nil override)
            )
        nil)))
  (add-to-list 'project-find-functions 'my-project-find-functions)
  (when (functionp 'which-key--show-keymap)
    (defadvice project--switch-project-command (around my-project--switch-project-command activate)
      (which-key--show-keymap "keymap" project-prefix-map nil nil 'no-paging)
      ad-do-it
      (which-key--hide-popup)
      ))
  
  ;; 好像自动识别find的输出了(git里的find)
  ;; (defadvice project--files-in-directory (around my-project--files-in-directory activate)
  ;;   (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
  ;;     (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
  ;;     ad-do-it
  ;;     (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)))
  )
  

(use-package compile
  :defer t
  :config
  (when compile-history
    ;; 自动使用最近的历史记录
    (setq compile-command (car compile-history))))

(global-set-key [f7] (lambda ()(interactive)
		       (progn
			 (call-interactively 'project-compile)
			 (setq compilation-read-command nil) ;; 不再提示
			 )))
(global-set-key [(shift f7)]
		(lambda ()(interactive)
		  (progn
		    (setq compilation-read-command t)
                    (call-interactively 'project-compile)
		    (setq compilation-read-command nil) ;; 不再提示
		    )))

;; rg，这个还挺好用的，带修改搜索的功能(需要buffer可写)，更多功能看菜单
(use-package rg
  :load-path "~/.emacs.d/packages/projectile"
  :commands(rg-define-search)
  :init
  (add-to-list 'load-path "~/.emacs.d/packages/projectile/rg.el-master")
  (defun my/rg-dwim()
    (interactive)
    ;; type为all，不然h就会当成c从而忽略了cpp文件。要指定类型可以在rg buffer按f修改
    (unless (functionp 'rg-dwim-project-dir-type-all)
      (rg-define-search rg-dwim-project-dir-type-all
                        :query point
                        :format literal
                        :files "everything"
                        :dir project
                        )
      )
    (call-interactively 'rg-dwim-project-dir-type-all)
    )
  
  (setq rg-ignore-case 'force) ;; 不知道为什么regex搜索的时候会区分大小，只能强制了
  (global-set-key (kbd "C-S-f") 'my/rg-dwim)
  :config
  (define-key rg-mode-map "w" 'scroll-down-command)
  (define-key rg-mode-map (kbd "C-o") 'avy-goto-word-1)
  (use-package rg-result
    :defer t
    :config
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
  )

;; consult-grep -> embark-export to grep-mode
;; grep mode里C-c C-p开启编辑，C-c C-c完成，C-c C-k放弃编辑
(use-package wgrep
  :after(grep)
  :init
  (setq wgrep-auto-save-buffer t) ;; 编辑好自动保存
  :config
  ;; 使wrep可编辑
  (defadvice wgrep-commit-file (around my-wgrep-commit-file activate)
    (setq tmp-disable-view-mode t)
    ad-do-it
    (setq tmp-disable-view-mode nil)
    )
  )

;; lsp，c++装个llvm(包含clangd)，python装pyright，rust装rust-analyzer

(add-to-list 'load-path "~/.emacs.d/packages/lsp")
(use-package lsp-bridge
  :diminish
  :load-path "~/.emacs.d/packages/lsp/lsp-bridge-master"
  :commands(lsp-bridge-mode)
  :init
  
  (setq acm-enable-english-helper nil  ;; english字典太大已经删除了
        )
  (defun lsp-ensure()
    (lsp-bridge-mode 1)
    )
  (use-package acm-backend-citre
    :defer t
    :init
    (setq acm-enable-citre t)
    :config
    (defun acm-backend-citre-candidates (keyword)
      "原函数去重，二次fuzzy，排序什么的都不需要"
      (when (and acm-enable-citre (>= (length keyword) 3)) ;; 必须设置长度，1会卡死，2有时会卡
        (let* ((candidates (list))
               (collection (all-completions keyword (tags-lazy-completion-table))))
          (when collection
            (dolist (candidate collection)
              (add-to-list 'candidates (list :key candidate
                                             :icon "tabnine"
                                             :label candidate
                                             :display-label candidate
                                             :annotation "tabnine" 
                                             :backend "citre")
                           t))
            candidates))))
    )
  :config
  (define-key lsp-bridge-mode-map [remap xref-find-definitions] 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-<return>") 'lsp-bridge-popup-complete) ; 手动调用补全
  (define-key lsp-bridge-mode-map [(meta f8)] 'lsp-bridge-code-format)

  ;; 在minbuffer时不显示
  (setq lsp-bridge-signature-function (lambda(msg)
                                        (unless (minibufferp)
                                          (message msg))
                                        ))
  ;; 显示project名
  (defadvice lsp-bridge--mode-line-format (after my-lsp-bridge--mode-line-format activate)
    (setq ad-return-value 
          (propertize (concat "lsp-bridge:" (file-name-base (directory-file-name  
                                                      (let ((pr  (project-current nil)))
                                                        (if pr
                                                            (project-root pr)
                                                          default-directory))))) 'face mode-face)  
          )
    )
  ;; 找不到定义就fallback to xref
  (defadvice lsp-bridge--eval-in-emacs-func (after my-lsp-bridge--eval-in-emacs-func activate)
    ;;(message (ad-get-arg 0))
    (when (string-match "No definition found" (ad-get-arg 0))
      (xref-find-definitions (find-tag--default))
      ))
  ;; 添加csharp，目前json里的路径是写死的，自己改改
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(csharp-mode . "csharp"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'csharp-mode-hook)
  (use-package acm
    :init
    ;; (setq acm-candidate-match-function 'orderless-flex) ;; flex有奇怪问题，如createprocessw，输入到w就卡死了
    :config
    (define-key acm-mode-map "\M-h" nil) ;; M-h用自己的绑定
    )
  )
(use-package flymake
  :defer t
  :config
  ;; fixed for eglot with emacs 28.0.50，就kill-buffer时有提示，实际好像没什么问题
  (unless (boundp 'flymake-list-only-diagnostics)
    (defvar flymake-list-only-diagnostics nil)
    ))
(use-package eglot
  :load-path "~/.emacs.d/packages/lsp"
  :init
  (setq eglot-confirm-server-initiated-edits nil ; 避免code action的yes/no提示
        eglot-send-changes-idle-time 0.01 ; 可以加rust的code action更新
        eglot-sync-connect nil ;; 打开新文件就不卡了，貌似没有副作用？
        )
  (defun my-eglot-ensure()
    (corfu-mode)
    (eglot-ensure)
    )
  :commands (eglot eglot-ensure eglot-rename)
  :config
  (add-to-list 'eglot-server-programs '(simpc-mode . ("clangd")))
  (advice-add 'jsonrpc--log-event :around
              (lambda (_orig-func &rest _))) ;; 禁止log buffer据说可以加快速度
  ;; flymake还是要开的，错误不处理的话，补全就不能用了。用跟cmake一样的vs版本可以解决很多错误
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  (setq eglot-autoshutdown t)            ;; 不关退出emacs会卡死
  (push :documentHighlightProvider       ;; 关闭光标下sybmol加粗高亮
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

;; 不能任意hook，不然右键无法打开文件，因为eglot找不到对应的server会报错

(defun enable-format-on-save()
  (add-hook 'before-save-hook (lambda()
				(when (featurep 'eglot)
				  (eglot-format))
                                (when (featurep 'lsp-mode)
                                  (lsp-format-buffer))
				) nil 'local))
;; pyright好像还需要node，低版本的还报错，装个支持的win7的最后版本就可以了(node-v13.14.0-x64.msi)
(add-hook 'python-mode-hook (lambda ()
                              (when (featurep 'lsp-pyright)
                                (require 'lsp-pyright)
                                )
                              (lsp-ensure)))
(use-package simpc-mode
  ;; cc-mode因为历史原因，很多功能都不是必须的，如mode-map和after/before change hook，但去掉hook的话有时候font lock会不正常，所以干脆换个major-mode
  ;; 还有个类似的https://github.com/veera-sivarajan/minc-mode/blob/master/highlight.el
  :commands(simpc-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.c\\'" . simpc-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . simpc-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . simpc-mode))
  (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . simpc-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . simpc-mode))
  :config
  ;; 实现defun范围获取，速度稍微比cc-mode慢了点
  (defun simpc-mode-beginning-of-defun(&optional arg)
    (interactive)
    (unless tree-sitter-mode
      (tree-sitter-mode +1))
    (tree-sitter-force-update)
    (if (re-search-backward "{" nil t) ;;粗暴解决end-of-defun的问题，但M-x beginning-of-defun有点问题
        (goto-char (match-beginning 0)))
    (let* ((inhibit-message t)
          (range (evil-textobj-tree-sitter-function--function\.outer arg))
          )
      (when (consp range)
        (goto-char (car range))
        t)))
  (defun simpc-mode-end-of-defun()
    "有个问题end-of-defun老是定位到你给的end位置的下一行，这时再beginning-of-defun就失败了"
    (interactive)
    (unless tree-sitter-mode
      (tree-sitter-mode +1))
    (tree-sitter-force-update)
    (let* ((inhibit-message t)
          (range (evil-textobj-tree-sitter-function--function\.outer 1))
          )
      (when (consp range)
        (goto-char (cdr range))
        t)))
  (add-hook 'simpc-mode-hook (lambda()
                               ;; 能成功实现defun的thing at point的要素是两次手动M-x beginning-of-defun和end-of-defun依然在函数边界上
                               ;; 不然就不会成功
                               (setq-local beginning-of-defun-function #'simpc-mode-beginning-of-defun)
                               (setq-local end-of-defun-function #'simpc-mode-end-of-defun)
                               ))
  )
(add-hook 'simpc-mode-hook 'my-eglot-ensure)
;; cc-mode包含java等
(add-hook 'c-mode-common-hook 
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      ;; 保存时自动format，因为下面的google style跟clang-format的google有点不一致
              ;; (enable-format-on-save)
              (my-c-mode-hook-set)
              (my-eglot-ensure) ;; lsp bridge配合clangd很容易遇到问题还没法解决。。
	      )
            (abbrev-mode -1) ;; 有yas就够了
            ;; (remove-hook 'before-change-functions 'c-before-change t) ;; cc-mode各种历史毒瘤
            ;; (remove-hook 'after-change-functions 'c-after-change t)            
            ))
(add-hook 'rust-mode-hook 'lsp-ensure)

;; pip install cmake-language-server，还需要将cmake加入PATH环境变量
(add-hook 'cmake-mode-hook 'lsp-ensure)

;; https://github.com/sumneko/lua-language-server 去下载bin
(defvar lua-server-path (cond ((file-exists-p "~/lua-language-server-3.2.4-win32-x64") "~/lua-language-server-3.2.4-win32-x64")
                              (t nil)))
(when lua-server-path
  (with-eval-after-load 'lua-mode
    (add-path-to-execute-path (expand-file-name "bin" lua-server-path)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server"))))
  (add-hook 'lua-mode-hook 'lsp-ensure) 
  )

;; https://github.com/OmniSharp/omnisharp-roslyn 本身就是net core编译的，不需要net6.0体积还小点
(defvar c-sharp-server-path (cond ((file-exists-p "H:/green/omnisharp-win-x64") "H:/green/omnisharp-win-x64")
                                  (t nil)))
(when c-sharp-server-path
  (with-eval-after-load 'csharp-mode
    (add-path-to-execute-path (expand-file-name c-sharp-server-path)))
  (defun my-csharp-hook()
    (my-eglot-ensure)
    (define-key csharp-mode-map "\C-d" nil) ;; 使用我们自己的hungry delete(cc自带hungry不好用)
    )
  (add-hook 'csharp-mode-hook 'my-csharp-hook) ;; lsp-bridge这个有bug，还是用eglot的
  )

(use-package quickrun
  :commands(quickrun quickrun-shell helm-quickrun)
  :init
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "<f5>") 'quickrun))
  (setq quickrun-option-shebang nil     ;; windows没有这东西
        quickrun-timeout-seconds 20 ;; 超过10秒不结束进程
        )
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
(use-package tree-sitter
  :commands(tree-sitter-mode tree-sitter-force-update tree-sitter-setup-timer)
  :defer t
  :config
  ;; elisp没有高亮
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(simpc-mode . cpp))
  (use-package tree-sitter-langs)
  
  (defvar tree-sitter-idle-timer nil
    "如果不需要hl功能，只需要按需调用tree-sitter-force-update即可，如defun范围功能")
  (defun my/tree-sitter--after-change(beg new-end old-len)
    (when tree-sitter-idle-timer
      (cancel-timer tree-sitter-idle-timer))
    (setq tree-sitter-idle-timer
          (run-with-idle-timer 1 nil #'tree-sitter-force-update) ))
  (defun tree-sitter-force-update()
    (setq tree-sitter-tree nil) ;; 必须设置为nil，否则不刷新
    (tree-sitter--do-parse))
  (defadvice tree-sitter--setup (after my-tree-sitter--setup activate)
    "去掉hook，改为timer模式"
    (remove-hook 'after-change-functions #'tree-sitter--after-change :local)
    (remove-hook 'before-change-functions #'tree-sitter--before-change :local)
    )
  (defun tree-sitter-setup-timer(&optional on)
    (if on
        (add-hook 'after-change-functions #'my/tree-sitter--after-change nil :local)
      (remove-hook 'after-change-functions #'my/tree-sitter--after-change :local)
      )))
;; tsc里的(require 'dired-aux) 导致dired被加载了
(use-package tree-sitter-hl
  :diminish(tree-sitter-mode)
  :commands(tree-sitter-hl-mode)
  ;; 来自`tree-sitter-major-mode-language-alist'
  :hook ((agda-mode
          sh-mode
          c-mode
          caml-mode
          csharp-mode
          c++-mode
          d-mode
          css-mode
          elm-mode
          elixir-mode
          go-mode
          haskell-mode
          hcl-mode
          terraform-mode
          html-mode
          mhtml-mode
          nix-mode
          java-mode
          javascript-mode
          js-mode
          js2-mode
          js3-mode
          json-mode
          jsonc-mode
          julia-mode
          ocaml-mode
          perl-mode
          php-mode
          prisma-mode
          python-mode
          pygn-mode
          rjsx-mode
          ruby-mode
          rust-mode
          rustic-mode
          scala-mode
          swift-mode
          tuareg-mode
          typescript-mode
          verilog-mode
          yaml-mode
          zig-mode
          simpc-mode
          emacs-lisp-mode
          ) . (lambda ()
	  ;; (tree-sitter-hl-mode)
	  (grammatical-edit-mode 1)
	  ))
  :config
  (add-hook 'tree-sitter-hl-mode-hook (lambda()
                                        (tree-sitter-setup-timer tree-sitter-hl-mode)
                                        ))
  )

(use-package evil-textobj-tree-sitter
  :load-path "~/.emacs.d/packages/tree-sitter/evil-textobj-tree-sitter-master"
  :defer t
  :commands(evil-textobj-tree-sitter-function--function\.outer)
  :init
  (defalias 'evil-define-text-object 'defun)
  (defalias 'evil-range 'cons)
  :config
  (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist '(simpc-mode . "cpp"))
  ;; 可惜没有elisp
  (evil-textobj-tree-sitter-get-textobj "function.outer")
  ;; (evil-textobj-tree-sitter-function--function\.outer 1)
  )

;; grammatical-edit bug太多了，pair用这个就够了
(use-package elec-pair
  :config
  (electric-pair-mode 1)
  ;; c++换行时输入{}自动indent
  (defadvice electric-pair--insert (after my-electric-pair--insert activate)
    (indent-according-to-mode))
  )

(use-package grammatical-edit
  :commands(grammatical-edit-mode)
  :config
  ;; 会影响kill-ring，作者还不改，暂时不用了
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
;; (ignore-errors (module-load (expand-file-name "~/.emacs.d/bin/pop_select.dll"))) ; 需要全路径加载(如果没把~/.emacs.d/bin加入环境变量的话)
(ignore-errors (module-load "F:/prj/rust/pop-select/target/release/pop_select.dll"))
(ignore-errors (module-load "H:/prj/rust/pop-select/target/release/pop_select.dll"))

(when (functionp 'pop-select/transparent-set-background)
  (when (version< emacs-version "29")
    (pop-select/ensure-all-window-dark-mode))
  (defvar cur-transparent 255)
  (defconst step-transparent 20)
  (defun dec-transparent()
    (interactive)
    (setq cur-transparent (min 255 (+ cur-transparent step-transparent)))
    (let* ((rgb (color-name-to-rgb (face-background 'default)))
           (r (round (*(nth 0 rgb) 255)))
           (g (round (*(nth 1 rgb) 255)))
           (b (round (*(nth 2 rgb) 255))))
      (pop-select/transparent-set-background cur-transparent r g b)
      )
    )
  (defun inc-transparent()
    (interactive)
    (setq cur-transparent (max 0 (- cur-transparent step-transparent)))
    (let* ((rgb (color-name-to-rgb (face-background 'default)))
           (r (round (*(nth 0 rgb) 255)))
           (g (round (*(nth 1 rgb) 255)))
           (b (round (*(nth 2 rgb) 255))))
      (pop-select/transparent-set-background cur-transparent r g b)
      ))
  ;; (global-set-key (kbd "<C-wheel-up>") 'dec-transparent)
  ;; (global-set-key (kbd "<C-wheel-down>") 'inc-transparent)
  )
(w32-send-sys-command #xf030) ;; 放大窗口，pop-select/ensure-all-window-dark-mode还有bug就是caption不能重绘，这里放大就OK了

(when (fboundp 'pop-select/pop-select)
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
      (setq sel (pop-select/pop-select vec_name (if backward
                                                    (1- (length vec_name))
                                                  1
                                                  )))
      (let ((buf (switch-to-buffer (nth sel myswitch-buffer-list))))
        (when (and (bufferp buf) (featurep 'wcy-desktop))
	  (with-current-buffer buf
	    (when (eq major-mode 'not-loaded-yet)
              (let ((inhibit-message t))
                (wcy-desktop-load-file))
	      )))
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
    (scroll-on-jump-advice-add session-jump-to-last-change)
    )
  (scroll-on-jump-advice-add jl-jump-backward)
  (scroll-on-jump-advice-add jl-jump-forward)
  (scroll-on-jump-advice-add push-button)
  (scroll-on-jump-advice-add git-gutter:previous-hunk)
  (scroll-on-jump-advice-add git-gutter:next-hunk)
  (scroll-on-jump-advice-add embark-next-symbol)
  (scroll-on-jump-advice-add embark-previous-symbol)
  
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

(use-package diff-hl
  :load-path "~/.emacs.d/themes/diff-hl-master"
  :commands(diff-hl-maybe-define-bitmaps diff-hl-update)
  :init
  (add-hook 'prog-mode-hook (lambda()
                              (diff-hl-maybe-define-bitmaps)
                              (diff-hl-update)
                              (setq-local diff-hl-mode t) ;; for 'diff-hl-magit-post-refresh
                              (add-hook 'after-save-hook
                                        (lambda()
                                          (when diff-hl-update-timer
                                            (cancel-timer diff-hl-update-timer))
                                          (setq diff-hl-update-timer
                                                (run-with-idle-timer 2 nil #'diff-hl-update-timer-function) )
                                          ) nil t)
                              ))
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)    
    )
  ;; 用timer避免各种hook
  (defvar diff-hl-update-timer nil)
  (defun diff-hl-update-timer-function()
    (diff-hl-update)
    )
  (defun diff-hl-update-manual()(interactive) (diff-hl-update))
  
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
    :commands(diff-hl-dired-mode)
    :init
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    )

  ;; 点击fringe或margin时，弹出菜单可以转到下一个chunk，很方便！
  (use-package diff-hl-show-hunk
    :commands(diff-hl-show-hunk)
    :init
    (global-set-key (kbd "C-c h") #'diff-hl-show-hunk)
    (global-set-key (kbd "C-c C-h") #'diff-hl-show-hunk);; 目前还没有绑定这个的
    :config
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
        (define-key map (kbd "RET") #'my-diff-hl-inline-popup-hide)
        map)
      )
    (set-keymap-parent diff-hl-inline-popup-transient-mode-map1
                       diff-hl-show-hunk-map)
    (setq diff-hl-show-hunk-function (lambda (buffer &optional _ignored-line)
                                       (funcall 'diff-hl-show-hunk-inline-popup buffer _ignored-line)
                                       (setq set-transient-map-exit-func (set-transient-map diff-hl-inline-popup-transient-mode-map1 t 'diff-hl-inline-popup-hide))
                                       )))
  )


(use-package maple-preview
  :load-path "~/.emacs.d/packages/org"
  :defer t
  :init
  (defalias 'note-preview 'maple-preview-mode)
  (autoload 'maple-preview-mode "emacs-maple-preview-master/maple-preview" "" t nil)
  (setq maple-preview:allow-modes '(org-mode markdown-mode html-mode web-mode mhtml-mode))
  :config
  (defadvice maple-preview:open-browser (around my-maple-preview:open-browser activate)
    (if (functionp 'eaf-open-browser)
        (eaf-open-browser (format "http://%s:%s/preview" maple-preview:host maple-preview:port))
      ad-do-it)
    )
  (defadvice maple-preview:init (after my-maple-preview:init activate)
    ;; 这个hook还达到了自动滚屏的效果? (需要用鼠标点击不同位置)
    (add-hook 'buffer-list-update-hook 'maple-preview:send-to-server)
    )
  (defadvice maple-preview:finalize (after my-maple-preview:finalize activate)
    (remove-hook 'buffer-list-update-hook 'maple-preview:send-to-server)
    )
  )

(use-package winner
  :disabled
  :defer 1.2
  :config
  (winner-mode +1)
  (define-key winner-mode-map (kbd "<C-left>") #'winner-undo)
  (define-key winner-mode-map (kbd "<C-right>") #'winner-redo)  
  )

(use-package emacsql
  :defer t
  :load-path "~/.emacs.d/packages/org/emacsql-master"
  )
;; 优点: 可以以文件名,tag和子标题(需要org-id-get-create创建id)来搜索。
;; roam buffer: 可以显示backlink，同时会根据鼠标位置动态更新内容
(use-package org-roam
  :defer t
  :load-path "~/.emacs.d/packages/org/org-roam"
  :init
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-database-connector (if (version< emacs-version "29")
                                        'sqlite
                                      'sqlite-builtin)) ; 使用29版本以上自营的sqlite，但是仍然需要上面的emacsql
  (when nil
    ;; 暂时不用模式，按目录结构自己创建org文件
    (setq org-roam-capture-templates
          '(
            ("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n") :unnarrowed t)
            ("t" "tech" plain "%?"
             :target (file+head "tech/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n") :unnarrowed t)
            ("w" "work" plain "%?"
             :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n") :unnarrowed t)
            ("h" "home" plain "%?"
             :target (file+head "home/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n") :unnarrowed t)
            )
          ))
  
  (require 'org-roam-autoloads)
  (defun call-project-find()
    (interactive)
    (let ((default-directory org-roam-directory))
      (call-interactively 'my-project-find-file)
      ))
  (defun call-project-search()
    (interactive)
    (let ((default-directory org-roam-directory))
      (call-interactively 'my-project-search)
      ))
  
  ;; 一些org命令也放这里，太实用了
  (defhydra hydra-org-roam ()
    "
_f_: find        _t_: add tag
_i_: add id      _n_: add node
_g_: get link    _l_: add link
_s_: rg search   _p_: find file
_c_: capture
_G_: graph       _L_: buffer toggle
_q_uit
"
    ("f" org-roam-node-find nil :color blue)
    ("t" org-roam-tag-add nil :color blue)
    ("i" org-id-get-create nil :color blue)
    ("n" org-roam-node-insert nil :color blue)
    ("g" org-store-link nil :color blue)
    ("l" org-insert-link nil :color blue)
    ("p" call-project-find nil :color blue)
    ("s" call-project-search nil :color blue)
    ("c" org-roam-capture nil :color blue)
    ("G" org-roam-graph nil :color blue)
    ("L" org-roam-buffer-toggle nil :color blue)
    ("q" nil "nil" :color blue))
  (global-set-key (kbd "C-c n") 'hydra-org-roam/body)
  :config
  (unless (version< emacs-version "29")
    (use-package emacsql-sqlite-builtin))
  ;; find时列表加入tag，这么好的功能居然不加入默认？
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:100}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; 屏蔽新建node功能，因为不喜欢capture，还是手动按目录结构创建笔记
  (defadvice org-roam-capture- (around my-org-roam-capture- activate)
    (let ((default-directory org-roam-directory))
      (call-interactively 'find-file)
      )
    )
  )

(use-package eww
  :defer t
  :config
  (define-key eww-mode-map "w" 'scroll-down-command)
  (use-package shr
    :defer t
    :config
    ;; 修复选中图片时按w不生效问题
    (define-key shr-image-map "w" nil)
    (define-key eww-link-keymap "w" nil)
    )
  )

(use-package popper
  :defer 1.0
  :commands(popper-mode popper-toggle-latest)
  :init
  (defun my-C-1()
    (interactive)
    ;; 在popper里按C-.转到定义了就无法C-1了，提示"Cannot make side window the only window"。先切换到其它窗口再关闭
    (when (window-parameter nil 'window-side)
      (call-interactively 'other-window))
    ;; 抄自keyboard-escape-quit的判断，只有当1个窗口时才弹出popper
    (let ((show-popper (cond ((eq last-command 'mode-exited) t)
	                     ((region-active-p)
	                      'a)
	                     ((> (minibuffer-depth) 0)
	                      'b)
	                     (current-prefix-arg
	                      t)
	                     ((> (recursion-depth) 0)
	                      'c)
	                     (buffer-quit-function
	                      'd)
	                     ((not (one-window-p t))
	                      'e)
	                     ((string-match "^ \\*" (buffer-name (current-buffer)))
	                      'f)
                             (t t))))
      ;; 有popper窗口先关闭
      (when (bound-and-true-p popper-open-popup-alist)
        (call-interactively 'popper-toggle-latest))
      (if (eq show-popper t)
          (call-interactively 'popper-toggle-latest)
        (call-interactively 'keyboard-escape-quit) ;; 关闭minibuffer，关闭其它窗口
        )))
  (global-set-key (kbd "C-1") 'my-C-1)
  (global-set-key (kbd "M-`") 'popper-toggle-latest)

  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Backtrace\\*"
          help-mode
          compilation-mode
          shell-mode eshell-mode ;; 初始化还是全屏，但后面切换没事
          ))
  :config
  (popper-mode +1)
  (add-hook 'popper-open-popup-hook (lambda()
                                      (tab-line-mode -1)))
  (require 'popper-echo)
  (popper-echo-mode +1)

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'popper-close-window-hack)
  )

;; 这个就是辅助设置`display-buffer-alist'的，设置弹出窗口很方便
(use-package shackle
  :defer 1.0
  :init
  (setq
   shackle-default-size 0.4
   shackle-default-rule nil
   shackle-default-alignment 'below
   shackle-rules '( ;; 更多设置参看shackle.el https://github.com/seagle0128/.emacs.d/blob/47c606e43a207922de6b26f03d15827f685b0b3e/lisp/init-window.el#L145
                   ;; (compilation-mode :noselect t :align 'below :size 0.2);; noselect只是cursor不移动过去
                   (" server log\\*\\'" :noselect t :align 'below :size 0.2) ; dap mode的log窗口
                   (magit-status-mode    :select t :inhibit-window-quit t :same t) ;; magit全屏舒服
                   (magit-log-mode       :select t :inhibit-window-quit t :same t)
                   ("\\*gud-.*" :regexp t :select t :align 'below :other t :size 0.3)
                   ))
  :config
  (shackle-mode 1))

;; 这个comment行只要有mark即可，不需要全部选中(对于lisp相关mode它还是会优先region)
;; 按两下就会跳到行尾， C-u开头会对齐注释 
(use-package comment-dwim-2
  :defer 1.1 ;; 经常首次使用时卡住，故而不要用延迟加载了
  :bind ([remap comment-dwim] . comment-dwim-2)
  :init
  ;; (setq cd2/region-command 'cd2/comment-or-uncomment-region) ;; comment-dwim那种模式，多行需要全部选中
  (global-set-key "\M-;" 'comment-dwim-2)
  )

;; 修改光标下的数字，这个支持十六进制(shift-number不支持)
(use-package evil-numbers
  :commands(evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "<C-wheel-up>") 'evil-numbers/dec-at-pt) ;; CTRL+鼠标滚动
  (global-set-key (kbd "<C-wheel-down>") 'evil-numbers/inc-at-pt)
  )

(use-package so-long
  :defer 1.3
  :config
  (global-so-long-mode))

(use-package drag-stuff
  :commands(drag-stuff-up drag-stuff-down drag-stuff-right drag-stuff-left)
  :init
  (global-set-key (kbd "C-<up>") 'drag-stuff-up)
  (global-set-key (kbd "C-<down>") 'drag-stuff-down)
  ;; (global-set-key (kbd "C-<left>") 'drag-stuff-left) ;; 左右移动貌似没什么用，还是留给winner-mode
  ;; (global-set-key (kbd "C-<right>") 'drag-stuff-right)
  )

(use-package elfeed
  :load-path "~/.emacs.d/packages/tools/elfeed-master"
  :commands(elfeed)
  :init
  (setq
   elfeed-use-curl t ;; win10好像自带curl
   ;; elfeed-curl-program-name "curl"
   elfeed-feeds
   '("http://tttang.com/rss.xml"
     "https://xz.aliyun.com/feed"
     "https://paper.seebug.org/rss"
     )
   elfeed-search-filter "@6-months-ago" ;; 默认不显示已经read了的，设置为都显示
   )
  :config
  (define-key elfeed-search-mode-map (kbd "RET") 'elfeed-search-browse-url) ; 不需要查看简介什么的，直接打开浏览器就行了
  )

;; 删除了csharp-compilation的引用，tree-sister好像没起作用，将就用了
(use-package csharp-mode
  :commands(csharp-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

;; text object，一直想要的东西
(use-package objed
  :commands(objed-activate)
  :init
  (global-set-key (kbd "M-t") 'objed-activate)
  (with-eval-after-load 'view
    (define-key view-mode-map "t" 'objed-activate)) ;; 暂时没有全局的激活，方便view-mode
  :config
  (setcdr objed-mode-map nil) ;; 默认绑定不需要，只需要objed-mode的hook就够了
  (add-to-list 'objed-keeper-commands 'undo-fu-only-undo)
  (add-to-list 'objed-keeper-commands 'undo-fu-only-redo)  
  (add-to-list 'objed-cmd-alist '(mwim-beginning-of-code-or-line . line))
  (add-to-list 'objed-cmd-alist '(mwim-end-of-code-or-line . line))
  ;; objed使用的是hl-line的overlay，这里解决overlay被symbol-overlay覆盖的问题
  (add-to-list 'objed-init-hook (lambda ()
                                  (when (overlayp hl-line-overlay)
                                    (overlay-put hl-line-overlay 'priority 999))
                                  ))
  (add-to-list 'objed-exit-hook (lambda ()
                                  (when (overlayp hl-line-overlay)
                                    (overlay-put hl-line-overlay 'priority -50))
                                  ))
  ;; (objed-mode +1);; hook一些操作如next-line，并自动activate objed模式，用得很爽！
  (define-key objed-map "l" 'objed-del-insert) ;; l和i互换
  (define-key objed-map "i" (objed--call-and-switch right-char char))
  (define-key objed-map (kbd "C-h") nil)
  (define-key objed-map "x" 'objed-toggle-side) ;; x和j互换
  (define-key objed-map "j" 'objed-op-map) ;; x和j互换
  (define-key objed-map "q" 'objed-quit) ;; 这个q有点理解不了
  ;; 默认要which-key mode才显示，这里让
  (when (functionp 'which-key--show-keymap)
    ;; TODO: 需要第2次调用，它才会修改which-key-replacement-alist 显示短名。暂时不管了
    (define-key objed-map "c"
                (lambda () (interactive)
                  (which-key--show-keymap "keymap" objed-object-map nil nil 'no-paging)
                  (set-transient-map objed-object-map nil 'which-key--hide-popup)
                  )))
  )

(use-package posframe
  :commands(posframe-hide posframe-show)
  )

(use-package simple
  :defer t
  :init
  (setq blink-matching-paren nil) ;; 不然blink-matching-open不显示
  :config
  ;; blink-matching-paren为nil的话顺便把这个hook给去掉
  (remove-hook 'post-self-insert-hook #'blink-paren-post-self-insert-function)
  (remove-hook 'minibuffer-setup-hook #'minibuffer-history-isearch-setup) ;; isarch不需要？
  )
;; from https://github.com/seagle0128/.emacs.d/blob/4d0a1f0f0f6ed99e6cf9c0e0888c4e323ce2ca3a/lisp/init-highlight.el#L43
;; 显示屏幕外的匹配行了，29自带，但是滚动时不显示，参考上面修改成posframe显示，并且不是光标附近更友好
(use-package paren
  :defer t
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t
              )
  :config
  (with-no-warnings
    (defun show-paren-off-screen (&rest _args)
      (posframe-hide " *my-posframe-buffer*")
      (when (and (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; 屏蔽substring-no-properties的调用，有颜色好区分一些
        (cl-letf (((symbol-function #'substring-no-properties)
                   (lambda (msg &rest args)
                     (posframe-show " *my-posframe-buffer*"
                                    :string msg
                                    :position (point)
                                    :border-width 1
                                    :border-color "red"
                                    )
                     msg
                     )))
          (blink-matching-open))
        ))
    ;;(advice-add #'show-paren-function :after #'show-paren-off-screen)
    ))

(when (string-equal system-type "windows-nt")
  (use-package w32-browser
    :commands (w32explore dired-mouse-w32-browser dired-w32-browser dired-multiple-w32-browser dired-w32explore)
    :init
    ;; windows中打开并选中buffer对应的文件，C-X 6 是2C mode的前辍
    (defun browse-file-in-explorer (&optional file)
      "browse file in windows explorer"
      (interactive)
      (w32explore (or file (buffer-file-name (current-buffer)) default-directory))
      )
    (global-set-key (kbd "C-x C-d") 'browse-file-in-explorer)
    )
  
  (use-package gud-cdb
    ;; from https://github.com/junjiemars/.emacs.d/blob/master/config/gud-cdb.el，目前就只有这个在一直更新
    ;; 唯一不足的是不支持speedbar，还有attach    
    :commands(cdb gud-kill-buffer)
    :init
    (defalias 'defcustom% 'defcustom)
    (defalias 'ignore* 'ignore)
    (defalias 'loop* 'loop)
    (defalias 'assoc** 'assoc)
    (defvar cdb-add-g nil)
    :config
    (defadvice cdb-command-line-list-source (after my-cdb-command-line-list-source activate)
      "追加-2参数让启动后新开一个命令窗口，-G忽略进程退出的breakpoint, 禁止从网络下载symbol"
      ;; TODO: "-g"忽略初始化breakpoint，目前需要在启动breakpoint时设置断点，还不支持预先设置断点和记忆断点
      (setq ad-return-value (append ad-return-value 
                                    (if cdb-add-g
                                        '("-2" "-G" "-netsymsno" "-g")
                                      '("-2" "-G" "-netsymsno"))))) ;; cdb /?不对啦(跟.netsyms命令对得上)
      ;; 清楚显示当前行 from cdb-gud
    (defvar gud-overlay
      (let* ((ov (make-overlay (point-min) (point-min))))
        (overlay-put ov 'face 'secondary-selection)
        ov)
      "Overlay variable for GUD highlighting.")
    (defadvice gud-display-line (after my-gud-highlight act)
      "Highlight current line."
      (let* ((ov gud-overlay)
             (bf (gud-find-file true-file)))
        (if bf
	    (save-excursion
	      (set-buffer bf)
	      (move-overlay ov (line-beginning-position) (line-end-position) (current-buffer))))))
    (defun gud-kill-buffer ()
      (if (eq major-mode 'gud-mode)
          (delete-overlay gud-overlay)))
    (add-hook 'gud-cdb-mode-hook (lambda()
                                   (add-hook 'kill-buffer-hook #'gud-kill-buffer nil :local)
                                   (gud-def gud-quit "q " "\C-q" "Quit Debug")
                                   ))
    (global-set-key (kbd "S-<f5>") 'gud-quit)
    ))

(use-package gud
  :defer t
  :init
  (setq gud-chdir-before-run nil) ;; 避免gud自动设置运行目录为exe所在目录
  (defvar f5-read-command t)
  (defun my-f5()
    (interactive)
    (when current-prefix-arg
      (setq f5-read-command t))
    (condition-case nil
        (call-interactively 'gud-cont)
      (error (if f5-read-command
                 (progn (setq f5-read-command nil)
                        (call-interactively 'cdb))
               (cdb (car gud-cdb-history))))))
  (global-set-key (kbd "<f4>") (lambda()
                                 (interactive)
                                 (if (and (bound-and-true-p gud-comint-buffer) (buffer-name gud-comint-buffer))
                                     (call-interactively 'gud-jump)
                                   (call-interactively 'next-error)
                                   )))
  (global-set-key (kbd "<f5>") 'my-f5) ;; C-u F5改变命令行
  (global-set-key (kbd "C-<f5>") (lambda() ;; cdb添加-g参数直接运行，不能下断点
                                   (interactive)
                                   (let ((cdb-add-g t))
                                     (call-interactively 'my-f5))))
  (global-set-key (kbd "<f9>") 'gud-break)
  (global-set-key (kbd "C-<f9>") 'gud-tbreak) ;; tempory breakpoint
  ;; (global-set-key (kbd "C-<f9>") 'gud-remove)
  (global-set-key (kbd "<f10>") 'gud-next)
  (global-set-key (kbd "<f11>") 'gud-step)
  (global-set-key (kbd "<f12>") 'gud-print) ;; 打印cursor所在变量，比输入dv(cdb)要快点。支持region
  :config
  (add-hook 'gud-mode-hook (lambda()
                             (corfu-mode) ;; gud有capf可用
                             ))
  (defadvice gud-sentinel (after my-gud-sentinel activate)
    "自动关闭Debugger finished的gud buffer，from https://www.reddit.com/r/emacs/comments/ggs0em/autoclose_comint_buffers_on_exit_or_process_end/"
    (let ((process (ad-get-arg 0)))
      (unless (process-live-p process)
        (kill-buffer (process-buffer process))
        (delete-other-windows)
        (message "Debugger finished")))
    )
  )

(use-package god-mode
  :diminish(god-local-mode)
  :commands(god-local-mode)
  :init
  (setq god-mode-enable-function-key-translation nil ;; 禁止F1等
        god-mode-alist '((nil . "C-")
                         ("m" . "M-") ;; g代表C-g，但是一但进入命令还是要C-g取消了
                         ("M" . "C-M-"))
        )

  ;; ESC代替C-g
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; 普通方法还不行
  (with-eval-after-load 'view
    (setq view-mode-map (make-sparse-keymap)))
  (add-hook 'view-mode-hook (lambda()
                              (god-local-mode (if buffer-read-only +1 -1))
                              ))
  ;; 方便god mode的键绑定
  (global-set-key (kbd "C-<") 'beginning-of-buffer)
  (global-set-key (kbd "C->") 'end-of-buffer)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  :config
  (define-key god-local-mode-map (kbd "i") 'view-mode)
  )

;; 好的theme特点:
;; treemacs里git非源码里区别明显(doom-one)，
;; eldoc参数当前哪个参数很明显
;; tabbar被修改的*文件有明显显示(spacemacs)
;; 当前buffer modeline背景色(doom-dark+)
;; helm occur关键字高亮明显，不明显就换了吧那个暂时还不知道如何定制
(if (display-graphic-p)
    (progn

      ;;(add-to-list 'load-path "~/.emacs.d/themes")

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
            ;; (message "load-doom-theme: %s" (symbol-name th))

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
                   (set-face-attribute 'show-paren-match nil :underline t :weight 'bold)
                   (custom-set-faces
	            '(line-number ((t (:foreground "#6F6F6F"))));; doom-one的行号实在看不清
	            '(corfu-current ((t (:foreground "#c678dd"))))
                    )
                   (when nil
                     ;; region有点看不清，单独设置
                     (set-face-attribute 'region nil :background "#4C7073")
                     )
                   )
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
      )
  (progn
    (set-face-attribute 'region nil :background "#4C7073" :foreground "Black"))
  )

;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
