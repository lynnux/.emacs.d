;; 非官方自带packages的设置 -*- lexical-binding: t -*-
;; benchmark: 使用profiler-start和profiler-report来查看会影响emacs性能，如造成卡顿的命令等
;; 拖慢gui测试：C-x 3开两个窗口，打开不同的buffer，C-s搜索可能出现比较多的词，测试出doom modeline和tabbar ruler比较慢
;; 参考https://gitee.com/advanceflow/elisp/blob/main/%E9%99%84%E5%BD%95H-%E6%A0%87%E5%87%86%E9%92%A9%E5%AD%90.org 查看各种hook，能去掉就去掉
;; use-package的好处之一是defer可以设置延迟几秒加载！光yas一项就提升了启动速度
;; :load-path不是延迟设置

;; 使用老版本提示
;; 28.0.50，1.查看eln-cache，会有几个文件的TMP一直生成，找到删除对应elc就可以了，不然每次emacs启动都会去编译 2.删除自带的python不然lsp有问题
;; 在使用了org-roam的功能后退出emacs会崩溃，最后发现应该是native-comp的问题，有个gnus-art的文件比较大，但是跟上面一样只有tmp生成，找到删除gnus-art.elc就可以了
;; 以后的崩溃问题都可以参考这个处理，一般是eln-cache里有tmp没编译好造成emacs退出时崩溃

(defun delay-require-libs (path list)
  "临时设置`load-path'并require需要的库"
  (add-to-list 'load-path path)
  (dolist (l list)
    (require l))
  (setq load-path (delete path load-path)))

(defmacro dec-placeholder-fun (fn feature path list)
  "对可以延迟调用的命令，延迟设置`load-path'，并在require后还原`load-path'"
  `(defun ,fn (&optional arg)
     (interactive)
     (unless (featurep ',feature)
       (fmakunbound ',fn) ;; 取消本函数定义
       (delay-require-libs ,path ,list)
       (if arg
           (,fn arg)
         ;; 没有参数调用还区分是否是`call-interactively'调用
         (if (called-interactively-p 'interactive)
             (call-interactively ',fn)
           (,fn))))))

(defun update-all-packages ()
  (interactive)
  "测试发现对启动速度还是有影响的，这里手动执行更新就可以了，tree-sistter那个用根目录的setup.py下载bin和解压"
  (ensure-latest "~/.emacs.d/themes/diff-hl-master.zip")
  (ensure-latest "~/.emacs.d/themes/themes-master.zip")
  (ensure-latest "~/.emacs.d/packages/citre/citre-master.zip")
  (ensure-latest "~/.emacs.d/packages/corfu/corfu-main.zip")
  (ensure-latest "~/.emacs.d/packages/dired/dired-hacks-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/expand-region/expand-region.el-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/easy-kill/easy-kill-extras.el-master.zip"
   t)
  (ensure-latest
   "~/.emacs.d/packages/multiple-cursors/multiple-cursors.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/vertico-main.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/embark-master.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/consult-main.zip")
  (ensure-latest "~/.emacs.d/packages/minibuffer/compat-main.zip")
  (ensure-latest "~/.emacs.d/packages/magit/magit-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/org/emacs-maple-preview-master.zip")
  (ensure-latest "~/.emacs.d/packages/org/org-roam-main.zip")
  (ensure-latest "~/.emacs.d/packages/org/emacsql-master.zip")
  (ensure-latest "~/.emacs.d/packages/tools/rg.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/tools/elfeed-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/use-package/use-package-master.zip")
  (ensure-latest "~/.emacs.d/packages/lsp/lsp-mode-master.zip")
  (ensure-latest "~/.emacs.d/themes/emacs-dashboard-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/cycle-at-point/emacs-cycle-at-point-main.zip"
   nil
   "emacs-cycle-at-point") ;; codeberg.org上zip里的文件夹名不含-main
  )

;; 用于use-package避免自动设置:laod-path
(defun my-eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun get-file-time (file)
  "获取文件的修改时间"
  (interactive "P")
  (time-convert (file-attribute-modification-time
                 (file-attributes file))
                'integer))

(defun straight--build-compile (dir)
  "核心就是调用byte-recompile-directory，但只有跨进程才不报错"
  (let* ((emacs (concat invocation-directory invocation-name))
         (program
          (format
           "(let ((default-directory %S))
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory %S 0 'force))"
           dir dir))
         (args (list "-Q" "-L" dir "--batch" "--eval" program)))
    (with-current-buffer (get-buffer-create
                          "*straight-byte-compilation*")
      (insert
       (format "\n$ %s %s %s \\\n %S\n"
               emacs
               (string-join (cl-subseq args 0 3) " ")
               (string-join (cl-subseq args 3 5) " ")
               program)))
    (apply #'call-process
           `(,emacs nil "*straight-byte-compilation*" nil ,@args))))
(defun ensure-latest (zip &optional no-compile dir-name)
  "自动更新zip包，zip里的目录组织有格式要求，直接用git下载的zip就可以了"
  (interactive "P")
  (let* ((check-file
          (expand-file-name (concat
                             ".cache/"
                             (file-name-nondirectory zip))
                            user-emacs-directory))
         (expand-zip (expand-file-name zip))
         (extdir (file-name-directory expand-zip))
         (target-dir
          (concat
           extdir
           (if dir-name
               dir-name
             (file-name-base expand-zip)))))
    (when (file-newer-than-file-p expand-zip check-file)
      (delete-directory target-dir t nil) ;先删除目录
      (call-process-shell-command
       (concat "unzip " expand-zip " -d " extdir))
      (unless no-compile
        (message (format "Building %s ..." target-dir))
        (straight--build-compile target-dir))
      ;; 创建空的时间戳文件
      (unless (file-exists-p
               (expand-file-name ".cache" user-emacs-directory))
        (make-directory (expand-file-name ".cache"
                                          user-emacs-directory)
                        t))
      (call-process-shell-command
       (concat "touch " check-file " -r " expand-zip)))))

(unless (symbol-function 'use-package)
  (add-to-list
   'load-path "~/.emacs.d/packages/use-package/use-package-master")
  (condition-case nil
      (require 'use-package)
    (error (update-all-packages))) ;; 首次检查是否解压，之后还是手动更新包吧
  )

(add-to-list 'load-path "~/.emacs.d/packages")

;; (ensure-latest "~/.emacs.d/settings/test.zip")
;; F1 v查看变量 sanityinc/require-times，正常一页就显示完了，目前11个包
;; 有个几年前的封装没有用，直接用的原版的 https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
(use-package init-benchmarking
  :disabled
  :config
  (add-hook
   'after-init-hook
   (lambda ()
     ;; 启动后就不再需要了，会把helm等算进去
     (advice-remove 'require 'sanityinc/require-times-wrapper))))

;; !themes要放到最后，内置theme查看 M-x customize-themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defvar use-my-face nil)

;; 消除mode line上的minor提示字符
(use-package diminish
  :commands (diminish))

(defvar repeat-message-function nil) ;; 必须定义，不然运行报错，因为被closure引用了
(defmacro defrepeater (name-or-command &optional command)
  "自定义repeat命令，不需要开启repeat-mode就能用，from https://github.com/alphapapa/defrepeater.el/blob/master/defrepeater.el"
  (let*
      ((name
        (if command
            ;; `defalias' style
            (cadr name-or-command)
          ;; Automatic repeater function name
          (intern
           (concat (symbol-name (cadr name-or-command)) "-repeat"))))
       (command (or command name-or-command))
       (docstring
        (concat
         (format "Repeatedly call `%s'." (cadr command)) "\n\n"
         (s-word-wrap
          80
          (format
           "You may repeatedly press the last key of the sequence bound to this command to repeatedly call `%s'."
           (cadr command))))))
    `(progn
       (when (fboundp ',name)
         (warn "Function is already defined: %s" ',name))
       (defun ,name ()
         ,docstring
         (interactive)
         (let ((repeat-message-function #'ignore))
           (setq last-repeatable-command ,command)
           (repeat nil)))
       ',name)))

(use-package dash
  :commands (-distinct))

(use-package s
  :commands (s-split s-word-wrap))

(use-package eldoc
  :defer t
  :diminish (eldoc-mode)
  :init
  (setq eldoc-echo-area-use-multiline-p nil) ;; 不要多行显示
  :config
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore) ;; 在pre-command-hook里影响性能
  (with-eval-after-load 'grammatical-edit
    ;; 自作聪明就给加了eldoc
    (eldoc-remove-command-completions "grammatical-edit-")))

(autoload 'defhydra "hydra" nil t)

(defhydra
 hydra-bookmark
 ()
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
  ;; :disabled
  :init
  (setq
   dired-compress-file-alist
   '(("\\.7z\\'" . "7z a -r %o %i")
     ("\\.zip\\'" . "7z a -r %o  %i")) ;; 大写Z用的命令
   dired-compress-files-alist ;; 支持多文件
   '(("\\.7z\\'" . "7z a -r %o %i")
     ("\\.zip\\'" . "7z a -r %o  %i")) ;; 小写z用的命令，统一用7z，解压的话是根据文件后辍调用shell，同步用!，异步用&
   dired-compress-directory-default-suffix ".7z" ;; 大写Z对文件夹默认压缩后辍
   dired-compress-file-default-suffix ".7z" ;; 大写Z对文件默认压缩后辍
   dired-do-revert-buffer t ;; 大写Z选中多个时是分别压缩，这点不如z
   dired-mouse-drag-files t ;; 没有作用
   )
  (put 'dired-find-alternate-file 'disabled nil) ;; 避免使用该函数时提示
  (global-set-key [remap dired] 'dired-jump) ;; 直接打开buffer所在目录，无须确认目录
  (use-package dired-recent
    :commands (dired-recent-mode dired-recent-open)
    :init
    (setq dired-recent-mode-map nil) ;; 禁止它注册C-x C-d
    (global-set-key (kbd "C-c d") 'dired-recent-open)
    (global-set-key (kbd "C-c C-d") 'dired-recent-open)
    :config
    (with-eval-after-load 'marginalia
      ;; 效果跟consult--read带:category 'file一样，embark也能正常识别了
      (add-to-list
       'marginalia-command-categories '(dired-recent-open . file))))

  ;; bug较多能用，好处是支持diredful、diff-hl显示
  (use-package dired-sidebar
    :commands (dired-sidebar-toggle-sidebar)
    :init
    (global-set-key (kbd "<C-f1>") 'dired-sidebar-toggle-sidebar)
    (setq
     dired-sidebar-theme 'ascii
     dired-sidebar-width 32
     ;; dired-sidebar-use-custom-font t 不好看，字变小了
     dired-sidebar-should-follow-file nil ; 太卡了！主要它的timer是repeat的，而且当emacs缩小窗口后会莫名弹出来？
     dired-sidebar-follow-file-idle-delay 0.5
     dired-sidebar-theme 'none
     dired-sidebar-delay-auto-revert-updates nil ;; 不需要自动刷新
     dired-sidebar-refresh-on-special-commands nil ;; 这个会导致buffer自动刷新(diff hl刷新很明显)
     dired-sidebar-refresh-on-project-switch nil ;; 有bug
     dired-sidebar-no-delete-other-windows t ;; C-1不关闭，最需要的！
     dired-sidebar-use-one-instance t ;; 有bug 不起效果，:开头的buffer还是很多
     )
    :config
    ;; 对部分命令执行后follow。不能用buffer-list-update-hook，会大量调用diff-hl卡死
    (defvar dired-sidebar-follow-to-file-timer nil)
    (defun dired-sidbar-follow-file-advice (&rest args)
      (when (dired-sidebar-showing-sidebar-p)
        (when dired-sidebar-follow-to-file-timer
          (cancel-timer dired-sidebar-follow-to-file-timer))
        (setq dired-sidebar-follow-to-file-timer
              (run-with-idle-timer
               dired-sidebar-follow-file-idle-delay
               nil
               #'dired-sidebar-follow-to-file))))
    (defvar dired-sibar-follow-file-commands
      '(my-pop-select
        my-project-search
        tab-line-select-tab
        volatile-kill-buffer
        consult-buffer
        ff-get-other-file)) ;; 随时加
    (cl-dolist
     (jc dired-sibar-follow-file-commands)
     (advice-add jc :after #'dired-sidbar-follow-file-advice))
    (defun dired-sidebar-follow-to-file ()
      "直接用dired-sidebar-follow-file，hl-line的行有问题，这里修复下"
      (interactive)
      (ignore-errors
        (dired-sidebar-follow-file)
        (when (dired-sidebar-showing-sidebar-p)
          (with-current-buffer (dired-sidebar-buffer)
            (hl-line-highlight) ;; dired-sidebar-theme需要设置为none或者icons，避免调用`dired-sidebar-tui-update-with-delay'来revert buffer失去hl-line效果
            ))))
    (define-key
     dired-sidebar-mode-map (kbd "C-l") 'dired-sidebar-up-directory)
    (define-key
     dired-sidebar-mode-map (kbd "l") 'dired-sidebar-up-directory)
    (define-key
     dired-sidebar-mode-map
     [double-mouse-1]
     'dired-sidebar-up-directory))
  ;; 以dired形式展示fd搜索的文件
  (use-package fd-dired
    :commands (fd-dired))

  :commands (dired dired-jump)
  :config
  (add-to-list 'load-path "~/.emacs.d/packages/dired")
  (when (string-equal system-type "windows-nt")
    (define-key
     dired-mode-map (kbd "C-x C-d")
     (lambda ()
       (interactive)
       (condition-case nil
           (call-interactively 'dired-w32explore)
         ;; 移到最下面那行就有问题，采用`browse-file-in-explorer'解决
         (error (call-interactively 'browse-file-in-explorer)))))
    (define-key dired-mode-map (kbd "<C-return>") 'dired-w32-browser) ;; 使用explorer打开
    (define-key dired-mode-map (kbd "<C-enter>") 'dired-w32-browser) ;; 使用explorer打开
    )

  (defvar dired-sort-by-history '("X")) ;; 列表session才会记录下来
  (setq dired-listing-switches
        (concat
         "--group-directories-first -alh"
         (nth 0 dired-sort-by-history))) ;; 更好的文件大小，默认以后辍排序。(必须测试支持dired-sidebar)
  (defun dired-sort-auto ()
    (interactive)
    (cond
     ((equal (nth 0 dired-sort-by-history) "S")
      (setq dired-sort-by-history '("t")))
     ((equal (nth 0 dired-sort-by-history) "t")
      (setq dired-sort-by-history '("X")))
     ((equal (nth 0 dired-sort-by-history) "X")
      (setq dired-sort-by-history '("")))
     ((equal (nth 0 dired-sort-by-history) "")
      (setq dired-sort-by-history '("S"))))
    (setq dired-listing-switches
          (concat
           "--group-directories-first -alh"
           (nth 0 dired-sort-by-history)))
    (dired-sort-other dired-listing-switches))
  (define-key dired-mode-map "s" 'dired-sort-auto)

  ;; allow dired to delete or copy dir
  (setq dired-recursive-copies (quote always)) ; “always” means no asking
  (setq dired-recursive-deletes (quote top)) ; “top” means ask once
  (setq
   dired-dwim-target t ;; 开两个dired的话会自动识别other dired为target
   dired-kill-when-opening-new-dired-buffer t ; 28.1新加的，避免打开目录时又多一个dired buffer
   )
  (define-key
   dired-mode-map (kbd "C-l")
   (lambda ()
     (interactive)
     (find-alternate-file ".."))) ;; C-l上级目录
  (define-key
   dired-mode-map (kbd "l")
   (lambda ()
     (interactive)
     (find-alternate-file ".."))) ;; l上级目录
  (define-key dired-mode-map (kbd "C-o") 'avy-goto-word-1)
  ;; dired里面再次C-x d可以设置路径
  (define-key
   dired-mode-map (kbd "C-x d")
   (lambda ()
     (interactive)
     (call-interactively 'dired)))
  (define-key dired-mode-map " " 'View-scroll-page-forward)
  (define-key dired-mode-map [?\S-\ ] 'View-scroll-page-backward)
  (define-key dired-mode-map "w" 'View-scroll-page-backward)
  (define-key dired-mode-map "W" 'dired-copy-filename-as-kill)
  (define-key dired-mode-map "1" 'delete-other-windows)
  (define-key dired-mode-map "\C-t" 'mark-word) ; 还有t, U等mark快捷键
  (define-key dired-mode-map [mouse-2] 'dired-find-file) ;; 避免鼠标点击目录打开多个目录，不要用`dired-find-alternate-file`它会在打开文件后直接会销毁dired，对有时误操作的恢复不了
  (define-key
   dired-mode-map [double-mouse-1]
   (lambda ()
     (interactive)
     (find-alternate-file ".."))) ;; 鼠标双击空白处返回上级目录，原来是选中好像也没什么用，直接替换了

  ;; consult-find -> embark-export to dired-mode工作流无敌！这里改成跟wgrep一样的快捷键
  (define-key
   dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)

  (defun my-dired-do-rename ()
    "重命名，默认就是当前文件"
    (interactive)
    (minibuffer-with-setup-hook
        (lambda ()
          (call-interactively 'next-history-element)) ;; 执行M-n future history
      (call-interactively 'dired-do-rename)))
  (define-key
   dired-mode-map [remap dired-do-rename] 'my-dired-do-rename)


  ;; 给目录加上[]以便跟普通buffer区分开
  (add-hook
   'dired-after-readin-hook
   (lambda ()
     (ignore-errors
       (unless (eq major-mode 'dired-sidebar-mode)
         (let ((count 1)
               (name
                (format "[%s]"
                        (file-name-nondirectory
                         (directory-file-name dired-directory)))))
           ;; buffer名不能重名
           (while (get-buffer name)
             (setq name
                   (format "[%s]%d"
                           (file-name-nondirectory
                            (directory-file-name dired-directory))
                           count))
             (setq count (+ count 1)))
           (rename-buffer name))))))

  (add-hook
   'dired-mode-hook
   (lambda ()
     (hl-line-mode +1)
     (face-remap-add-relative 'hl-line '(:background "#666"))
     (unless (file-remote-p default-directory)
       (auto-revert-mode) ;; 还有个选项也可以global-auto-revert-non-file-buffers
       )))

  (defun delay-dired-relate-init ()
    "因为tree-sittr加载会导致dired加载，这里把dired相关mode延迟到dired-mode-hook"
    (remove-hook 'dired-mode-hook 'delay-dired-relate-init)

    (dired-recent-mode 1)
    (dired-recent-path-save) ;; 调用一次修复延迟加载导致的问题

    (use-package dired-hist
      :config
      (define-key dired-mode-map [backspace] 'dired-hist-go-back)
      (define-key dired-mode-map (kbd "M-p") #'dired-hist-go-back)
      (define-key dired-mode-map (kbd "M-n") #'dired-hist-go-forward)
      (dired-hist-mode 1)
      (dired-hist--update) ;; 调用一次修复延迟加载导致的问题
      )

    (use-package dired-filter
      :defer t
      :init
      (defhydra
       dired-filter-map-select
       ()
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
       ("!" dired-filter-negate nil :color blue) ;; 饜磁rename岆淩籟B陛ㄐ
       ("*" dired-filter-decompose nil :color blue)
       ("." dired-filter-by-extension nil :color blue)
       ("/" dired-filter-pop-all nil :color blue)
       ("A" dired-filter-add-saved-filters nil :color blue) ;; 珆尨祥賸ˋ
       ("D" dired-filter-delete-saved-filters nil :color blue)
       ("L" dired-filter-load-saved-filters nil :color blue) ;; 祥雅
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
      (define-key dired-mode-map "/" 'dired-filter-map-select/body)
      (define-advice dired-filter-map-select/body
          (:around (orig-fn &rest args))
        (unless (featurep 'dired-filter)
          (delay-require-libs
           "~/.emacs.d/packages/dired/dired-hacks-master"
           '(dired-filter)))
        (apply orig-fn args)))

    (use-package diredful
      :init
      (setq diredful-init-file
            "~/.emacs.d/packages/dired/diredful-conf.el")
      :config (diredful-mode 1)))
  (add-hook 'dired-mode-hook 'delay-dired-relate-init))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map "w" 'scroll-down-command)
  (define-key help-mode-map [backspace] 'help-go-back)
  (define-key help-mode-map (kbd "M-p") 'help-go-back)
  (define-key help-mode-map (kbd "M-n") 'help-go-forward)
  (define-key help-mode-map (kbd "DEL") 'help-go-back))

;; 保存cursor位置
(use-package saveplace
  :init
  (setq
   save-place-file
   (expand-file-name ".saveplace" user-emacs-directory)
   save-place-forget-unreadable-files t)
  :hook (after-init . save-place-mode)
  :config
  (add-hook
   'save-place-after-find-file-hook
   (lambda ()
     (if buffer-file-name
         ;; find-file-hook执行时，window-buffer还没有变成新buffer，这时recenter会出错，
         ;; 所以这里用timer来执行recenter
         (with-current-buffer (get-file-buffer buffer-file-name)
           (run-with-local-idle-timer
            0.1 nil
            (lambda ()
              (ignore-errors
                (recenter))))))))
  (define-advice save-place-to-alist (:around (orig-fn &rest args))
    "不记录so-long-mode打开的文件，不然下次打开可能会很卡"
    (unless (eq major-mode 'so-long-mode)
      (apply orig-fn args))))

;; 还是放弃session的那个file-name-history吧，现在都用这个了
(use-package recentf
  :init
  (setq recentf-save-file
        (expand-file-name ".recentf" user-emacs-directory))
  (setq recentf-max-saved-items 500)
  ;; Disable recentf-cleanup on Emacs start, because it can cause problems with
  ;; remote files.
  ;; recentf-auto-cleanup 'never
  (setq
   recentf-exclude
   '(".cache"
     ".cask"
     "bookmarks$"
     "/G?TAGS$"
     "COMMIT_EDITMSG\\'"
     "/ssh:"
     "/sudo:"
     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\|zip\\|xz\\)$"
     "^/tmp/"
     (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  :hook (after-init . recentf-mode))

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
  (setq session-name-disable-regexp
        "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\|COMMIT_EDITMSG\\)")
  ;; (setq session-set-file-name-exclude-regexp ) ; 过滤 file-name-history
  (setq session-save-file-coding-system 'utf-8)
  (setq
   session-globals-include
   '(
     ;;(kill-ring 50) ;; 用ditto就行了
     (session-file-alist 100 t)
     (file-name-history 300)
     query-replace-defaults ;; M-r replace-string默认值
     )
   session-locals-include nil)

  :config (add-hook 'after-init-hook 'session-initialize)
  ;; 使用一段时间后，可以自行查看~/.emacs.d/.session里占用太多，然后添加到排除列表里
  (add-to-list 'session-globals-exclude 'rg-history)
  (add-to-list 'session-globals-exclude 'helm-grep-ag-history)
  (add-to-list 'session-globals-exclude 'helm-grep-history)
  (add-to-list 'session-globals-exclude 'helm-occur-history)
  (add-to-list 'session-globals-exclude 'log-edit-comment-ring)
  (add-to-list
   'session-globals-exclude 'helm-source-complex-command-history)
  (add-to-list 'session-globals-exclude 'kmacro-ring)
  (add-to-list 'session-globals-exclude 'file-name-history) ;; recentf记录了
  (add-to-list 'session-globals-exclude 'vertico-repeat-history)
  (add-to-list 'session-globals-exclude 'org-roam-node-history)
  (add-to-list 'session-globals-exclude 'consult--buffer-history)
  (add-to-list 'session-globals-exclude 'buffer-name-history) ;; 这个wcy已经记录了
  (add-to-list 'session-globals-exclude 'consult-xref--history)
  (add-to-list 'session-globals-exclude 'kill-ring))

;; 用session就够了
(use-package savehist
  :disabled
  :init
  (setq savehist-file
        (expand-file-name ".savehist" user-emacs-directory))
  ;;(setq history-delete-duplicates nil)
  (setq savehist-autosave-interval nil) ; 只emacs退出时保存，不要timer
  (setq savehist-save-minibuffer-history nil) ;; 我们只给dired的sort用
  :config
  (defun my-init-savehist ()
    (savehist-mode +1)
    (remove-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)))


(global-set-key (kbd "C-x f") 'hydra-find-file-select)
(global-set-key (kbd "C-x C-r") 'files-recent-visited)
(global-set-key (kbd "C-c o") 'hydra-occur-select)
(global-set-key (kbd "C-c m") 'hydra-hideshow-select) ; bug:最后一个第3参数必须带名字，否则上面最后一行不显示  

(defun files-recent-type (src)
  (interactive)
  (let* ((tocpl
          src ;; 全路径好些，可以通过项目名搜索，也解决了文件名相同时的bug
          ;; (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
          ;; 	src)
          )
         (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file ;; (cdr (assoc-string fname tocpl))
       fname))))
(defun files-recent-visited ()
  (interactive)
  (files-recent-type
   (if (featurep 'recentf)
       recentf-list ; 这个只在exit emacs时更新file-name-history
     file-name-history)))
;; 目前只发现session有changed file列表
(defun files-recent-changed ()
  (interactive)
  ;; 需要配合session.el使用
  (files-recent-type
   (mapcar (lambda (x) (car x)) session-file-alist)))

(defun hydra-find-file-select ()
  (interactive)
  (unless (functionp 'hydra-find-file/body)
    (defun prj-find-file ()
      (interactive)
      (call-interactively 'project-find-file))
    (defhydra
     hydra-find-file
     ()
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
     ("v" files-recent-visited nil :color blue)
     ("a" find-file-at-point nil :color blue)
     ("r" files-recent-visited nil :color blue)
     ("p" prj-find-file nil :color blue)
     ("d" dired-recent-open nil :color blue)
     ("q" nil "nil" :color blue)))
  (funcall 'hydra-find-file/body))

(defun hydra-occur-select ()
  (interactive)
  (unless (functionp 'hydra-occur/body)
    (defhydra
     hydra-occur
     ()
     "
_a_: all   _t_: type
_m_: mode  _RET_: this buffer
_q_uit"
     ("a" all-occur nil :color blue)
     ("t" type-occur nil :color blue)
     ("m" mode-occur nil :color blue)
     ("RET" occur nil :color blue)
     ("q" nil "nil" :color blue)))
  (funcall 'hydra-occur/body))

;; hydra使用autoload的方式 https://github.com/abo-abo/hydra/issues/149
(defun hydra-hideshow-select ()
  (interactive)
  (hs-minor-mode)
  (unless (functionp 'hydra-hideshow/body)
    (defhydra
     hydra-hideshow
     ()
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

(defvar my-hs-hide nil
  "Current state of hideshow for toggling all.")
;;;###autoload
(defun hs-toggle-hiding-all ()
  "Toggle hideshow all."
  (interactive)
  (setq-local my-hs-hide (not my-hs-hide))
  (if my-hs-hide
      (hs-hide-all)
    (hs-show-all)))

;; display-line-numbers是C实现的，最快！
(use-package display-line-numbers
  :commands (display-line-numbers-mode)
  :init (setq display-line-numbers-width-start 3)
  :hook (find-file . display-line-numbers-mode))

;;; better C-A C-E
(use-package mwim
  :commands
  (mwim-beginning-of-code-or-line
   mwim-beginning-of-line-or-code
   mwim-end-of-code-or-line
   mwim-end-of-line-or-code)
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x u") (defrepeater #'undo-redo)) ;; 这个其实是undo，习惯undo tree这个快捷键了

;; undo-fu小巧才15K
(use-package undo-fu
  :disabled
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  (global-set-key (kbd "M-/") 'undo-fu-only-redo)
  (global-set-key (kbd "C-x u") (defrepeater #'undo-fu-only-redo)) ;; 这个其实是undo，习惯undo tree这个快捷键了
  )

(use-package vundo
  :commands (vundo))

;; 经常C-x C-s按错，还是用这个吧
(setq
 auto-save-visited-interval 1
 save-silently t)
(auto-save-visited-mode 1)
(when auto-save-visited-mode
  ;; 参考super save，对于某些命令立即调用保存，避免save buffer yes no提示
  (defvar super-save-triggers
    '(magit
      my-project-magit
      project-compile
      quickrun
      save-buffers-kill-terminal
      volatile-kill-buffer))
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
     super-save-triggers-after))
  (defun super-save-command ()
    (when auto-save-visited-mode ;; 暂时没有remove advice
      ;; 调用auto-save-visited-mode的timer省一些逻辑
      (apply (timer--function auto-save--timer)
             (timer--args auto-save--timer))))
  (super-save-advise-trigger-commands))

(use-package tempel
  :bind
  ( ;; ("M-+" . tempel-complete)
   ("M-<return>" . tempel-insert)
   :map
   tempel-map
   ("<tab>" . tempel-next)
   ("<backtab>" . tempel-previous))
  :commands (tempel-insert tempel--templates tempel-expand)
  :init
  (setq
   tempel-path
   (expand-file-name "packages/tempel_templates" user-emacs-directory)
   tempel-auto-reload nil ;; 不需要检测模板是否更新了
   )
  (defun my-get-str-at-point ()
    "lc|aaa得到lc，<aaa|>得到aaa"
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                (beg (car bounds))
                (end (cdr bounds)))
      (when (> end (point))
        (setq end (point)))
      (list (buffer-substring-no-properties beg end) beg end)))
  (defun my-tempel-expandable-p ()
    "from https://gitlab.com/daanturo/e/-/blob/main/autoload/16-my-functions-snippet.el#L47"
    (when (and
           t ;; (memq (char-after) '(?\C-j ?  nil)) ;; 位置必须是最后或者后面有空格
           (require 'tempel nil 'noerror))
      (let ((s
             (car-safe (my-get-str-at-point))
             ;; (thing-at-point 'symbol)
             ))
        (when (and s (assoc (intern s) (tempel--templates)))
          t))))
  (defmacro tab-try-tempel-first (cmd)
    `(lambda ()
       (interactive)
       (if (my-tempel-expandable-p)
           (call-interactively 'tempel-expand)
         (call-interactively ,cmd))))
  (global-set-key
   [remap indent-for-tab-command]
   (tab-try-tempel-first 'indent-for-tab-command))
  (with-eval-after-load 'corfu
    ;; 解决补全弹出过快，TAB对tempel无效的问题，TAB优先tempel
    (defun corfu-complete-pass-tempel (&rest app)
      (if (my-tempel-expandable-p)
          (call-interactively 'tempel-expand)
        (apply app)))
    (advice-add #'corfu-insert :around #'corfu-complete-pass-tempel))
  (with-eval-after-load 'cc-mode
    (define-key
     c-mode-base-map
     [remap c-indent-line-or-region]
     (tab-try-tempel-first 'c-indent-line-or-region)))
  :config
  (defadvice tempel--prefix-bounds
      (around my-tempel--prefix-bounds activate)
    "支持lc|aaa得到lc"
    (let ((bounds (cdr-safe (my-get-str-at-point))))
      (when bounds
        (setq ad-return-value (cons (car bounds) (nth 1 bounds)))))))

;; 利用tempel实现yas功能
(use-package yasnippet
  :defer t
  :config
  (setq yas-global-mode t)
  (defvar yas-minor-mode t)
  (defun yas-minor-mode ()) ;; 最新版eglot需要
  (defun yas-expand-snippet (snippet &optional start end expand-env)
    "不需要参数，只需要额外输入个()就行了，这里把参数那些都去掉"
    ;; 参数表达式：`$1`, `$2`和 `${3:foo}`
    ;; (message "%S" snippet)
    (let ((new (convert-yas-to-tempel snippet t)))
      (ignore-errors
        (when start
          (delete-region start end)))
      ;; (message "%S" new)
      (tempel-insert new)))

  (defun convert-yas-to-tempel (str &optional remove_multi_args)
    "转化yas字符串为tempel模板"
    ;; 第1步先处理${1:xxx}，尤其注意${1:xxx}123这种
    (let ((temp
           (my-replace-regexp-in-string
            (if remove_multi_args
                "${\\([0-9]+\\).*}"
              "${\\([0-9]+\\):.*?}" ;; ?是“non-greedy”
              )
            "(s \\1)" str))
          temp2)
      (mapcar
       #'(lambda (s)
           (if (stringp s)
               (let ((x
                      (my-replace-regexp-in-string
                       "$\\([0-9]+\\)" "(s \\1)" s)))
                 ;; (message "%S" x)
                 (cl-dolist
                  (y x) (setq temp2 (append temp2 (list y)))))
             (setq temp2 (append temp2 (list s)))))
       temp)
      (setq temp2 (append temp2 (list 'q))) ;; 避免多按一次tab才结束(参考eglot-tempel)
      temp2))
  (when nil
    (progn
      (cl-assert
       (equal
        (convert-yas-to-tempel "$2 $1") '("" (s 2) " " (s 1) "")))
      (cl-assert
       (equal
        (convert-yas-to-tempel
         "${123:aaaa}123,${456:aaaa} $1")
        '("" (s 123) "123," (s 456) " " (s 1) "")))
      (cl-assert
       (equal
        (convert-yas-to-tempel
         "${1:class Kty}, ${2:class Ty}")
        '("" (s 1) ", " (s 2) "")))
      (cl-assert
       (equal
        (convert-yas-to-tempel
         "${123:aaaa}123,${456:aaaa} $1" t)
        '("" (s 123) " " (s 1) "")))
      (cl-assert
       (equal
        (convert-yas-to-tempel
         "${1:class Kty}, ${2:class Ty}" t)
        '("" (s 1) "")))))
  (defun my-replace-regexp-in-string
      (regexp rep string &optional fixedcase literal subexp start)
    "将yas里的${1:aa}直接转化为(s 1)，并返回list"
    (let ((l (length string))
          (start (or start 0))
          matches
          str
          mb
          me)
      (save-match-data
        (while (and (< start l) (string-match regexp string start))
          (setq
           mb (match-beginning 0)
           me (match-end 0))
          (when (= me mb)
            (setq me (min l (1+ mb))))
          (match-data--translate (- mb))
          (setq str (substring string mb me))
          (setq matches
                (cons
                 (read ;; 这个把"(s 1)"变为(s 1)，跟prin1-to-string相反
                  (replace-match (if (stringp rep)
                                     rep
                                   (funcall rep (match-string 0 str)))
                                 fixedcase literal str subexp))
                 (cons
                  (substring string start mb) ; unmatched prefix
                  matches)))
          (setq start me))
        ;; Reconstruct a string from the pieces.
        (setq matches (cons (substring string start l) matches)) ; leftover
        ;; (apply #'concat (nreverse matches))
        (nreverse matches)))))
(load "lsp/yasnippet") ;; lsp需要先load yas

(use-package auto-yasnippet
  :commands (aya-create aya-expand)
  :init
  ;; 这个其实还挺好用的，用~xxx代替要替换的，或者`xxx'，多行要选中单行不用选中
  (global-set-key (kbd "C-c y") #'aya-create)
  (global-set-key (kbd "C-c e") #'aya-expand))

;; from tabbar-ruler
(setq EmacsPortable-included-buffers
      '("*scratch*"
        "*rg*"
        "*eww*"
        "*xref*"
        "*org-roam*"
        "*elfeed-entry*"
        "*elfeed-search*"
        "*dashboard*"))
(defun ep-tabbar-buffer-list ()
  (delq
   nil
   (mapcar
    #'(lambda (b)
        (cond
         ;; Always include the current buffer.
         ((eq (current-buffer) b)
          b)
         ((string-match
           "^TAGS\\(<.*>\\)?$" (format "%s" (buffer-name b)))
          nil)
         ;;((string-match "^magit.*:.*" (format "%s" (buffer-name b))) nil)
         ((string-match "^magit-.*:.*" (format "%s" (buffer-name b)))
          nil) ;; 排除magit-process
         ((buffer-file-name b)
          b)
         ((string-match "^\*gud-.*" (format "%s" (buffer-name b)))
          b) ;; gud buffer
         ((string-match "^\*Embark .*" (format "%s" (buffer-name b)))
          b)
         ((string-match "^\*SQLite .*" (format "%s" (buffer-name b)))
          b) ;; *SQLite test.db*
         ((member (buffer-name b) EmacsPortable-included-buffers)
          b)
         ((char-equal ?\  (aref (buffer-name b) 0))
          nil)
         ((char-equal ?* (aref (buffer-name b) 0))
          nil)
         ((char-equal ?: (aref (buffer-name b) 0))
          nil) ;; 排除dired-sidebar buffer
         ((buffer-live-p b)
          b)))
    (buffer-list))))

;; 27.1自带tab-bar-mode和tab-line-mode，试了下tab-line跟tabbar-ruler是一样的效果
(when (functionp 'global-tab-line-mode)
  (use-package tab-line
    :defer 0.5
    :init
    (setq
     tab-line-tabs-function 'ep-tabbar-buffer-list
     tab-line-new-button-show nil)
    (unless (version< emacs-version "29")
      ;; 28.0.50 设置后鼠标不能选择buffer了
      (setq
       tab-line-close-button-show nil
       tab-line-separator
       (if (display-graphic-p)
           ;; unicode符号展示网站 https://www.fuhaoku.net/block/Misc_Symbols
           (propertize (format " %s "
                               (char-to-string
                                (let ((tl
                                       (list
                                        #x2618
                                        #x266B
                                        #x266E
                                        #x266F
                                        #x2665
                                        #x2666
                                        #x26DF
                                        #o22666 ;; old
                                        #x26FA
                                        #x2691
                                        #x267B
                                        #x2615
                                        #x2622
                                        #x262F
                                        #x26F4
                                        #x26AB)))
                                  (nth
                                   (mod (random t) (length tl)) tl))))
                       'face '(foreground-color . "cyan"))
         " | ") ;; 这个比close button好看 
       ))
    :config
    (global-tab-line-mode 1)
    (add-to-list 'tab-line-exclude-modes 'speedbar-mode)
    (add-to-list 'tab-line-exclude-modes 'dired-sidebar-mode)))

;; undo-fu作者写的有保障，不添加post-command-hook，高亮只是屏幕可见区域
(use-package idle-highlight-mode
  :commands (idle-highlight--faces-at-point)
  :defer 1.1
  :init
  (setq
   idle-highlight-idle-time 0.35 ;; 别太小，同一位置它会一直调用高亮显示
   idle-highlight-exclude-point t ;; 可以设置不高亮光标下
   ;;idle-highlight-ignore-modes (list 'minibuffer-mode)
   idle-highlight-exceptions-syntax nil ;; 默认光标在单词末尾是不高亮的，有点不习惯
   idle-highlight-exceptions-face
   '(hi-yellow
     hi-pink hi-green hi-blue hi-salmon hi-aquamarine
     hi-black-b hi-blue-b hi-red-b hi-green-b hi-black-hb) ;; 排除CTRL+F3高亮的
   )
  :config
  ;; 仅在编辑时才不高亮光标下的
  (with-eval-after-load 'view
    (add-hook
     'view-mode-hook
     (lambda ()
       (unless (local-variable-p 'idle-highlight-exclude-point)
         (make-local-variable 'idle-highlight-exclude-point))
       (setq idle-highlight-exclude-point (not buffer-read-only)))))
  (with-eval-after-load 'hi-lock
    ;; 让CTRL+F3立即显示高亮效果
    (defadvice highlight-symbol-at-point
        (before my-highlight-symbol-at-point activate)
      (idle-highlight--unhighlight)))
  ;; 输入时高亮效果不是那么好，这里设置鼠标点击时高亮点击处
  (defadvice idle-highlight--highlight
      (around my-idle-highlight--highlight activate)
    (if (memq
         last-command
         '(mouse-set-point embark-next-symbol
                           embark-previous-symbol
                           up-slightly
                           down-slightly
                           next-line
                           previous-line
                           forward-char
                           backward-char)) ;; this-command是nil
        (let ((idle-highlight-exclude-point nil))
          ad-do-it)
      ad-do-it))
  (with-eval-after-load 'multiple-cursors-core
    (add-hook
     'multiple-cursors-mode-enabled-hook 'idle-highlight--unhighlight)
    (add-hook
     'multiple-cursors-mode-disabled-hook
     (lambda () (idle-highlight--time-callback-or-disable))))
  (global-idle-highlight-mode)
  ;; (set-face-attribute 'idle-highlight nil :inverse-video t) ;; 这个是反色效果，不同的位置颜色可能不同
  (custom-set-faces '(idle-highlight ((t (:inherit isearch)))))

  ;; 让支持region选中高亮
  (defun should-highlight-regin ()
    (and (use-region-p)
         (eq
          (line-number-at-pos (region-beginning))
          (line-number-at-pos (region-end)))))
  (define-advice idle-highlight--word-at-point-args
      (:around (orig-fn &rest args))
    (if (should-highlight-regin)
        (cons
         (buffer-substring-no-properties
          (region-beginning) (region-end))
         (cons (region-beginning) (region-end)))
      (apply orig-fn args)))
  (define-advice idle-highlight--highlight
      (:around (orig-fn &rest args))
    (if (should-highlight-regin)
        (cl-letf (((symbol-function #'concat)
                   (lambda (&rest _)
                     (regexp-quote (ad-get-argument args 0)))))
          (apply orig-fn args))
      (apply orig-fn args)))
  ;; `cl-letf'hook`concat'会有warning提示，直接禁掉提示
  (custom-set-variables
   '(warning-suppress-log-types
     '((emacs) (comp) (comp)))
   '(warning-suppress-types '((comp)))))

(progn
  (defvar last-readonly-state t) ;; 设为t让*scratch*可以正常显示
  (defun my-cursor-chg ()
    (when (not (eq last-readonly-state buffer-read-only))
      (setq last-readonly-state buffer-read-only)
      (let ((cursor-type
             (if last-readonly-state
                 'box
               'bar)))
        (modify-frame-parameters
         (selected-frame) (list (cons 'cursor-type cursor-type))))))
  (add-hook 'view-mode-hook 'my-cursor-chg)
  (defvar disable-cursor-chg nil)
  (defvar cursor-chg-timer nil)
  (defun cursor-chg-function ()
    (when (and
           (not disable-cursor-chg)
           ;; (not (minibufferp)) ;; minibuffer有bug没有变，但C-s回来又变了
           (not (eq (current-buffer) eldoc--doc-buffer)) ;; 排除eldoc
           )
      ;; (when buffer-read-only
      ;;   (message (buffer-name));; 补全时dabbrev会扫描其它buffer导致调用buffer-list-update-hook
      ;;   )
      (my-cursor-chg)))

  (add-hook
   'buffer-list-update-hook
   (lambda ()
     (when cursor-chg-timer
       (cancel-timer cursor-chg-timer))
     (setq cursor-chg-timer
           (run-with-idle-timer 0.1 nil 'cursor-chg-function)))))

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
        ))))

(defun esy/file-capf ()
  "File completion at point function."
  (let ((bs (bounds-of-thing-at-point 'filename)))
    (when bs
      (let* ((start (car bs))
             (end (cdr bs)))
        `(,start
          ,end completion--file-name-table . (:exclusive no))))))

;; corfu的原理是添加completion-at-point-functions，很标准的做法
;; company机制不清楚。eglot+ctags用corfu好配一点
(use-package corfu
  :defer t
  :commands (corfu-mode)
  :init
  (autoload 'corfu-mode "corfu/corfu-main/corfu" "" nil)
  (autoload 'cape-wrap-buster "corfu/cape" "" nil)
  (autoload 'cape-wrap-noninterruptible "corfu/cape" "" nil)
  (setq
   corfu-cycle t
   corfu-auto t
   corfu-auto-prefix 1
   corfu-preview-current nil ; 避免直接上屏，有时候输入完了要马上C-n/C-p，这时只需要按个C-g就可以了，而不需要再删除上屏的
   ;; corfu-auto-delay 0.2      ;; 避免输完后马上C-n/C-p也补全
   ;; corfu-quit-at-boundary nil ;; 可以用M-空格来分词
   corfu-quit-no-match t ;; 没有match时退出，不然有个No match影响操作
   corfu-min-width 30 ;; 不知道怎么回事，有时候显示不全但补全功能正常
   )
  (add-to-list
   'load-path "~/.emacs.d/packages/corfu/corfu-main/extensions")
  (add-hook 'emacs-lisp-mode-hook 'corfu-mode)

  ;; 偷偷隐藏创建corfu的frame加快首次使用
  (run-with-idle-timer
   1.5 nil
   (lambda ()
     (let ((inhibit-message t))
       (load "corfu/corfu-main/corfu"))
     (cl-letf (((symbol-function #'make-frame-visible)
                (lambda (frame))))
       (corfu--popup-show
        (posn-at-point (point))
        0
        8
        '(#("No match" 0 8 (face italic)))))))
  :config
  (when nil
    (setq
     corfu-preselect-first nil
     corfu-quit-no-match 'separator)
    (define-key corfu-map "TAB" 'my-corfu-tab)
    (define-key corfu-map [tab] 'my-corfu-tab)
    (define-key corfu-map "\r" 'my-corfu-ret)

    ;; tab and go方式的补全，优点是不打扰正常输入
    (defun my-corfu-ret ()
      "参考corfu-insert，让RET不需要按两次才换行"
      (interactive)
      (if (>= corfu--index 0)
          (corfu--insert 'finished)
        (corfu-quit)
        ;; 退出corfu，执行非corfu的RET
        (call-interactively (key-binding "\r"))))
    (defun my-corfu-tab ()
      "SPACE直接上屏"
      (interactive)
      (when (> corfu--total 0) ;; No Match
        (when (>= corfu--index 0)
          (corfu--goto 0))
        (corfu--insert 'finished))))

  (global-corfu-mode t)
  (defadvice corfu--update (around my-corfu--update activate)
    (let ((inhibit-message t)) ;; 我们hack的dabbrev运行时会提示错误，实际功能正常
      ad-do-it))

  (defadvice corfu--make-frame (after my-corfu--make-frame activate)
    (let ((frame ad-return-value))
      (when (frame-live-p frame)
        (set-face-background (if (facep 'child-frame-border)
                                 'child-frame-border
                               'internal-border)
                             "#6F6F6F"
                             frame))))

  ;; 历史输入排前！
  (use-package corfu-history
    :init
    ;; session自己就记录了corfu-history了，savehist需要配置
    :config (corfu-history-mode 1))
  (use-package corfu-quick
    :commands (corfu-quick-insert corfu-quick-complete)
    :init
    (setq
     corfu-quick1 "arstne"
     corfu-quick2 "ioh")
    (define-key corfu-map (kbd "C-o") 'corfu-quick-insert) ; corfu-quick-complete 效果是一样的，分不清
    )
  (use-package corfu-info
    :commands (corfu-info-documentation corfu-info-location)
    :init
    ;; 对于elisp-completion-at-point后端，这两个是可以用的，但是提示是Dabbrev的话就无法用了
    (define-key corfu-map (kbd "<f1>") 'corfu-info-documentation)
    (define-key corfu-map "\M-l" 'corfu-info-location))

  (global-set-key (kbd "<C-return>") 'completion-at-point)
  (define-key corfu-map (kbd "M-n") 'corfu-scroll-up)
  (define-key corfu-map (kbd "M-p") 'corfu-scroll-down)
  (define-key corfu-map "\M-h" nil) ;; 屏蔽帮助文档
  (define-key corfu-map (kbd "<tab>") 'corfu-insert) ;; 解决`cape-wrap-buster'可能输入点圆点的bug
  (define-key corfu-map (kbd "TAB") 'corfu-insert)

  ;; 将补全移动到minibuffer进行，这样就可以用embark了！
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold
          completion-cycling)
      (apply #'consult-completion-in-region
             completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  (use-package dabbrev
    :commands (dabbrev--reset-global-variables)
    :init
    ;; from https://eshelyaron.com/esy.html
    ;; 直接用dabbrev-capf有问题，cape的dabbrev也有问题(如它忽略了dabbrev-abbrev-char-regexp导致中文设置不生效，另外补全项好像没有dabbrev-completion多？)
    (defvar has-dabbrev-capf nil)
    (defun my-dabbrev-capf ()
      "Workaround for issue with `dabbrev-capf'."
      (let ((inhibit-message t)
            (disable-cursor-chg t)) ;; 屏蔽dabbrev和corfu的消息
        (dabbrev--reset-global-variables)
        (setq dabbrev-case-fold-search nil)
        (if has-dabbrev-capf
            (ignore-errors
              (dabbrev-capf))
          (cl-letf (((symbol-function #'completion-in-region)
                     (lambda (beg end table &rest args)
                       (list beg end table))))
            (dabbrev-completion)) ;; hack dabbrev-completion to return list
          )))
    (add-to-list 'completion-at-point-functions 'my-dabbrev-capf)
    :config (setq has-dabbrev-capf (functionp 'dabbrev-capf)))

  (add-to-list 'completion-at-point-functions 'esy/file-capf t)

  ;; eglot的capf没有:exclusive标识，所以是独占的，这里补充tag补全
  ;; 参考https://github.com/seagle0128/.emacs.d/blob/8f1a2fc483da8cb430f3bd53e6a5f7ce392c3c4f/lisp/init-ctags.el
  (defun lsp-other-capf-function ()
    (let ((lsp-result (funcall my-lsp-completion-at-point)))
      (if (and lsp-result
               (try-completion
                (buffer-substring
                 (nth 0 lsp-result) (nth 1 lsp-result))
                (nth 2 lsp-result)))
          lsp-result
        ;; TODO: 添加其它后端
        (tags-completion-at-point-function))))

  (defmacro enable-lsp-other-backend (use-eglot)
    "替换lsp的补全"
    `(let ((lsp-capf
            (if (eq ,use-eglot 'eglot)
                'eglot-completion-at-point
              'lsp-completion-at-point)))
       (lambda ()
         (defvar-local my-lsp-completion-at-point lsp-capf)
         (setq-local completion-at-point-functions
                     (cl-nsubst
                      #'lsp-other-capf-function
                      lsp-capf
                      completion-at-point-functions)))))
  ;; 会影响`cape-wrap-buster'，暂时不用tags补全了
  ;; (with-eval-after-load 'eglot
  ;;   (add-hook 'eglot-managed-mode-hook (enable-lsp-other-backend 'eglot)))
  (with-eval-after-load 'lsp-mode
    (add-hook
     'lsp-managed-mode-hook (enable-lsp-other-backend 'lsp-mode))) ;
  )

(use-package google-c-style
  :commands (google-set-c-style))

(use-package cc-mode
  :defer t
  :init
  (defun my-c-mode-hook-set ()
    (c-toggle-hungry-state t)
    (c-toggle-electric-state t)
    (c-toggle-auto-newline t)
    (google-set-c-style)
    (setq c-basic-offset 4) ;; tab4个空格习惯了

    (when nil
      ;; cc mode自己实现貌似要好一些
      (make-local-variable 'my-auto-newline)
      (setq my-auto-newline t)
      (defun semicolon-auto-newline ()
        (interactive)
        (self-insert-command 1 ?\;)
        ;; 只在行尾才auto newline，这样可以排除for(auto xxx;)
        (when (memq (char-after) '(?\C-j nil))
          (call-interactively 'new-line-dwim)))
      (define-key c++-mode-map (kbd ";") 'semicolon-auto-newline)))
  :config (define-key c-mode-base-map (kbd "C-h") 'c-electric-backspace)
  ;; (define-key c-mode-base-map ";" nil)
  )

(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist
      (append
       '(("\\.\\([Nn][Ss][Ii]\\)$" . nsis-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append
       '(("\\.\\([Nn][Ss][Hh]\\)$" . nsis-mode)) auto-mode-alist))

(autoload 'protobuf-mode "protobuf-mode" "protobuf mode" t)
(setq auto-mode-alist
      (append '(("\\.proto\\'" . protobuf-mode)) auto-mode-alist))

;; 这个hook了`push-mark'对内置`marker'支持很好！
(use-package backward-forward
  :defer 0.7
  :custom (backward-forward-mark-ring-max 100)
  :config
  ;; from https://emacs-china.org/t/emacs/19171/17?u=lynnux
  (defun my/backward-forward-previous-location ()
    "A `backward-forward-previous-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge
           (< backward-forward-mark-ring-traversal-position
              (1- (length backward-forward-mark-ring))))
          (recent (point-marker)))
      (backward-forward-previous-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-previous-location))))
  (defun my/backward-forward-next-location ()
    "A `backward-forward-next-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (> backward-forward-mark-ring-traversal-position 0))
          (recent (point-marker)))
      (backward-forward-next-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-next-location))))
  (global-set-key (kbd "M-n") 'my/backward-forward-next-location)
  (global-set-key (kbd "M-p") 'my/backward-forward-previous-location)
  (defvar jump-commands
    '(beginning-of-buffer
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
      avy-goto-word-1
      embark-act
      consult-imenu-multi
      keyboard-escape-quit
      embark-next-symbol
      embark-previous-symbol
      my-pop-select))
  (cl-dolist
   (jc jump-commands)
   (advice-add jc :before #'backward-forward-push-mark-wrapper))
  (advice-add 'push-mark :after #'backward-forward-after-push-mark)
  (define-advice backward-forward-after-push-mark
      (:around (orig-fn &rest args))
    "poe管理的buffer不用记录，如果是help-mode，那么M-n/M-p就不正常了"
    (unless (bound-and-true-p poe-popup-mode)
      (apply orig-fn args))))
;; 其它jump包
;; back-button global跟local是区分开的，这就很麻烦了
;; history可能会

(use-package iss-mode
  :init
  (setq iss-compiler-path "D:/Programme/Inno Setup 5/")
  (setq auto-mode-alist
        (append '(("\\.iss$" . iss-mode)) auto-mode-alist))
  :commands (iss-mode)
  :config
  (define-key iss-mode-map [f6] 'iss-compile)
  (define-key iss-mode-map [(meta f6)] 'iss-run-installer))

(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(defun my-elisp-hook ()
  (make-local-variable 'eldoc-idle-delay)
  (setq eldoc-idle-delay 0.5) ;; 设置为0会导致鼠标点击时不显示
  (turn-on-eldoc-mode)
  (setq-local tab-width 8) ;; 官方源码很多TAB，又是按8
  )
(find-function-setup-keys) ;直接定位函数变量定义位置的快捷键，C-x F/K/V，注意是大写的
(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-elisp-hook)

(use-package expand-region
  :defer t
  :init
  (dec-placeholder-fun
   er--expand-region-1 ;; 实际使用的是这个，被`easy-kill-er'调用
   expand-region
   "~/.emacs.d/packages/expand-region/expand-region.el-master"
   '(expand-region
     python-mode-expansions
     cc-mode-expansions
     text-mode-expansions
     subword-mode-expansions
     the-org-mode-expansions
     python-el-fgallina-expansions)) ;; 到时有错误再添加什么吧，用得也不多
  (global-set-key (kbd "C-S-t") 'er/contract-region))

(use-package smart-region
  :commands (smart-region)
  :init
  ;; C-q不动再按C-q触发expand region，移动到其它行，同一列触发multiple-cursors，不同列是rectangle-mark
  ;; 同列触发mc这个要常用，不过有bug有汉字的话判定为rectangle-mark
  (global-set-key (kbd "C-q") 'smart-region)
  :config
  (when (functionp 'which-key--show-keymap)
    (defadvice smart-region (after my-smart-region activate)
      (when rectangle-mark-mode
        (which-key--show-keymap
         "keymap" ctl-x-r-map nil nil 'no-paging)
        (set-transient-map ctl-x-r-map
                           (lambda () rectangle-mark-mode)
                           'which-key--hide-popup)))))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(modify-coding-system-alist 'file "\\.lua\\'" 'utf-8) ; 不要带BOM，BOM是ms特有的

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files"
  t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; 类似sublime的多光标功能(以M键更像是visual code)
(use-package multiple-cursors
  :load-path "~/.emacs.d/packages/multiple-cursors/multiple-cursors.el-master"
  :init
  ;; mc询问你的命令都保存在这里面了
  (setq
   mc/list-file "~/.emacs.d/packages/multiple-cursors/my-cmds.el"
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
  :commands
  (multiple-cursors-mode
   mc/add-cursor-on-click
   mc/unmark-next-like-this
   mc/unmark-previous-like-this
   mc/mark-previous-like-this
   mc/mark-next-like-this
   mc/edit-lines
   mc/mark-all-dwim
   mc/skip-to-next-like-this
   mc/mark-all-like-this-dwim)
  :config
  (define-key mc/keymap (kbd "C-v") nil)
  (define-key mc/keymap (kbd "RET") 'multiple-cursors-mode) ;; 退出，C-J输入换行
  ;; 改变指针颜色以示区别 
  (defvar last-cursor-color nil)
  (add-hook
   'multiple-cursors-mode-enabled-hook
   (lambda ()
     (unless last-cursor-color
       (setq last-cursor-color (frame-parameter nil 'cursor-color)))
     (set-cursor-color "yellow")))
  (add-hook
   'multiple-cursors-mode-disabled-hook
   (lambda ()
     (when last-cursor-color
       (set-cursor-color last-cursor-color)))))

;; avy可以配得跟ace jump完全一样，就没必要保留ace jump了
(use-package avy
  :commands (avy-goto-char-timer avy-goto-word-1 avy-goto-line avy-resume)
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
    (select-window (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind
       (start . end)
       (bounds-of-thing-at-point 'line)
       (copy-region-as-kill start end)))
    (select-window (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank))
    t)
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
        (select-window (cdr (ring-ref avy-ring 0))))
      t)
    (setf
     (alist-get ?. avy-dispatch-alist) 'avy-action-embark
     (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay ; 自带用的X，x会kill并跳过去
     (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
     ;; (alist-get ?y avy-dispatch-alist) 'avy-action-yank
     (alist-get ?w avy-dispatch-alist) 'avy-action-copy ; 原n是不能用了
     (alist-get ?c avy-dispatch-alist) 'avy-action-copy-whole-line
     (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
     ;; (alist-get ?t avy-dispatch-alist) 'avy-action-teleport ; 目标剪切到当前处
     (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line ;; 目标整行剪切到此处
     )))

;; 删除当前位置到某个位置，要学会常用啊，比选中再删除快多了
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim) ("M-Z" . avy-zap-up-to-char-dwim)))

;;; autohotkey文件编辑
(autoload 'xahk-mode "xahk-mode"
  "Major mode for editing Markdown files"
  t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;;; rust mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(modify-coding-system-alist 'file "\\.rs\\'" 'utf-8-with-signature) ; 带中文必须这个编码，干脆就默认

(use-package sr-speedbar
  :commands (sr-speedbar-toggle)
  :init
  (setq
   sr-speedbar-right-side nil
   sr-speedbar-auto-refresh t
   dframe-update-speed 0.2 ;; 自动刷新时间
   speedbar-show-unknown-files t ;; 我去rust文件都不认识？
   speedbar-obj-do-check nil ;; 默认检查如c文件的obj文件是否更新
   speedbar-vc-do-check nil ;; 好像不支持git吧
   speedbar-directory-unshown-regexp "^\(\.\.*$\)\'" ;; 显示unkown dir
   ;; speedbar-hide-button-brackets-flag t
   speedbar-use-images t ;; 图标太丑了，或许可以用all-the-icon?
   ;; 关键函数 speedbar-make-tag-line  speedbar-image-dump 可以查看所有图标
   )
  ;; (global-set-key (kbd "<C-f1>") 'sr-speedbar-toggle)
  :config
  (unless sr-speedbar-auto-refresh
    ;; 重新打开时更新目录
    (defadvice sr-speedbar-toggle
        (before my-sr-speedbar-toggle activate)
      (unless (sr-speedbar-exist-p)
        (message default-directory)
        (sr-speedbar-refresh)
        t)))
  (define-key speedbar-mode-map (kbd "q") 'sr-speedbar-close)
  (define-key speedbar-mode-map (kbd "l") 'speedbar-up-directory)
  (define-key speedbar-mode-map (kbd "w") 'speedbar-scroll-down)
  (define-key speedbar-mode-map (kbd "SPC") 'speedbar-scroll-up)
  (define-key speedbar-file-key-map (kbd "<") 'beginning-of-buffer)
  (define-key speedbar-file-key-map (kbd ">") 'end-of-buffer)
  (define-key speedbar-file-key-map (kbd "SPC") 'speedbar-scroll-up)
  (define-key
   speedbar-file-key-map (kbd "S-SPC") 'speedbar-scroll-down)
  (define-key
   speedbar-file-key-map (kbd "RET")
   'speedbar-toggle-line-expansion) ;; 原来是直接进入目录，只需要展开就行了
  )


(use-package imenu-list
  :commands (imenu-list-smart-toggle)
  :init
  (setq imenu-list-idle-update-delay 0.5)
  (global-set-key [(control f4)] 'imenu-list-smart-toggle)
  :hook
  (imenu-list-major-mode
   .
   (lambda ()
     (when (display-graphic-p)
       ;; 指示当前是在哪个函数里     
       (face-remap-add-relative 'hl-line '(:background "#666"))))))

;; imenu不能显示cpp类里的函数，而lsp mode的imenu太卡了，想用ctag只对当前文件parse，counsel-etags的实现跟我想的一致，用得很爽！
(use-package counsel-etags
  :commands (imenu-setup-for-cpp)
  :init
  (defun ivy-set-occur (a b))
  (defun ivy-configure (a b c))
  :config
  (defun my-counsel-etags-imenu-default-create-index-function ()
    "修改原版不用临时文件"
    (let* ((ctags-program "ctags")
           (code-file buffer-file-name)
           cmd
           imenu-items
           cands)
      (when (and code-file (file-exists-p code-file))
        (setq cmd
              (cond
               (counsel-etags-command-to-scan-single-code-file
                (concat
                 counsel-etags-command-to-scan-single-code-file
                 "\""
                 code-file
                 "\""))
               (t
                (counsel-etags-get-scan-command
                 ctags-program code-file))))
        (setq cands
              (counsel-etags-imenu-scan-string
               (shell-command-to-string cmd)))
        (save-excursion
          (dolist (c cands)
            (let* ((name (car c)))
              (goto-char (point-min))
              (counsel-etags-forward-line (cdr c))
              (when (search-forward name (point-at-eol) t)
                (forward-char (- (length name))))
              (push (cons name (point-marker)) imenu-items)))))
      imenu-items))
  (defun imenu-setup-for-cpp ()
    (setq-local
     imenu-create-index-function
     'my-counsel-etags-imenu-default-create-index-function)))

(defun chinese-char-p (char)
  (if (string-match "\\cC\\{1\\}" (string char))
      t
    nil))
(defun chinese-word-chinese-string-p (string)
  "Return t if STRING is a Chinese string."
  (cl-find-if 'chinese-char-p (string-to-list string)))

(progn
  (add-to-list 'load-path "~/.emacs.d/packages/minibuffer")
  ;; 主要参考https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el
  (use-package vertico
    :load-path "~/.emacs.d/packages/minibuffer/vertico-main"
    :init
    (setq
     enable-recursive-minibuffers t ; 允许minibuffer里再执行命令
     vertico-cycle t ;; vertico不同于其它的是当前行之前的结果被列在了最后
     vertico-resize nil ;; 让它初始化就固定大小
     vertico-count 20 ;; 高度多少行
     vertico-sort-function nil ;; 对需要排序的，添加到`vertico-multiform-commands'里
     )
    :defer 0.3
    :config (vertico-mode 1)
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

    ;; 另一种设置的方法 https://github.com/minad/consult/wiki#add-category-specific-minibuffer-keybindings
    (define-key
     vertico-map (kbd "C-l") 'vertico-directory-delete-word)
    ;; (define-key vertico-map (kbd "<tab>") 'vertico-next-group)
    ;; (define-key vertico-map (kbd "<backtab>") 'vertico-previous-group)
    (define-key vertico-map (kbd "C-j") 'vertico-exit-input) ; 避免选中项，比如新建文件，但列表有命中项时。默认绑定M-r
    (define-key vertico-map (kbd "M-o") 'vertico-next-group) ;; 下个组,C-o给avy了
    (define-key vertico-map (kbd "M-O") 'vertico-previous-group) ;; 上个组

    ;; 当前行加下划线
    (custom-set-faces
     '(vertico-current
       ((t
         (:inherit
          unspecified
          :underline t
          :background nil
          :distant-foreground nil
          :foreground nil))))
     '(consult-preview-line ((t (:underline t :background nil)))))

    ;; 只会恢复关键词
    ;; consult-line需要配合(setq consult-line-start-from-top nil)，这样首行就是当前位置
    ;; consult-ripgrep暂时没有好方法
    (use-package vertico-repeat
      :load-path "~/.emacs.d/packages/minibuffer/vertico-main/extensions"
      :commands (vertico-repeat vertico-repeat-save)
      :init
      (global-set-key [f6] #'vertico-repeat) ; C-u F6还可以选择
      (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))
    (use-package vertico-directory
      :commands (vertico-directory-delete-word))
    (use-package vertico-quick
      :commands (vertico-quick-exit)
      :init
      (setq
       vertico-quick1 "arstne"
       vertico-quick2 "ioh")
      ;; 类似avy，我一直想在helm中实现的
      (define-key vertico-map "\C-o" #'vertico-quick-exit))
    (use-package vertico-grid
      :commands (vertico-multiform-grid vertico-grid-mode)
      :init (define-key vertico-map "\M-G" #'vertico-multiform-grid))
    (use-package vertico-reverse
      :commands (vertico-multiform-reverse vertico-reverse-mode)
      :init (define-key vertico-map "\M-R" #'vertico-multiform-reverse))
    (use-package vertico-multiform
      :init
      (when nil
        ;; 调试用
        (defadvice vertico-multiform--setup
            (before my-vertico-multiform--setup activate)
          (message
           (concat "last command:" (symbol-name this-command)))))
      (setq
       vertico-multiform-commands
       '(
         ;; (consult-line buffer) ; buffer 可以显示更多搜索出来的内容
         ;; (my-consult-ripgrep buffer)
         ;; (my-project-search buffer)
         ;; (project-find-regexp buffer) ;; project内置搜索，xarg发送files给rg可以搜索被ignore目录里的force add文件
         ;; (dired-do-find-regexp buffer) ;; dired里的A，可以只搜索mark了的文件
         ;; (my-consult-ripgrep-only-current-dir buffer)
         ;; (consult-ripgrep buffer) 
         (execute-extended-command grid
                                   (vertico-sort-function
                                    . vertico-sort-history-alpha)) ; M-x
         (yas-insert-snippet grid)
         (dired-recent-open (vertico-sort-function . nil)) ;; 已经是排好序了的
         (describe-function
          (vertico-sort-function . vertico-sort-history-alpha))
         (describe-variable
          (vertico-sort-function . vertico-sort-history-alpha))
         (my-project-find-file
          (vertico-sort-function . vertico-sort-history-length-alpha)) ;; 按file-name-history排序
         (search-in-browser
          (vertico-sort-function . vertico-sort-history-alpha))
         (consult-bookmark
          (vertico-sort-function . vertico-sort-history-alpha))
         (tempel-insert
          (vertico-sort-function . vertico-sort-history-alpha))))
      ;; (setq vertico-multiform-categories
      ;;       '((file buffer grid)
      ;;         (imenu (:not indexed mouse))
      ;;         (symbol (vertico-sort-function . vertico-sort-alpha))))
      (define-key vertico-map "\M-V" #'vertico-multiform-vertical)
      :config (vertico-multiform-mode))

    (use-package vertico-buffer
      :commands (vertico-buffer-mode)
      :init
      ;; (setq vertico-buffer-display-action '(display-buffer-in-side-window))
      )
    (use-package vertico-mouse
      :config (vertico-mouse-mode)))
  (with-eval-after-load 'vertico
    (use-package orderless
      ;; 启动用无序匹配，本质是一个自动正则生成器，结果给completion-styles用
      :config
      ;; https://github.com/minad/consult/wiki#minads-orderless-configuration
      (defun +orderless--consult-suffix ()
        "Regexp which matches the end of string with Consult tofu support."
        (if (and (boundp 'consult--tofu-char)
                 (boundp 'consult--tofu-range))
            (format "[%c-%c]*$"
                    consult--tofu-char
                    (+ consult--tofu-char consult--tofu-range -1))
          "$"))
      ;; 以这些开头或者结尾都是可以的
      ;; (defcustom orderless-affix-dispatch-alist
      ;;   `((?% . ,#'char-fold-to-regexp) ;; 默认就是支持regex，这里%可以转换欧州那些字母为英文
      ;;     (?! . ,#'orderless-without-literal) ;; !不包含文本
      ;;     (?, . ,#'orderless-initialism) ;; 每个字母都是一个词的开头，如,or匹配oaaa rbbb
      ;;     (?= . ,#'orderless-literal) ;; 纯文本包含(regexp-quote实现)
      ;;     (?~ . ,#'orderless-flex))) ;; fuzzy，不过是有顺序的
      (defun +orderless-consult-dispatch (word _index _total)
        (cond
         ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
         ((string-suffix-p "$" word)
          `(orderless-regexp
            .
            ,(concat
              (substring word 0 -1) (+orderless--consult-suffix))))
         ;; File extensions
         ((and (or minibuffer-completing-file-name
                   (derived-mode-p 'eshell-mode))
               (string-match-p "\\`\\.." word))
          `(orderless-regexp
            .
            ,(concat
              "\\."
              (substring word 1)
              (+orderless--consult-suffix))))))

      ;; +orderless-with-initialism直接进completion-styles-alist了
      (orderless-define-completion-style
       +orderless-with-initialism
       (orderless-matching-styles
        '(orderless-initialism orderless-literal orderless-regexp)))
      ;; 不能直接用orderless-flex，不然consult-everything运行有问题
      (orderless-define-completion-style
       +orderless-flex
       (orderless-matching-styles '(orderless-flex))) ;; 这个还不是最强大的，如果反序需要分词，如xmlopti匹配不了dlg_optimize.xml
      (defun my/orderless-dispatch-flex-first (_pattern index _total)
        "https://github.com/minad/corfu/wiki#advanced-example-configuration-with-orderless"
        (and (eq index 0) 'orderless-flex))
      ;; eglot本身就是flex的，只需要设置elisp就行了，也可以用空格分词(需要设置 corfu-quit-at-boundary)
      (add-hook
       'emacs-lisp-mode-hook
       (lambda ()
         (make-local-variable 'orderless-style-dispatchers)
         (setq orderless-style-dispatchers
               '(orderless-affix-dispatch
                 my/orderless-dispatch-flex-first))))
      (setq
       completion-styles '(orderless basic)
       completion-category-defaults nil
       completion-category-overrides
       '((multi-category (styles orderless basic +orderless-flex)) ;; 用于consult buffer，project buffer等
         (file (styles orderless basic +orderless-flex)) ;; 在`completion-styles'的基础上加上flex就够用了
         ;; (command (styles +orderless-with-initialism)) ; 相当于ivy的^吧？
         ;; (variable (styles +orderless-with-initialism))
         ;; (symbol (styles +orderless-with-initialism))
         )
       orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
       orderless-style-dispatchers
       (list
        #'+orderless-consult-dispatch #'orderless-affix-dispatch))

      ;; https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind
      ;; 让rg也用上上面的特殊符号等，这个很爽！
      (defun consult--orderless-regexp-compiler
          (input type &rest _config)
        (setq input (orderless-pattern-compiler input))
        (cons
         (mapcar
          (lambda (r) (consult--convert-regexp r type)) input)
         (lambda (str) (orderless--highlight input str))))
      (setq consult--regexp-compiler
            #'consult--orderless-regexp-compiler))

    (use-package fzf-native
      :init
      ;; 这个lspmodeel匹配比lsp-mode.el比fussy好，毕竟是fzf算法
      (setq fussy-score-fn 'fussy-fzf-native-score)
      (ignore-errors
        (module-load
         (expand-file-name
          "~/.emacs.d/packages/minibuffer/fzf-native-module.dll"))))
    (use-package fussy
      :init
      (setq fussy-filter-fn 'fussy-filter-default) ;; fussy-filter-default和fussy-filter-orderless据说更快，但fussy-filter-orderless测试不支持packex匹配package_extra啊！
      :config
      (when t
        (setq completion-category-overrides
              (assq-delete-all
               'multi-category completion-category-overrides))
        (setq completion-category-overrides
              (assq-delete-all 'file completion-category-overrides))
        ;; +orderless-flex其实也是可以用，但是它没有打分机制。应该优先部分匹配，再是flex
        ;; 另外这个好像也是支持分词反序匹配，如ext pac匹配package_extra（不加空格的extpac都不支持)
        (add-to-list
         'completion-category-overrides
         '(multi-category (styles orderless basic fussy)))
        (add-to-list
         'completion-category-overrides
         '(file (styles orderless basic fussy)))))

    ;; 美化
    (use-package marginalia
      :commands (marginalia-mode)
      :bind
      (("M-A" . marginalia-cycle)
       :map
       minibuffer-local-map
       ("M-A" . marginalia-cycle))
      :init (marginalia-mode)
      :config)
    (use-package embark
      :load-path "~/.emacs.d/packages/minibuffer/embark-master"
      :commands
      (embark-export
       embark-act
       embark-act-with-completing-read
       embark-next-symbol
       embark-previous-symbol
       embark-toggle-highlight
       embark-open-externally)
      :bind
      (("M-." . embark-act) ;; 按这个后，还可以按其它prefix key如C-x调用C-x开头的键，起了which-key的作用！
       ;; ("M-." . embark-dwim) ;; 除非你知道每个object默认的操作，否则还是embark-act吧
       ("<f1> B" . embark-bindings) ;; 列举当前可用的键及其命令
       )
      :init
      (setq
       embark-mixed-indicator-delay 0 ;; 按钮提示菜单延迟，熟练后可以设置长点
       ;; embark-quit-after-action nil     ;; 默认就退出minibuffer了
       )
      (global-set-key [f3] 'embark-next-symbol)
      (global-set-key [(shift f3)] 'embark-previous-symbol)
      (global-set-key [(control f3)] 'embark-toggle-highlight)
      (with-eval-after-load 'vertico
        (define-key vertico-map (kbd "C-.") 'embark-act)
        (define-key vertico-map (kbd "C-c C-o") 'embark-export)
        (define-key vertico-map (kbd "C-c C-c") 'embark-act)
        (define-key vertico-map [?\H-m] (kbd "C-. SPC")) ; C-m选中mark当前行，之后C-. A(embark-act-all)就可以这已mark的进行操作了！
        )
      (defun find-file-auto (orig-fun &rest args)
        "对一些后辍直接用shell打开"
        (let ((filename (car args)))
          (if (cl-find-if
               (lambda (regexp) (string-match regexp filename))
               '("\\.pdf\\'"
                 "\\.docx?\\'"
                 "\\.xlsx?\\'"
                 "\\.pcapn?g?\\'"))
              (embark-open-externally filename)
            (apply orig-fun args))))
      (advice-add 'find-file :around 'find-file-auto)
      :config
      (setq prefix-help-command #'embark-prefix-help-command) ;; C-h可以输入命令，有时候显示不全或许记不住命令行
      (define-key
       embark-file-map (kbd "C-x C-d")
       (lambda (file)
         (interactive "f")
         (browse-file-in-explorer file)))
      (defun embark-consult-rg (target)
        (if (file-exists-p target)
            (let ((default-directory
                   (if (file-directory-p target)
                       target
                     (file-name-directory target))))
              (call-interactively 'my-consult-ripgrep))
          (message "not exist! %S" target)) ; TODO: 支持buffer bookmark等
        )
      ;; from https://github.com/glen-dai/highlight-global 
      (defun get-thing-to-highlight ()
        "Get thing to highlight. If active region, get reigon, else get
symbol under cursor"
        (if (use-region-p)
            (buffer-substring-no-properties
             (region-beginning) (region-end))
          (if (thing-at-point 'symbol)
              (buffer-substring-no-properties
               (car (bounds-of-thing-at-point 'symbol))
               (cdr (bounds-of-thing-at-point 'symbol))))))
      ;; 使CTRL+F3支持region高亮
      (define-advice embark-toggle-highlight
          (:around (orig-fn &rest args))
        (let ((find-tag-default-function 'get-thing-to-highlight))
          (apply orig-fn args)))
      (define-key embark-general-map [f2] 'embark-consult-rg)))
  ;; 预览功能很快！好像不是真的加载
  (use-package consult
    :load-path "~/.emacs.d/packages/minibuffer/consult-main"
    :commands
    (consult-ripgrep
     consult-buffer ;; buffer+recent+bookmark
     consult-line
     consult-buffer-other-window
     consult-buffer-other-frame
     consult-goto-line ;; 带预览
     consult-completion-in-region ;; 将补全移动到minibuffer进行
     consult-project-buffer ;; buffer+file
     consult-locate ;; everything!
     consult-find ;; minad说fd不太成熟，就用find吧
     consult--read
     consult-bookmark
     consult-recent-file
     consult-fd)
    :init
    (defalias 'files-recent-visited 'consult-recent-file)
    (setq
     consult-line-start-from-top nil ;; nil前面行会排后面，但t初始行是最前面那个
     consult-line-point-placement 'match-beginning ; jump后跳到匹配词的开头
     consult-async-min-input 1 ;; 确保单个汉字也可以搜索
     ;; consult-fontify-max-size 1024 ;; 不设置的话大文件第1次用consult-line会卡几秒，设置consult-fontify-preserve就不需要设置这个了
     consult-fontify-preserve nil ;; 直接禁用fontify，副作用是搜索到的没有color，没什么影响
     consult-async-split-style nil ;; 默认async是'perl会有个#在开头，而consult-eglot过滤的话还要删除那个#按f空格才可以
     consult-locate-args (encode-coding-string "es.exe -n 30 -p -r" 'gbk)
     consult-preview-key nil ;; 默认禁止preview，后面有设置哪些开启
     recentf-filename-handlers nil
     consult-fd-args '("fd" "--full-path --color=never") ;; 不知道是否一直执行executable-find，所以给写死
     bs--intern-show-never "^#xa12$" ;; consult-buffer-sources不再显示隐藏buffer，用C-x b来显示
     bs--show-all t
     consult-buffer-sources
     '(consult--source-buffer
       consult--source-recent-file consult--source-bookmark))
    ;; consult的异步没有通过cmd proxy，这点很棒！
    (add-to-list
     'process-coding-system-alist
     '("[rR][gG]" . (utf-8 . gbk-dos))) ;; rg支持中文
    (add-to-list
     'process-coding-system-alist '("[fF][dD]" . (utf-8 . gbk-dos)))
    (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
    (add-to-list
     'process-coding-system-alist
     '("[fF][iI][nN][dD]" . (utf-8 . gbk-dos))) ;; find支持中文

    ;; https://github.com/phikal/compat.el
    (use-package compat
      :defer t
      :load-path "~/.emacs.d/packages/minibuffer/compat-main"
      :init
      (unless (functionp 'ensure-list)
        (defun ensure-list (object)
          ;; for 28.0.5
          (if (listp object)
              object
            (list object)))))

    (defun my-consult-ripgrep (&optional dir initial)
      (interactive "P") ;; C-u F2可以选择dir，否则就是当前目录
      (when (autoloadp (symbol-function 'consult-ripgrep))
        (require 'consult))
      (setq this-command 'my-consult-ripgrep)
      ;; 不忽略ignore
      (let ((consult-ripgrep-args
             (concat
              consult-ripgrep-args
              " --no-ignore --ignore-file "
              (expand-file-name "rg_ignore.txt" "~/.emacs.d/bin/"))))
        (consult-ripgrep (or dir default-directory) initial)))
    (defun my-consult-ripgrep-only-current-dir (&optional dir)
      (interactive "P") ;; C-u F2可以选择dir，否则就是当前目录
      (when (autoloadp (symbol-function 'consult-ripgrep))
        (require 'consult))
      ;; 不忽略ignore
      (let ((consult-ripgrep-args
             (concat
              consult-ripgrep-args
              " --no-ignore -g!*/ --ignore-file "
              (expand-file-name "rg_ignore.txt" "~/.emacs.d/bin/"))))
        (consult-ripgrep (or dir default-directory))))
    (global-set-key [f2] 'my-consult-ripgrep)
    (global-set-key [S-f2] 'my-consult-ripgrep-only-current-dir)

    ;; 按f/b/m/p加上空格可以只显示相应files/buffer/bookmark/project！(project默认hidden)
    (global-set-key (kbd "C-x C-b") 'consult-buffer)
    (global-set-key (kbd "C-s") 'consult-line) ;; consult-line -> embark-export to occur-mode 按e进行编辑，是实时更新buffer的
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key
     [remap switch-to-buffer-other-window]
     'consult-buffer-other-window)
    (global-set-key
     [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)
    (global-set-key [remap bookmark-jump] 'consult-bookmark)
    ;; 默认有点问题，解决办法来自 https://github.com/minad/consult/issues/317#issuecomment-980797343，
    ;; 处理w32-quote-process-args后，上面consult--regexp-compiler设置为orderless也有效了！
    (defun my-find-file-prj (&optional dir initial)
      (interactive)
      (when (autoloadp (symbol-function 'consult-fd))
        (require 'consult))
      (let ((consult-async-split-style 'semicolon))
        (consult-fd nil (or initial ".*;"))))
    (global-set-key [(control f2)] 'my-find-file-prj)

    ;; 带递归的find-file，直接展示所有文件，并用上了consult的filter过滤无须再启动async进程
    (defun my-find-file (&optional dir initial)
      (interactive)
      (when (autoloadp (symbol-function 'consult-fd))
        (require 'consult))
      (let
          ((consult-async-split-style 'semicolon) ;; perl的话C-l会额外多出一个#，用`semicolon'和`comma'都可以
           ;; 禁用高亮，这样embark-select选中就有效果了。最终调用`add-face-text-property'参数3填nil也没事
           ;; 在`consult--async-indicator'中检测状态恢复高亮
           (orderless-match-faces
            [nil
             orderless-match-face-1
             orderless-match-face-2
             orderless-match-face-3]))
        (consult-fd (or dir default-directory) (or initial ".*;"))))
    (define-advice consult--async-indicator
        (:around (orig-fn &rest args))
      (let ((org-ret (apply orig-fn args)))
        (lambda (action &optional state)
          (let ((result (funcall org-ret action state)))
            (when (and (eq action 'indicator) (eq state 'finished))
              ;; 恢复正常高亮匹配
              (setq orderless-match-faces
                    [orderless-match-face-0
                     orderless-match-face-1
                     orderless-match-face-2
                     orderless-match-face-3]))
            result))))
    (global-set-key [remap find-file] 'my-find-file)

    (defun my-project-imenu ()
      (interactive)
      (cond
       ((bound-and-true-p eglot--managed-mode)
        (call-interactively 'consult-eglot-symbols))
       ((bound-and-true-p lsp-managed-mode)
        (call-interactively 'consult-lsp-symbols)) ;; 支持narrow字符！
       (t
        (call-interactively 'consult-imenu-multi))))
    (global-set-key [(control ?\,)] 'my-project-imenu)

    (use-package consult-imenu
      :commands (consult-imenu consult-imenu-multi consult-imenu--items)
      :init
      ;; 所有project打开的buffer中查找，太爽了！因为函数名/变量等没有多少，所以没有效率问题
      ;; (global-set-key [(control ?\,)] 'consult-imenu-multi)
      (global-set-key (kbd "M-m") 'consult-imenu)
      (keyboard-translate ?\C-m ?\H-m)
      (global-set-key (kbd "C-x H-m") mule-keymap)
      (global-set-key [?\H-m] 'consult-imenu))
    (use-package consult-org
      :commands (consult-org-agenda) ; 这个很卡啊，还是不替换C-c a了(C-c a再按t也比较卡，应该是org mode问题)
      )
    (use-package consult-xref
      :commands (consult-xref)
      :init
      (setq
       xref-show-xrefs-function #'consult-xref
       xref-show-definitions-function #'consult-xref))
    ;; 还有个consult-lsp如果用lsp mode的话
    (use-package consult-eglot
      :commands (consult-eglot-symbols)
      :init
      ;; 好像没办法过滤，只有用vertico的f+空格过滤function，其它见`consult-eglot-narrow'
      )
    (use-package consult-lsp
      :commands (consult-lsp-symbols))
    ;; 正是我需要的，给marginalia添加yas应该不简单，这个能显示按短字符，还能看预览插入效果！
    (use-package consult-yasnippet
      :commands
      (consult-yasnippet
       consult-yasnippet-visit-snippet-file ;; 打开编辑snippet文件
       ))
    ;; 目前仅能用简单的orderless，不支持上面的~=等，将就用吧
    (use-package consult-everything
      :commands (consult-everything)
      :init
      (setq consult-everything-args "es -p -r") ;; -i是区分大小写
      (global-set-key (kbd "C-c f") 'consult-everything)
      (global-set-key (kbd "C-c C-f") 'consult-everything)
      (with-eval-after-load 'elisp-mode
        (define-key emacs-lisp-mode-map (kbd "C-c C-f") nil))
      ;; 它默认用consult--regexp-compiler，跟我们的设置冲突
      (defun consult--with-orderless (&rest args)
        (minibuffer-with-setup-hook
            (lambda ()
              (setq-local consult--regexp-compiler
                          #'consult--default-regexp-compiler))
          ;; vertico--filter-files会过滤一些后辍，如.map .dll等，既然是everything，肯定啥都不能过滤掉
          (let ((completion-ignored-extensions nil))
            (apply args))))
      (advice-add
       #'consult-everything
       :around #'consult--with-orderless)
      :config)
    (use-package embark-consult
      :after (embark)
      :demand t
      :hook (embark-collect-mode . consult-preview-at-point-mode))
    ;; 随时选择路径
    (use-package consult-dir
      :commands (consult-dir)
      :init
      (with-eval-after-load 'vertico
        (define-key vertico-map "\M-D" 'consult-dir))
      ;; (define-key vertico-map (kbd "C-x C-d") 'consult-dir)
      :config
      (defadvice consult-dir--recentf-dirs
          (around my-consult-dir--recentf-dirs activate)
        (unless (boundp 'dired-recent-directories)
          (require 'dired-recent)
          (dired-recent-load-list))
        (setq ad-return-value dired-recent-directories)))
    (defun search-in-browser ()
      "在浏览器里打开搜索当前symbol(region)，主要参考`xahk-lookup-ahk-ref'"
      (interactive)
      (let (myword
            myurl
            backend)
        (setq myword
              (if (region-active-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end))
                (thing-at-point 'symbol)))
        (unless myword
          (setq myword (read-string "请输入要搜索的内容：")))
        (if myword
            (progn
              (setq backend
                    (consult--read
                     '("baidu"
                       "google"
                       "rust winapi"
                       "msdn"
                       "emacs china"
                       "everything"
                       "project search"
                       "cppreference"
                       ;; "project file"
                       )
                     :prompt (format "Search %s on: " myword)
                     ;; :history t ;; 不需要这个，选择会记录在minibuffer-history里
                     :require-match t
                     :sort t ; 这个会方住上次的选择
                     :category 'file ;; 按前面的设置这个是flex匹配
                     ))
              (cond
               ((equal backend "everything")
                (consult-everything myword))
               ((equal backend "project search")
                (my-project-search nil myword))
               ((equal backend "project file")
                (call-interactively 'my-project-find-file))
               (t
                (progn
                  (setq myword
                        (replace-regexp-in-string " " "%20" myword))
                  (setq
                   myurl
                   (cond
                    ((equal backend "google")
                     (concat
                      "https://www.google.com/search?q=" myword))
                    ((equal backend "msdn")
                     (concat
                      "https://www.baidu.com/s?wd="
                      myword
                      "%20site%3Amsdn.microsoft.com"))
                    ((equal backend "rust winapi")
                     (concat
                      "https://docs.rs/winapi/latest/winapi/index.html?search="
                      myword))
                    ((equal backend "emacs china")
                     (concat
                      "https://emacs-china.org/search?q=" myword))
                    ((equal backend "cppreference")
                     (concat
                      "https://duckduckgo.com/?sites=cppreference.com&q="
                      myword))
                    (t
                     (concat "https://www.baidu.com/s?wd=" myword))))
                  (message "%S" myurl)
                  (browse-url myurl))))))))
    (global-set-key (kbd "<f1> <f1>") 'search-in-browser) ;; 原命令 `help-for-help'可以按f1 ?
    :config
    ;; C-l命令
    (defvar C-L-string nil)
    (defmacro run-cmd-parent-dir (cmd)
      `(lambda ()
         (interactive)
         (let ((text
                (substring-no-properties
                 (or (car-safe vertico--input) "")))
               (dir
                (file-name-directory
                 (directory-file-name default-directory))))
           ;; (delete-minibuffer-contents) ;; 参考vertico-directory-up
           ;; (insert text) ;; 只改内容，preview和RET都不正常，还是要重新搜索下
           ;; minad大佬的解决办法跟我的一样 https://github.com/minad/consult/issues/596
           (run-at-time
            0 nil
            (lambda ()
              (let
                  ((this-command ',cmd) ;; 以consult-buffer形式查看
                   (C-L-string text))
                (,cmd dir text))))
           (abort-recursive-edit) ;; 用vertio-exit C-g就不能回到原来位置
           )))

    ;; C-x C-f的C-j为创建文件
    (defun my-find-file-create (&rest args)
      (interactive)
      (let* ((str
              (buffer-substring-no-properties
               (minibuffer-prompt-end) (point-max))))
        ;; .*;aaa取aaa，aaa就取aaa
        (when (string-match ".*;" str)
          (setq str (replace-regexp-in-string ".*;" "" str)))
        ;; (message "%S" str    )
        (delete-minibuffer-contents)
        (insert str)
        (find-file str))
      (vertico-exit-input))
    (defvar my-find-file-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-j" #'my-find-file-create)
        (define-key map "\C-l" (run-cmd-parent-dir my-find-file))
        map))
    (consult-customize my-find-file :keymap my-find-file-map)

    (defvar my-consult-ripgrep-map
      (let ((map (make-sparse-keymap)))
        (define-key
         map "\C-l" (run-cmd-parent-dir my-consult-ripgrep))
        map))
    (consult-customize
     my-consult-ripgrep
     my-consult-ripgrep-only-current-dir
     :keymap my-consult-ripgrep-map)

    ;; 想把running改为...貌似不好做到，换个颜色吧
    (custom-set-faces
     '(consult-async-running ((t (:inherit mode-line-inactive)))))

    (defun consult-delete-default-contents ()
      (remove-hook 'pre-command-hook 'consult-delete-default-contents)
      (cond
       ((and (member this-command '(self-insert-command yank))
             (not
              (and (eq this-command 'self-insert-command)
                   (eq last-command-event 32)))) ;; 输入空格时不删除内容
        (delete-minibuffer-contents))
       (t
        (put-text-property
         (minibuffer-prompt-end) (point-max) 'face 'default))))
    (defmacro initial-string ()
      `(when-let ((string
                   (or (bound-and-true-p C-L-string)
                       (seq-some
                        (lambda (thing) (thing-at-point thing t))
                        '(region symbol)) ;; url sexp
                       "")))
         (add-hook 'pre-command-hook 'consult-delete-default-contents)
         (propertize string 'face 'shadow)))
    (consult-customize
     consult-line
     my-consult-ripgrep
     my-consult-ripgrep-only-current-dir
     my-project-search
     :initial (initial-string))

    ;; 实现consult-line定位当前行 https://github.com/minad/consult/wiki#pre-select-nearest-heading-for-consult-org-heading-and-consult-outline-using-vertico
    (defvar consult--previous-point nil)
    (defun consult--set-previous-point (&rest app)
      (setq consult--previous-point (point))
      (let ((consult-line-start-from-top t))
        (apply app)))
    (advice-add #'consult-line :around #'consult--set-previous-point)
    ;; (advice-add #'my-consult-ripgrep :around #'consult--set-previous-point)
    (advice-add
     #'vertico--update
     :after #'consult-vertico--update-choose)
    (defun consult-vertico--update-choose (&rest _)
      "Pick the nearest candidate rather than the first after updating candidates."
      (when (and consult--previous-point
                 (memq
                  current-minibuffer-command
                  '(consult-line my-consult-ripgrep)))
        (setq
         vertico--index
         (max
          0 ; if none above, choose the first below
          (1-
           (or
            (seq-position
             vertico--candidates
             consult--previous-point
             (lambda
               (cand point-pos) ; counts on candidate list being sorted
               (> (cl-case
                   current-minibuffer-command
                   (consult-line (car (consult--get-location cand)))
                   (my-consult-ripgrep
                    nil (car (consult--get-location cand)))
                   (consult-org-heading
                    (get-text-property 0 'consult--candidate cand)))
                  point-pos)))
            (length vertico--candidates))))))
      (setq consult--previous-point nil))

    (setq consult-ripgrep-args
          (string-replace " --search-zip" "" consult-ripgrep-args)) ;; 支持搜索el.gz，但我不需要这个功能(同时也跟--pre冲突)
    ;; 只对这部分命令开启preview，`consult-preview-key'
    (consult-customize
     consult-line
     my-consult-ripgrep
     my-consult-ripgrep-only-current-dir
     my-project-search
     call-project-search
     consult-xref
     :preview-key 'any)
    (setq consult-ripgrep-args
          (concat
           consult-ripgrep-args
           " --ignore-file "
           (expand-file-name "rg_ignore.txt" "~/.emacs.d/bin/")))
    ;; 含中文字符搜索时添加--pre rgpre
    (define-advice consult--ripgrep-make-builder
        (:around (fn &rest args) fallback)
      (let ((maker (apply fn args)))
        (lambda (input)
          (let ((result (funcall maker input)))
            (when (chinese-word-chinese-string-p input)
              (let ((cmds (car result)))
                ;; 经测试不能包含--search-zip参数，不然搜索不到
                (setq cmds (delete "--search-zip" cmds))
                (setq result
                      (append
                       (list
                        (append
                         (list (car cmds))
                         (list "--pre" "rgpre")
                         (cdr cmds)))
                       (cdr result)))))
            result))))
    (define-key occur-mode-map (kbd "C-c C-p") 'occur-edit-mode)))

(use-package smart-hungry-delete
  :defer 0.8
  :config
  (smart-hungry-delete-add-default-hooks)
  (global-set-key
   [remap delete-forward-char] 'smart-hungry-delete-forward-char)
  (global-set-key
   [remap delete-char] 'smart-hungry-delete-forward-char)
  (global-set-key
   [remap delete-backward-char] 'smart-hungry-delete-backward-char)
  (global-set-key
   [remap backward-delete-char-untabify]
   'smart-hungry-delete-backward-char))

(defun copy-buffer-name (choice &optional use_win_path)
  (let ((new-kill-string)
        (name
         (if (eq major-mode 'dired-mode)
             (dired-get-filename)
           (or (buffer-file-name) ""))))
    (cond
     ((eq choice ?f)
      (setq new-kill-string name))
     ((eq choice ?d)
      (setq new-kill-string (file-name-directory name)))
     ((eq choice ?a)
      (setq new-kill-string (file-name-nondirectory name)))
     (t
      (message "Quit")))
    (when new-kill-string
      (if use_win_path
          (let ((win-path
                 (replace-regexp-in-string
                  "/" "\\\\" new-kill-string)))
            (message "%s copied" win-path)
            (kill-new win-path))
        (message "%s copied" new-kill-string)
        (kill-new new-kill-string)))))
(defun hydra-copybf4/body ()
  (interactive)
  (require 'hydra)
  (funcall
   (defhydra
    hydra-copybf4
    ()
    "
Copy Buffer Name: _f_ull, _d_irectoy, n_a_me ?
"
    ("f" (copy-buffer-name ?f) nil :color blue)
    ("d" (copy-buffer-name ?d) nil :color blue)
    ("a" (copy-buffer-name ?a) nil :color blue)
    ("q" nil "" :color blue))))
(global-set-key (kbd "C-4") 'hydra-copybf4/body)
(defun hydra-copybf3/body ()
  ()
  (interactive)
  (require 'hydra)
  (funcall
   (defhydra
    hydra-copybf3
    ()
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
  :defer t
  :init
  (use-package magit-base
    :defer t
    :config
    (delay-require-libs
     "~/.emacs.d/packages/magit"
     (if (symbol-function 'transient-define-prefix) ;; emacs高版本自带`transient'
         '(with-editor)
       '(with-editor transient))))
  (dec-placeholder-fun
   magit-status
   magit
   "~/.emacs.d/packages/magit/magit-master/lisp"
   '(magit magit-status))

  (modify-coding-system-alist 'file "\\.git/COMMIT_EDITMSG\\'" 'utf-8)
  (setq
   magit-version "3.3.0"
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
     ))
  (with-eval-after-load 'magit-git
    ;; https://github.com/magit/magit/issues/2982#issuecomment-1081204026
    (defun magit-rev-format (format &optional rev args)
      (let ((str
             (magit-git-string
              "log" "-1" "--no-patch" (concat "--format=" format) args
              (if rev
                  (concat rev "^{commit}")
                "HEAD")
              "--")))
        (unless (string-equal str "")
          str))))
  (with-eval-after-load 'magit-mode
    (remove-hook 'pre-command-hook #'magit-pre-command-hook))
  :config
  (define-key magit-status-mode-map "L" 'magit-section-up) ;; diff差异太多，按L返回所属文件
  (define-key magit-status-mode-map (kbd "<C-tab>") nil) ;; 使切换buffer
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))
(defun git-add-file ()
  "Adds (with force) the file from the current buffer to the git repo"
  (interactive)
  (shell-command
   (concat "git add -f " (shell-quote-argument buffer-file-name))))
(defun git-set-proxy ()
  "Adds (with force) the file from the current buffer to the git repo"
  (interactive)
  (shell-command
   (read-string
    "http: "
    "git config --global http.proxy \"socks5://127.0.0.1:10808\""))
  (shell-command
   (read-string
    "https: "
    "git config --global https.proxy \"socks5://127.0.0.1:10808\"")))
(defun git-unset-proxy ()
  "Adds (with force) the file from the current buffer to the git repo"
  (interactive)
  (shell-command "git config --global --unset http.proxy")
  (shell-command "git config --global --unset https.proxy"))

(use-package which-key
  :init
  (setq
   which-key-popup-type
   'side-window ;；用minibuffer当easy-mark时会移动屏幕，需要easy-kill-init-candidate里屏蔽narrow-to-region，并且非set-transient-map一闪而过
   which-key-use-C-h-commands nil ;; 避免C-h时调用成which-key的
   )
  :commands (which-key--show-keymap which-key--hide-popup)
  :config)

;; easy-kill，添加类似vim里yi/a的东西！
(use-package easy-kill
  :commands (easy-kill easy-mark)
  :init
  (autoload 'easy-kill "easy-kill/easy-kill" "" nil)
  (autoload 'easy-mark "easy-kill/easy-kill" "" nil)
  (global-set-key [remap kill-ring-save] 'easy-kill)
  ;; 有选中或者mark时用expand-region，否则用easy-mark
  ;; 替换expand-region
  (global-set-key
   "\C-t"
   (lambda ()
     (interactive)
     (if (region-active-p)
         (call-interactively 'er/expand-region)
       (call-interactively 'easy-mark))))
  :config
  (delay-require-libs
   "~/.emacs.d/packages/easy-kill/easy-kill-extras.el-master"
   (list 'easy-kill-er 'extra-things 'easy-kill-mc 'easy-kill-extras))
  ;; (add-to-list
  ;;  'load-path
  ;;  "~/.emacs.d/packages/easy-kill/easy-kill-extras.el-master")
  ;; (require 'easy-kill-er)
  ;; (require 'extra-things)
  ;; (require 'easy-kill-extras)
  ;; (setq easy-kill-try-things '(my-line)) ; 只复制line
  (setq easy-kill-try-things '(line-with-yank-handler)) ; 只复制line
  (defun easy-kill-on-line-with-yank-handler (n)
    "必须以easy-kill-on-开头，easy-kill-adjust-candidate可以设置位置并正确添加overlay，或者直接传string"
    (let ((beg (easy-kill-get start))
          (end (easy-kill-get end)))
      (save-excursion
        (pcase n
          (`+
           (goto-char end) ;; easy-kill-expand
           (setq end (line-beginning-position 2)))
          (`-
           (goto-char beg) ;; easy-kill-shrink
           (previous-line) (setq beg (line-beginning-position)))
          (1 (setq
              beg (line-beginning-position)
              end (line-beginning-position 2)))))
      (easy-kill-adjust-candidate 'line-with-yank-handler beg end))
    ;; (easy-kill-adjust-candidate 'line-with-yank-handler (line-beginning-position) (line-beginning-position 2))
    )
  (defadvice easy-kill-candidate
      (after my-easy-kill-candidate activate)
    "获取所选文字最关键的函数，这里判断是beg end位置方式，再判断是否是自定义thing，就给返回字符追yank-handler"
    (with-current-buffer (easy-kill-get buffer)
      (pcase (easy-kill-get bounds)
        (`(,_x . ,_x) () ;; 这就是字符串形式
         )
        (`(,beg . ,end) ;; begin end位置模式 
         (let ((string ad-return-value))
           (if (functionp 'whole-line-or-region-kill-ring-save)
               (when (eq
                      (easy-kill-get thing) 'line-with-yank-handler)
                 (progn
                   (call-interactively
                    'whole-line-or-region-kill-ring-save)
                   (setq ad-return-value (nth 0 kill-ring))))
             (if (eq (easy-kill-get thing) 'line-with-yank-handler)
                 (put-text-property
                  0 (length string) 'yank-handler '(yank-line)
                  string)
               ;; 解决非line-with-yank-handler偶尔带上yank-handler的问题
               (remove-text-properties 0 (length string) 'yank-handler
                                       string))
             (setq ad-return-value string)))))))

  (defun easy-kill-on-forward-word (n)
    (let ((beg (easy-kill-get start))
          (end (easy-kill-get end)))
      (save-excursion
        (goto-char end)
        (forward-word)
        (setq end (point)))
      (easy-kill-adjust-candidate 'line-with-yank-handler beg end)))
  (defun easy-kill-on-backward-word (n)
    (let ((beg (easy-kill-get start))
          (end (easy-kill-get end)))
      (save-excursion
        (goto-char beg)
        (backward-word)
        (setq beg (point)))
      (easy-kill-adjust-candidate 'line-with-yank-handler beg end)))

  ;; 当光标在屏幕下一半，minibuffer显示有换行的拷贝内容，会导致C-l效果，需要去掉换行
  ;; 测试带汉字也会。。所以屏蔽echo
  (defun easy-kill-echo-around (orig-fun format-string &rest args))
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
  (assq-delete-all ?b easy-kill-alist) ;; 删除内置的，否则which-key提示不正确
  ;;(add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?b backward-word ""))
  ;; (add-to-list 'easy-kill-alist '(?a buffer-file-name ""))
  (add-to-list 'easy-kill-alist '(?a backward-line-edge ""))
  (assq-delete-all ?e easy-kill-alist) ;; 删除内置的，否则which-key提示不正确
  (add-to-list 'easy-kill-alist '(?e forward-line-edge ""))
  (assq-delete-all ?f easy-kill-alist) ;; 删除内置的，否则which-key提示不正确
  (add-to-list 'easy-kill-alist '(?f forward-word ""))
  ;;(add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?W WORD " ") t)
  (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
  (assq-delete-all ?s easy-kill-alist) ;; 删除内置的，否则which-key提示不正确
  (add-to-list 'easy-kill-alist '(?s quoted-string "") t) ;; 选择string，支持上面\'\"\`三种
  ;;(add-to-list 'easy-kill-alist '(?s symbol "")) ;; 初始就是symbol了
  (add-to-list 'easy-kill-alist '(?Q quoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\( parentheses-pair-content "\n")
               t)
  (add-to-list 'easy-kill-alist '(?\) parentheses-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\[ brackets-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\] brackets-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?{ curlies-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?} curlies-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?< angles-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?> angles-pair "\n") t)

  (when (functionp 'which-key--show-keymap)
    ;; 简化which-key提示
    (with-eval-after-load 'which-key
      ;; 去掉easy-kill-前辍
      ;; (push '((nil . "easy-kill-digit-argument") . (nil . "")) which-key-replacement-alist)
      (push '((nil . "easy-kill-") . (nil . ""))
            which-key-replacement-alist)

      ;; 额外居然都显示easy-kill-thing，这里替换它们的显示
      ;; easy-kill-help 可以显示所有功能
      ;; (push '(("s" . "easy-kill-thing") . (nil . "symbol")) which-key-replacement-alist)
      (cl-dolist
       (one easy-kill-alist)
       (push `((,(regexp-quote (char-to-string (car one)))
                .
                "easy-kill-thing")
               . (nil . ,(symbol-name (nth 1 one))))
             which-key-replacement-alist)))
    (defvar my-easy-kill-map nil)
    (unless my-easy-kill-map
      (let ((easy-kill-base-map easy-kill-base-map))
        ;; remove number keys
        (cl-loop
         for i from 0 to 9 do
         (define-key easy-kill-base-map (number-to-string i) nil))
        (setq my-easy-kill-map (easy-kill-map))))
    (if t ;; 有两种显示方式1.按?显示 2.一直显示。2会影响性能不用
        (define-key
         easy-kill-base-map "?"
         (lambda ()
           (interactive)
           (which-key--show-keymap
            "keymap" my-easy-kill-map nil nil 'no-paging)
           (set-transient-map my-easy-kill-map
                              nil
                              'which-key--hide-popup)))
      (progn
        (defadvice easy-kill-activate-keymap
            (before my-easy-kill-activate-keymap activate)
          (unless my-easy-kill-map
            (let ((easy-kill-base-map easy-kill-base-map))
              ;; remove number keys
              (cl-loop
               for i from 0 to 9 do
               (define-key
                easy-kill-base-map (number-to-string i) nil))
              (setq my-easy-kill-map (easy-kill-map))))
          ;; 有poe窗口时不弹出
          (if (and (functionp 'poe-toggle-latest)
                   (boundp 'poe-open-popup-alist))
              (unless poe-open-popup-alist
                (which-key--show-keymap
                 "keymap" my-easy-kill-map nil nil 'no-paging))
            (which-key--show-keymap
             "keymap" my-easy-kill-map nil nil 'no-paging)))
        (defadvice set-transient-map
            (before my-set-transient-map activate)
          (let ((map (ad-get-arg 0)))
            ;; 判断是否是easy-kill的keymap
            (when (eq (lookup-key map "?") 'easy-kill-help)
              (ad-set-arg 2 'which-key--hide-popup) ;; easy-kill按n时overlay还是消除不掉，暂时不管了
              )))))))

(use-package whole-line-or-region
  :config
  (define-key
   whole-line-or-region-local-mode-map [remap kill-ring-save]
   nil) ;; 这个仍然用`easy-kill'
  (define-key
   whole-line-or-region-local-mode-map
   [remap copy-region-as-kill]
   nil)
  (define-key
   whole-line-or-region-local-mode-map [remap delete-region] nil)
  (define-key
   whole-line-or-region-local-mode-map [remap comment-dwim] nil)
  (define-key
   whole-line-or-region-local-mode-map [remap comment-region] nil)
  (define-key
   whole-line-or-region-local-mode-map [remap uncomment-region] nil)
  (whole-line-or-region-global-mode))

;; project内置查找会用到，支持ripgrep了！
(use-package xref
  :defer t
  :init
  (setq
   xref-search-program 'ripgrep
   xref-auto-jump-to-first-definition 'show ;; 自动跳转到第一个
   xref-auto-jump-to-first-xref 'show)
  (global-set-key (kbd "C-.") 'xref-find-definitions)
  (global-set-key (kbd "<C-down-mouse-1>") 'xref-find-definitions)
  :config
  (define-key xref--xref-buffer-mode-map (kbd "C-n") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "C-p") 'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "M-n") 'xref-next-group)
  (define-key xref--xref-buffer-mode-map (kbd "M-p") 'xref-prev-group)
  (define-key
   xref--xref-buffer-mode-map
   [remap keyboard-quit]
   'xref-quit-and-pop-marker-stack)
  (define-key
   xref--xref-buffer-mode-map "q" 'xref-quit-and-pop-marker-stack)
  (define-key xref--xref-buffer-mode-map "w" 'scroll-down-command)

  (defadvice xref-matches-in-files
      (around my-xref-matches-in-files activate)
    (let ((cmdproxy-old-encoding
           (cdr
            (assoc
             "[cC][mM][dD][pP][rR][oO][xX][yY]"
             process-coding-system-alist))))
      ;; 注意project search是xargs实现的，会向rg先发送文件列表，因此对输入编码有要求
      (modify-coding-system-alist
       'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . utf-8))
      ;; 检查是否含中文
      (if (chinese-word-chinese-string-p (ad-get-arg 0))
          (let ((xref-search-program-alist
                 (list
                  (cons
                   'ripgrep
                   (concat
                    (cdr (assoc 'ripgrep xref-search-program-alist))
                    " --pre rgpre")))))
            ad-do-it)
        ad-do-it)
      (modify-coding-system-alist
       'process
       "[cC][mM][dD][pP][rR][oO][xX][yY]"
       cmdproxy-old-encoding)))

  ;; 修复xref只调用一个backend的问题
  (defun my/xref-find-backends ()
    (let (backends)
      ;; 好像没有办法获取全局变量，但run hook是可以遍历local和全局的
      (run-hook-wrapped
       'xref-backend-functions
       (lambda (f)
         (when (functionp f)
           (setq backend (funcall f))
           (when backend
             (cl-pushnew backend backends)))
         nil))
      (reverse backends)))
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
            (ignore-errors
              (goto-char orig-position)))
          (let (xrefs)
            (cl-dolist
             (backend backends)
             (ignore-errors
               (message
                (format "Xref using backend: %s to find definition."
                        (symbol-name backend)))
               (setq xrefs (funcall method backend arg))
               (if xrefs
                   (cl-return)
                 ;; (message (format "Xref: failed to find definition using backend: %s." (symbol-name backend))) ;; 有错误的话是没有提示的
                 )))
            (unless xrefs
              (xref--not-found-error kind input))
            xrefs)))))
  (advice-add
   #'xref--create-fetcher
   :override #'zjy/xref--create-fetcher))

;; 利用imenu信息实现xref，对cpp比较好，因为用的counsel-etags是根据单个文件生成的。对elisp貌似也有不错的效果
(cl-defmethod xref-backend-identifier-at-point
    ((_backend (eql 'imenu-xref)))
  (find-tag--default))
(cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql 'imenu-xref)))
  ;; 参考的https://github.com/zbelial/lspce/blob/master/lspce.el，这个可以实现空白处列举所有符号，暂时不需要这个功能
  (list
   (propertize (or (thing-at-point 'symbol) "")
               'identifier-at-point
               t)))
(cl-defmethod xref-backend-definitions
    ((_backend (eql 'imenu-xref)) symbol)
  (let (xrefs
        column
        line)
    ;; 直接用consult-imenu，因为它会缓存imenu
    (cl-dolist
     (v (consult-imenu--items))
     ;; 一些前面会加上Type，Variable什么的来分类，只需要取最后一个对比就可以了
     (when (equal symbol (car-safe (last (split-string (car v)))))
       ;; 从point-marker得到column和line
       (save-excursion
         (goto-char (cdr v))
         (setq line (line-number-at-pos))
         (setq column (current-column))
         (when (equal column 0)
           ;; 某些imenu只有行信息，而column为0，这里参考counsel-etags搜索下symbol定位
           (when (search-forward symbol (line-end-position) t)
             (forward-char (- (length symbol)))
             (setq column (current-column)))))
       (cl-pushnew
        (xref-make
         (car v)
         (xref-make-file-location (buffer-file-name) line column))
        xrefs)))
    xrefs))
(defun imenu-xref-backend ()
  'imenu-xref)
(add-hook 'xref-backend-functions 'imenu-xref-backend)

;; xref最后的选择，直接调用consult-ripgrep搜索
(cl-defmethod xref-backend-identifier-at-point
    ((_backend (eql 'xref-to-consult-rg)))
  (find-tag--default))
(cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql 'xref-to-consult-rg)))
  ;; 参考的https://github.com/zbelial/lspce/blob/master/lspce.el，这个可以实现空白处列举所有符号，暂时不需要这个功能
  (list
   (propertize (or (thing-at-point 'symbol) "")
               'identifier-at-point
               t)))
(cl-defmethod xref-backend-definitions
    ((_backend (eql 'xref-to-consult-rg)) symbol)
  (run-at-time
   0 nil
   (lambda ()
     (let
         ((this-command 'my-project-search)) ;; 以consult-buffer形式查看
       (my-project-search))))
  nil)
(defun xref-to-consult-rg-backend ()
  'xref-to-consult-rg)
(add-hook 'xref-backend-functions 'xref-to-consult-rg-backend 101) ;; 排到最后，实际上这直接破坏了xref流程


;; TODO: ctags生成好像还含有外部引用？另外--exclude需要自己加上
;; 测试问题：xref空白处会卡死，补全时也会卡死emacs(尤其是el文件写注释的时候，会创建process并提示失败)
;; 所以目前仅用它来创建TAGS文件
(use-package citre-ctags
  :defer t
  :init
  (dec-placeholder-fun
   citre-create-tags-file
   citre-ctags
   "~/.emacs.d/packages/citre/citre-master"
   '(citre-ctags))
  (dec-placeholder-fun
   citre-update-this-tags-file
   citre-ctags
   "~/.emacs.d/packages/citre/citre-master"
   '(citre-ctags))
  (setq
   citre-default-create-tags-file-location 'project-cache
   citre-use-project-root-when-creating-tags t
   citre-tags-file-per-project-cache-dir "" ;; 强制tag放到根目录，需配合后面设置
   ;; citre-prompt-language-for-ctags-command t 设置这个就没有确认对话框了 
   citre-edit-cmd-buf-default-cmd
   "ctags
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
")
  (use-package citre-util
    :defer t
    :init
    (dec-placeholder-fun
     citre-tags-file-path
     citre-util
     "~/.emacs.d/packages/citre/citre-master"
     '(citre-util)))
  (use-package etags
    :defer t
    :commands
    (find-tag--default ;; 修复eglot的xref at-point
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
      (cl-defmethod xref-backend-identifier-at-point
          ((_backend (eql eglot)))
        (find-tag--default))
      ;; 在空白处运行M-. eglot提示没实现，那就直接换成etags的了。此功能用consult-eglot也可以(C-,)
      (cl-defmethod xref-backend-identifier-completion-table
          ((_backend (eql eglot)))
        (tags-lazy-completion-table)))
    ;; 避免每次都提示查找TAG文件
    (defconst ask-when-to-tag nil) ;; xref-find-definitions advise没成功。
    (defun check_tags ()
      ;; 参考`citre-update-this-tags-file'
      (if-let* ((tagsfile (citre-tags-file-path)))
        (visit-tags-table tagsfile t)
        (when
            (and
             ask-when-to-tag
             (y-or-n-p
              "Can't find tags file for this buffer.  Create one? "))
          (citre-create-tags-file)
          (setq tagsfile (citre-tags-file-path))
          (when tagsfile
            (visit-tags-table tagsfile t)))))
    (add-hook 'prog-mode-hook 'check_tags)
    (setq
     tags-add-tables nil ;; 打开其它工程时保留tag提示，不保留，否则会混起
     tags-revert-without-query t ;; 当TAGS更新后不提示是否revert TAGS buffer
     )
    (defun tag-find-definition (identifier)
      "有时如duilib的头文件里eglot就不行，用tag还行"
      (interactive (list
                    (xref--read-identifier "Find definitions of: ")))
      (let ((xref-backend-functions '(etags--xref-backend)))
        (xref--find-definitions identifier nil)))
    :config
    ;; 跟citre生成的TAGS兼容(头两行带有更新TAGS的命令)
    (defadvice etags-verify-tags-table
        (around my-etags-verify-tags-table activate)
      ;; 原来的判断是开头0xC字符
      (setq ad-return-value t))
    (defadvice visit-tags-table-buffer
        (around my-visit-tags-table activate)
      "屏蔽xref提示选择TAGS"
      (cl-letf
          (((symbol-function #'read-file-name)
            (lambda (prompt
                     &optional dir default-filename &rest others)
              ;; (message "dir:%S, default-filename:%S" dir default-filename)
              ;; 直播返回，相当于按RET
              default-filename)))
        ad-do-it)))
  :config
  ;; 强制tag名
  (defadvice citre--path-to-cache-tags-file-name
      (around my-citre--path-to-cache-tags-file-name activate)
    (setq ad-return-value "TAGS"))
  (define-key
   citre-edit-cmd-buf-map
   (kbd "C-c C-l")
   'citre-edit-cmd-buf-add-lang))

(defun my-project-search (&optional dir initial)
  (interactive)
  (setq this-command 'my-project-search) ;; 使C-; s的preview生效
  (consult-ripgrep dir initial))

(defun my-project-find-file ()
  (interactive)
  (call-interactively 'project-find-file))
(defun my-project-buffer ()
  (interactive)
  (if (functionp 'consult-project-buffer)
      (call-interactively 'consult-project-buffer)
    (call-interactively 'project-switch-to-buffer)))

;; 没有cache查找文件也超快！
(use-package project
  :defer t
  :commands (project-compile project-find-regexp project-root)
  :init
  (defun invoke_project ()
    (interactive)
    (when (functionp' which-key--show-keymap)
      (which-key--show-keymap
       "keymap" project-prefix-map nil nil 'no-paging))
    (set-transient-map project-prefix-map nil 'which-key--hide-popup))
  (global-set-key (kbd "C-;") 'invoke_project)

  ;; eglot+ctags补全是不错的方案(clangd flymake很多错误时补全失灵)
  (defun my/generate-tags ()
    (interactive)
    ;; projectile那个tags太简单了，不能指定语言很慢
    (when (functionp 'citre-update-this-tags-file)
      (call-interactively 'citre-update-this-tags-file)))
  (defun my-project-magit ()
    (interactive)
    (magit-status (project-root (project-current t))))
  ;; p切换project时显示的命令
  (setq
   project-switch-commands
   `((?f "File" my-project-find-file)
     (?s "Search" my-project-search)
     (?d "Dired" project-dired)
     (?m "Magit" my-project-magit)
     (?b "Buffer" my-project-buffer) ; 这个当recent buffer使用了
     (?v "VC-Dir" project-vc-dir)
     (?e "Eshell" project-eshell))
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
    (let ((override (locate-dominating-file dir ".project")))
      (if override
          (if (version< emacs-version "29")
              (cons 'vc override)
            (list 'vc 'nil override))
        nil)))
  (add-to-list 'project-find-functions 'my-project-find-functions)
  (when (functionp 'which-key--show-keymap)
    (defadvice project--switch-project-command
        (around my-project--switch-project-command activate)
      (which-key--show-keymap
       "keymap" project-prefix-map nil nil 'no-paging)
      ad-do-it
      (which-key--hide-popup)))

  ;; 好像自动识别find的输出了(git里的find)
  ;; (defadvice project--files-in-directory (around my-project--files-in-directory activate)
  ;;   (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
  ;;     (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
  ;;     ad-do-it
  ;;     (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)))
  )

(use-package shell
  :defer t
  :init (setq confirm-kill-processes nil)
  :config
  ;; No confirm kill process: for *shell* and *compilation*
  ;; https://emacs.stackexchange.com/q/24330
  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (add-hook 'shell-mode-hook 'set-no-process-query-on-exit)
  ;; Kill the buffer when the shell process exits
  ;; https://emacs.stackexchange.com/a/48307
  (defun kill-buffer-on-shell-logout ()
    (let* ((proc (get-buffer-process (current-buffer)))
           (sentinel (process-sentinel proc)))
      (set-process-sentinel
       proc
       `(lambda (process signal)
          ;; Call the original process sentinel first.
          (funcall #',sentinel process signal)
          ;; Kill the buffer on an exit signal.
          (and (memq (process-status process) '(exit signal))
               (buffer-live-p (process-buffer process))
               (kill-buffer (process-buffer process)))))))
  (add-hook 'shell-mode-hook 'kill-buffer-on-shell-logout))

(use-package compile
  :defer t
  :init
  (global-set-key
   [f7]
   (lambda ()
     (interactive)
     (when current-prefix-arg ;; C-u F7同S-F7
       (setq compilation-read-command t))
     (progn
       (call-interactively 'project-compile)
       (setq compilation-read-command nil) ;; 不再提示
       )))
  (global-set-key
   [(shift f7)]
   (lambda ()
     (interactive)
     (progn
       (setq compilation-read-command t)
       (call-interactively 'project-compile)
       (setq compilation-read-command nil) ;; 不再提示
       )))
  :config
  (when compile-history
    ;; 自动使用最近的历史记录
    (setq compile-command (car compile-history))
    (setq compilation-read-command nil) ;; 大部分时间都在同一个项目，无须再按RET确认了
    ))

;; rg，这个还挺好用的，带修改搜索的功能(需要buffer可写)，更多功能看菜单
(use-package rg
  :defer t
  :init
  (defun my/rg-dwim ()
    (interactive)
    ;; type为all，不然h就会当成c从而忽略了cpp文件。要指定类型可以在rg buffer按f修改
    (unless (functionp 'rg-dwim-project-dir-type-all)
      (delay-require-libs
       "~/.emacs.d/packages/tools/rg.el-master" '(rg))
      (rg-define-search
       rg-dwim-project-dir-type-all
       :query point
       :format literal
       :files "everything"
       :dir project))
    (call-interactively 'rg-dwim-project-dir-type-all))

  (setq rg-ignore-case 'force) ;; 不知道为什么regex搜索的时候会区分大小，只能强制了
  (global-set-key (kbd "C-S-f") 'my/rg-dwim)
  :config
  (define-key rg-mode-map "?" 'rg-menu)
  (define-key rg-mode-map "w" 'scroll-down-command)
  (define-key rg-mode-map (kbd "C-o") 'avy-goto-word-1)
  (use-package rg-result
    :defer t
    :config
    ;; 解决rg输出为utf-8，导致emacs不能正常处理中文路径的问题
    ;; 临时设置cmdproxy编码
    (defadvice rg-run (around my-run activate)
      (let ((cmdproxy-old-encoding
             (cdr
              (assoc
               "[cC][mM][dD][pP][rR][oO][xX][yY]"
               process-coding-system-alist))))
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         '(utf-8 . gbk-dos))
        ad-do-it
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         cmdproxy-old-encoding)))
    (defadvice rg-recompile (around my-rg-recompile activate)
      (let ((cmdproxy-old-encoding
             (cdr
              (assoc
               "[cC][mM][dD][pP][rR][oO][xX][yY]"
               process-coding-system-alist))))
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         '(utf-8 . gbk-dos))
        ad-do-it
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         cmdproxy-old-encoding)))
    ;; 中文
    (defadvice rg-build-command (around my-rg-build-command activate)
      (if (chinese-word-chinese-string-p (ad-get-arg 0))
          (let ((rg-command-line-flags (list "--pre rgpre")))
            ad-do-it)
        ad-do-it))))

;; consult-grep -> embark-export to grep-mode
;; grep mode里C-c C-p开启编辑，C-c C-c完成，C-c C-k放弃编辑
(use-package wgrep
  :after (grep)
  :init
  (setq wgrep-auto-save-buffer t) ;; 编辑好自动保存
  :config
  ;; 使wrep可编辑
  (defadvice wgrep-commit-file (around my-wgrep-commit-file activate)
    (setq tmp-disable-view-mode t)
    ad-do-it
    (setq tmp-disable-view-mode nil)))

(use-package flymake
  :defer t
  :config
  ;; 当lsp开启时，去掉自己的hook
  (add-hook
   'flymake-mode-hook
   (lambda ()
     (when (and flymake-mode
                (or (bound-and-true-p eglot--managed-mode)
                    (bound-and-true-p lsp-managed-mode)))
       (remove-hook
        'after-change-functions 'flymake-after-change-function
        t)
       (remove-hook 'after-save-hook 'flymake-after-save-hook t)
       (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
       (remove-hook
        'eldoc-documentation-functions 'flymake-eldoc-function
        t))))
  ;; fixed for eglot with emacs 28.0.50，就kill-buffer时有提示，实际好像没什么问题
  (unless (boundp 'flymake-list-only-diagnostics)
    (defvar flymake-list-only-diagnostics nil)))

(use-package flycheck
  :commands (flycheck-mode)
  :init
  (autoload 'flycheck-mode "lsp/flycheck")
  (with-eval-after-load 'lsp-diagnostics
    (load "lsp/flycheck"))
  :config
  ;; 当lsp开启时，去掉自己的hook
  (add-hook
   'flycheck-mode-hook
   (lambda ()
     (when (and flycheck-mode
                (or (bound-and-true-p eglot--managed-mode)
                    (bound-and-true-p lsp-managed-mode)))
       (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
         (remove-hook hook fn 'local))))))

;; sqlite3编辑时eglot卡得不行，lsp mode一点事也没有，真是出乎意料！
;; lsp-mode缺点：偶尔补全delete-region报错，导致补全失效(进入yas之前就报错)
;; eglot缺点。另外flymake不是idle时更新的。
(defconst lsp-use-which 'eglot)
(cond
 ((eq lsp-use-which 'lsp-mode)
  ;; 额外需要ht和spinner
  (use-package lsp-mode
    :init
    (add-to-list 'load-path "~/.emacs.d/packages/lsp/lsp-mode-master")
    (add-to-list
     'load-path "~/.emacs.d/packages/lsp/lsp-mode-master/clients")
    (defun lsp-ensure ()
      (lsp-deferred))
    ;; lens和modeline没效果？好像要配合lsp ui用
    (setq
     lsp-lens-enable nil
     lsp-modeline-code-actions-enable nil
     lsp-modeline-diagnostics-enable nil
     lsp-modeline-workspace-status-enable nil
     lsp-headerline-breadcrumb-enable nil ;; 遮挡tabbar了
     lsp-enable-symbol-highlighting nil ;; 高亮光标下的词，除了能限定作用域没什么大用
     lsp-enable-folding nil ;; 不需要折叠
     lsp-semantic-tokens-enable nil
     lsp-enable-links nil
     lsp-enable-text-document-color nil
     lsp-enable-snippet t
     lsp-completion-provider
     :none ;; 不用company就要设置这个
     lsp-enable-on-type-formatting nil ;; 输入后format，完全不需要这个功能，太影响体验了
     lsp-enable-indentation nil ;; 我设置过粘贴后indent，但lsp mode也advice indent-region-function这个函数了，它format反而不正常
     lsp-enable-suggest-server-download nil ;;不需要下载server
     lsp-restart 'ignore ;; 避免project-kill时提示是否重启
     lsp-enable-imenu nil ;; buffer初次使用时太卡了，用cc mode自带的分析就足够了
     lsp-display-inline-image nil ;; 好像会开启markdown?
     )

    (defun my/lsp-mode-setup-completion ()
      "必须设置这个，不然会让人以为补全有问题"
      (setf (alist-get
             'styles
             (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
    :commands (lsp lsp-deferred lsp-completion-at-point)
    :config
    ;; 使重命名可用
    (defadvice lsp--apply-workspace-edit
        (around my-lsp--apply-workspace-edit activate)
      (setq tmp-disable-view-mode t)
      ad-do-it
      (setq tmp-disable-view-mode nil))
    (setq lsp-diagnostics-provider :flycheck) ;; 实测flymake是server返回就马上刷新可能会造成输入卡，而flycheck是idle（查看代码知道)时刷新
    (require 'lsp-diagnostics) ;; flymake
    (define-key
     lsp-mode-map [(meta f8)]
     (lambda ()
       (interactive)
       (if (use-region-p)
           (call-interactively 'lsp-format-region)
         (call-interactively 'lsp-format-buffer))))
    (defun lsp--workspace-print (workspace)
      "替换成显示root目录名"
      (let* ((status (lsp--workspace-status workspace))
             (server-id
              (->
               workspace
               lsp--workspace-client
               lsp--client-server-id
               symbol-name))
             (root (file-name-base (lsp--workspace-root workspace))))
        (if (eq 'initialized status)
            (format "%s:%s" server-id root)
          (format "%s:%s/%s" server-id root status)))))

  ;; dap-mode 依赖treemacs,bui,lsp-treemacs,posframe,lsp-docker,yaml
  (use-package dap-mode
    :disabled
    :load-path "~/.emacs.d/packages/lsp/dap-mode/dap-mode-master"
    :commands (dap-mode dap-auto-configure-mode)
    :init
    (add-to-list 'load-path "~/.emacs.d/packages/lsp/dap-mode")
    (setq
     dap-cpptools-debug-program
     (expand-file-name
      ".extension/vscode/cpptools/extension/debugAdapters/vsdbg/bin/vsdbg.exe"
      user-emacs-directory)
     lsp-enable-dap-auto-configure nil ;; 禁止自动配置dap-mode
     )
    (defmacro forward-to-dap (key)
      "调试键启动dap，并调用原功能"
      `(lambda ()
         (interactive)
         (unless dap-auto-configure-mode
           (dap-auto-configure-mode +1))
         (call-interactively (key-binding (kbd ,key)))))
    (global-set-key (kbd "<f5>") (forward-to-dap "f5"))
    (global-set-key (kbd "<f9>") (forward-to-dap "f9"))
    (global-set-key (kbd "<f10>") (forward-to-dap "f10"))
    (global-set-key (kbd "<f11>") (forward-to-dap "f11"))
    (global-set-key (kbd "<f12>") (forward-to-dap "f12"))
    (require 'dap-autoloads)
    ;; controls目前有bug
    (setq dap-auto-configure-features
          '(sessions locals breakpoints expressions tooltip))
    :config
    (use-package bui
      :init
      ;; 避免多加个load-path
      (load "lsp/dap-mode/bui.el-master/bui-utils")
      (load "lsp/dap-mode/bui.el-master/bui-button")
      (load "lsp/dap-mode/bui.el-master/bui-history")
      (load "lsp/dap-mode/bui.el-master/bui-core")
      (load "lsp/dap-mode/bui.el-master/bui-entry")
      (load "lsp/dap-mode/bui.el-master/bui-info")
      (load "lsp/dap-mode/bui.el-master/bui-list")
      (load "lsp/dap-mode/bui.el-master/bui"))

    (use-package treemacs
      :commands
      (treemacs
       treemacs-add-and-display-current-project
       treemacs-current-visibility)
      :init
      (use-package cfrs
        :init
        ;; (autoload 'cfrs-read "lsp/dap-mode/treemacs/cfrs")
        :commands (cfrs-read))
      ;; (load "lsp/dap-mode/treemacs/pfuture")
      (add-to-list
       'load-path "~/.emacs.d/packages/lsp/dap-mode/treemacs")
      (add-to-list
       'load-path
       "~/.emacs.d/packages/lsp/dap-mode/treemacs/treemacs-master/src/elisp")
      (require 'treemacs-autoloads)
      ;; (setq treemacs-recenter-after-file-follow t ;好像没效果啊
      ;;       treemacs-recenter-after-tag-follow t
      ;;       treemacs-recenter-distance 0.5
      ;;       ;; treemacs-no-png-images t
      ;;       )
      :config (add-to-list 'tab-line-exclude-modes 'treemacs-mode)
      ;; (with-eval-after-load 'treemacs-mode
      ;;   (when (display-graphic-p)
      ;;     ;; 改变高亮行背景色
      ;;     (add-hook 'treemacs-mode-hook
      ;;               (lambda ()
      ;;                 (face-remap-add-relative 'hl-line '(:background "#666")))))
      ;;   )
      ;; 避免treemacs persistence文件变成只读
      ;; (with-eval-after-load 'treemacs-persistence
      ;;   (defadvice treemacs--persist (around my-treemacs--persist activate)
      ;;     (setq tmp-disable-view-mode 2);; 2不恢复只读
      ;;     ad-do-it
      ;;     (setq tmp-disable-view-mode nil)
      ;;     )
      ;;   )
      ;; (treemacs-follow-mode t)      ; 切换buffer自动定位，特牛，目录再深都能定位到
      ;; (treemacs-fringe-indicator-mode -1) ; 有高亮行就不需要fringe了(本身也被disable)
      ;; (treemacs-filewatch-mode t)          ; 监视系统文件变化
      )
    (use-package lsp-treemacs
      :init
      (load "lsp/dap-mode/lsp-treemacs-master/lsp-treemacs-themes")
      (load "lsp/dap-mode/lsp-treemacs-master/lsp-treemacs-generic")
      (load "lsp/dap-mode/lsp-treemacs-master/lsp-treemacs"))
    ;; f2设置断点跟rg冲突了，所以用vs那套按钮(lsp启动时也会启动dap mode)
    (define-key
     dap-mode-map (kbd "<f5>")
     (lambda ()
       (interactive)
       (let ((cs (dap--cur-session)))
         (if cs
             (if (dap--session-running cs)
                 (call-interactively 'dap-continue)
               (call-interactively 'dap-debug-restart))
           (call-interactively 'dap-debug)))))
    (define-key dap-mode-map (kbd "<f12>") 'dap-hydra)
    (define-key dap-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
    (define-key dap-mode-map (kbd "<f11>") 'dap-step-in)
    (define-key dap-mode-map (kbd "<f10>") 'dap-next)

    ;; 解决hl line不及时更新问题
    ;; (add-hook 'dap-stack-frame-changed-hook (lambda (debug-session)
    ;;                                           (when global-hl-line-mode
    ;;                                             (global-hl-line-highlight))
    ;;                                           ))
    (use-package dap-python
      ;; 需要pip install "ptvsd>=4.2"
      ;; ptvsd调试bug: https://github.com/emacs-lsp/dap-mode/issues/625
      :after (dap-mode python)
      :init
      ;; (setq dap-python-debugger 'debugpy) ;; 这个好像bug要少些
      )

    ;; 需要调用dap-debug-edit-template，或者dap-hydra里d e，来编辑运行参数，类似vscode那样设置
    (use-package dap-cpptools
      :after (dap-mode cc-mode)
      :config
      (dap-register-debug-template
       "cpptools::Run Configuration"
       (list
        :type "cppdbg"
        :request "launch"
        :name "cpptools::Run Configuration"
        :program "${workspaceFolder}/../Autoruns_build/autoruns.exe"
        :cwd "${workspaceFolder}")))

    (use-package dap-hydra
      :commands (dap-hydra)
      :init
      ;; (add-hook 'dap-stopped-hook
      ;;           (lambda (arg)
      ;;             (call-interactively #'dap-hydra)))
      )
    ;; TODO: Locals里的icon显示不正常
    ))
 ((eq lsp-use-which 'eglot)
  (use-package eglot
    :init
    ;; 最新emacs自带eglot并依赖external-completion
    (when (featurep 'external-completion)
      (autoload 'eglot-ensure "lsp/eglot" "" nil)
      (autoload 'eglot "lsp/eglot" "" nil)
      (autoload 'eglot-rename "lsp/eglot" "" nil))
    (defun lsp-ensure ()
      (eglot-ensure))
    (setq
     eglot-confirm-server-initiated-edits nil ; 避免code action的yes/no提示
     eglot-send-changes-idle-time 0 ; 加快补全，实际上corfu-auto-delay的关系更大
     eglot-sync-connect nil ;; 打开新文件就不卡了，貌似没有副作用？
     eglot-events-buffer-size 0 ;; 
     )
    :commands (eglot eglot-ensure eglot-rename eglot-completion-at-point)
    :config
    (use-package lsp-snippet-tempel
      :defer t
      :config
      (lsp-snippet-tempel-eglot-init)
      (define-advice lsp-snippet-tempel--placeholder-fn
          (:around (orig-fn &rest args))
        "去掉placeholder文字提示"
        (let ((result
               (apply orig-fn (list (ad-get-argument args 0) ""))))
          result)))
    (load "lsp/lsp-snippet")
    (load "lsp/lsp-snippet-tempel")

    (add-to-list
     'eglot-server-programs
     '((c-mode c-ts-mode c++-mode c++-ts-mode)
       .
       ("clangd" "-header-insertion=never")))
    (when (featurep 'fussy)
      ;; 配合cape-wrap-buster爽翻！
      (add-to-list
       'completion-category-overrides '(eglot (styles fussy))))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) ;; corfu wiki新增的方法，让输入时强制更新capf
    (advice-add
     'eglot-completion-at-point
     :around #'cape-wrap-noninterruptible)
    (eldoc-add-command 'c-electric-paren)
    (eldoc-add-command 'c-electric-semi&comma) ;; 输入,后提示参数
    (eldoc-add-command 'corfu-insert) ;; 补全后提示参数
    (when nil
      ;; 获取不显示signature的方法
      (defadvice eldoc--message-command-p
          (before my-eldoc--message-command-p activate)
        (and (symbolp (ad-get-arg 0)) (message "%S" (ad-get-arg 0)))))
    (advice-add
     'jsonrpc--log-event
     :around
     (lambda (_orig-func &rest _))) ;; 禁止log buffer据说可以加快速度
    ;; flymake还是要开的，错误不处理的话，补全就不能用了。用跟cmake一样的vs版本可以解决很多错误
    ;; (add-to-list 'eglot-stay-out-of 'flymake)
    ;; (add-to-list 'eglot-stay-out-of 'imenu) ;; 用counsel-etags的imenu
    ;; (setq flymake-no-changes-timeout 1.0) ;; 这个是没有效果的
    (setq eglot-autoshutdown t) ;; 不关退出emacs会卡死
    (setq eglot-ignored-server-capabilities
          (list
           :documentHighlightProvider ;; 关闭光标下sybmol加粗高亮
           :hoverProvider ;; hover没什么用，在sqlite3中还会卡
           :inlayHintProvider ;; 参数提示，但编辑时会错位，不太需要
           :codeActionProvider ;; 当有flymake错误时，code action非常讨厌
           :documentOnTypeFormattingProvider))
    ;; 临时禁止view-mode，使重命名可用
    (defadvice eglot--apply-workspace-edit
        (around my-eglot--apply-workspace-edit activate)
      (setq tmp-disable-view-mode t)
      ad-do-it
      (setq tmp-disable-view-mode nil))
    ;; clang-format不需要了，默认情况下会sort includes line，导致编译不过，但clangd的却不会，但是要自定义格式需要创建.clang-format文件
    (define-key eglot-mode-map [(meta f8)] 'eglot-format)
    ;; cpp用counsel-etags的imenu
    (add-hook
     'eglot-managed-mode-hook
     (lambda ()
       (when (or (derived-mode-p 'c-mode 'c++-mode)
                 (derived-mode-p 'c-ts-base-mode))
         (remove-function
          (local 'imenu-create-index-function) #'eglot-imenu))))
    ;; 禁止didChangeWatchedFiles，一些lsp server会调用它，导致调用project-files，大型项目会卡住(如kill-buffer时)。 等同于lsp-enable-file-watchers
    (cl-defmethod eglot-register-capability
        (server
         (method
          (eql workspace/didChangeWatchedFiles))
         id &key watchers)
      "不要didChangeWatchedFiles这个功能，试了修改eglot--trampish-p不行，只有这样"
      (eglot-unregister-capability server method id)))))

;; 不能任意hook，不然右键无法打开文件，因为eglot找不到对应的server会报错
(defun enable-format-on-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (when (featurep 'eglot)
                (eglot-format))
              (when (featurep 'lsp-mode)
                (lsp-format-buffer)))
            nil 'local))
;; pyright好像还需要node，低版本的还报错，装个支持的win7的最后版本就可以了(node-v13.14.0-x64.msi)
(add-hook
 'python-mode-hook
 (lambda ()
   (when (eq lsp-use-which 'lsp-mode)
     (load "lsp/lsp-pyright"))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ;; h当成c++文件

;; cc-mode包含java等
(add-hook
 'c-mode-common-hook
 (lambda ()
   (when (derived-mode-p 'c-mode 'c++-mode)
     (my-c-mode-hook-set)
     (imenu-setup-for-cpp))
   (abbrev-mode -1)))
(setq c-ts-mode-indent-offset 4)
(add-hook
 'c-ts-base-mode-hook
 (lambda ()
   (imenu-setup-for-cpp) ;; 对于sqlite这样的文件treesit创建imenu仍然会卡死
   (setq-local forward-sexp-function nil) ;; 修复`fingertip' `C-k'问题
   ))

;; pip install cmake-language-server，还需要将cmake加入PATH环境变量

;; https://github.com/sumneko/lua-language-server 去下载bin
(defvar lua-server-path
  (cond
   ((file-exists-p "~/lua-language-server-3.2.4-win32-x64")
    "~/lua-language-server-3.2.4-win32-x64")
   (t
    nil)))
(when lua-server-path
  (with-eval-after-load 'lua-mode
    (add-path-to-execute-path
     (expand-file-name "bin" lua-server-path)))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs '(lua-mode . ("lua-language-server"))))
  (setq lsp-clients-lua-language-server-install-dir lua-server-path))

;; https://github.com/OmniSharp/omnisharp-roslyn 本身就是net core编译的，不需要net6.0体积还小点
(defvar c-sharp-server-path
  (cond
   ((file-exists-p "H:/green/omnisharp-win-x64")
    "H:/green/omnisharp-win-x64")
   (t
    nil)))
(when c-sharp-server-path
  (with-eval-after-load 'csharp-mode
    (add-path-to-execute-path (expand-file-name c-sharp-server-path)))
  (defun my-csharp-hook ()
    (define-key csharp-mode-map "\C-d" nil) ;; 使用我们自己的hungry delete(cc自带hungry不好用)
    )
  (add-hook 'csharp-mode-hook 'my-csharp-hook))

;; xml 这里https://github.com/redhat-developer/vscode-xml的下载https://download.jboss.org/jbosstools/vscode/stable/lemminx-binary/无需要java环境(好像是自动下载java环境)
;; 主要是使用xml的格式化功能
(defvar xml-server-path
  (cond
   ((file-exists-p "~/lemminx-win32.exe")
    "~/lemminx-win32.exe")
   (t
    nil)))
(when xml-server-path
  (with-eval-after-load 'nxml-mode
    (add-path-to-execute-path (expand-file-name "" "~")))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs '(nxml-mode . ("lemminx-win32")))))

;; 仅在view mode off时开启lsp，文件本身只读的(如tfs管理的)更不会开启lsp！
(defvar-local lsp-inited nil)
(add-hook
 'view-mode-hook
 (lambda ()
   (unless lsp-inited
     (when (not buffer-read-only)
       (setq-local lsp-inited t)
       (when (or (memq
                  major-mode
                  '(c-mode
                    c++-mode
                    python-mode
                    rust-mode
                    cmake-mode
                    c-ts-mode
                    c++-ts-mode
                    python-ts-mode
                    rust-ts-mode
                    cmake-ts-mode))
                 (and lua-server-path (eq major-mode 'lua-mode))
                 (and c-sharp-server-path
                      (memq major-mode '(csharp-mode csharp-ts-mode)))
                 (and xml-server-path (eq major-mode 'nxml-mode)))
         (lsp-ensure))))))

;; tfs，还有Team Explorer Everywhere但没用起来，直接用vs自带的根本不用配置(前提在vs项目里用过)
;; 请在init里设置tfs/tf-exe
(defun hydra-tfs-select1 ()
  (interactive)
  (unless (functionp 'hydra-tfs-select/body)
    (require 'tfs)
    (defhydra
     hydra-tfs-select
     ()
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
     ("q" nil "nil" :color blue)))
  (funcall 'hydra-tfs-select/body))
(global-set-key "\C-ct" 'hydra-tfs-select1)
;; 解决checkout后revert buffer光标位置不对的问题
(with-eval-after-load 'tfs
  (when (featurep 'saveplace)
    (defadvice tfs/checkout (before my-tfs/checkout activate)
      (save-place-to-alist))))

;; 自动转换文本中的RGB颜色
(use-package rainbow-mode
  :commands (rainbow-mode)
  :hook (help-mode . rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))


;; grammatical-edit bug太多了，pair用这个就够了
(use-package elec-pair
  :init
  (defvar my-auto-newline nil
    "自己实现auto newline")
  :config
  ;; 去掉elec-pair的post-self-insert-hook，改为直接对char绑定并调用elec-pair的函数
  (when t
    ;; 把所有会成对的char都放这里 (elec-pair是从`syntax-table'找到成对的)
    (defvar elec-pair-chars
      '(?\'
        ?\" ?\` ?\( ?\{ ?\[ ?\<
        ?\) ?\} ?\] ?\>)) ;; 右边也加上，有时候人工会输入
    (add-hook
     'electric-pair-mode-hook
     (lambda ()
       (when electric-pair-mode
         ;; 删除无用的hook
         (remove-hook
          'post-self-insert-hook
          #'electric-pair-post-self-insert-function)
         (remove-hook
          'post-self-insert-hook
          #'electric-pair-open-newline-between-pairs-psif)
         (remove-hook
          'self-insert-uses-region-functions
          #'electric-pair-will-use-region))))
    (cl-dolist
     (key elec-pair-chars)
     (let* ((key_str (char-to-string key))
            (key_symbol
             (make-symbol
              (format "my-elec-pair-%s" (char-to-string key)))))
       (fset key_symbol
             (lambda ()
               (interactive)
               (let ((this-command 'self-insert-command)
                     (last-command-event key))
                 (self-insert-command 1 key)
                 (unless (minibufferp)
                   (electric-pair-post-self-insert-function)
                   (when my-auto-newline
                     (when (eq key ?\{)
                       (call-interactively 'new-line-dwim)))))))
       (global-set-key key_str key_symbol))))

  ;; 修复eldoc不显示参数名问题
  (with-eval-after-load 'eldoc
    (eldoc-add-command "my-elec-pair-\("))

  (electric-pair-mode 1) ;; 这个是必须开的，有些mode会添加自己的chars
  ;; c++换行时输入{}自动indent
  ;; (defadvice electric-pair--insert (after my-electric-pair--insert activate)
  ;;   (indent-according-to-mode)
  ;;   )
  )


(with-eval-after-load 'python
  (define-key python-mode-map "\177" nil) ;; 不需要python自带的DEL键处理
  )

;; 由emacs module实现ctrl tab切换窗口
;; (ignore-errors (module-load (expand-file-name "~/.emacs.d/bin/pop_select.dll"))) ; 需要全路径加载(如果没把~/.emacs.d/bin加入环境变量的话)
(ignore-errors
  (module-load
   "F:/prj/rust/pop-select/target/release/pop_select.dll"))
(ignore-errors
  (module-load
   "H:/prj/rust/pop-select/target/release/pop_select.dll"))

(when (functionp 'pop-select/transparent-set-background)
  (when (version< emacs-version "29")
    (pop-select/ensure-all-window-dark-mode))
  (defvar cur-transparent 255)
  (defconst step-transparent 20)
  (defun dec-transparent ()
    (interactive)
    (setq cur-transparent
          (min 255 (+ cur-transparent step-transparent)))
    (let* ((rgb (color-name-to-rgb (face-background 'default)))
           (r (round (* (nth 0 rgb) 255)))
           (g (round (* (nth 1 rgb) 255)))
           (b (round (* (nth 2 rgb) 255))))
      (pop-select/transparent-set-background cur-transparent r g b)))
  (defun inc-transparent ()
    (interactive)
    (setq cur-transparent
          (max 0 (- cur-transparent step-transparent)))
    (let* ((rgb (color-name-to-rgb (face-background 'default)))
           (r (round (* (nth 0 rgb) 255)))
           (g (round (* (nth 1 rgb) 255)))
           (b (round (* (nth 2 rgb) 255))))
      (pop-select/transparent-set-background cur-transparent r g b)))
  ;; (global-set-key (kbd "<C-wheel-up>") 'dec-transparent)
  ;; (global-set-key (kbd "<C-wheel-down>") 'inc-transparent)
  )
(when (version< emacs-version "29")
  ;; 放大窗口，pop-select/ensure-all-window-dark-mode还有bug就是caption不能重绘，这里放大就OK了
  (w32-send-sys-command #xf030))

(when (fboundp 'pop-select/pop-select)
  (defun my-pop-select (&optional backward)
    (interactive)
    (let* ((myswitch-buffer-list
            (copy-sequence (ep-tabbar-buffer-list)))
           (vec_name [])
           sel)
      (cl-dolist
       (buf myswitch-buffer-list)
       (setq vec_name (vconcat vec_name (list (buffer-name buf)))))
      ;; 返回序号
      (setq sel
            (pop-select/pop-select
             vec_name
             (if backward
                 (1- (length vec_name))
               1)))
      (let ((buf (switch-to-buffer (nth sel myswitch-buffer-list))))
        (when (and (bufferp buf) (featurep 'wcy-desktop))
          (with-current-buffer buf
            (when (eq major-mode 'not-loaded-yet)
              (let ((inhibit-message t))
                (wcy-desktop-load-file))))))))
  (global-set-key (kbd "<C-tab>") 'my-pop-select)
  (global-set-key
   (if (string-equal system-type "windows-nt")
       (kbd "<C-S-tab>")
     (kbd "<C-S-iso-lefttab>"))
   (lambda ()
     (interactive)
     (my-pop-select t))))
(when (functionp 'pop-select/popup-shell-menu)
  (defun print-paths (vec)
    (message "%S" vec)
    (let ((len (length vec))
          (s "")
          (i 0))
      (while (< i len)
        (setq s (concat s "\n" (file-name-nondirectory (aref vec i))))
        (setq i (1+ i)))
      s))
  (with-eval-after-load 'dired
    (defun get-region-select-path ()
      "获取选中的路径，抄的dired-mark和dired-mark-files-in-region"
      (let (paths)
        (when (region-active-p)
          (setq paths [])
          (save-excursion
            (let* ((beg (region-beginning))
                   (end (region-end))
                   (start
                    (progn
                      (goto-char beg)
                      (line-beginning-position)))
                   (end
                    (progn
                      (goto-char end)
                      (if (if (eq dired-mark-region 'line)
                              (not (bolp))
                            (get-text-property
                             (1- (point)) 'dired-filename))
                          (line-end-position)
                        (line-beginning-position)))))
              (goto-char start) ; assumed at beginning of line
              (while (< (point) end)
                ;; Skip subdir line and following garbage like the `total' line:
                (while (and (< (point) end) (dired-between-files))
                  (forward-line 1))
                (if (and (not (looking-at-p dired-re-dot))
                         (dired-get-filename nil t))
                    (setq paths
                          (vconcat
                           paths
                           (list
                            (replace-regexp-in-string
                             "/"
                             "\\\\"
                             (dired-get-filename nil t))))))
                (forward-line 1)))))
        paths))
    (defun get-select-or-current-path ()
      (let ((paths (get-region-select-path))
            current)
        (setq paths
              (vconcat
               paths
               (nreverse
                (dired-map-over-marks (dired-get-filename) nil)))) ;; 添加mark项
        (setq paths (vconcat (-distinct (append paths nil)) [])) ;; 去重复
        (unless paths
          (setq current (dired-get-filename nil t))
          (when current
            (setq paths [])
            (setq paths
                  (vconcat
                   paths
                   (list
                    (replace-regexp-in-string "/" "\\\\" current))))))
        paths))
    (define-key
     dired-mode-map (kbd "<mouse-3>")
     (lambda (event)
       (interactive "e")
       (let ((pt (posn-point (event-end event)))
             (paths (get-region-select-path))
             path)
         (if paths
             (pop-select/popup-shell-menu paths 0 0 1)
           ;; 单个文件直接跳过去
           (select-window (posn-window (event-end event)))
           (goto-char pt)
           (setq path (dired-get-filename nil t))
           (unless path ;可能是点击了空白处，那么就取当前目录
             (setq path (dired-current-directory)))
           (setq paths
                 (vconcat
                  paths
                  (list (replace-regexp-in-string "/" "\\\\" path))))
           ;; 延迟调用使当前选中项更新hl-line等
           (run-at-time
            0.1 nil
            (lambda () (pop-select/popup-shell-menu paths 0 0 1)))))))
    (defun dired-w32-shell-copy ()
      (interactive)
      (let ((paths (get-select-or-current-path)))
        (when paths
          (pop-select/shell-copyfiles paths)
          (message (concat "Copy: " (print-paths paths))))))
    (defun dired-w32-shell-cut ()
      (interactive)
      (let ((paths (get-select-or-current-path)))
        (when paths
          (pop-select/shell-cutfiles paths)
          (message (concat "Cut: " (print-paths paths))))))
    (defun dired-w32-shell-paste ()
      (interactive)
      (let ((current-dir (dired-current-directory)))
        (when current-dir
          (pop-select/shell-pastefiles current-dir)
          (message "Paste in: %S" current-dir))))
    (define-key dired-mode-map "c" 'dired-w32-shell-copy)
    (define-key dired-mode-map (kbd "C-c C-c") 'dired-w32-shell-copy)
    (define-key dired-mode-map "v" 'dired-w32-shell-paste)
    (define-key dired-mode-map (kbd "C-v") 'dired-w32-shell-paste)
    (define-key dired-mode-map (kbd "C-w") 'dired-w32-shell-cut))

  (with-eval-after-load 'embark
    (define-key
     embark-file-map (kbd "C-c C-c")
     (lambda (file)
       (interactive "f")
       (setq file (string-trim file nil "[/\\]")) ;去掉结尾的\/，不然目录显示有问题
       (message
        (concat
         "Copy: "
         (print-paths (make-vector 1 (expand-file-name file)))))
       (pop-select/shell-copyfiles
        (make-vector 1 (expand-file-name file)))))
    (define-key
     embark-file-map (kbd "C-w")
     (lambda (file)
       (interactive "f")
       (setq file (string-trim file nil "[/\\]")) ;去掉结尾的\/，不然目录显示有问题
       (message
        (concat
         "Cut: "
         (print-paths (make-vector 1 (expand-file-name file)))))
       (pop-select/shell-cutfiles
        (make-vector 1 (expand-file-name file)))))))

(when nil ;;(fboundp 'pop-select/beacon-animation)
  (defun show-cursor-animation ()
    (ignore-errors
      (let* ((p (window-absolute-pixel-position))
             (pp (point))
             (x (car p))
             (w
              (if (equal cursor-type 'bar)
                  1
                (if-let ((glyph
                          (when (< pp (point-max))
                            (aref
                             (font-get-glyphs
                              (font-at pp) pp (1+ pp))
                             0))))
                  (aref glyph 4)
                  (window-font-width))))
             (h (line-pixel-height))
             (y
              (if header-line-format
                  (- (cdr p) h) ;; 修复开启`header-line-format'时y值不正确
                (cdr p))))
        (when p
          (if (memq
               this-command
               '(up-slightly
                 down-slightly
                 mwim-beginning-of-code-or-line
                 mwim-end-of-code-or-line
                 backward-word
                 forward-word))
              (pop-select/beacon-animation-update-pos x y w h)
            (pop-select/beacon-animation
             x y w h
             100 ; timer
             50 ; timer step
             233 86 120 ; r g b
             20 ; diff min，根据自己需要试验
             ))))))
  (add-hook 'post-command-hook #'show-cursor-animation))

;; jump后自动把屏幕居中
(use-package scroll-on-jump
  :defer 0.3
  :config
  (setq scroll-on-jump-duration 0.0)
  (setq scroll-on-jump-smooth nil)
  (with-eval-after-load 'session
    (scroll-on-jump-advice-add session-jump-to-last-change))
  (scroll-on-jump-advice-add jl-jump-backward)
  (scroll-on-jump-advice-add jl-jump-forward)
  (scroll-on-jump-advice-add push-button)
  (scroll-on-jump-advice-add git-gutter:previous-hunk)
  (scroll-on-jump-advice-add git-gutter:next-hunk)
  (scroll-on-jump-advice-add embark-next-symbol)
  (scroll-on-jump-advice-add embark-previous-symbol)
  (scroll-on-jump-advice-add my/backward-forward-previous-location)
  (scroll-on-jump-advice-add my/backward-forward-next-location)
  ;; 调用了set-window-start的，要用scroll-on-jump-with-scroll-..
  )

;; 对于scroll-on-jump没效果的，可以手动开启centered-cursor-mode
(use-package centered-cursor-mode
  :commands (centered-cursor-mode))

(use-package yaml-mode
  :commands (yaml-mode)
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package diff-hl
  :defer t
  :init
  (add-hook
   'prog-mode-hook
   (lambda ()
     (unless (featurep 'diff-hl)
       (delay-require-libs
        "~/.emacs.d/themes/diff-hl-master"
        '(diff-hl diff-hl-dired diff-hl-show-hunk)))
     (diff-hl-maybe-define-bitmaps)
     (diff-hl-update)
     (setq-local diff-hl-mode t) ;; for 'diff-hl-magit-post-refresh
     (add-hook 'after-save-hook
               (lambda ()
                 (when diff-hl-update-timer
                   (cancel-timer diff-hl-update-timer))
                 (setq diff-hl-update-timer
                       (run-with-idle-timer
                        2 nil #'diff-hl-update-timer-function)))
               nil t)))
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  ;; 用timer避免各种hook
  (defvar diff-hl-update-timer nil)
  (defun diff-hl-update-timer-function ()
    (ignore-errors
      (diff-hl-update)))
  (defun diff-hl-update-manual ()
    (interactive)
    (diff-hl-update))

  (use-package diff-hl-dired
    :commands (diff-hl-dired-mode)
    :init (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

  ;; 点击fringe或margin时，弹出菜单可以转到下一个chunk，很方便！
  (use-package diff-hl-show-hunk
    :commands (diff-hl-show-hunk)
    :init
    (global-set-key (kbd "C-c h") #'diff-hl-show-hunk)
    (global-set-key (kbd "C-c C-h") #'diff-hl-show-hunk) ;; 目前还没有绑定这个的
    :config
    ;; 解决相关按钮被view mode覆盖的问题，如q。这里用set-transient-map完全覆盖了原来的按键！
    (defvar set-transient-map-exit-func nil)
    (defun my-diff-hl-inline-popup-hide ()
      (interactive)
      (diff-hl-inline-popup-hide)
      (when (functionp set-transient-map-exit-func)
        (funcall set-transient-map-exit-func)))
    (defvar diff-hl-inline-popup-transient-mode-map1
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-g") #'my-diff-hl-inline-popup-hide)
        (define-key map [escape] #'my-diff-hl-inline-popup-hide)
        (define-key map (kbd "q") #'my-diff-hl-inline-popup-hide)
        (define-key map (kbd "RET") #'my-diff-hl-inline-popup-hide)
        map))
    (set-keymap-parent
     diff-hl-inline-popup-transient-mode-map1 diff-hl-show-hunk-map)
    (setq diff-hl-show-hunk-function
          (lambda (buffer &optional _ignored-line)
            (funcall 'diff-hl-show-hunk-inline-popup
                     buffer
                     _ignored-line)
            (setq set-transient-map-exit-func
                  (set-transient-map
                   diff-hl-inline-popup-transient-mode-map1
                   t 'diff-hl-inline-popup-hide))))))

(use-package maple-preview
  :defer t
  :init
  (defun note-preview ()
    (interactive)
    (unless (featurep 'maple-preview)
      (delay-require-libs
       "~/.emacs.d/packages/org" '(websocket simple-httpd))
      (dec-placeholder-fun
       maple-preview-mode
       maple-preview
       "~/.emacs.d/packages/org/emacs-maple-preview-master"
       '(maple-preview)))
    (call-interactively 'maple-preview-mode))

  (setq maple-preview:allow-modes
        '(org-mode markdown-mode html-mode web-mode mhtml-mode))
  :config
  (defadvice maple-preview:init (after my-maple-preview:init activate)
    ;; 这个hook还达到了自动滚屏的效果? (需要用鼠标点击不同位置)
    (add-hook 'buffer-list-update-hook 'maple-preview:send-to-server))
  (defadvice maple-preview:finalize
      (after my-maple-preview:finalize activate)
    (remove-hook
     'buffer-list-update-hook 'maple-preview:send-to-server)))

(use-package winner
  :defer t
  :init
  :config
  (define-key winner-mode-map (kbd "<C-left>") #'winner-undo)
  (define-key winner-mode-map (kbd "<C-right>") #'winner-redo))

(use-package org
  :defer t
  :config
  (define-key org-mode-map (kbd "M-h") nil)
  (define-key org-mode-map (kbd "C-c C-d") nil)
  (define-key org-mode-map (kbd "C-'") nil))

;; 优点: 可以以文件名,tag和子标题(需要org-id-get-create创建id)来搜索。
;; roam buffer: 可以显示backlink，同时会根据鼠标位置动态更新内容
(use-package org-roam
  :defer t
  :init
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-database-connector
        (if (version< emacs-version "29")
            'sqlite
          'sqlite-builtin)) ; 使用29版本以上自营的sqlite，但是仍然需要上面的emacsql
  (when nil
    ;; 暂时不用模式，按目录结构自己创建org文件
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target
             (file+head
              "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
             :unnarrowed t)
            ("t" "tech" plain "%?"
             :target
             (file+head
              "tech/%<%Y%m%d%H%M%S>-${slug}.org"
              "#+title: ${title}\n")
             :unnarrowed t)
            ("w" "work" plain "%?"
             :target
             (file+head
              "work/%<%Y%m%d%H%M%S>-${slug}.org"
              "#+title: ${title}\n")
             :unnarrowed t)
            ("h" "home" plain "%?"
             :target
             (file+head
              "home/%<%Y%m%d%H%M%S>-${slug}.org"
              "#+title: ${title}\n")
             :unnarrowed t))))

  (defun call-project-find ()
    (interactive)
    (let ((default-directory org-roam-directory))
      (call-interactively 'my-project-find-file)))
  (defun call-project-search ()
    (interactive)
    (let ((default-directory org-roam-directory))
      (call-interactively 'my-project-search)))

  ;; 一些org命令也放这里，太实用了
  (defhydra
   hydra-org-roam
   ()
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
  (define-advice hydra-org-roam/body (:around (orig-fn &rest args))
    (unless (featurep 'org-roam)
      (delay-require-libs
       "~/.emacs.d/packages/org/emacsql-master"
       '(emacsql emacsql-sqlite))
      (unless (version< emacs-version "29")
        (delay-require-libs
         "~/.emacs.d/packages/org" '(emacsql-sqlite-builtin)))
      (delay-require-libs
       "~/.emacs.d/packages/magit/magit-master/lisp" '(magit-section))
      (delay-require-libs
       "~/.emacs.d/packages/org/org-roam-main" '(org-roam)))
    (apply orig-fn args))
  :config
  ;; find时列表加入tag，这么好的功能居然不加入默认？
  (setq org-roam-node-display-template
        (concat
         "${title:*} " (propertize "${tags:100}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; 屏蔽新建node功能，因为不喜欢capture，还是手动按目录结构创建笔记
  (defadvice org-roam-capture- (around my-org-roam-capture- activate)
    (let ((default-directory org-roam-directory))
      (call-interactively 'find-file))))

(use-package eww
  :defer t
  :config
  (define-key eww-mode-map "w" 'scroll-down-command)
  (use-package shr
    :defer t
    :config
    ;; 修复选中图片时按w不生效问题
    (define-key shr-image-map "w" nil)
    (define-key eww-link-keymap "w" nil)))

(defun my-C-1 ()
  (interactive)
  (while (or (window-parameter nil 'window-side)
             (bound-and-true-p poe-popup-mode))
    ;; 在side window里，切换到其它窗口。other有可能还是side bar所以用了while
    (call-interactively 'other-window))
  (let ((wcount (length (window-list))))
    (poe-popup-close)
    (call-interactively 'keyboard-escape-quit)
    ;; 如果窗口数没变，就可以弹出pop窗口了
    (if (eq wcount (length (window-list)))
        (call-interactively 'poe-popup-toggle))))

;; 这个C-tab切换buffer不会切换到side window，而且焦点也不回到pop buffer里去(也可以设置:select t切换到)。
;; 规则跟shackle是一样的，不过设置:same有bug所以还不能去掉shackle
(use-package poe
  ;; from https://github.com/endofunky/emacs.d/blob/master/site-lisp/poe.el
  :defer 1.0
  :commands (poe-popup poe-rule poe-popup-toggle poe-popup-close)
  :init
  (global-set-key (kbd "C-1") 'my-C-1)
  (setq
   poe-remove-fringes-from-popups nil
   poe-dim-popups nil)
  :config
  ;; https://github.com/endofunky/emacs.d/blob/master/lisp/core/core-popup.el#L18
  (poe-popup " *Metahelp*") ;; :ephemeral t是临时buffer，toggle后会消失
  (poe-popup "*Apropos*" :size .3 :shrink t)
  (poe-popup "*Backtrace*")
  (poe-popup "*Checkdoc Status*")
  (poe-popup "*Compile-Log*")
  (poe-popup "*Command History*")
  (poe-popup "*Help*" :size 0.4 :shrink t :select t)
  (poe-popup "*Messages*")
  (poe-popup "*Occur*")
  (poe-popup "*Pp Eval Output*")
  (poe-popup "*Warnings*")
  (poe-popup "*compilation*")
  (poe-popup "\\`\\*WoMan.*?\\*\\'" :regexp t :size 0.5 :shrink t)
  (poe-popup 'calendar-mode)
  (poe-popup 'comint-mode)
  (poe-popup 'compilation-mode)
  (poe-popup 'Man-mode :size 0.4 :shrink t)
  (poe-popup "\\*gud-.*" :regexp t :select t) ;; (poe-popup 'gud-mode) 没效果
  (poe-popup "*eshell*" :select t)
  (poe-popup "*shell*" :select t)

  (add-hook 'poe-popup-mode-hook (lambda () (tab-line-mode -1))) ;; 实际上关闭buffer自身的tab-line
  (remove-hook 'poe-popup-mode-hook #'poe--popup-dim-h) ;; 不需要改变background color
  (remove-hook 'poe-popup-mode-hook #'poe--popup-remove-fringes-h) ;; 貌似也没什么用
  (define-key poe-popup-mode-map (kbd "<C-tab>") 'poe-popup-next)
  (define-key poe-popup-mode-map (kbd "<C-S-tab>") 'poe-popup-prev)
  (global-set-key (kbd "M-1") 'poe-popup-next)
  (global-set-key (kbd "M-`") 'poe-popup-next)
  (defface poe-echo-area-buried '((t :inherit shadow))
    "Echo area face for buried popups.")
  (defface poe-echo-area '((t :inverse-video t :weight bold))
    "Echo area face for opened popup.")
  (defun echo-poe-buffers (&rest _app)
    "简单实现poe-echo那样的效果"
    ;; 手动执行C-x C-e message居然没有高亮效果
    (message
     (cl-reduce
      #'concat
      (cons
       (propertize (funcall #'identity
                            (buffer-name
                             (car poe--popup-buffer-list)))
                   'face 'poe-echo-area)
       (cl-mapcar
        (lambda (buf)
          (concat
           (propertize ", " 'face 'poe-echo-area-buried)
           (propertize (funcall #'identity (buffer-name buf))
                       'face 'poe-echo-area-buried)))
        (cdr poe--popup-buffer-list))))))
  (advice-add #'poe-popup-toggle :after #'echo-poe-buffers)
  (advice-add #'poe-popup-next :after #'echo-poe-buffers)
  (advice-add #'poe-popup-prev :after #'echo-poe-buffers)
  (defun poe-close-window-hack (&rest _)
    "Close poe window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               (poe--popup-windows))
      (let ((window (car (poe--popup-windows))))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'poe-close-window-hack)

  (poe-mode +1))

;; 这个就是辅助设置`display-buffer-alist'的，设置弹出窗口很方便
(use-package shackle
  :defer 1.0
  :init
  (setq
   shackle-default-size 0.4
   shackle-default-rule nil
   shackle-default-alignment 'below
   shackle-rules
   '( ;; 更多设置参看shackle.el https://github.com/seagle0128/.emacs.d/blob/47c606e43a207922de6b26f03d15827f685b0b3e/lisp/init-window.el#L145
     ;; (compilation-mode :noselect t :align 'below :size 0.2);; noselect只是cursor不移动过去
     (" server log\\*\\'" :noselect t :align 'below :size 0.2) ; dap mode的log窗口
     (magit-status-mode :select t :inhibit-window-quit t :same t) ;; magit全屏舒服
     (magit-log-mode :select t :inhibit-window-quit t :same t)
     ("\\*SQLite .*"
      :regexp t
      :select t
      :inhibit-window-quit t
      :same t) ;; sqlite-mode全屏
     ("\\*gud-.*"
      :regexp t
      :select t
      :align 'below
      :other t
      :size 0.3)))
  :config (shackle-mode 1))

;; 这个comment行只要有mark即可，不需要全部选中(对于lisp相关mode它还是会优先region)
;; 按两下就会跳到行尾， C-u开头会对齐注释 
(use-package comment-dwim-2
  :defer 1.1 ;; 经常首次使用时卡住，故而不要用延迟加载了
  :bind ([remap comment-dwim] . comment-dwim-2)
  :init
  ;; (setq cd2/region-command 'cd2/comment-or-uncomment-region) ;; comment-dwim那种模式，多行需要全部选中
  (global-set-key "\M-;" 'comment-dwim-2))

;; 修改光标下的数字，这个支持十六进制(shift-number不支持)
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "<C-wheel-up>") 'evil-numbers/dec-at-pt) ;; CTRL+鼠标滚动
  (global-set-key (kbd "<C-wheel-down>") 'evil-numbers/inc-at-pt))

(use-package so-long
  :defer 1.3
  :config (global-so-long-mode))

(use-package drag-stuff
  :commands (drag-stuff-up drag-stuff-down drag-stuff-right drag-stuff-left)
  :init
  (global-set-key (kbd "C-<up>") 'drag-stuff-up)
  (global-set-key (kbd "C-<down>") 'drag-stuff-down)
  ;; (global-set-key (kbd "C-<left>") 'drag-stuff-left) ;; 左右移动貌似没什么用，还是留给winner-mode
  ;; (global-set-key (kbd "C-<right>") 'drag-stuff-right)
  )

(use-package elfeed
  :defer t
  :init
  (dec-placeholder-fun
   elfeed elfeed "~/.emacs.d/packages/tools/elfeed-master" '(elfeed))
  (setq
   elfeed-use-curl t ;; win10好像自带curl
   ;; elfeed-curl-program-name "curl"
   elfeed-feeds
   '("http://tttang.com/rss.xml"
     "https://xz.aliyun.com/feed"
     "https://paper.seebug.org/rss")
   elfeed-search-filter "@6-months-ago" ;; 默认不显示已经read了的，设置为都显示
   )
  :config
  (define-key
   elfeed-search-mode-map (kbd "RET")
   'elfeed-search-browse-url) ; 不需要查看简介什么的，直接打开浏览器就行了
  )

;; 删除了csharp-compilation的引用，tree-sister好像没起作用，将就用了
(use-package csharp-mode
  :commands (csharp-mode)
  :init (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

(use-package posframe
  :commands (posframe-hide posframe-show))

(use-package simple
  :defer t
  :init
  (setq blink-matching-paren nil) ;; 不然blink-matching-open不显示
  :config
  ;; blink-matching-paren为nil的话顺便把这个hook给去掉
  (remove-hook
   'post-self-insert-hook #'blink-paren-post-self-insert-function)
  (remove-hook
   'minibuffer-setup-hook
   #'minibuffer-history-isearch-setup) ;; isarch不需要？
  )

;; 弥补topsy没有timer机制，减少卡顿
(use-package mode-line-idle
  :defer 1.0
  :commands (mode-line-idle)
  :config
  (setq old-mode-line-format mode-line-format)
  (setq-default mode-line-format
                '(:eval
                  (mode-line-idle
                   0.5
                   '(:eval
                     (format-mode-line old-mode-line-format))
                   ""))))

;; topsy不同于which-key和breadcrumb之处是，它显示不是cursor所在函数，而是顶行所在函数，这样更直观一些
;; breadcrumb是依赖imenu，而我们是ctags生成的，显然没lsp那么好用了
(use-package topsy
  :commands (topsy-mode)
  :init
  (defun my-topsy-headline-click (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (when (> (window-start) 1)
        (goto-char (window-start))
        (beginning-of-defun))))
  (defvar my-topsy-headline-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] 'my-topsy-headline-click)
      (define-key map [header-line down-mouse-1] 'ignore)
      (define-key map [header-line mouse-1] 'my-topsy-headline-click)
      map))
  (defun my-topsy-headline ()
    "抄的`topsy--beginning-of-defun'，加上行号跟github一样的效果"
    ;; TODO: 对于一些c声明是两行
    (ignore-errors
      (let ((ret
             (when (> (window-start) 1)
               (save-excursion
                 (goto-char (window-start))
                 (beginning-of-defun)
                 (font-lock-ensure (point) (point-at-eol))
                 (concat
                  (propertize (format
                               (format
                                " %%%dd "
                                (or display-line-numbers-width 0))
                               (line-number-at-pos (point)))
                              'face '(foreground-color . "cyan"))
                  (propertize (buffer-substring
                               (point) (point-at-eol))
                              'mouse-face
                              'show-paren-match
                              'local-map
                              my-topsy-headline-keymap))))))
        ;; 空内容
        (unless ret
          (setq ret
                (concat
                 (propertize (format
                              (format
                               " %%%ds "
                               (or display-line-numbers-width 0))
                              "Col")
                             'face '(foreground-color . "cyan"))
                 "")))
        ret)))
  (defvar my-topsy-dired-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] 'my-topsy-dired-click)
      (define-key map [header-line down-mouse-1] 'ignore)
      (define-key map [header-line mouse-1] 'my-topsy-dired-click)
      map))
  (defun my-topsy-dired-click (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (dired-sort-auto)))
  (defun my-topsy-headline-for-dired ()
    (concat
     (propertize dired-directory 'face '(foreground-color . "cyan"))
     (propertize (cond
                  ((equal (nth 0 dired-sort-by-history) "S")
                   " Sort by size")
                  ((equal (nth 0 dired-sort-by-history) "t")
                   " Sort by time")
                  ((equal (nth 0 dired-sort-by-history) "X")
                   " Sort by extension")
                  ((equal (nth 0 dired-sort-by-history) "")
                   " Sort by name"))
                 'face
                 'show-paren-mismatch
                 'mouse-face
                 'isearch
                 'local-map
                 my-topsy-dired-keymap)))
  (setq topsy-mode-functions
        '((dired-mode . my-topsy-headline-for-dired)
          (nil . my-topsy-headline)))
  (defun topsy-hook ()
    (when (autoloadp (symbol-function 'topsy-mode))
      (require 'topsy))
    (setf
     topsy-fn
     (or (alist-get major-mode topsy-mode-functions)
         (alist-get nil topsy-mode-functions))
     header-line-format
     (list
      '(:eval (mode-line-idle 0.5 topsy-header-line-format "")))))
  (add-hook 'prog-mode-hook 'topsy-hook)
  (add-hook 'dired-mode-hook 'topsy-hook)
  (with-eval-after-load 'mode-line-idle
    (define-advice mode-line-idle--tree-to-string
        (:around (orig-fn &rest args))
      "解决M-x自动跳到行首问题，hook`topsy-fn'没用，好像是closure函数"
      (when (equal (buffer-name) (buffer-name (window-buffer)))
        (apply orig-fn args)))))

(when (string-equal system-type "windows-nt")
  (use-package w32-browser
    :commands
    (w32explore
     dired-mouse-w32-browser
     dired-w32-browser
     dired-multiple-w32-browser
     dired-w32explore)
    :init
    ;; windows中打开并选中buffer对应的文件，C-X 6 是2C mode的前辍
    (defun browse-file-in-explorer (&optional file)
      "browse file in windows explorer"
      (interactive)
      (w32explore
       (or file
           (buffer-file-name (current-buffer))
           default-directory)))
    (global-set-key (kbd "C-x C-d") 'browse-file-in-explorer))

  ;; 需要删除gud-cdb.elc，不然运行报错
  (use-package gud-cdb
    ;; from https://github.com/junjiemars/.emacs.d/blob/master/config/gud-cdb.el，目前就只有这个在一直更新
    ;; 唯一不足的是不支持speedbar，还有attach    
    :commands (cdb)
    :init
    (defalias 'defcustom% 'defcustom)
    (defalias 'ignore* 'ignore)
    (defalias 'loop* 'loop)
    (defalias 'assoc** 'assoc)
    (defvar cdb-add-g nil)
    :config
    (defadvice cdb-command-line-list-source
        (after my-cdb-command-line-list-source activate)
      "追加-2参数让启动后新开一个命令窗口，-G忽略进程退出的breakpoint, 禁止从网络下载symbol"
      ;; TODO: "-g"忽略初始化breakpoint，目前需要在启动breakpoint时设置断点，还不支持预先设置断点和记忆断点
      (setq ad-return-value
            (append
             ad-return-value
             (if cdb-add-g
                 '("-2" "-G" "-netsymsno" "-g")
               '("-2" "-G" "-netsymsno"))))) ;; cdb /?不对啦(跟.netsyms命令对得上)
    ))

(use-package gud
  :defer t
  :commands (gud-quit)
  :init
  (setq gud-chdir-before-run nil) ;; 避免gud自动设置运行目录为exe所在目录
  (defvar f5-read-command t)
  (defun gud-auto ()
    (interactive)
    (when current-prefix-arg ;; C-u F5重设参数
      (setq f5-read-command t))
    ;; 设置为project根目录减少麻烦
    (let ((default-directory
           (directory-file-name
            (let ((pr (project-current nil)))
              (if pr
                  (project-root pr)
                default-directory)))))
      ;; 自动继续调试或者开始调试
      (condition-case nil
          (call-interactively 'gud-cont)
        (error
         (if f5-read-command
             (progn
               (setq f5-read-command nil)
               (cond
                ((eq major-mode 'python-mode)
                 (call-interactively 'pdb))
                (t
                 ;; 默认还是cdb，因为这个用得多
                 (call-interactively 'cdb))))
           (cond
            ((eq major-mode 'python-mode)
             (pdb (car gud-pdb-history)))
            ;; 默认cdb
            (t
             (cdb (car gud-cdb-history)))))))))
  (global-set-key
   (kbd "<f4>")
   (lambda ()
     (interactive)
     (if (and (bound-and-true-p gud-comint-buffer)
              (buffer-name gud-comint-buffer))
         (call-interactively 'gud-jump)
       (call-interactively 'next-error))))

  (global-set-key
   (kbd "C-<f5>")
   (lambda () ;; cdb添加-g参数直接运行，不能下断点
     (interactive)
     (let ((cdb-add-g t))
       (call-interactively 'gud-auto))))
  (global-set-key (kbd "<f9>") 'gud-break)
  (global-set-key (kbd "C-<f9>") 'gud-tbreak) ;; tempory breakpoint
  ;; (global-set-key (kbd "C-<f9>") 'gud-remove)
  (global-set-key (kbd "<f10>") 'gud-next)
  (global-set-key (kbd "<f11>") 'gud-step)
  (global-set-key (kbd "<f12>") 'gud-print) ;; 打印cursor所在变量，比输入dv(cdb)要快点。支持region
  (global-set-key
   (kbd "S-<f5>")
   #'(lambda ()
       (interactive)
       (when (and (bound-and-true-p gud-comint-buffer)
                  (buffer-name gud-comint-buffer))
         (with-current-buffer gud-comint-buffer
           (let ((quit 'comint-quit-subjob)) ; C-c C-\
             (when (eq gud-minor-mode 'cdb)
               ;; 测试先发送q再C-c C-c可以结束在运行的程序
               (call-interactively 'gud-quit)
               (setq quit 'comint-interrupt-subjob) ; C-c C-c
               )
             (call-interactively quit))))))
  ;; 不需要RET确认参数再运行，很多时间都是同一个项目。放gud或者gud-cdb的:config里没用，这里等等待session初始化后再设置
  (add-to-list
   'emacs-startup-hook
   (lambda ()
     (when gud-cdb-history
       (setq f5-read-command nil))))
  :config
  (defadvice gud-sentinel (after my-gud-sentinel activate)
    "自动关闭Debugger finished的gud buffer，from https://www.reddit.com/r/emacs/comments/ggs0em/autoclose_comint_buffers_on_exit_or_process_end/"
    (let ((process (ad-get-arg 0)))
      (unless (process-live-p process)
        (kill-buffer (process-buffer process))
        (delete-other-windows)
        (message "Debugger finished"))))
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
            (move-overlay
             ov (line-beginning-position) (line-end-position)
             (current-buffer))))))
  (defun gud-kill-buffer ()
    (if (eq major-mode 'gud-mode)
        (delete-overlay gud-overlay)))
  (defun my-gud-hook ()
    "安装退出时去掉overlay的钩子，定义gud-quit供S-f5使用"
    (add-hook 'kill-buffer-hook #'gud-kill-buffer nil :local)
    (gud-def gud-quit "q " "\C-q" "Quit Debug"))
  (with-eval-after-load 'gud-cdb
    (add-hook 'gud-cdb-mode-hook 'my-gud-hook))
  (add-hook 'pdb-mode-hook 'my-gud-hook)
  (add-hook 'gud-gdb-mode-hook 'my-gud-hook))

;; 关于dap，lldb和gdb本身都不支持这个协议(gdb 14版本将会直接支持)，需要中间层处理dap协议
;; lldb需要codelldb，或者自带lldb-vscode，它这个支持msvc编译的pdb
;; gdb则需要cpptools，或者等14版本发布，不过不能调试msvc编译的程序对我们用处不大
;; 而msvc调试器的dap中转有vsdbg(cpptools里包含)支持，但在dap协议里加了handshake防止其它产品使用
(use-package dape
  :defer t
  :init
  (autoload 'dape "lsp/dape" "" t)
  (autoload 'dape-toggle-breakpoint "lsp/dape" "" t)
  (autoload 'dape-step-out "lsp/dape" "" t)
  (autoload 'dape-step-in "lsp/dape" "" t)
  (autoload 'dape-quit "lsp/dape" "" t)
  (global-set-key (kbd "<f9>") 'dape-toggle-breakpoint)
  (global-set-key (kbd "<f10>") 'dape-next)
  (global-set-key (kbd "<f11>") 'dape-step-in)
  (global-set-key (kbd "S-<f11>") 'dape-step-out)
  (global-set-key (kbd "S-<f5>") 'dape-quit)
  (global-set-key (kbd "<f12>") 'dape-watch-dwim)
  (defun dape-auto ()
    (interactive)
    (when (autoloadp (symbol-function 'dape))
      (load "lsp/dape"))
    (if (dape--live-process t)
        (call-interactively 'dape-continue)
      (call-interactively 'dape)))
  :config

  (winner-mode 1) ;; C-left恢复窗口
  (global-eldoc-mode 1) ;; 点击变量时显示值

  ;; 下载https://github.com/vadimcn/codelldb/releases 文档https://github.com/vadimcn/codelldb/blob/v1.10.0/MANUAL.md
  ;; 测试命令行程序不能弹窗口，输出也不知道去了哪里。还有就是很慢。
  (setq
   lldb-cmd
   (expand-file-name
    "~/.emacs.d/.extension/codelldb-x86_64-windows/extension/adapter/codelldb.exe"))
  (add-to-list
   'dape-configs
   '(codelldb
     modes
     (c-mode c-ts-mode c++-mode c++-ts-mode rust-ts-mode rust-mode)
     command
     lldb-cmd
     host
     "localhost"
     port
     5818
     command-args
     ("--port" "5818")
     :type "lldb"
     :request "launch"
     :cwd dape-cwd-fn
     :terminal "external" ;; 没有效果？
     :program dape-find-file))

  ;; 会跑飞
  (setq lldb-vscode-cmd "c:/LLVM/bin/lldb-vscode.exe")
  (add-to-list
   'dape-configs
   '(lldb-vscode
     modes
     (c-mode c-ts-mode c++-mode c++-ts-mode rust-ts-mode rust-mode)
     command
     lldb-vscode-cmd
     host
     "localhost"
     port
     5818
     command-args
     ("--port" "5818")
     :type "lldb-vscode"
     :stopAtEntry t
     :request "launch"
     :cwd dape-cwd-fn
     :program dape-find-file))

  ;; cpp, rust https://github.com/microsoft/vscode-cpptools/releases ，解压vsix
  ;; 测试在启动WindowsDebugLauncher那里卡住了
  (setq
   dape-cppdbg-command
   (expand-file-name
    "~/.emacs.d/.extension/extension/debugAdapters/bin/OpenDebugAD7.exe"))
  (add-to-list
   'dape-configs
   `(cppdbg
     modes
     (c-mode c-ts-mode c++-mode c++-ts-mode)
     command-cwd
     ,(file-name-directory dape-cppdbg-command)
     command
     dape-cppdbg-command
     :type "cppdbg"
     :request "launch"
     :cwd dape-cwd-fn
     :program dape-find-file
     :stopAtEntry t
     :logging (:engineLogging t) ;; 开启调试
     :externalConsole t ;; 避免以WindowsDebugLauncher启动，卡住了都不知道哪里错了
     :MIMode "gdb"
     ;; ,(cond
     ;;   ((executable-find "gdb")
     ;;    "gdb")
     ;;   ((executable-find "lldb")
     ;;    "lldb"))
     ;; :miDebuggerPath ;" H:\\msys64\\usr\\bin\\gdb.exe"
     ;; ,(cond
     ;;   ((executable-find "gdb")
     ;;    (executable-find "gdb"))
     ;;   ((executable-find "lldb")
     ;;    (executable-find "lldb")))
     ))

  (when t
    (setq dape--timeout 20)
    ;; x64dbg在WriteFile下断
    )


  (defun decode-hex-string (hex-string)
    (let ((res nil))
      (dotimes (i
                (/ (length hex-string) 2)
                (apply #'concat (reverse res)))
        (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
          (push (format "%c" (string-to-number hex-byte 16)) res)))))
  (defun my-calc (str)
    (let
        ((l
          (list
           "596F75206D6179206F6E6C79207573652074686520432F432B2B20457874656E73696F6E20666F722056697375616C2053747564696F20436F646520616E6420432320457874656E73696F6E20666F722056697375616C2053747564696F20436F6465"
           "776974682056697375616C2053747564696F20436F64652C2056697375616C2053747564696F206F722058616D6172696E2053747564696F20736F66747761726520746F2068656C7020796F7520646576656C6F7020616E64207465737420796F7572206170706C69636174696F6E732E"
           "54686520736F667477617265206973206C6963656E7365642C206E6F7420736F6C642E"
           "546869732061677265656D656E74206F6E6C7920676976657320796F7520736F6D652072696768747320746F207573652074686520736F6674776172652E"
           "4D6963726F736F667420726573657276657320616C6C206F7468657220726967687473"
           "596F75206D6179206E6F7420776F726B2061726F756E6420616E7920746563686E6963616C206C696D69746174696F6E7320696E2074686520736F6674776172653B"
           "7265766572736520656E67696E6565722C206465636F6D70696C65206F7220646973617373656D626C652074686520736F667477617265"
           "72656D6F76652C206D696E696D697A652C20626C6F636B206F72206D6F6469667920616E79206E6F7469636573206F66204D6963726F736F6674206F7220"
           "69747320737570706C6965727320696E2074686520736F6674776172652073686172652C207075626C6973682C2072656E742C206F72206C6561736520"
           "74686520736F6674776172652C206F722070726F766964652074686520736F6674776172652061732061207374616E642D616C6F6E6520686F7374656420617320736F6C7574696F6E20666F72206F746865727320746F207573652E"))
         (s
          (list
           "562B792C2848607626415C40782B3B3447754B3C247A5D2E2E3F382377565A6E272A2B7D6A31455C246B30242F6C766B70623834364B3A6B662243495C596C2A6434202F202E522C7B20"
           "42252642483C2F27657B55603E463E6B73336C6B6753583E4554717B5673752D693C6B56637D2950284860774B6C5476755045443E424C41582943305831734E5C5B75342C48"
           "626B40774A722637682B4E5C604A666B3444246E6263644B656E5E566B4F483C274B4E3A2575564F274733657623292E24674D24722F3D3D7174595D504A5B"
           "2E6F77436F5C315C423A393634273432297B63303C712C3E5C5C3122202D20214031777D5874"
           "56272B7C69353F7D5D57504C537A65315745363B277D54673B3833763856327A7D6F7626782A"
           "24586A6D23583A76634B64596E30566E6B724C51444F7779223B202E3352425357255249644C4F5A3728474B52202F20313E287763696D653A35714473203F205B6C356235444B52203D2039"
           "3155484A5E3751742E535074633C513E363E23246A452B3E3E6522502A4D62207C20624C202F206048352768376E503A6F77202B207D7A61714442322C225C2838365861"
           "3B4A4E437B3662422C3E5232474D793E7421264A5E5253797D3232407950387C513B70683A5C6D6A563D784C23792729792B4E7C63"
           "2F764A7B234F6324786764754F245C3655523E435F35733F4D32585B65586D613A2955797278624B"
           "3F4D59222C345F62394C702279714775333768342E7D5D77232876624E30634B5E3F525D22763C584667245C4E5A605B4B36"
           "635F234C65662456505A4B52295225662359455364544F64503B7124626B2B38603A2C752948225278665F5C202F204E444F67")))
      (concat
       "010"
       (base64-encode-string
        (secure-hash 'sha256
                     (concat
                      str
                      (decode-hex-string (nth 0 l))
                      (decode-hex-string (nth 1 l))
                      (decode-hex-string (nth 0 s)))
                     nil nil t)))))
  (defun dape-request-response (process seq command body &optional cb)
    (let ((object (and body (list :body body))))
      (dape-send-object
       process seq
       (thread-first
        object
        (plist-put :type "response")
        (plist-put :success t)
        (plist-put :request_seq seq)
        (plist-put :command command)))))
  (cl-defmethod dape-handle-request
      (process (command (eql handshake)) arguments)
    (message "hand: %S" (plist-get arguments :value))
    (dape-request-response
     process 2 "handshake"
     (list :signature (my-calc (plist-get arguments :value)))))

  (setq
   dape-cppvsdbg-command
   (expand-file-name
    "~/.emacs.d/.extension/extension/debugAdapters/vsdbg/bin/vsdbg.exe"))
  (add-to-list
   'dape-configs
   `(cppvsdbg
     modes
     (c-mode c-ts-mode c++-mode c++-ts-mode)
     command-cwd
     ,(file-name-directory dape-cppvsdbg-command)
     command
     dape-cppvsdbg-command
     command-args
     ("--interpreter=vscode")
     :type "cppvsdbg"
     :request "launch"
     :cwd dape-cwd-fn
     :program dape-find-file
     :stopAtEntry t
     :logging (:engineLogging t) ;; 开启调试
     ;; :MIMode "gdb"
     ;; ,(cond
     ;;   ((executable-find "gdb")
     ;;    "gdb")
     ;;   ((executable-find "lldb")
     ;;    "lldb"))
     ;; ,(cond
     ;;   ((executable-find "gdb")
     ;;    (executable-find "gdb"))
     ;;   ((executable-find "lldb")
     ;;    (executable-find "lldb"))))
     ))


  ;; python，测试没问题
  (add-to-list
   'dape-configs
   `(debugpy
     modes
     (python-ts-mode python-mode)
     command
     "python"
     command-args
     ("-m" "debugpy.adapter")
     :type "executable"
     :request "launch"
     :cwd dape-cwd-fn
     :program dape-find-file-buffer-default)))

(defvar f5-history '()) ;; session只记录list的值
(defun my-shell-switch ()
  "最近buffer和最近shell切换"
  (interactive)
  (let ((to-shell
         (not
          (string-match
           "\*[e]?shell\*" (buffer-name (current-buffer)))))
        (create-shell t)
        (showed-buffer-list (ep-tabbar-buffer-list)))
    ;; 当前为shell buffer，切换到最近其它buffer
    (cl-dolist
     (b (buffer-list))
     (if (string-match "\*[e]?shell\*" (buffer-name b))
         (when to-shell
           (switch-to-buffer b)
           (setq create-shell nil)
           (cl-return))
       (unless to-shell
         (when (memq b showed-buffer-list)
           (switch-to-buffer b)
           (cl-return)))))
    (when (and create-shell to-shell)
      (call-interactively 'shell))))
(defun my-f5 ()
  (interactive)
  (when current-prefix-arg
    (setq f5-history nil))
  (unless f5-history
    (setq f5-history (list (consult--read (list "debug" "shell")))))
  (if (equal "debug" (nth 0 f5-history))
      (if (functionp 'dape)
          (call-interactively 'dape-auto)
        (call-interactively 'gud-auto))
    (let ((current-prefix-arg nil)) ;; 屏蔽C-u的副作用
      (call-interactively 'my-shell-switch))))
(global-set-key (kbd "<f5>") 'my-f5)

(use-package god-mode
  :diminish (god-local-mode)
  :commands (god-local-mode)
  :init
  (setq
   initial-major-mode 'view-mode ;; 开启view以便使用god-mode
   god-mode-enable-function-key-translation nil ;; 禁止F1等
   god-mode-alist
   '((nil . "C-")
     ("m" . "M-") ;; g代表C-g，但是一但进入命令还是要C-g取消了
     ("M" . "C-M-")))

  ;; ESC代替C-g
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; 普通方法还不行
  (with-eval-after-load 'view
    (setq view-mode-map (make-sparse-keymap)))
  (add-hook
   'view-mode-hook
   (lambda ()
     (god-local-mode
      (if buffer-read-only
          +1
        -1))))
  ;; 方便god mode的键绑定
  (global-set-key (kbd "C-<") 'beginning-of-buffer)
  (global-set-key (kbd "C->") 'end-of-buffer)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  :config
  (define-key god-local-mode-map (kbd "i") 'view-mode)
  (define-key
   god-local-mode-map (kbd "SPC")
   (lambda ()
     (interactive)
     (if (eq major-mode 'dashboard-mode)
         (call-interactively 'dashboard-jump-to-projects)
       (call-interactively 'scroll-up-command)))) ;; C-SPC也可以
  (define-key god-local-mode-map (kbd "S-SPC") 'scroll-down-command)
  (define-key god-local-mode-map (kbd "w") 'scroll-down-command))

(add-to-list 'auto-mode-alist '("\\.jsfl\\'" . js-mode))

(use-package dashboard
  :load-path "~/.emacs.d/themes/emacs-dashboard-master"
  :init
  (setq
   dashboard-center-content t
   dashboard-banner-logo-title (format-time-string "%Y-%m-%d  %H:%M:%S")
   dashboard-projects-switch-function
   (lambda (project)
     (interactive)
     (message "%S" project)
     (let ((default-directory project))
       (project-switch-project project)))
   dashboard-recentf-show-base t
   dashboard-recentf-item-format "%-30s   %s"
   dashboard-projects-show-base t
   dashboard-projects-item-format "%-30s   %s"
   dashboard-set-footer nil
   dashboard-set-init-info nil
   dashboard-startup-banner 'logo
   dashboard-projects-backend 'project-el
   dashboard-items '((recents . 20) (projects . 10))
   dashboard-item-shortcuts '((recents . "r") (projects . "SPC")))
  ;; bypass require 'ffap加快启动速度
  (defun dashboard-load-bypass-ffap (orig feature &rest args)
    (unless (eq feature 'ffap)
      (apply orig feature args)))
  (advice-add 'require :around 'dashboard-load-bypass-ffap)
  :config
  (advice-remove 'require 'dashboard-load-bypass-ffap)
  (use-package ffap
    :commands (ffap-guesser))
  (custom-set-faces
   '(dashboard-items-face ((t (:weight unspecified)))))
  ;; 用god-mode的快捷键
  (define-key
   dashboard-mode-map (kbd "C-r") 'dashboard-jump-to-recents)
  (define-key
   dashboard-mode-map (kbd "C-SPC") 'dashboard-jump-to-projects) ;; P
  (define-key dashboard-mode-map (kbd "C-g") 'revert-buffer)
  (define-key
   dashboard-mode-map (kbd "C-d") 'dashboard-remove-item-under)
  (defun my-god-mode-init ()
    (god-local-mode 1)
    (dashboard-jump-to-recents)
    ;; (dashboard-jump-to-projects) ;; 初始化直接跳到project
    )
  (add-hook
   'dashboard-after-initialize-hook
   (lambda ()
     (my-god-mode-init)
     (add-hook 'dashboard-mode-hook 'my-god-mode-init) ;; fix revert-buffer没有god mode
     ))
  (dashboard-setup-startup-hook))

;; `sqlite-mode-open-file'查看sqlite数据库很好用啊！
(when (functionp 'sqlite-mode-open-file)
  (use-package sqlite-mode
    :defer t
    :config (add-hook 'sqlite-mode-hook 'god-local-mode)
    ;; for god-mode
    (define-key sqlite-mode-map (kbd "C-c") 'sqlite-mode-list-columns)
    (define-key sqlite-mode-map (kbd "C-g") 'sqlite-mode-list-tables)
    (define-key sqlite-mode-map (kbd "C-d") 'sqlite-mode-delete)))

(use-package ggtags
  :commands
  (ggtags--xref-backend
   ggtags-create-tags ggtags-update-tags ggtags--xref-find-tags)
  :init
  (add-hook 'xref-backend-functions #'ggtags--xref-backend 98)
  (setenv "GTAGSFORCECPP" "1") ;; 默认h不以cpp分析，导致分析不出c++类
  (defvar force-search-variable nil)

  ;; 对于找不到的如成员变量，就查reference。（其目的是为了少启动一次进程加快速度）
  (defun ggtags-reference-backend ()
    'ggtags-reference)
  (cl-defmethod xref-backend-identifier-at-point
      ((_backend (eql 'ggtags-reference)))
    (find-tag--default))
  (cl-defmethod xref-backend-identifier-completion-table
      ((_backend (eql 'ggtags-reference)))
    ;; 参考的https://github.com/zbelial/lspce/blob/master/lspce.el，这个可以实现空白处列举所有符号，暂时不需要这个功能
    (list
     (propertize (or (thing-at-point 'symbol) "")
                 'identifier-at-point
                 t)))
  (cl-defmethod xref-backend-definitions
      ((_backend (eql 'ggtags-reference)) symbol)
    (let ((force-search-variable t))
      (ggtags--xref-find-tags symbol 'definition)))
  (add-hook 'xref-backend-functions #'ggtags-reference-backend 99) ;; 非在原版后面，找不到就按global -srax查找
  ;; 实现xref preview对ggtags的支持，基本就是照搬`consult-xref--preview'逻辑，以后升级只要改动不大还是可以用的
  (with-eval-after-load 'consult-xref
    (define-advice consult-xref--preview
        (:around (fn &rest args) fallback)
      (let ((result (apply fn args))
            (open (consult--temporary-files))
            (preview (consult--jump-preview)))
        (lambda (action cand)
          (when
              (funcall result action cand) ;; 经测试没有正常preview返回的是marker，正常返回nil
            (let ((consult--buffer-display (ad-get-argument args 0)))
              (funcall preview
                       action
                       (when-let (loc
                                  (and cand
                                       (eq action 'preview)
                                       (xref-item-location cand)))
                         (let ((type (type-of loc)))
                           (pcase type
                             ('ggtags-xref-location
                              (consult--marker-from-line-column
                               (funcall open
                                        (ggtags-xref-location-file
                                         loc) ;; 要全路径不然open临时文件会失败
                                        )
                               (ggtags-xref-location-line loc)
                               (ggtags-xref-location-column
                                loc)))))))))))))
  :config
  (defadvice ggtags-global-build-command
      (after my-ggtags-global-build-command activate)
    "加上-srax支持成员变量，参考https://github.com/austin-----/code-gnu-global/issues/29"
    (when force-search-variable
      (setq ad-return-value (concat ad-return-value " -srax"))))
  (defadvice ggtags--xref-backend
      (around my-ggtags--xref-backend activate)
    "不检查`ggtags-completion-table'，避免多启动一次global进程影响速度"
    (setq ad-return-value 'ggtags) ;; 原版非函数时返回nil(GTAGS里没有成员变量，而GRTAGS里有，用global -sax可以查到)
    )
  (defadvice ggtags--xref-find-tags
      (around my-ggtags--xref-find-tags activate)
    "修复xref无效，问题出在调用`call-process'多了双引号，去掉就可以了"
    (cl-letf (((symbol-function #'shell-quote-argument)
               (lambda (argument &optional posix) argument)))
      ad-do-it)))


;; 这个比grugru要更灵活些，缺点是cursor会变，也没有`grugru-highlight-mode'高亮
(use-package cycle-at-point
  :defer t
  :init
  (global-set-key (kbd "C-j") 'cycle-at-point)
  (dec-placeholder-fun
   cycle-at-point
   cycle-at-point
   "~/.emacs.d/packages/cycle-at-point/emacs-cycle-at-point"
   '(cycle-at-point
     cycle-at-point-find-alphabet
     cycle-at-point-find-integer
     cycle-at-point-preset-c++-mode
     cycle-at-point-preset-c-mode
     cycle-at-point-preset-cmake-mode
     cycle-at-point-preset-emacs-lisp-mode
     cycle-at-point-preset-lang-en
     cycle-at-point-preset-python-mode))
  :config
  (defadvice cycle-at-point-preset-c-mode
      (after my-cycle-at-point-preset-c-mode activate)
    "追加自定义的"
    (setq ad-return-value
          (append
           (list
            (lambda () (cycle-at-point-find-include))) ;; 要排在<>的前面
           ad-return-value
           (list (lambda () (cycle-at-point-name-convertion))))))
  (define-advice cycle-at-point-preset (:around (orig-fn &rest args))
    "修复ts-mode的规则加载"
    (if (memq
         major-mode
         '(c-ts-mode c++-ts-mode python-ts-mode cmake-ts-mode))
        (let ((cycle-at-point-preset-override
               (replace-regexp-in-string
                "-ts-mode" "-mode" (symbol-name major-mode))))
          (apply orig-fn args))
      (apply orig-fn args)))
  (defun cycle-at-point-find-include ()
    "改变include的双引号和<>切换"
    (let ((result (list))
          (word (bounds-of-thing-at-point 'sexp))) ;; 目前仅支持在"<>所在位置
      (when word
        (setq word
              (buffer-substring-no-properties (car word) (cdr word)))
        (when (string-match-p "\"\\([a-z-_.]+\\)\"" word)
          (setq result
                (list
                 word
                 (replace-regexp-in-string
                  "\"\\([a-z-_.]+\\)\"" "<\\1>" word))))
        (when (string-match-p "<\\([a-z-_.]+\\)>" word)
          (setq result
                (list
                 word
                 (replace-regexp-in-string
                  "<\\([a-z-_.]+\\)>" "\"\\1\"" word)))))
      (list :data result)))

  ;; from https://github.com/akicho8/string-inflection/blob/master/string-inflection.el
  (defun string-inflection-underscore-function (str)
    "FooBar => foo_bar"
    (let ((case-fold-search nil))
      (setq str
            (replace-regexp-in-string
             "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
      (setq str
            (replace-regexp-in-string
             "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
      (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
      (setq str (replace-regexp-in-string "_+" "_" str))
      (downcase str)))
  (defun string-inflection-pascal-case-function (str)
    "foo_bar => FooBar"
    (setq str (string-inflection-underscore-function str))
    (mapconcat 'capitalize (split-string str "_") ""))
  (defun string-inflection-camelcase-function (str)
    "foo_bar => fooBar"
    (setq str
          (split-string (string-inflection-underscore-function str)
                        "_"))
    (concat
     (downcase (car str)) (mapconcat 'capitalize (cdr str) "")))
  (defun string-inflection-capital-underscore-function (str)
    "foo_bar => Foo_Bar"
    (setq str (string-inflection-underscore-function str))
    (mapconcat 'capitalize (split-string str "_") "_"))
  (defun string-inflection-upcase-function (str)
    "FooBar => FOO_BAR"
    (upcase (string-inflection-underscore-function str)))
  (defun string-inflection-kebab-case-function (str)
    "foo_bar => foo-bar"
    (let ((case-fold-search nil))
      (setq str (string-inflection-underscore-function str))
      (setq str (replace-regexp-in-string "_" "-" str))))
  (defun cycle-at-point-name-convertion ()
    "函数名位置"
    (let ((result (list))
          (word (bounds-of-thing-at-point 'symbol)))
      ;; 当前位置在函数名
      (when
          (and word
               (memq
                'font-lock-function-name-face
                (idle-highlight--faces-at-point (car word)))) ;; 如果在单词末尾就取不到face，需要在单词开始取face
        (setq word
              (buffer-substring-no-properties (car word) (cdr word)))
        (setq
         result
         (append
          (list word)
          (delete
           word
           (list
            (string-inflection-pascal-case-function word)
            ;; (string-inflection-camelcase-function word)
            ;; (string-inflection-capital-underscore-function word)
            (string-inflection-underscore-function word)
            (string-inflection-upcase-function word)
            (if (derived-mode-p 'emacs-lisp-mode)
                (string-inflection-kebab-case-function word)
              word))))))
      (list :data result))))

(use-package goto-addr
  :defer 0.8
  :init
  (global-set-key (kbd "C-c C-o") 'goto-address-at-point) ;; 跟org快捷键一致
  :config (global-goto-address-mode 1))

(if (fboundp 'treesit-language-available-p)
    ;; bin下载 https://github.com/lynnux/tree-sitter-module/releases
    (progn
      (use-package treesit
        :commands (treesit-parser-create)
        :defer t
        :init
        ;; M-x -ts-mode提取出来的
        (setq
         treesit-font-lock-level 4 ;; 最大化高亮
         all-ts-mode
         '((c++-ts-mode . cpp)
           bash-ts-mode
           c-ts-mode
           cmake-ts-mode
           (csharp-ts-mode. c-sharp)
           css-ts-mode
           dockerfile-ts-mode
           elixir-ts-mode
           go-mod-ts-mode
           go-ts-mode
           heex-ts-mode
           html-ts-mode
           java-ts-mode
           js-ts-mode
           json-ts-mode
           python-ts-mode
           ruby-ts-mode
           rust-ts-mode
           toml-ts-mode
           tsx-ts-mode
           typescript-ts-mode
           yaml-ts-mode))
        (cl-dolist
         (ts1 all-ts-mode)
         (let (ts
               dll)
           (pcase ts1
             (`(,ts-mode . ,ts-file)
              (setq ts ts-mode)
              (setq dll ts-file))
             (`,ts-mode
              (setq ts ts-mode)
              (setq dll
                    (intern
                     (replace-regexp-in-string
                      "-ts-mode" "" (symbol-name ts))))))
           ;; `fingertip'的`C-k'有问题，设置`forward-sexp-function'为nil修复
           (if nil ;;(memq ts '(c++-ts-mode))
               ;; 不开`ts-mode'的仍然加上treesit的parser，不然`fingertip'不能用
               (add-hook
                (intern
                 (concat
                  (replace-regexp-in-string
                   "-ts-mode" "" (symbol-name ts))
                  "-mode-hook"))
                #'(lambda () (treesit-parser-create dll)))
             (add-to-list
              'major-mode-remap-alist
              (cons
               (intern
                (concat
                 (replace-regexp-in-string
                  "-ts-mode" "" (symbol-name ts))
                 "-mode"))
               ts)))))

        ;; 对于一些没有`-ts-mode'的，需要下面这样，不然用`fingertip'会报错。 参考https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-treesit.el
        (add-hook
         'emacs-lisp-mode-hook
         #'(lambda () (treesit-parser-create 'elisp)))
        (add-hook
         'markdown-mode-hook
         #'(lambda () (treesit-parser-create 'markdown)))
        (add-hook
         'zig-mode-hook #'(lambda () (treesit-parser-create 'zig)))
        (add-hook
         'json-mode-hook #'(lambda () (treesit-parser-create 'json))))
      (use-package fingertip
        :commands (fingertip-mode)
        :init
        (dolist (hook
                 (list
                  'c-mode-common-hook
                  'c-mode-hook
                  'c++-mode-hook
                  'java-mode-hook
                  'haskell-mode-hook
                  'emacs-lisp-mode-hook
                  'lisp-interaction-mode-hook
                  'lisp-mode-hook
                  'maxima-mode-hook
                  'ielm-mode-hook
                  'sh-mode-hook
                  'makefile-gmake-mode-hook
                  'php-mode-hook
                  'python-mode-hook
                  'js-mode-hook
                  'go-mode-hook
                  'qml-mode-hook
                  'jade-mode-hook
                  'css-mode-hook
                  'ruby-mode-hook
                  'coffee-mode-hook
                  'rust-mode-hook
                  'rust-ts-mode-hook
                  'qmake-mode-hook
                  'lua-mode-hook
                  'swift-mode-hook
                  'web-mode-hook
                  'markdown-mode-hook
                  'llvm-mode-hook
                  'conf-toml-mode-hook
                  'nim-mode-hook
                  'typescript-mode-hook
                  'c-ts-mode-hook
                  'c++-ts-mode-hook
                  'cmake-ts-mode-hook
                  'toml-ts-mode-hook
                  'css-ts-mode-hook
                  'js-ts-mode-hook
                  'json-ts-mode-hook
                  'python-ts-mode-hook
                  'bash-ts-mode-hook
                  'typescript-ts-mode-hook))
          (add-hook hook #'(lambda () (fingertip-mode 1))))
        :config (define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

        ;; 也可以C-q 选中在直接(，[等
        (define-key
         fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
        (define-key
         fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
        (define-key
         fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
        (define-key
         fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
        (define-key fingertip-mode-map (kbd "M-s") 'fingertip-unwrap)

        ;; 修复cpp`C-k'bug
        ;; 测试for(auto | xxx)在|位置(fingertip-end-of-list-p (point) (line-end-position))执行为nil，而正常返回t(关闭ts-mode就正常了)
        (define-advice fingertip-common-mode-kill
            (:around (orig-fn &rest args) fixcpp)
          "不知道为啥对c/cpp只调用`kill-line'了事？"
          (cl-letf (((symbol-function #'derived-mode-p)
                     (lambda (_) nil)))
            (apply orig-fn args)))))
  (use-package tree-sitter
    :commands
    (tree-sitter-mode
     tree-sitter-force-update tree-sitter-setup-timer)
    :defer t
    :init
    (add-to-list 'load-path "~/.emacs.d/packages/tree-sitter")
    (add-to-list 'load-path "~/.emacs.d/packages/tree-sitter/core")
    (add-to-list 'load-path "~/.emacs.d/packages/tree-sitter/lisp")
    (add-to-list 'load-path "~/.emacs.d/packages/tree-sitter/langs")
    (setq
     tree-sitter-langs--testing t ;; 禁止联网check bin
     tsc-dyn-get-from nil ;; 
     tree-sitter-langs-git-dir nil ;; 禁止调用git
     tree-sitter-langs--dir "~/.emacs.d/packages/tree-sitter/langs"
     tsc-dyn-dir "~/.emacs.d/packages/tree-sitter/core")
    (defvar use-tree-sitter-hl-mode-hack nil) ;; 高亮用after-change-hook变timer模式
    :config
    ;; elisp没有高亮
    (add-to-list
     'tree-sitter-major-mode-language-alist
     '(emacs-lisp-mode . elisp))
    (use-package tree-sitter-langs)

    (defvar tree-sitter-idle-timer nil
      "如果不需要hl功能，只需要按需调用tree-sitter-force-update即可，如defun范围功能")
    (defun my/tree-sitter--after-change (beg new-end old-len)
      (when tree-sitter-idle-timer
        (cancel-timer tree-sitter-idle-timer))
      (setq tree-sitter-idle-timer
            (run-with-idle-timer 0.2 nil #'tree-sitter-force-update)))
    (defun tree-sitter-force-update ()
      (setq tree-sitter-tree nil) ;; 必须设置为nil，否则不刷新
      (tree-sitter--do-parse))
    (when use-tree-sitter-hl-mode-hack
      (defadvice tree-sitter--setup
          (after my-tree-sitter--setup activate)
        "去掉hook，改为timer模式"
        (remove-hook
         'after-change-functions #'tree-sitter--after-change
         :local)
        (remove-hook
         'before-change-functions #'tree-sitter--before-change
         :local))
      (defun tree-sitter-setup-timer (&optional on)
        (if on
            (add-hook
             'after-change-functions #'my/tree-sitter--after-change
             nil
             :local)
          (remove-hook
           'after-change-functions #'my/tree-sitter--after-change
           :local)))))
  ;; tsc里的(require 'dired-aux) 导致dired被加载了
  (use-package tree-sitter-hl
    :diminish (tree-sitter-mode)
    :commands (tree-sitter-hl-mode)
    ;; 来自`tree-sitter-major-mode-language-alist'
    :hook
    ((agda-mode
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
      emacs-lisp-mode)
     .
     (lambda ()
       ;; (tree-sitter-hl-mode)
       (grammatical-edit-mode 1)))
    :config
    ;; 必须去掉jit-lock-after-change，否则一输入会造成后面显示不正常
    (defun remove-jit-lock-after-change ()
      (when tree-sitter-hl-mode
        (remove-hook 'after-change-functions 'jit-lock-after-change
                     t)))
    (when use-tree-sitter-hl-mode-hack
      (add-hook 'font-lock-mode-hook 'remove-jit-lock-after-change) ;; font-lock-mode是较后开启，所以需要hook
      (add-hook
       'tree-sitter-hl-mode-hook
       (lambda ()
         (tree-sitter-setup-timer tree-sitter-hl-mode)
         (if tree-sitter-hl-mode
             (remove-jit-lock-after-change)
           (add-hook 'after-change-functions 'jit-lock-after-change
                     nil
                     t))))))
  (use-package grammatical-edit
    :commands (grammatical-edit-mode)
    :config
    ;; 会影响kill-ring，作者还不改，暂时不用了
    (define-key
     grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

    ;; 也可以C-q 选中在直接(，[等
    (define-key
     grammatical-edit-mode-map
     (kbd "M-\"")
     'grammatical-edit-wrap-double-quote)
    (define-key
     grammatical-edit-mode-map
     (kbd "M-[")
     'grammatical-edit-wrap-bracket)
    (define-key
     grammatical-edit-mode-map
     (kbd "M-{")
     'grammatical-edit-wrap-curly)
    (define-key
     grammatical-edit-mode-map
     (kbd "M-(")
     'grammatical-edit-wrap-round)
    (define-key
     grammatical-edit-mode-map (kbd "M-s") 'grammatical-edit-unwrap)

    ;; (define-key grammatical-edit-mode-map (kbd "M-<return>") 'grammatical-edit-jump-out-pair-and-newline)

    (when nil
      ;; 支持hungry delete
      (dolist (key '([remap delete-char] [remap delete-forward-char]))
        (define-key
         grammatical-edit-mode-map key
         ;; menu-item是一个symbol，而且很有趣的是，F1-K能实时知道是调用哪个函数
         '(menu-item
           "maybe-grammatical-edit-forward-delete" nil
           :filter
           (lambda (&optional _)
             (unless (looking-at-p "[[:space:]\n]")
               #'grammatical-edit-forward-delete)))))

      (dolist (key
               '([remap backward-delete-char-untabify]
                 [remap backward-delete-char]
                 [remap delete-backward-char]))
        (define-key
         grammatical-edit-mode-map key
         '(menu-item
           "maybe-grammatical-edit-backward-delete" nil
           :filter
           (lambda (&optional _)
             (unless (looking-back "[[:space:]\n]" 1)
               #'grammatical-edit-backward-delete))))))))

;; 保存当前windows配置为bookmark
(use-package burly
  :commands
  (burly-bookmark-handler
   burly-bookmark-windows
   ;; burly-bookmark-frames
   burly-kill-buffer-url
   ;; burly-kill-frames-url
   burly-kill-windows-url
   burly-open-bookmark
   burly-open-last-bookmark
   burly-open-url))

(autoload 'elisp-autofmt-buffer "tools/elisp-autofmt.el" "" t)
(use-package elisp-autofmt
  :defer t
  :init
  (with-eval-after-load 'elisp-mode
    (define-key
     emacs-lisp-mode-map [(meta f8)]
     (lambda ()
       "禁止message提示"
       (interactive)
       (if buffer-read-only
           (signal 'text-read-only nil)
         (let ((inhibit-message t))
           (call-interactively 'elisp-autofmt-buffer))
         (message "elisp format done.")))))
  :config
  ;; 使用内置的use-package格式化会有问题，必须加把use-package加入到`load-path'
  ;; see https://codeberg.org/ideasman42/emacs-elisp-autofmt/issues/4
  (add-to-list
   'load-path "~/.emacs.d/packages/use-package/use-package-master")
  (setq-default elisp-autofmt-load-packages-local '("use-package")))

(use-package format
  :defer t
  :init
  ;; (setq format-alist nil) ;; `format-decode'会被c函数`insert-file-contents'调用，而里面都是些用不到的文件头，故而可以屏蔽加快启动。
  )
(use-package enriched
  :defer t
  :init
  (with-eval-after-load 'font-lock
    (define-advice turn-on-font-lock (:around (orig-fn &rest args))
      "对`enriched-mode'禁用font-lock-mode"
      (when (not (and (boundp 'enriched-mode) enriched-mode))
        (apply orig-fn args))))
  :config
  ;; TODO：`enriched-toggle-markup'切换后字符的属性并没有去掉
  )

(use-package eshell
  :defer t
  :config
  ;; eshell的tab就是补全，默认很卡的，改为只枚举当前目录的文件
  (define-advice pcomplete-completions-at-point
      (:around (orig-fn &rest args))
    (esy/file-capf)))

;; 处理shell的补全，也改为只枚举当前目录
(use-package comint
  :defer t
  :config
  (define-advice comint-completion-at-point
      (:around (orig-fn &rest args))
    (if (eq major-mode 'shell-mode)
        (esy/file-capf)
      (apply orig-fn args))))

;; 好的theme特点:
;; treemacs里git非源码里区别明显(doom-one)，
;; eldoc参数当前哪个参数很明显
;; tabbar被修改的*文件有明显显示(spacemacs)
;; 当前buffer modeline背景色(doom-dark+)
;; helm occur关键字高亮明显，不明显就换了吧那个暂时还不知道如何定制


;; 在modeline提示bell，这个功能太实用了，因为bell被禁止发声了
(autoload
  'doom-themes-visual-bell-config
  "~/.emacs.d/themes/themes-master/extensions/doom-themes-ext-visual-bell"
  ""
  nil
  nil)
(doom-themes-visual-bell-config)

;; 这是需要最后加载
;; doom搜集themes系列
;; https://github.com/doomemacs/themes
(use-package doom-themes
  :disabled
  :if (display-graphic-p)
  :load-path "~/.emacs.d/themes/themes-master"
  :config
  (setq
   doom-themes-enable-bold nil
   doom-themes-enable-italic nil)
  (autoload 'doom-themes-org-config "extensions/doom-themes-ext-org"
    ""
    nil
    nil)

  (defun get-theme (x)
    (intern
     (replace-regexp-in-string
      ".*/" ""
      (string-replace "-theme.el" "" x))))
  ;; 随机加载theme，不对的话建议重启emacs，不然上次的theme可能会干扰本次theme
  (defun random-load-doom-theme (tl)
    (interactive)
    (let* ((th (nth (mod (random t) (length tl)) tl)))
      (message "load-doom-theme: %s" (symbol-name th))

      ;; before load
      (cond
       ((eq th 'spacemacs-dark)
        ;; 背景色合适，但颜色绿色太多了，部分颜色要改
        ;; https://github.com/nashamri/spacemacs-theme#override-themes-colors
        (custom-set-variables
         '(spacemacs-theme-custom-colors
           '((base . "#bbc2cf") ;文本 tangotango
             (comment . "#888a85") ; 注释 tangotango
             (border . "#292b2e") ; border太丑了
             ))))
       ((eq th 'doom-one)
        ;; (setq doom-one-brighter-comments t)
        ))

      (load-theme th t)

      ;; after load
      ;; (doom-themes-org-config)
      ))
  ;; (random-load-doom-theme (mapcar 'get-theme (directory-files "~/.emacs.d/themes/themes-master/themes" t "^[a-zA-Z0-9].*.el$")))
  (random-load-doom-theme
   (list
    ;; 'doom-horizon
    'modus-vivendi
    ;; 'modus-vivendi-tritanopia
    ;; 'doom-snazzy
    ;; 'doom-city-lights
    ;; 'doom-material
    ;; 'doom-tomorrow-night
    ;; 'doom-one
    ;; 'spacemacs-dark
    )))
(load-theme 'modus-vivendi t)

;; 各种theme修补
(let ((th (car custom-enabled-themes)))
  (cond
   ((eq th 'spacemacs-dark)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#888a85"))
   ((eq th 'doom-horizon)
    (custom-set-faces
     '(tab-line-tab-current ((t (:foreground "#fdf0ed"))))
     '(tab-line-tab-inactive ((t (:foreground "#c7c9cb"))))
     '(tab-line-tab-special ((t (:weight unspecified))))
     '(line-number ((t (:foreground "#6F6F6F")))) ;; 行号
     '(font-lock-comment-face ((t (:foreground "#6F6F6F")))) ;; 注释 
     '(corfu-current ((t (:foreground "red"))))
     '(consult-preview-cursor ((t (:inherit highlight)))) ;; TODO: 整个单词高亮，虽然很多情况下不一定是一个词
     '(outline-2 ((t (:foreground "#f9cec3")))) ;; org-org-level-2和org-headline-done互换
     '(org-headline-done ((t (:foreground "#6c6f93")))))
    (set-face-attribute 'region nil :background "#555555")
    ;; (set-face-attribute 'region nil
    ;;                     :background "#4C7073"
    ;;                     :foreground "Black"
    )
   ((eq th 'modus-vivendi)
    (custom-set-faces
     '(consult-file ((t (:foreground nil))))
     '(consult-bookmark ((t (:foreground nil))))))
   (t))
  (when (string-prefix-p "doom" (symbol-name th))
    ;; doom没有处理hi-lock的颜色
    (custom-set-faces
     '(hi-yellow ((t (:foreground "black" :background "yellow"))))
     '(hi-pink ((t (:foreground "black" :background "HotPink"))))
     '(hi-green ((t (:foreground "black" :background "green"))))
     '(hi-blue ((t (:foreground "black" :background "DodgerBlue"))))
     '(hi-salmon
       ((t (:foreground "black" :background "light salmon"))))
     '(hi-aquamarine
       ((t (:foreground "black" :background "aquamarine"))))
     '(hi-black-b ((t (:foreground "white" :background "black"))))
     '(hi-blue-b ((t (:foreground "DodgerBlue" :background "black"))))
     '(hi-red-b ((t (:foreground "red" :background "black"))))
     '(hi-green-b ((t (:foreground "green" :background "black"))))
     '(hi-black-hb ((t (:foreground "orange" :background "black"))))))
  (when (not (string-prefix-p "doom" (symbol-name th)))
    (set-face-attribute 'doom-themes-visual-bell nil
                        :background "#ff6c6b")))

;; 所有theme共用
;; paren加下划线，参考的spacemacs
(set-face-attribute 'show-paren-match nil :underline t :weight 'bold)
(custom-set-faces '(header-line ((t (:weight bold)))))
