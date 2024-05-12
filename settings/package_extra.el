;; -*- coding: utf-8; lexical-binding: t; -*-
;; 非官方自带packages的设置
;; benchmark: 使用profiler-start和profiler-report来查看会影响emacs性能，如造成卡顿的命令等
;; 拖慢gui测试：C-x 3开两个窗口，打开不同的buffer，C-s搜索可能出现比较多的词，测试出doom modeline和tabbar ruler比较慢
;; 参考https://gitee.com/advanceflow/elisp/blob/main/%E9%99%84%E5%BD%95H-%E6%A0%87%E5%87%86%E9%92%A9%E5%AD%90.org 查看各种hook，能去掉就去掉
;; use-package的好处之一是defer可以设置延迟几秒加载！光yas一项就提升了启动速度
;; :load-path不是延迟设置

;; 使用老版本提示
;; 28.0.50，1.查看eln-cache，会有几个文件的TMP一直生成，找到删除对应elc就可以了，不然每次emacs启动都会去编译 2.删除自带的python不然lsp有问题
;; 在使用了org-roam的功能后退出emacs会崩溃，最后发现应该是native-comp的问题，有个gnus-art的文件比较大，但是跟上面一样只有tmp生成，找到删除gnus-art.elc就可以了
;; 以后的崩溃问题都可以参考这个处理，一般是eln-cache里有tmp没编译好造成emacs退出时崩溃

(when (bound-and-true-p enable-feature-win32-only)
  (setq enable-feature-win32-only
        (string-equal system-type "windows-nt")))

(defun delay-require-libs (path list &optional keep-load-path)
  "临时设置`load-path'并require需要的库"
  (add-to-list 'load-path path)
  (dolist (l list)
    (require l))
  (unless keep-load-path
    (setq load-path (delete path load-path))))

(defmacro dec-placeholder-fun
    (fn feature path list &optional keep-load-path)
  "对可以延迟调用的命令，延迟设置`load-path'，并在require后还原`load-path'"
  `(defun ,fn (&optional arg)
     (interactive)
     (unless (featurep ',feature)
       (fmakunbound ',fn) ;; 取消本函数定义
       (delay-require-libs ,path ,list ,keep-load-path)
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
  (ensure-latest "~/.emacs.d/packages/org/org-roam-main.zip")
  (ensure-latest "~/.emacs.d/packages/org/emacsql-master.zip")
  (ensure-latest "~/.emacs.d/packages/tools/rg.el-master.zip")
  (ensure-latest "~/.emacs.d/packages/tools/elfeed-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/use-package/use-package-master.zip")
  (ensure-latest "~/.emacs.d/themes/emacs-dashboard-master.zip")
  (ensure-latest
   "~/.emacs.d/packages/cycle-at-point/emacs-cycle-at-point-main.zip"
   nil
   "emacs-cycle-at-point") ;; codeberg.org上zip里的文件夹名不含-main
  (ensure-latest "~/.emacs.d/packages/tools/zhengma.zip" t)
  (ensure-latest "~/.emacs.d/packages/lsp/lspce-master.zip"))

;; 用于use-package避免自动设置:load-path
(defun my-eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun get-file-time (file)
  "获取文件的修改时间"
  (interactive "P")
  (time-convert (file-attribute-modification-time
                 (file-attributes file))
                'integer))
(defun get-file-times (file)
  "获取文件的修改时间"
  (interactive "P")
  (file-attribute-modification-time (file-attributes file)))

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
       (concat "7z x -o" extdir " " expand-zip))
      (unless no-compile
        (message (format "Building %s ..." target-dir))
        (straight--build-compile target-dir))
      ;; 创建空的时间戳文件
      (unless (file-exists-p
               (expand-file-name ".cache" user-emacs-directory))
        (make-directory (expand-file-name ".cache"
                                          user-emacs-directory)
                        t))
      (f-touch check-file)
      (set-file-times check-file (get-file-times expand-zip)) ;; 修改文件时间为zip的时间
      )))

(add-to-list 'load-path "~/.emacs.d/packages")

;; (ensure-latest "~/.emacs.d/settings/test.zip")
;; F1 v查看变量 sanityinc/require-times，或者M-x `sanityinc/require-times' 正常一页就显示完了，目前11个包
;; 有个几年前的封装没有用，直接用的原版的 https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
(use-package init-benchmarking
  :config
  (add-hook
   'after-init-hook
   (lambda ()
     ;; (advice-remove 'require 'sanityinc/require-times-wrapper)
     )))

;; !themes要放到最后，内置theme查看 M-x customize-themes
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; https://github.com/phikal/compat.el
(use-package compat
  :defer t
  :load-path "~/.emacs.d/packages/minibuffer/compat-main"
  :config
  ;; 加载后可以直接从`load-path'去掉了
  (setq load-path
        (delete
         "~/.emacs.d/packages/minibuffer/compat-main" load-path)))

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
  :commands (-distinct -replace-at -filter))

(use-package s
  :commands (s-split s-word-wrap))

(use-package f
  :commands (f-mkdir-full-path f-touch)
  :init (provide 'f-shortdoc))

(use-package eldoc
  :if (bound-and-true-p enable-feature-builtin)
  :defer t
  :diminish (eldoc-mode)
  :init
  (setq eldoc-echo-area-use-multiline-p nil) ;; 不要多行显示
  :config
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore) ;; 在pre-command-hook里影响性能
  (with-eval-after-load 'grammatical-edit
    ;; 自作聪明就给加了eldoc
    (eldoc-remove-command-completions "grammatical-edit-"))
  (with-eval-after-load 'fingertip
    ;; 自作聪明就给加了eldoc
    (eldoc-remove-command-completions "fingertip-"))

  ;; 使用tooltip显示eldoc，`x-show-tip'是原生c实现的，不卡
  (defun get-eldoc-msg ()
    (or eldoc-last-message
        (when eldoc--doc-buffer
          (with-current-buffer eldoc--doc-buffer
            (buffer-substring
             (goto-char (point-min))
             (progn
               (end-of-visible-line)
               (point)))))))
  (defun eldoc-tooltip-display (docs _interactive)
    ;; 可能在focus-out后才会调用到，这里检查是否已经focus-out了
    (when (frame-parameter nil 'last-focus-update)
      (let* ((p (window-absolute-pixel-position))
             (x (car p))
             (h (line-pixel-height))
             (y
              (if header-line-format
                  (- (cdr p) h) ;; 修复开启`header-line-format'时y值不正确
                (cdr p)))
             (text (get-eldoc-msg)))
        (when (and p text (not (equal text "")))
          (setq y (+ y h))
          ;; (add-face-text-property 0 (length text) 'tooltip t text)
          (x-show-tip
           text
           (selected-frame)
           `((name . "tooltip")
             (internal-border-width . 2)
             (border-width . 1)
             (no-special-glyphs . t)
             (left . ,x) ;; 设置left后会忽略`x-show-tip'最后的DX参数
             (top . ,y) ;; 设置top后会忽略`x-show-tip'最后的DY参数
             (foreground-color
              . ,(face-attribute 'tooltip :foreground))
             (background-color
              . ,(face-attribute 'tooltip :background))
             ;; (border-color . "#ffff00")
             )
           20
           0
           0)))))
  ;; 目前就仅在dape调试时用吧
  (with-eval-after-load 'dape
    (define-advice dape--add-eldoc-hook (:after (&rest args) my2)
      ;; 因为我们是用的`eldoc--doc-buffer'，所以必须运行在它更新后
      (add-hook 'eldoc-display-functions #'eldoc-tooltip-display 10 t)
      (add-hook 'pre-command-hook 'x-hide-tip nil t) ;; 不隐藏的话，就一直显示不太好
      (add-hook 'focus-out-hook 'x-hide-tip nil t))))

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

(use-package neotree
  :commands (neotree-toggle)
  :init
  (setq
   neo-theme 'nerd
   neo-window-width 32
   neo-create-file-auto-open t
   neo-show-updir-line t
   neo-mode-line-type 'neotree
   neo-smart-open t
   neo-show-hidden-files t
   neo-auto-indent-point t
   neo-vc-integration nil)
  (global-set-key (kbd "<C-f1>") 'neotree-toggle)
  :config
 (add-hook
  'neotree-mode-hook
  (lambda ()
    (hl-line-mode 1) ;; 指明当前行
    (face-remap-add-relative 'hl-line '(:background "#666")) ;; 使更清楚
    ;; (face-remap-add-relative 'default :height 105) ;; 仅大小
    (defface neotree-face
      '((t :family "BlinkMacSystemFont" :height 110)) ;; 参考的github字体
      "")
    (buffer-face-set 'neotree-face)
    (setq line-spacing 0.1) ;; 行高
    ))
  (define-key neotree-mode-map (kbd "C-l") 'neotree-select-up-node)
  (defun projectile-project-root ()
    (project-root (project-current t)))
  (defun projectile-project-p ()
    (project-current nil))
  
  ;; 实现自动follow
  (defvar neotree-follow-to-file-timer nil)
  (defvar neotree-follow-file-idle-delay 0.5)
  (defun neotree-follow-file-advice (&rest args)
    (when (neo-global--window-exists-p)
      (when neotree-follow-to-file-timer
        (cancel-timer neotree-follow-to-file-timer))
      (setq neotree-follow-to-file-timer
            (run-with-idle-timer
             neotree-follow-file-idle-delay
             nil
             #'neotree-follow-to-file))))
 (defvar neotree-follow-file-commands
   '(volatile-kill-buffer
     switch-to-buffer
     xref-pop-to-location ;; for all xref
     ))
  (cl-dolist
      (jc neotree-follow-file-commands)
    (advice-add jc :after #'neotree-follow-file-advice))
  (defun neotree-follow-to-file ()
    "直接用neotree-follow-file，hl-line的行有问题，这里修复下"
    (interactive)
    (ignore-errors
      (let ((neo-toggle-window-keep-p t)) ;; 不选中neotree窗口
        (neotree-show))
      ;; 修复高亮
      (when (neo-global--window-exists-p)
        (with-current-buffer (neo-global--get-buffer nil)
          (hl-line-highlight)
          ))
      ))
  )

;; helm M-x也会导致dired被加载，加载tree-sittr也会，只能在scratch里执行(featurep 'dired+)看
(use-package dired
  :if (bound-and-true-p enable-feature-dired)
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
   dired-clean-confirm-killing-deleted-buffers nil ;; 不访问是否kill已删除掉的文件的buffer
   )
  (put 'dired-find-alternate-file 'disabled nil) ;; 避免使用该函数时提示
  (global-set-key [remap dired] 'dired-jump) ;; 直接打开buffer所在目录，无须确认目录
  (use-package dired-recent
    :defer t
    :init
    (dec-placeholder-fun dired-recent-open
                         dired-recent
                         "~/.emacs.d/packages/dired"
                         '(dired-recent)
                         t)
    (dec-placeholder-fun dired-recent-mode
                         dired-recent
                         "~/.emacs.d/packages/dired"
                         '(dired-recent)
                         t)
    (setq dired-recent-mode-map nil) ;; 禁止它注册C-x C-d
    (bind-key* (kbd "C-c C-d") 'dired-recent-open)
    (bind-key* (kbd "C-c d") 'dired-recent-open)
    :config
    (with-eval-after-load 'marginalia
      ;; 效果跟consult--read带:category 'file一样，embark也能正常识别了
      (add-to-list
       'marginalia-command-categories '(dired-recent-open . file))))

  ;; bug较多能用，好处是支持diredful、diff-hl显示
  (use-package dired-sidebar
    :disabled
    :commands (dired-sidebar-toggle-sidebar)
    :init
    (global-set-key
     (kbd "<C-f1>")
     (lambda ()
       (interactive)
       (unless (featurep 'dired-subtree)
         (delay-require-libs
          "~/.emacs.d/packages/dired/dired-hacks-master"
          '(dired-subtree)))
       (call-interactively 'dired-sidebar-toggle-sidebar)))
    (setq
     dired-sidebar-theme 'ascii
     dired-sidebar-width 50
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
    '(volatile-kill-buffer
      switch-to-buffer
      xref-pop-to-location ;; for all xref
      ))
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

(use-package minibuffer
  :defer t
  :config
  ;; 屏蔽鼠标点击最下一行显示message buffer
  (define-key minibuffer-inactive-mode-map (kbd "<mouse-1>") nil))

;; 保存cursor位置
(use-package saveplace
  :if (bound-and-true-p enable-feature-builtin)
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
  :if (bound-and-true-p enable-feature-builtin)
  :init
  (setq recentf-save-file
        (expand-file-name ".recentf" user-emacs-directory))
  (setq
   recentf-max-saved-items 500
   recentf-auto-cleanup 'never ; 默认造成开机启动卡10秒！
   )
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
  :hook (after-init . recentf-mode)
  :config
  ;; 去掉不必要的hook
  (defconst recentf-used-hooks
    '((find-file-hook recentf-track-opened-file)
      (kill-emacs-hook recentf-save-list))
    "Hooks used by recentf."))

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
  (setq session-save-file-coding-system 'utf-8)
  (setq
   session-globals-include
   '(
     ;;(kill-ring 50) ;; 用ditto就行了
     (session-file-alist 100 t)
     query-replace-defaults ;; M-r replace-string默认值
     )
   session-locals-include nil)
  (defvar initial-scratch-message-history nil) ;; 测试大于`session-globals-max-string'(默认1024保存不下)
  :config
  (define-advice session-initialize (:after (&rest args) my)
    (when initial-scratch-message-history
      (with-current-buffer (get-buffer-create "*scratch*")
        (insert (car initial-scratch-message-history))
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil) ;; 防止undo
        ;; 这里开启view-mode还不行，只能设置`initial-major-mode'有效
        )))
  (define-advice session-save-session (:before (&rest args) my)
    (with-current-buffer (get-buffer-create "*scratch*")
      (setq initial-scratch-message-history (list (buffer-string)))))
  (add-hook 'after-init-hook 'session-initialize)
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

(use-package goto-chg
  :commands (goto-last-change)
  :init
  (setq session-initialize (list 'session))
  (defun goto-last-change-check-in-session ()
    "对于新打开的文件，就在session的`session-file-alist'找(`session-jump-to-last-change'使用有bug)"
    (interactive)
    (condition-case nil
        (call-interactively 'goto-last-change)
      (error
       (ignore-errors
         (goto-char
          (nth
           6
           (assoc (session-buffer-file-name) session-file-alist)))))))
  (global-set-key (kbd "C-x C-/") 'goto-last-change-check-in-session))

;; 用session就够了
(use-package savehist
  :disabled
  :if (bound-and-true-p enable-feature-builtin)
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
  :if (bound-and-true-p enable-feature-builtin)
  :commands (display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start 3)
  (defun enable-display-line-numbers-mode ()
    ;; org会开启`olivetti'，为了美观不需要显示行号
    (unless (eq major-mode 'org-mode)
      (display-line-numbers-mode)))
  :hook (find-file . enable-display-line-numbers-mode))

;;; better C-A C-E
(use-package mwim
  :if (bound-and-true-p enable-feature-navigation)
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
  :if (bound-and-true-p enable-feature-edit)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key [remap undo] 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  (global-set-key (kbd "M-/") 'undo-fu-only-redo)
  (global-set-key (kbd "C-x u") (defrepeater #'undo-fu-only-redo)) ;; 这个其实是undo，习惯undo tree这个快捷键了
  )

(use-package vundo
  :if (bound-and-true-p enable-feature-edit)
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
      volatile-kill-buffer
      vc-diff))
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
  :if (bound-and-true-p enable-feature-edit)
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
    (when
        (and
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
  (define-advice tempel--prefix-bounds
      (:around (orig-fn &rest args) my)
    "支持lc|aaa得到lc"
    (let ((bounds (cdr-safe (my-get-str-at-point))))
      (when bounds
        (cons (car bounds) (nth 1 bounds))))))

;; 利用tempel实现yas功能
(use-package yasnippet
  :if (bound-and-true-p enable-feature-edit)
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
  :if (bound-and-true-p enable-feature-edit)
  :commands (aya-create aya-expand)
  :init
  ;; 这个其实还挺好用的，用~xxx代替要替换的，或者`xxx'，多行要选中单行不用选中
  (global-set-key (kbd "C-c y") #'aya-create)
  (global-set-key (kbd "C-c e") #'aya-expand))


;; undo-fu作者写的有保障，不添加post-command-hook，高亮只是屏幕可见区域
(use-package idle-highlight-mode
  :if (bound-and-true-p enable-feature-gui)
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
  (add-hook 'before-revert-hook #'idle-highlight--unhighlight) ;; 修复revert之前高亮不消除问题
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
    (advice-add
     #'highlight-symbol-at-point
     :before #'idle-highlight--unhighlight))
  ;; 输入时高亮效果不是那么好，这里设置鼠标点击时高亮点击处
  (define-advice idle-highlight--highlight
      (:around (orig-fn &rest args) my)
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
          (apply orig-fn args))
      (apply orig-fn args)))
  (with-eval-after-load 'multiple-cursors-core
    (add-hook
     'multiple-cursors-mode-enabled-hook 'idle-highlight--unhighlight)
    (add-hook
     'multiple-cursors-mode-disabled-hook
     (lambda () (idle-highlight--time-callback-or-disable))))
  (global-idle-highlight-mode)

  ;; 让支持region选中高亮
  (defun should-highlight-regin ()
    (and (use-region-p)
         ;; 仅支持单行内的选中高亮，可以避免选中多行时显示的小问题
         (eq
          (line-number-at-pos (region-beginning))
          (line-number-at-pos (region-end)))
         ;; 排除选中一个字符，经常鼠标点击会选中一个字符
         (not (eq 1 (- (region-end) (region-beginning))))))
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

(use-package cursor-chg
  :init (setq curchg-change-cursor-on-input-method-flag nil)
  :config
  ;; (setq curchg-default-cursor-type '(hbar . 3)) ;; F1 v查看`cursor-type'有哪些类型
  (change-cursor-mode 1))

;;crosshairs不好用，只要vline就行了		
(autoload 'vline-mode "vline" nil t)
(global-set-key [(control ?|)] 'vline-mode)

(use-package hl-line
  :disabled
  :if (and (display-graphic-p) (bound-and-true-p enable-feature-gui))
  :defer 0.6
  :config (global-hl-line-mode t))

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
  :if (bound-and-true-p enable-feature-edit)
  :defer t
  :init
  (autoload 'corfu-mode "corfu/corfu-main/corfu" "" nil)
  (autoload 'cape-wrap-buster "corfu/cape" "" nil)
  (autoload 'cape-wrap-noninterruptible "corfu/cape" "" nil)
  (autoload 'cape-capf-buster "corfu/cape" "" nil)
  (autoload 'cape-capf-noninterruptible "corfu/cape" "" nil)
  (autoload 'cape-capf-super "corfu/cape" "" nil) ;; 只对静态有效
  (autoload 'cape-wrap-silent "corfu/cape" "" nil)
  (use-package cape-keyword
    :defer t
    :init
    (dec-placeholder-fun cape-keyword cape-keyword "~/.emacs.d/packages/corfu" '(cape cape-keyword))
    :config
    (add-to-list 'cape-keyword-list '(conf-toml-mode "features" "default-features")))
  (setq
   corfu-cycle t
   corfu-auto t
   corfu-auto-prefix 2
   corfu-preview-current nil ; 避免直接上屏，有时候输入完了要马上C-n/C-p，这时只需要按个C-g就可以了，而不需要再删除上屏的
   corfu-auto-delay 0.01 ;; 这个很影响体验的！
   ;; corfu-quit-at-boundary nil ;; 可以用M-空格来分词
   corfu-quit-no-match t ;; 没有match时退出，不然有个No match影响操作
   corfu-min-width 30 ;; 不知道怎么回事，有时候显示不全但补全功能正常
   )

  ;; 偷偷隐藏创建corfu的frame加快首次使用
  (run-with-idle-timer
   0.5 nil
   (lambda ()
     (let ((inhibit-message t))
       (load "corfu/corfu-main/corfu"))
     (cl-letf (((symbol-function #'make-frame-visible)
                (lambda (frame) frame)))
       (corfu--popup-show
        (posn-at-point (point))
        0
        8
        '(#("No match" 0 8 (face italic)))))))
  :config
  ;; (add-to-list 'completion-at-point-functions 'cape-keyword)
  ;; 所有capf一起出结果！测试有问题`cape-capf-super'只能处理简单的
  (defmacro my-capf-super (orig-fn args1)
    `(cl-letf* ((org-run-hook-wrapped
                 (symbol-function #'run-hook-wrapped))
                ((symbol-function #'run-hook-wrapped)
                 (lambda (&rest args)
                   ;; 其它功能也会调用run-hook-wrapped，这里只处理capf补全
                   (if (eq (car args) 'completion-at-point-functions)
                       (let (backends)
                         (dolist (f
                                  (append
                                   completion-at-point-functions
                                   (default-value
                                    'completion-at-point-functions)))
                           (when (functionp f)
                             (cl-pushnew f backends)))
                         (setq backends
                               (reverse (delete-dups backends)))
                         (let ((completion-at-point-functions
                                (list
                                 (apply 'cape-capf-super backends))))
                           (apply org-run-hook-wrapped args)))
                     (apply org-run-hook-wrapped args)))))
       (apply ,orig-fn ,args1)))
  ;; (define-advice corfu--auto-complete-deferred
  ;;     (:around (orig-fn &rest args1) my)
  ;;   (my-capf-super orig-fn args1))
  ;; (define-advice completion-at-point
  ;;     (:around (orig-fn &rest args1) my)
  ;;   (my-capf-super orig-fn args1))
  
  ;; 不知道为什么加这个，加了反而在弹出corfu时eldoc消失了，先试试去掉
  (advice-remove
   #'eldoc-display-message-no-interference-p #'corfu--eldoc-advice)
  (add-to-list
   'load-path "~/.emacs.d/packages/corfu/corfu-main/extensions")
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
  (define-advice corfu--update (:around (orig-fn &rest args) my)
    (let
        ((inhibit-message t)) ;; 我们hack的dabbrev运行时会提示错误，实际功能正常
      (apply orig-fn args)))
  (define-advice corfu--make-frame (:around (orig-fn &rest args) my)
    (let ((frame (apply orig-fn args)))
      (when (frame-live-p frame)
        (set-face-background (if (facep 'child-frame-border)
                                 'child-frame-border
                               'internal-border)
                             "#6F6F6F"
                             frame))
      frame))

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
      (dabbrev--reset-global-variables)
      (setq dabbrev-case-fold-search nil)
      (if has-dabbrev-capf
          (dabbrev-capf)
        (cl-letf (((symbol-function #'completion-in-region)
                   (lambda (beg end table &rest args)
                     (list beg end table))))
          (dabbrev-completion)) ;; hack dabbrev-completion to return list
        ))
    (defun dabbrev-capf-silent()
      (cape-wrap-silent #'my-dabbrev-capf))
    (if (functionp 'cape-keyword)
        (add-to-list 'completion-at-point-functions (cape-capf-super #'dabbrev-capf-silent #'cape-keyword))
      (add-to-list 'completion-at-point-functions #'dabbrev-capf-silent))
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
  )

(use-package google-c-style
  :if (bound-and-true-p enable-feature-prog)
  :commands (google-set-c-style))

(use-package cc-mode
  :if (bound-and-true-p enable-feature-prog)
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
  :config (define-key c-mode-base-map (kbd "C-h") 'c-electric-backspace))

(use-package c-ts-mode
  :defer t
  :config
  (define-advice c-ts-mode-set-modeline
      (:around (orig-fn &rest args) my)
    (let
        ((comment-start "-ts")) ;; 默认是显示comment style，没什么用来显示是否启动ts
      (apply orig-fn args))))

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
  :if (bound-and-true-p enable-feature-navigation)
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
  (advice-add 'push-mark :after #'backward-forward-after-push-mark))
;; 其它jump包
;; back-button global跟local是区分开的，这就很麻烦了
;; history可能会

(use-package iss-mode
  :if (bound-and-true-p enable-feature-prog)
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
  :if (bound-and-true-p enable-feature-edit)
  :load-path "~/.emacs.d/packages/expand-region/expand-region.el-master"
  :commands (er--expand-region-1 er/contract-region er/expand-region)
  :defer t
  :init (global-set-key (kbd "C-S-t") 'er/contract-region)
  :config
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-available-p))
    (defun my/treesit-mark-bigger-node ()
      "https://emacs-china.org/t/treesit-expand-region-el/23406"
      (let* ((root (treesit-buffer-root-node))
             (node
              (treesit-node-descendant-for-range
               root (region-beginning) (region-end)))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= (region-beginning) node-start)
                   (= (region-end) node-end))
          (when-let ((node (treesit-node-parent node)))
            (setq
             node-start (treesit-node-start node)
             node-end (treesit-node-end node))))
        (set-mark node-end)
        (goto-char node-start)))
    (add-to-list 'er/try-expand-list 'my/treesit-mark-bigger-node)))

(use-package smart-region
  :if (bound-and-true-p enable-feature-edit)
  :defer t
  :init
  (defun smart-region ()
    (interactive)
    (unless (featurep 'smart-region)
      (fmakunbound 'smart-region)
      (save-excursion
        (with-temp-buffer
          (er--expand-region-1))
        (load-multiple-cursors))
      (require 'smart-region))
    (call-interactively 'smart-region))
  ;; C-q不动再按C-q触发expand region，移动到其它行，同一列触发multiple-cursors，不同列是rectangle-mark
  ;; 同列触发mc这个要常用，不过有bug有汉字的话判定为rectangle-mark
  (global-set-key (kbd "C-q") 'smart-region)
  :config
  (when (functionp 'which-key--show-keymap)
    (define-advice smart-region (:after (&rest args) my)
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
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
   .
   markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files"
  t)
(add-to-list
 'auto-mode-alist '("[Rr][Ee][Aa][Dd][Mm][Ee]\\.md\\'" . gfm-mode))

;;; 类似sublime的多光标功能(以M键更像是visual code)
(use-package multiple-cursors
  :if (bound-and-true-p enable-feature-edit)
  :defer t
  :init
  (defun load-multiple-cursors ()
    (unless (featurep 'multiple-cursors)
      (delay-require-libs
       "~/.emacs.d/packages/multiple-cursors/multiple-cursors.el-master"
       '(multiple-cursors))))
  (defmacro my-mc-cmd (fn)
    `(lambda ()
       (interactive)
       (load-multiple-cursors)
       (call-interactively ,fn)))
  ;; 这个不能用`my-mc-cmd'去wrap
  (dec-placeholder-fun
   mc/mark-next-like-this
   multiple-cursors
   "~/.emacs.d/packages/multiple-cursors/multiple-cursors.el-master"
   '(multiple-cursors))
  ;; mc询问你的命令都保存在这里面了
  (setq
   mc/list-file "~/.emacs.d/packages/multiple-cursors/my-cmds.el"
   mc/always-run-for-all t ;; 禁止提示，需要run once的在my-cmds.el里加
   )
  (global-unset-key (kbd "<M-down-mouse-1>"))
  (global-set-key
   (kbd "M-<mouse-1>") (my-mc-cmd 'mc/add-cursor-on-click))
  (global-set-key (kbd "<f8>") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<f8>") (my-mc-cmd 'mc/mark-all-dwim)) ;; 最智能！无须选中自动选中当前symbol，也支持region，多行region是选里面的！先是选中defun里的，再按是所有！
  ;; Tips: C-'可以隐藏没有选中的项，modeline有提示当前有多少mc项
  :config
  (global-set-key (kbd "C-<f8>") 'mc/mark-all-dwim) ;; 最智能！无须选中自动选中当前symbol，也支持region，多行region是选里面的！先是选中defun里的，再按是所有！
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "M-<wheel-up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-<wheel-down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "S-<f8>") 'mc/skip-to-next-like-this) ;; 跳过当前选中
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim) ;; dwim的更智能
  (define-key mc/keymap (kbd "C-v") nil) ;; 使粘贴可用
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
  :if (bound-and-true-p enable-feature-navigation)
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
  :if (bound-and-true-p enable-feature-navigation)
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
(use-package rust-ts-mode
  :defer t
  :config
  ;; `rust-ts-mode'没有compile error的正则
  (require 'rust-mode))

(use-package sr-speedbar
  :if (bound-and-true-p enable-feature-navigation)
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
    (define-advice sr-speedbar-toggle (:before (&rest args) my)
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
  :if (bound-and-true-p enable-feature-navigation)
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
  :if (bound-and-true-p enable-feature-prog)
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
           (let* ((name (car c))
                  pos
                  more_begin
                  more_end)
             (goto-char (point-min))
             (counsel-etags-forward-line (cdr c))
             (when (search-forward name (point-at-eol) t)
               (forward-char (- (length name))))
             (setq pos (point-marker))
             ;; 获取类名等更多信息
             (skip-syntax-forward "^\s(")
             (setq more_end (point))
             (goto-char pos)
             (skip-syntax-backward "^\s(")
             (setq more_begin (point))
             (put-text-property
              0 1 'read-only (buffer-substring more_begin more_end)
              name)
             (push (cons name pos) imenu-items)))))
     imenu-items))
  (with-eval-after-load 'marginalia
   (define-advice marginalia-annotate-imenu
       (:around (orig-fn &rest args) my)
     "marginalia中添加"
     (if (eq imenu-create-index-function
          'my-counsel-etags-imenu-default-create-index-function)
         (if-let ((prop-text (get-text-property 0 'read-only (ad-get-argument args 0))))
             (marginalia--fields
              (prop-text :truncate 1.0 :face 'marginalia-function))
             nil)
       (apply orig-fn args))))
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
    :if (bound-and-true-p enable-feature-minibuffer)
    :defer t
    :init
    (setq
     enable-recursive-minibuffers t ; 允许minibuffer里再执行命令
     vertico-cycle t ;; vertico不同于其它的是当前行之前的结果被列在了最后
     vertico-resize nil ;; 让它初始化就固定大小
     vertico-count 20 ;; 高度多少行
     vertico-sort-function nil ;; 对需要排序的，添加到`vertico-multiform-commands'里
     )
    (run-with-idle-timer
     0.3 nil
     #'(lambda ()
         (add-to-list
          'load-path "~/.emacs.d/packages/minibuffer/vertico-main")
         (require 'vertico)))
    :config
    (vertico-mode 1)
    (add-to-list
     'load-path
     "~/.emacs.d/packages/minibuffer/vertico-main/extensions")
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

    ;; 只会恢复关键词
    ;; consult-line需要配合(setq consult-line-start-from-top nil)，这样首行就是当前位置
    ;; consult-ripgrep暂时没有好方法
    (use-package vertico-repeat
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
      :after (vertico)
      :init
      (when nil
        ;; 调试用
        (define-advice vertico-multiform--setup
            (:before (&rest args) my)
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
         (project-find-file
          (vertico-sort-function . vertico-sort-history-length-alpha)) ;; 按file-name-history排序
         (search-in-browser
          (vertico-sort-function . vertico-sort-history-alpha))
         (consult-bookmark
          (vertico-sort-function . vertico-sort-history-alpha))
         (tempel-insert
          (vertico-sort-function . vertico-sort-history-alpha))
         (search-in-browser
          (vertico-sort-function . vertico-sort-history-alpha))
         (radio-select
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
         (command (styles basic)) ; 这几个命令经常卡，去掉orderless，但测试乱序仍然可以？
         (variable (styles basic))
         (function (styles basic))) ;; 分类category可以在marginalia看到一些，目前只需要M-x(command)，F1-v(variable), F1-f(function)
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
         (lambda (str) (orderless--highlight input t str))))
      (setq consult--regexp-compiler
            #'consult--orderless-regexp-compiler))

    (defmacro common-fuzz-bakcend-setting (backend)
      `(progn
         (with-eval-after-load 'eglot
           ;; 新版eglot的改名为eglot-capf了
           (dolist (l completion-category-defaults)
             (when (string-prefix-p "eglot" (symbol-name (car l)))
               (add-to-list
                'completion-category-overrides
                ;; flex仅补充位，不然可能会导致C-g按很多次
                (list (car l) '(styles orderless basic ,backend))))))
         (when (featurep 'orderless)
           ;; +orderless-flex其实也是可以用，但是它没有打分机制。应该优先部分匹配，再是flex
           ;; 另外这个好像也是支持分词反序匹配，如ext pac匹配package_extra（不加空格的extpac都不支持)
           (setcdr
            (assoc
             'multi-category completion-category-overrides)
            '((styles orderless basic ,backend)))
           (setcdr
            (assoc 'file completion-category-overrides)
            '((styles orderless basic ,backend))))))

    ;; hotfuzz比fussy更快，fussy有时候会卡(如M-x`load-theme')
    ;; 缺点：测试有时加了空格反而找不到。function/command/variable设置后排序有问题
    (use-package hotfuzz
      :init
      (ignore-errors
        (module-load
         (expand-file-name
          "D:/prj/rust/hotfuzz-for-windows/hotfuzz-module.dll")))
      (ignore-errors
        (module-load
         (expand-file-name
          "F:/prj/cpp/hotfuzz-for-windows/hotfuzz-module.dll")))
      :config
      (with-eval-after-load 'consult
        ;; 修复hotfuzz报错 https://github.com/axelf4/hotfuzz/issues/12#issuecomment-1615621506
        (setq consult--tofu-char #x20000)
        (setq consult--tofu-range #x90000))
      (common-fuzz-bakcend-setting hotfuzz))

    (use-package fussy
      :disabled ;; 28上测试command/variable/function等速度确实不如hotfuzz
      :init
      (use-package fzf-native
        :init
        (setq fussy-score-fn 'fussy-fzf-native-score)
        (ignore-errors
          (module-load
           (expand-file-name
            "~/.emacs.d/packages/minibuffer/fzf-native-module.dll"))))
      (setq fussy-filter-fn 'fussy-filter-default) ;; fussy-filter-default和fussy-filter-orderless据说更快，但fussy-filter-orderless测试不支持packex匹配package_extra啊！
      :config (common-fuzz-bakcend-setting fussy))

    ;; 美化
    (use-package marginalia
      :commands (marginalia-mode)
      :bind
      (("M-A" . marginalia-cycle)
       :map
       minibuffer-local-map
       ("M-A" . marginalia-cycle))
      :init (marginalia-mode)
      :config
      ;; 去掉file等，因为consult-fd可能上万文件，会影响速度
      (setf (alist-get 'file marginalia-annotator-registry) '(none)))
    (use-package embark
      :load-path "~/.emacs.d/packages/minibuffer/embark-master"
      :commands
      (embark-export
       embark-act
       embark-act-with-completing-read
       embark-next-symbol
       embark-previous-symbol
       embark-toggle-highlight
       embark-open-externally
       embark-prefix-help-command)
      :bind
      (("M-." . embark-act) ;; 按这个后，还可以按其它prefix key如C-x调用C-x开头的键，起了which-key的作用！
       ;; ("M-." . embark-dwim) ;; 除非你知道每个object默认的操作，否则还是embark-act吧
       ("<f1> B" . embark-bindings) ;; 列举当前可用的键及其命令
       )
      :init
      (setq
       embark-mixed-indicator-delay
       1.0 ;; 按钮提示菜单延迟，熟练后可以设置长点
       ;; embark-quit-after-action nil     ;; 默认就退出minibuffer了
       )
      ;; (setq prefix-help-command #'embark-prefix-help-command) ;; 按prefix键后再按F1
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
      (defun embark-store-select (&rest _)
        "将F1 v/f选中项加入history 参考https://github.com/oantolin/embark/issues/452"
        (when-let ((current
                    (plist-get
                     (car-safe (embark--targets))
                     :orig-target)))
          (when (memq
                 embark--command
                 '(describe-variable
                   describe-function execute-extended-command))
            (add-to-history minibuffer-history-variable current))))
      (advice-add 'embark-act :before #'embark-store-select)
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
    :if (bound-and-true-p enable-feature-navigation)
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
     consult-line-start-from-top
     nil ;; nil前面行会排后面，但t初始行是最前面那个
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
      (bind-keys* ("C-c C-f" . consult-everything))
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
      ;; (bind-key* (kbd "C-c C-d") 'consult-dir)
      :config
      (setq
       consult-dir-sources
       '(consult-dir--source-recentf
         consult-dir--source-bookmark
         ;; consult-dir--source-default
         consult-dir--source-project ;projectile if available, project.el otherwise
         ))
      (define-advice consult-dir--recentf-dirs
          (:around (orig-fn &rest args) my)
        (unless (boundp 'dired-recent-directories)
          (unless (featurep 'dired-recent)
            (delay-require-libs
             "~/.emacs.d/packages/dired" '(dired-recent)))
          (dired-recent-load-list))
        dired-recent-directories))
    :config
    ;; C-l命令
    (defvar C-L-string nil)
    (defmacro run-cmd-parent-dir (cmd)
      `(lambda ()
         (interactive)
         (let ((text
                (ignore-errors
                  (buffer-substring-no-properties
                   (minibuffer-prompt-end) (point-max))))
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
            (if (chinese-word-chinese-string-p input)
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
                         (cdr result))))
              ;; C-u开头加上--search-zip用于搜索el.gz等文件
              (when current-prefix-arg
                (let* ((cmds (car result))
                       (path (car (last cmds))))
                  (when (string-match
                         "share/emacs/.*/lisp"
                         (expand-file-name path))
                    (setq result
                          (append
                           (list
                            (append
                             (list (car cmds))
                             (list "--search-zip")
                             (cdr cmds)))
                           (cdr result)))))))
            result))))
    (define-key occur-mode-map (kbd "C-c C-p") 'occur-edit-mode)))

(use-package smart-hungry-delete
  :if (bound-and-true-p enable-feature-edit)
  :defer 0.8
  :config
  (smart-hungry-delete-add-default-hooks)
  (bind-key*
   [remap delete-forward-char] 'smart-hungry-delete-forward-char)
  (bind-key* [remap delete-char] 'smart-hungry-delete-forward-char)
  (bind-key*
   [remap delete-backward-char] 'smart-hungry-delete-backward-char)
  (bind-key*
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

;; https://stackoverflow.com/a/750933
(aset standard-display-table ?\^M []) ;; 直接全局不显示^M


;; 给commit添加conventionalcommits关键字 https://www.conventionalcommits.org/
(defun git-conventionalcommits-capf (&optional interactive)
  "capf的写法参考`completion-at-point-functions'说明"
  (let ((bounds
         (or (bounds-of-thing-at-point 'symbol)
             (cons (point) (point)))))
    `(,(car bounds)
      ,(cdr bounds)
      ("feat: "
       "fix: "
       "docs: "
       "style: "
       "refactor: "
       "perf: "
       "test: "
       "build: "
       "chore: "
       "ci: "
       "revert: ")
      :exclusive no)))
(defun git-conventionalcommits-capf-setup ()
  (add-hook
   'completion-at-point-functions #'git-conventionalcommits-capf
   nil t))
(with-eval-after-load 'with-editor
  (add-to-list
   'with-editor-mode-hook 'git-conventionalcommits-capf-setup))
(with-eval-after-load 'vc-git
  (add-hook
   'vc-git-log-edit-mode-hook 'git-conventionalcommits-capf-setup))

(use-package log-edit
  :defer t
  :config
  ;; 在checkin时显示diff
  (agitate-log-edit-informative-mode))

;; 对vc很好的补充
(use-package agitate
  :defer t
  :init
  (dec-placeholder-fun agitate-log-edit-informative-mode
                       agitate
                       "~/.emacs.d/packages/magit"
                       '(agitate))
  :config
  (define-advice agitate--log-edit-informative-kill-buffer
      (:after (&rest args) my)
    "提交的同时也删除`vc-diff' buffer"
    (when-let ((buf (get-buffer "*vc-diff*")))
      (kill-buffer buf))))

;; 让文件加载过程中vc失效，从而加快文件加载
(use-package vc-defer
  :diminish
  :defer t
  :init
  (autoload 'vc-defer-mode "magit/vc-defer" "" nil)
  (vc-defer-mode 1)
  :config
  (add-to-list 'vc-defer-backends 'Git)
  (add-hook
   'prog-mode-hook
   (lambda ()
     ;; 恢复vc状态，修复diff-hl及mode-line的vc-mode等显示
     (run-with-local-idle-timer 0.3 nil #'vc-refresh-state))))

;; magit
(use-package magit
  :if (bound-and-true-p enable-feature-tools)
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
  (dec-placeholder-fun magit-status
                       magit
                       "~/.emacs.d/packages/magit/magit-master/lisp"
                       '(magit magit-status))

  (modify-coding-system-alist 'file "\\.git/COMMIT_EDITMSG\\'" 'utf-8)
  (setq
   magit-version "3.3.0"
   magit-commit-show-diff nil ;; commit时不用显示diff，在stage时一般就检查了
   magit-diff-hide-trailing-cr-characters nil ;; 会影响性能
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
(defun github-download (&optional url downdir)
  (interactive "sDownload url: ")
  ;; win10自带的curl不行，需要git里的curl
  ;; 遇到curl 60错误就需要创建~/.curlrc，内容insecure（powershell创建的含bom不行）
  ;; "c:/Git/mingw64/bin/curl.exe -L --ssl-no-revoke --http1.1 -oH:/download/test.zip https://download.fastgit.org/antlr/antlr4/archive/refs/heads/dev.zip"
  (let* ((filename (file-name-nondirectory (directory-file-name url)))
         (downdir "H:/download/")
         command
         (default-directory downdir))
    ;; (let ((us (s-split "/" url)))
    ;;   (setq filename (concat (nth 4 us) "-" filename)))
    ;; (setq
    ;;  command
    ;;  (concat
    ;;   "c:/Git/mingw64/bin/curl.exe -L --ssl-no-revoke --http1.1 -o"
    ;;   downdir
    ;;   filename
    ;;   " "
    ;;   (replace-regexp-in-string
    ;;    "https://github.com" "https://download.fastgit.org" url)))
    ;; (shell-command command)
    ;; fastgit服务端有问题，需要不断重试才能下载下来，所以跳浏览器下载
    (browse-url
     (replace-regexp-in-string
      "https://github.com" "https://download.fastgit.org" url))))
(defun git-add-repo-to-origin (&optional repo)
  (interactive "sAdd another repo to origin: ")
  (shell-command (concat "git remote set-url --add origin " repo)))

(use-package which-key
  :if (bound-and-true-p enable-feature-gui)
  :init
  (setq
   which-key-popup-type
   'side-window ;；用minibuffer当easy-mark时会移动屏幕，需要easy-kill-init-candidate里屏蔽narrow-to-region，并且非set-transient-map一闪而过
   which-key-use-C-h-commands nil ;; 避免C-h时调用成which-key的
   which-key-max-display-columns 4 ;; 限制列更好看点
   which-key-side-window-max-height 1.0 ;; 设置更高点，不然会显示不完全
   )
  :commands (which-key--show-keymap which-key--hide-popup)
  :config
  ;; 不显示`digit-argument'命令
  (add-to-list
   'which-key-replacement-alist '((nil . "digit-argument") . t)))

;; easy-kill，添加类似vim里yi/a的东西！
(use-package easy-kill
  :if (bound-and-true-p enable-feature-edit)
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
   (list 'easy-kill-er 'extra-things 'easy-kill-extras))
  (provide 'easy-kill-mc) ;; 避免出错提示，实际用不到这个
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
  (define-advice easy-kill-candidate (:around (orig-fn &rest args) my)
    "获取所选文字最关键的函数，这里判断是beg end位置方式，再判断是否是自定义thing，就给返回字符追yank-handler"
    (let ((result (apply orig-fn args)))
      (with-current-buffer (easy-kill-get buffer)
        (pcase (easy-kill-get bounds)
          (`(,_x . ,_x) () ;; 这就是字符串形式
           )
          (`(,beg . ,end) ;; begin end位置模式 
           (let ((string result))
             (if (functionp 'whole-line-or-region-kill-ring-save)
                 (when (eq
                        (easy-kill-get thing) 'line-with-yank-handler)
                   (progn
                     (call-interactively
                      'whole-line-or-region-kill-ring-save)
                     (setq result (nth 0 kill-ring))))
               (if (eq (easy-kill-get thing) 'line-with-yank-handler)
                   (put-text-property
                    0 (length string) 'yank-handler '(yank-line)
                    string)
                 ;; 解决非line-with-yank-handler偶尔带上yank-handler的问题
                 (remove-text-properties
                  0 (length string) 'yank-handler
                  string))
               (setq result string))))))
      result))

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
  (advice-add 'easy-kill-echo :override #'ignore)
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
        (define-advice easy-kill-activate-keymap
            (:before (&rest args) my)
          (unless my-easy-kill-map
            (let ((easy-kill-base-map easy-kill-base-map))
              ;; remove number keys
              (cl-loop
               for i from 0 to 9 do
               (define-key
                easy-kill-base-map (number-to-string i) nil))
              (setq my-easy-kill-map (easy-kill-map))))
          (which-key--show-keymap
           "keymap" my-easy-kill-map nil nil 'no-paging))
        (define-advice set-transient-map (:before (&rest args) my)
          (let ((map (ad-get-argument args 0)))
            ;; 判断是否是easy-kill的keymap
            (when (eq (lookup-key map "?") 'easy-kill-help)
              (ad-set-argument args 2 'which-key--hide-popup) ;; easy-kill按n时overlay还是消除不掉，暂时不管了
              )))))))

(use-package whole-line-or-region
  :disabled
  :if (bound-and-true-p enable-feature-edit)
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
  :if (bound-and-true-p enable-feature-navigation)
  :commands(xref--read-identifier)
  :defer t
  :init
  (setq
   xref-search-program 'ripgrep
   xref-auto-jump-to-first-definition 'show ;; 自动跳转到第一个
   xref-auto-jump-to-first-xref 'show)
  (global-set-key (kbd "C-.") 'xref-find-definitions)
  (global-set-key (kbd "<C-down-mouse-1>") 'xref-find-definitions)
 (defun my-xref-find-apropos ()
   (interactive)
   "自动搜索光标下，如果没有才提示输入"
   (let ((pattern
          (xref-backend-identifier-at-point (xref-find-backend))))
     (if pattern
         (xref-find-apropos pattern)
       (call-interactively 'xref-find-apropos))))
  (global-set-key [(control ?\,)] 'my-xref-find-apropos)
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

  (define-advice xref-matches-in-files
      (:around (orig-fn &rest args) my)
    (let ((cmdproxy-old-encoding
           (cdr
            (assoc
             "[cC][mM][dD][pP][rR][oO][xX][yY]"
             process-coding-system-alist))))
      ;; 注意project search是xargs实现的，会向rg先发送文件列表，因此对输入编码有要求
      (modify-coding-system-alist
       'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . utf-8))
      ;; 检查是否含中文
      (if (chinese-word-chinese-string-p (ad-get-argument args 0))
          (let ((xref-search-program-alist
                 (list
                  (cons
                   'ripgrep
                   (concat
                    (cdr (assoc 'ripgrep xref-search-program-alist))
                    " --pre rgpre")))))
            (apply orig-fn args))
        (apply orig-fn args))
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
   :override #'zjy/xref--create-fetcher)
  (with-eval-after-load 'eglot
    ;; 禁止eglot的错误提示，让其它xref可以继续找下去
    (cl-defmethod xref-backend-identifier-at-point
        ((_backend (eql eglot)))
      (find-tag--default))))

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
  :disabled ;; ggtags支持xref了更好用，这个项目大就不行了
  :if (bound-and-true-p enable-feature-navigation)
  :defer t
  :init
  (dec-placeholder-fun citre-create-tags-file
                       citre-ctags
                       "~/.emacs.d/packages/citre/citre-master"
                       '(citre-ctags))
  (dec-placeholder-fun citre-update-this-tags-file
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
    (dec-placeholder-fun citre-tags-file-path
                         citre-util
                         "~/.emacs.d/packages/citre/citre-master"
                         '(citre-util)))
  :config
  ;; 强制tag名
  (define-advice citre--path-to-cache-tags-file-name
      (:around (orig-fn &rest args) my)
    "TAGS")
  (define-key
   citre-edit-cmd-buf-map
   (kbd "C-c C-l")
   'citre-edit-cmd-buf-add-lang))

(use-package etags
  :defer t
  :commands
  (find-tag--default ;; 修复eglot的xref at-point
   tags-lazy-completion-table ;; 空白处M-.
   tags-completion-at-point-function ;; etags的capf
   )
  :init
  (with-eval-after-load 'eglot
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
      (when (and
             ask-when-to-tag
             (y-or-n-p
              "Can't find tags file for this buffer.  Create one? "))
        (citre-create-tags-file)
        (setq tagsfile (citre-tags-file-path))
        (when tagsfile
          (visit-tags-table tagsfile t)))))
  ;; (add-hook 'prog-mode-hook 'check_tags)
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
  (define-advice etags-verify-tags-table
      (:around (orig-fn &rest args) my)
    ;; 原来的判断是开头0xC字符
    t)
  (define-advice visit-tags-table-buffer
      (:around (orig-fn &rest args) my)
    "屏蔽xref提示选择TAGS"
    (cl-letf
        (((symbol-function #'read-file-name)
          (lambda (prompt &optional dir default-filename &rest others)
            ;; (message "dir:%S, default-filename:%S" dir default-filename)
            ;; 直播返回，相当于按RET
            default-filename)))
      (apply orig-fn args))))

(defun my-project-search (&optional dir initial)
  (interactive)
  (setq this-command 'my-project-search) ;; 使C-; s的preview生效
  (consult-ripgrep dir initial))

(defun my-project-buffer ()
  (interactive)
  (if (functionp 'consult-project-buffer)
      (call-interactively 'consult-project-buffer)
    (call-interactively 'project-switch-to-buffer)))

;; 没有cache查找文件也超快！
(use-package project
  :if (bound-and-true-p enable-feature-prog)
  :defer t
  :commands (project-compile project-find-regexp project-root)
  :init
  (defun invoke_project ()
    (interactive)
    (if (functionp' which-key--show-keymap)
        (progn
          (which-key--show-keymap
           "keymap" project-prefix-map nil nil 'no-paging)
          (set-transient-map project-prefix-map
                             nil
                             'which-key--hide-popup))
      (set-transient-map project-prefix-map nil 'ignore)))
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
   `((?f "File" project-find-file)
     (?s "Search" my-project-search)
     (?d "Dired" project-dired)
     (?m "Magit" my-project-magit)
     (?b "Buffer" my-project-buffer) ; 这个当recent buffer使用了
     (?v "VC-Dir" project-vc-dir)
     (?e "Eshell" project-eshell))
   project-switch-use-entire-map t ; switch prj时也可以按其它键
   )
  ;; 这个还可以避免projectile search的bug(比如搜索 projectile-indexing-method，projectile.el就被忽略了)
  (define-key project-prefix-map "f" 'project-find-file)
  (define-key project-prefix-map "s" 'my-project-search)
  (define-key project-prefix-map "S" 'project-shell)
  (define-key project-prefix-map "m" 'my-project-magit) ; v保留，那个更快更精简
  (define-key project-prefix-map "t" 'ggtags-create)
  (define-key project-prefix-map "u" 'ggtags-update)
  (define-key project-prefix-map "b" 'my-project-buffer)

  :config
  (when (boundp 'project-mode-line)
    (setq project-mode-line t) ;; mode line显示项目名，30.1版本才有
    (setq project-mode-line-face '(foreground-color . "orange")))
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
    (define-advice project--switch-project-command
        (:around (orig-fn &rest args) my)
      (let (result)
        (which-key--show-keymap
         "keymap" project-prefix-map nil nil 'no-paging)
        (setq result (apply orig-fn args))
        (which-key--hide-popup)
        result)))

  ;; 好像自动识别find的输出了(git里的find)
  ;; (defadvice project--files-in-directory (around my-project--files-in-directory activate)
  ;;   (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
  ;;     (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
  ;;     ad-do-it
  ;;     (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)))
  )

(use-package compile
  :if (bound-and-true-p enable-feature-builtin)
  :defer t
  :init
  (defun my-project-compile ()
    (interactive)
    (when current-prefix-arg ;; C-u F7同S-F7
      (setq compilation-read-command t))
    (progn
      (call-interactively 'project-compile)
      (setq compilation-read-command nil) ;; 不再提示
      ))
  (global-set-key [f7] 'my-project-compile)
  (global-set-key [(shift f7)] 'my-project-compile)
  :config
  (when compile-history
    ;; 自动使用最近的历史记录
    (setq compile-command (car compile-history))
    (setq compilation-read-command nil) ;; 大部分时间都在同一个项目，无须再按RET确认了
    ))

;; `cmake-integration'是直接调用cmake编译的，需要集成vcvarsall.bat的环境变量

(defvar vc-toolchain
  '(("vs2010 x86"
     "c:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/bin/vcvars32.bat")
    ("vs2010 x64"
     "c:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/bin/amd64/vcvars64.bat")
    ("vs2019 x86"
     "c:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Auxiliary/Build/vcvars32.bat")
    ("vs2019 x64"
     "c:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Auxiliary/Build/vcvars64.bat"))
  "目前写死路径自己够用，这个路径本身就没什么规律")
(defun set-vc-env (name)
  (setq vc-toolchain-already-set t)
  (let* ((env-string
          (shell-command-to-string
           (format "\"%s\" && set"
                   (second (assoc name vc-toolchain)))))
         (all (s-split "\n" env-string)))
    (dolist (line all)
      (setq l (s-split "=" line))
      (when-let* ((left (car-safe l))
                  (right (car-safe (cdr-safe l))))
        ;; `add-path-to-execute-path'会破坏PATH，这里参考它的实现添加到`exec-path'里
        (when (string-equal-ignore-case left "path")
          (add-to-list 'exec-path right))
        (setenv left right))))
  (fmakunbound 'set-vc-env)
  (defun set-vc-env (name)
    (error "已设置过vc环境，请重启emacs生效！")))
(defvar select-compiler-history nil)
(defvar vc-toolchain-already-set nil)
(defun select-compiler ()
  "TODO：如果设置两次，前次的环境变量将会保留，但是后面设置会靠前。介意可以重启emacs"
  (interactive)
  (let ((cp-list
         (mapcar
          (lambda (x) (car x))
          (-filter
           (lambda (x) (file-exists-p (second x))) vc-toolchain))))
    (setq cp-list (append cp-list '("project-compile")))
    (setq select-compiler-history (list (consult--read cp-list))))
  (setq compilation-read-command t) ;; 重新提示compile命令
  (let ((vc-toolchain-already-set nil))
    (check-vc-toolchain-set)))
(defun check-vc-toolchain-set ()
  (unless vc-toolchain-already-set
    (unless (equal
             (car-safe select-compiler-history) "project-compile")
      (set-vc-env (car-safe select-compiler-history)))))
(with-eval-after-load 'eglot
  (check-vc-toolchain-set))
(with-eval-after-load 'cmake-integration
  (check-vc-toolchain-set))

(use-package cmake-integration
  :commands
  (cmake-integration-save-and-compile
   cmake-integration-save-and-compile-no-completion
   cmake-integration-get-codemodel-reply-json-filename
   cmake-integration-cmake-reconfigure)
  :init
  ;; 像vs一样含有Debug/Release，还有个RelWithDebInfo
  (setq cmake-integration-generator "Ninja Multi-Config")
  (defun cmake-compile ()
    (interactive)
    (if (cmake-integration-get-codemodel-reply-json-filename)
        (call-interactively
         (if (or (not my-cmake-integration-current-target-history)
                 current-prefix-arg)
             'cmake-integration-save-and-compile
           'my-cmake-integration-save-and-compile-last-target))
      (call-interactively 'cmake-integration-cmake-reconfigure)
      (setq my-cmake-integration-current-target-history nil)))
  (global-set-key
   [f7]
   (lambda ()
     (interactive)
     (if (equal (car-safe select-compiler-history) "project-compile")
         (call-interactively 'my-project-compile)
       (call-interactively 'cmake-compile))))
  (defvar my-cmake-integration-current-target-history nil)
  (defun my-cmake-integration-save-and-compile-last-target ()
    "让target能被session记录"
    (interactive)
    (cmake-integration-save-and-compile-no-completion
     (or (car my-cmake-integration-current-target-history) "all")))
  :config
  (define-advice cmake-integration-save-and-compile-no-completion
      (:after (&rest args) my)
    (setq my-cmake-integration-current-target-history
          (list (or cmake-integration-current-target "all"))))
  (define-advice cmake-integration-get-build-folder
      (:around (orig-fn &rest args) my)
    (let ((ret
           (or (if (equal
                    (or (car-safe select-compiler-history) "32") "32")
                   (and (bound-and-true-p my-cmake-build-dir)
                        my-cmake-build-dir) ; 由`.dir-locals.el'设置
                 (and (bound-and-true-p my-cmake-build-dir64)
                      my-cmake-build-dir64))
               (apply orig-fn args))))
      (setq compilation-search-path
            (append (list ret) (delete ret compilation-search-path)))
      ret))
  (define-advice cmake-integration-create-empty-codemodel-file
      (:around (orig-fn &rest args) my)
    "解决windows上调用问题"
    (unless (file-exists-p
             (cmake-integration-get-path-of-codemodel-query-file))
      ;; Create the folder if it does not exists yet
      (unless (file-exists-p (cmake-integration-get-query-folder))
        (f-mkdir-full-path (cmake-integration-get-query-folder)))
      ;; Create the codemodel file
      (f-touch
       (cmake-integration-get-path-of-codemodel-query-file)))))

;; rg，这个还挺好用的，带修改搜索的功能(需要buffer可写)，更多功能看菜单
(use-package rg
  :if (bound-and-true-p enable-feature-tools)
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
    (define-advice rg-run (:around (orig-fn &rest args) my)
      (let ((cmdproxy-old-encoding
             (cdr
              (assoc
               "[cC][mM][dD][pP][rR][oO][xX][yY]"
               process-coding-system-alist))))
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         '(utf-8 . gbk-dos))
        (apply orig-fn args)
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         cmdproxy-old-encoding)))
    (define-advice rg-recompile (:around (orig-fn &rest args) my)
      (let ((cmdproxy-old-encoding
             (cdr
              (assoc
               "[cC][mM][dD][pP][rR][oO][xX][yY]"
               process-coding-system-alist))))
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         '(utf-8 . gbk-dos))
        (apply orig-fn args)
        (modify-coding-system-alist
         'process
         "[cC][mM][dD][pP][rR][oO][xX][yY]"
         cmdproxy-old-encoding)))
    ;; 中文
    (define-advice rg-build-command (:around (orig-fn &rest args) my)
      (if (chinese-word-chinese-string-p (ad-get-argument args 0))
          (let ((rg-command-line-flags (list "--pre rgpre")))
            (apply orig-fn args))
        (apply orig-fn args)))))

;; consult-grep -> embark-export to grep-mode
;; grep mode里C-c C-p开启编辑，C-c C-c完成，C-c C-k放弃编辑
(use-package wgrep
  :if (bound-and-true-p enable-feature-tools)
  :after (grep)
  :init
  (setq wgrep-auto-save-buffer t) ;; 编辑好自动保存
  :config
  ;; 使wrep可编辑
  (define-advice wgrep-commit-file (:around (orig-fn &rest args) my)
    (let ((tmp-disable-view-mode t))
      (apply orig-fn args))))

(use-package flymake
  :if (bound-and-true-p enable-feature-prog)
  :defer t
  :init
  ;; (setq flymake-show-diagnostics-at-end-of-line t) ;; 用overlay在行尾显示错误！但是太卡
  (setq flymake-set-for-eglot t) ;; 同样对lspce也起作用
  (when flymake-set-for-eglot
    (setq flymake-no-changes-timeout nil) ;; 禁止eglot的flymake，因为它不是idle timer更新的
    )
  :config
  (when flymake-set-for-eglot
    (define-advice flymake--schedule-timer-maybe
        (:around (orig-fn &rest args) my)
      "配合eglot实现idle timer更新"
      (let (timer
            (flymake-no-changes-timeout 3.0))
        (setq timer (apply orig-fn args))
        (when timer
          ;; timer的callback lambda无法改变`flymake-no-changes-timeout'，所以直接重写timer的callback，照抄就行
          (timer-set-function timer
                              (lambda (buffer)
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (when flymake-mode
                                      (flymake-start t))
                                    (setq flymake-timer nil))))
                              (list (current-buffer))))
        timer)))

  ;; 当lsp开启时，去掉自己的hook
  (add-hook
   'flymake-mode-hook
   (lambda ()
     (when (and flymake-mode
                (or (bound-and-true-p eglot--managed-mode)
                    (bound-and-true-p lsp-managed-mode)
                    (bound-and-true-p lspce-mode)))
       (unless flymake-set-for-eglot
         (remove-hook
          'after-change-functions
          'flymake-after-change-function
          t) ;; `flymake-no-changes-timeout'改为nil就需要这个了
         )
       (remove-hook 'after-save-hook 'flymake-after-save-hook t)
       (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
       (remove-hook
        'eldoc-documentation-functions 'flymake-eldoc-function
        t))))
  ;; fixed for eglot with emacs 28.0.50，就kill-buffer时有提示，实际好像没什么问题
  (unless (boundp 'flymake-list-only-diagnostics)
    (defvar flymake-list-only-diagnostics nil)))

(defun init-lsp-snippet-tempel ()
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
  (load "lsp/lsp-snippet-tempel"))

;; https://github.com/zbelial/lspce
;; 这个很多都是参考eglot实现，比如`after-change-functions'，这样可以避免windows上的一些问题
(use-package lspce
  :defer t
  :init
  (setq
   lspce-send-changes-idle-time 0
   lspce-enable-logging nil
   lspce-connect-server-timeout 5 ;; lspce的connect是同步的，server有问题会卡死emacs
   lspce-modes-enable-single-file-root '(python-mode python-ts-mode))
  (defun lsp-ensure ()
    "eglot抄过来改改"
    (unless (featurep 'lspce-module)
      (ignore-errors
        (module-load
         (expand-file-name
          "D:/prj/rust/lspce/target/release/lspce_module.dll")))
      (ignore-errors
        (module-load
         (expand-file-name
          "F:/prj/rust/lspce-master/target/release/lspce_module.dll")))
      (delay-require-libs
       "~/.emacs.d/packages/lsp/lspce-master" '(lspce)))
    (let ((buffer (current-buffer)))
      (cl-labels
       ((maybe-connect
         () (remove-hook 'post-command-hook #'maybe-connect nil)
         (lspce--when-live-buffer
          buffer
          (unless lspce-mode
            (lspce-mode 1)))))
       (when buffer-file-name
         (add-hook 'post-command-hook #'maybe-connect 'append nil)))))
  :config (advice-add 'lspce-completion-at-point :around #'cape-wrap-buster)
  ;; 不知道为什么lspce屏蔽了flex
  (with-eval-after-load 'hotfuzz
    (add-to-list
     'completion-category-defaults '(lspce-capf (styles hotfuzz))))
  ;; `lspce--choose-server'有bug，多个选择反而有问题，所以这里去掉多余的选择
  (setq lspce-server-programs (assoc-delete-all "python" lspce-server-programs))
  (add-to-list 'lspce-server-programs '("python" "pylsp" ""))
  (setq lspce-server-programs (assoc-delete-all "rust" lspce-server-programs))
  (defun my-lspce-ra-initializationOptions ()
    "cargo启动太多了，只有禁用checkOnSave了，没有错误检查了。参考`lspce-ra-initializationOptions', https://hw0lff.github.io/rust-analyzer-docs/2023-06-05/index.html"
    (let ((options (make-hash-table :test #'equal)))
      (setq options (lspce--add-option "checkOnSave" :json-false options))
      options))
  (add-to-list 'lspce-server-programs '("rust" "rust-analyzer" "" my-lspce-ra-initializationOptions))
  ;; 设置clangd参数
  (setq lspce-server-programs (assoc-delete-all "C" lspce-server-programs))
  (add-to-list
   'lspce-server-programs
   '("C"
     "clangd"
     "-j=8 --background-index -header-insertion=never --clang-tidy --header-insertion-decorators=0 --completion-style=detailed --pch-storage=memory"))
  (add-to-list
   'lspce-server-programs '("cmake" "cmake-language-server"))
  (init-lsp-snippet-tempel)
  ;; lspce使用了较多的yas函数，参考`lsp-snippet-tempel-eglot-init'修正
  (advice-add
   'lspce--snippet-expansion-fn
   :override #'lsp-snippet-tempel--eglot-expand-snippet))

(use-package eglot
  :if (bound-and-true-p enable-feature-lsp-dap)
  :init
  ;; 最新emacs自带eglot并依赖external-completion
  (let ((eglot-load-path
         (if (autoloadp (symbol-function 'eglot))
             "eglot"
           "lsp/eglot")))
    (autoload 'eglot-ensure eglot-load-path "" nil)
    (autoload 'eglot eglot-load-path "" nil)
    (autoload 'eglot-rename eglot-load-path "" nil)
    (autoload 'eglot-completion-at-point eglot-load-path "" nil))
  (unless (boundp 'lspce-send-changes-idle-time)
    (defun lsp-ensure ()
      (eglot-ensure)))
  (setq
   eglot-confirm-server-initiated-edits
   nil ; 避免code action的yes/no提示
   eglot-send-changes-idle-time 0 ; 加快补全，实际上corfu-auto-delay的关系更大
   eglot-sync-connect nil ;; 打开新文件就不卡了，貌似没有副作用？
   eglot-events-buffer-size 0 ;; 
   eglot-autoshutdown t ;; 不关退出emacs会卡死
   )
  :commands (eglot eglot-ensure eglot-rename eglot-completion-at-point)
  :config
  (setf (plist-get eglot-events-buffer-config :size) 0) ;; eglot-events-buffer-size 自1.16废弃了
  ;; 正确显示#ifdef/#endif宏，需要clangd 17版本以上
  (use-package clangd-inactive-regions
    :defer t
    :init
    (autoload
      'clangd-inactive-regions-mode "lsp/clangd-inactive-regions"
      "" nil)
    (defun ensure-clangd-inactive-regions-mode (&rest _)
      (clangd-inactive-regions-mode 1))
    (add-hook
     'eglot-server-initialized-hook
     #'ensure-clangd-inactive-regions-mode)
    (add-hook
     'eglot-managed-mode-hook #'ensure-clangd-inactive-regions-mode)
    :config
    ;; (clangd-inactive-regions-set-method "darken-foreground")
    ;; (clangd-inactive-regions-set-opacity 0.55)
    )
  (add-to-list
   'eglot-server-programs
   '((c-mode c-ts-mode c++-mode c++-ts-mode)
     .
     ("clangd"
      "-j=8" ;; 线程数量
      "--background-index"
      "-header-insertion=never" ;; 不要插入头文件
      "--clang-tidy" ;; 高级提示
      "--header-insertion-decorators=0" ;; 不要在前面插入圆点
      "--completion-style=detailed" "--pch-storage=memory")))
  (fset #'jsonrpc--log-event #'ignore) ;; 禁止log buffer据说可以加快速度
  ;; flymake还是要开的，错误不处理的话，补全就不能用了。用跟cmake一样的vs版本可以解决很多错误
  (setq eglot-ignored-server-capabilities
        (list
         :documentHighlightProvider ;; 关闭光标下sybmol加粗高亮
         :hoverProvider ;; hover没什么用，在sqlite3中还会卡
         :inlayHintProvider ;; 参数提示，但编辑时会错位，不太需要
         :codeActionProvider ;; 当有flymake错误时，code action非常讨厌
         :documentOnTypeFormattingProvider))
  ;; 临时禁止view-mode，使重命名可用
  (define-advice eglot--apply-workspace-edit
      (:around (orig-fn &rest args) my)
    (let ((tmp-disable-view-mode t))
      (apply orig-fn args)))
  ;; clang-format不需要了，默认情况下会sort includes line，导致编译不过，但clangd的却不会，但是要自定义格式需要创建.clang-format文件
  (define-key eglot-mode-map [(meta f8)] 'eglot-format)
  (define-advice eglot-format (:around (orig-fn &rest args) my)
    (if (memq major-mode '(python-mode python-ts-mode))
        (my-format-all)
      (apply orig-fn args)))

  (defun remove-cpp-imenu ()
    ;; cpp用ctags生成的imenu，够用且不卡
    (when (or (derived-mode-p 'c-mode 'c++-mode)
              (derived-mode-p 'c-ts-base-mode))
      (remove-function
       (local 'imenu-create-index-function) #'eglot-imenu)))
  (if (boundp 'lspce-send-changes-idle-time)
      (progn
        (add-to-list 'eglot-stay-out-of 'flymake)
        (add-to-list 'eglot-stay-out-of 'xref)
        (add-to-list 'eglot-stay-out-of 'eldoc)
        (setq eglot-ignored-server-capabilities
              (list
               :hoverProvider ;; hover没什么用，在sqlite3中还会卡
               :completionProvider
               :signatureHelpProvider
               :definitionProvider
               :typeDefinitionProvider
               :implementationProvider
               :declarationProvider
               :referencesProvider
               :documentHighlightProvider ;; 关闭光标下sybmol加粗高亮
               :documentSymbolProvider
               :workspaceSymbolProvider
               :codeActionProvider
               :codeLensProvider
               ;; :documentFormattingProvider
               ;; :documentRangeFormattingProvider
               :documentOnTypeFormattingProvider
               :renameProvider
               :documentLinkProvider
               :colorProvider
               :foldingRangeProvider
               :executeCommandProvider
               :inlayHintProvider ;; 参数提示，但编辑时会错位，不太需要
               ))
        (add-hook
         'eglot-managed-mode-hook
         (lambda ()
           ;; hook关多了，format功能不正常
           (remove-hook
            'completion-at-point-functions #'eglot-completion-at-point
            t)
           (remove-cpp-imenu))))
    (progn
      (eldoc-add-command 'c-electric-paren)
      (eldoc-add-command 'c-electric-semi&comma) ;; 输入,后提示参数
      (when nil
        ;; 获取不显示signature的方法
        (define-advice eldoc--message-command-p
            (:before (&rest args) my)
          (and (symbolp (ad-get-argument args 0))
               (message "%S" (ad-get-argument args 0)))))
      (init-lsp-snippet-tempel)
      ;; 似乎下面的`cape-capf-noninterruptible'的配置更好点？
      ;; (advice-add
      ;;  'eglot-completion-at-point
      ;;  :around #'cape-wrap-buster) ;; corfu wiki新增的方法，让输入时强制更新capf
      ;; (advice-add
      ;;  'eglot-completion-at-point
      ;;  :around #'cape-wrap-noninterruptible)
      (add-hook
       'eglot-managed-mode-hook
       (lambda ()
         ;; 抄自https://www.reddit.com/r/emacs/comments/17uyy08/frustrating_python_lsp_experience/ 效果似乎更好点
         (setq-local
          eldoc-documentation-strategy ; eglot has it's own strategy by default
          'eldoc-documentation-compose-eagerly)
         (setq-local completion-at-point-functions
                     (cl-nsubst
                      (cape-capf-noninterruptible
                       (cape-capf-buster
                        #'eglot-completion-at-point
                        ;; #'string-prefix-p, 默认是equal
                        (lambda (old new)
                          ;; 实测old new是一样的，因此我们强制返回nil
                          nil)))
                      'eglot-completion-at-point
                      completion-at-point-functions))
         (remove-cpp-imenu)))))

  ;; 禁止didChangeWatchedFiles，一些lsp server会调用它，导致调用project-files，大型项目会卡住(如kill-buffer时)。 等同于lsp-enable-file-watchers
  (cl-defmethod eglot-register-capability
      (server
       (method (eql workspace/didChangeWatchedFiles))
       id
       &key
       watchers)
    "不要didChangeWatchedFiles这个功能，试了修改eglot--trampish-p不行，只有这样"
    (eglot-unregister-capability server method id))

  ;; https://github.com/blahgeek/emacs-lsp-booster 
  ;; 这个是作为中间exe wrap lsp进程，加快处理速度，无缝跟eglot/lsp-mode集成！
  (load "lsp/eglot-booster")
  (eglot-booster-mode 1))

;; 不能任意hook，不然右键无法打开文件，因为eglot找不到对应的server会报错
(defun enable-format-on-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (when (featurep 'eglot)
                (eglot-format)))
            nil 'local))

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
    (add-path-to-execute-path
     (expand-file-name c-sharp-server-path))))

;; 仅在view mode off时开启lsp，文件本身只读的(如tfs管理的)更不会开启lsp！
(defvar-local lsp-inited nil)
(add-hook
 'view-mode-hook
 (lambda ()
   ;; 编辑`*scratch*'时自动转为`emacs-lisp-mode'
   (when (and (not view-mode)
              (equal (buffer-name) "*scratch*")
              (eq major-mode 'fundamental-mode))
     (emacs-lisp-mode))
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
                      (memq
                       major-mode '(csharp-mode csharp-ts-mode))))
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
    (define-advice tfs/checkout (:before (&rest args) my)
      (save-place-to-alist))))

;; 自动转换文本中的RGB颜色
(use-package rainbow-mode
  :if (bound-and-true-p enable-feature-gui)
  :commands (rainbow-mode)
  :hook (help-mode . rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))


;; grammatical-edit bug太多了，pair用这个就够了
(use-package elec-pair
  :if (bound-and-true-p enable-feature-edit)
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
            (key-name
             (format "my-elec-pair-%s" (char-to-string key))))
       (eval
        `(defun ,(intern key-name) ()
           (interactive)
           (let ((this-command 'self-insert-command)
                 (last-command-event ,key))
             ;; (self-insert-command 1 ,key)
             (call-interactively 'self-insert-command)
             (unless (minibufferp)
               (ignore-errors
                 (electric-pair-post-self-insert-function)
                 (when my-auto-newline
                   (when (eq ,key ?\{)
                     (call-interactively 'new-line-dwim))))))))
       (eval `(global-set-key ,key_str ',(intern key-name)))))

    (with-eval-after-load 'corfu
      (add-to-list 'corfu-auto-commands "my-elec-pair.*")))

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
   "D:/prj/rust/pop-select/target/release/pop_select.dll"))

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
      (switch-to-buffer (nth sel myswitch-buffer-list))))
  (bind-keys* ("<C-tab>" . my-pop-select))
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
  :if (bound-and-true-p enable-feature-navigation)
  :defer 0.3
  :config
  (setq scroll-on-jump-duration 0.0)
  (setq scroll-on-jump-smooth nil)
  (scroll-on-jump-advice-add goto-last-change-check-in-session)
  (scroll-on-jump-advice-add push-button)
  (scroll-on-jump-advice-add embark-next-symbol)
  (scroll-on-jump-advice-add embark-previous-symbol)
  (scroll-on-jump-advice-add my/backward-forward-previous-location)
  (scroll-on-jump-advice-add my/backward-forward-next-location)
  (scroll-on-jump-advice-add diff-hl-next-hunk)
  (scroll-on-jump-advice-add diff-hl-previous-hunk)
  ;; 调用了set-window-start的，要用scroll-on-jump-with-scroll-..
  )

;; 对于scroll-on-jump没效果的，可以手动开启centered-cursor-mode
(use-package centered-cursor-mode
  :if (bound-and-true-p enable-feature-navigation)
  :commands (centered-cursor-mode))

(use-package yaml-mode
  :if (bound-and-true-p enable-feature-prog)
  :commands (yaml-mode)
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package diff-hl
  :if (bound-and-true-p enable-feature-gui)
  :defer t
  :init
  (defun diff-hl-after-save-hook-local ()
    (when diff-hl-update-timer
      (cancel-timer diff-hl-update-timer))
    (setq diff-hl-update-timer
          (run-with-idle-timer
           2.0 nil #'diff-hl-update-timer-function)))
  (defun diff-hl-prog-mode-hook ()
    (unless (featurep 'diff-hl)
      (delay-require-libs
       "~/.emacs.d/themes/diff-hl-master"
       '(diff-hl diff-hl-dired diff-hl-show-hunk)))
    ;; 不在file加载过程中显示，因为vc-defer的hack会导致vc相关调用无效
    (run-with-local-idle-timer
     0.5 nil
     (lambda ()
       (diff-hl-maybe-define-bitmaps)
       (diff-hl-update)))
    (setq-local diff-hl-mode t) ;; for 'diff-hl-magit-post-refresh
    (add-hook 'after-save-hook 'diff-hl-after-save-hook-local nil t))
  (add-hook 'prog-mode-hook 'diff-hl-prog-mode-hook)
  :config
  (define-advice diff-hl-diff-against-reference
      (:around (orig-fn &rest args) my)
    "`vc-git-diff-switches'必须设置为nil，不然diff会显示Binary files"
    (let ((vc-git-diff-switches nil))
      (apply orig-fn args)))
  (defhydra
   hydra-diff-hl
   ()
   "goto hunk: "
   ("p" diff-hl-previous-hunk "previous")
   ("n" diff-hl-next-hunk "next")
   ("q" nil "quit"))
  (global-set-key (kbd "C-c h") #'hydra-diff-hl/body)
  (global-set-key (kbd "C-c C-h") #'hydra-diff-hl/body)

  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (with-eval-after-load 'log-edit
    ;; vc checkin后也需要更新状态
    (add-hook
     'log-edit-done-hook
     (lambda ()
       ;; 立即调用没效果，需要timer
       (run-with-idle-timer 0.5 nil #'diff-hl-reset-reference-rev))))
  ;; 用timer避免各种hook
  (defvar diff-hl-update-timer nil)
  (defun diff-hl-update-timer-function ()
    (ignore-errors
      (diff-hl-update)))
  (defun diff-hl-update-manual ()
    (interactive)
    (diff-hl-update))

  (use-package diff-hl-dired
    :disabled                           ; chrome的out目录进入失败
    :commands (diff-hl-dired-mode)
    :init (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(ignore-errors
  (module-load
   "D:/prj/rust/emacs-preview-rs/target/release/emacs_preview_rs.dll"))
(ignore-errors
  (module-load
   "f:/prj/rust/emacs-preview-rs/target/release/emacs_preview_rs.dll"))
(use-package emacs-preview
  :if (functionp 'emacs-preview-rs/web-server-start)
  :defer t
  :init
  (defun note-preview ()
    (interactive)
    (unless (featurep 'emacs-preview)
      (dec-placeholder-fun emacs-preview-mode emacs-preview
                           (if (file-exists-p
                                "h:/prj/rust/emacs-preview-rs/src")
                               "h:/prj/rust/emacs-preview-rs/src"
                             "f:/prj/rust/emacs-preview-rs/src")
                           '(emacs-preview)))
    (call-interactively 'emacs-preview-mode))
  :config
  (setq maple-preview:allow-modes
        '(org-mode
          markdown-mode html-mode web-mode mhtml-mode css-mode)) ;; html打开后落在css上就是`css-mode'
  (with-eval-after-load 'dired-sidebar
    (define-advice dired-sidebar-find-file (:after (&rest args) my)
      "解决click点击不更新preview问题"
      (emacs-preview:send-to-server))))

(use-package zhengma
  :defer t
  :if (functionp 'emacs-preview-rs/web-server-start)
  :init
  (dec-placeholder-fun
   zhengma:start zhengma "~/.emacs.d/packages/tools" '(zhengma)))

(use-package winner
  :defer t
  :init
  :config
  (define-key winner-mode-map (kbd "<C-left>") #'winner-undo)
  (define-key winner-mode-map (kbd "<C-right>") #'winner-redo))

;; 优点: 可以以文件名,tag和子标题(需要org-id-get-create创建id)来搜索。
;; roam buffer: 可以显示backlink，同时会根据鼠标位置动态更新内容
(use-package org-roam
  :if (bound-and-true-p enable-feature-tools)
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
      (call-interactively 'project-find-file)))
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
  (define-advice org-roam-capture- (:around (orig-fn &rest args) my)
    (let ((default-directory org-roam-directory))
      (call-interactively 'find-file))))

(use-package olivetti
  :defer t
  :init
  (setq
   olivetti-body-width 200
   olivetti-lighter nil)
  (autoload 'olivetti-mode "org/olivetti" nil t)
  :hook ((Info-mode org-mode) . olivetti-mode))

;; 大神维护的bug少，其它美化多少有点问题或者性能问题
(use-package org-modern
  :defer t
  :init
  (setq
   org-modern-star '("●" "●" "▶")
   ;; '("●" "●" "▶") ;; "◌" '("●" "○" "▶") 突出标题让人更安心写内容
   ;; https://www.fuhaoku.net/block/Dingbats
   org-modern-list '((?+ . "●") (?- . "●")) ;;'((?+ . "✚") (?- . "▬"))
   ) ;; ▶ ▬ ，*不好输出就算了
  :init (autoload 'org-modern-mode "org/org-modern" nil t)
  :hook (org-mode . org-modern-mode))

;; 中英文对齐的工具
(use-package cnfonts
  :defer t
  :init (autoload 'cnfonts-edit-profile "tools/cnfonts" nil t)
  :init (autoload 'cnfonts--set-font-1 "tools/cnfonts" nil t)
  :config
  (load "tools/cnfonts-ui")
  (define-advice cnfonts-edit-profile
      (:around (orig-fn &rest args) my)
    (cl-letf (((symbol-function #'locate-library)
               (lambda (&rest _)
                 "~/.emacs.d/packages/tools/cnfonts.el")))
      (apply orig-fn args))))

;; foobar音质最好！网速好像比mpv稍微慢点，特别网络卡时容错也没mpv好
(when t
  ;; foobar里需要设置
  ;; 1.添加歌曲不显示gui，Preference>Shell Integretion，Bring to front.. 去掉勾选
  ;; 2.缩小到托盘图标，【File】->【Preferences】->【Advanced】->【Display】->【Default User Interface】->【Windows minimize and close】勾选Minize hide，close exits
  ;; 3.提升音质WASAPI篇，官网 https://www.foobar2000.org/components 下载插件WASAPI shared output 0.6.21(这个支持64位) 7z解压放到components目录，重启。 Preferences>Playback>Output>Device选择WASAPI设备
  ;; 也可以ASIO好像更好！官网下载asio插件，我装了水果自带asio驱动，Device选择FL Studio ASIO(选择ASIO4ALL V2没有声音不知道怎么回事)。
  ;; 最后发现好像都差不多，甚至打开mpv听效果一样！mpv默认就是wasapi输出，不过不支持win7懒得折腾了。
  (defvar foobar-binary nil)
  (defvar foobar-running nil)
  (defun foobar-play (uri)
    (foobar-exit) ;; 当切换下一个比较耗时电台时开头仍然会串音
    ;; win7 foobar不能播放重定向的url，需要自己获取重定向后的URL
    (when (equal (list 6 1 7601) (w32-version))
      (setq uri
            (string-trim
             (shell-command-to-string
              (concat
               (expand-file-name "packages/tools/url_get_redirect.py"
                                 user-emacs-directory)
               " " uri)))))
    (w32-shell-execute nil foobar-binary "/hide")
    (w32-shell-execute nil foobar-binary (format "/play %s" uri))
    (setq foobar-running t))
  (defun foobar-exit ()
    (interactive)
    (w32-shell-execute nil foobar-binary "/hide")
    (w32-shell-execute nil foobar-binary "/stop") ;; 没必要/exit，不然后面再播放可能会显示Proccesing files对话框
    (setq foobar-running nil))
  (defun foobar-is-running ()
    foobar-running)

  ;; mpv
  (defvar mpv-binary nil)
  (defvar mpv-process nil)
  (defvar empv-mpv-args
    `("--no-video" "--no-terminal" "--idle" "--volume=65"))
  (defun mpv-exit ()
    (interactive)
    (ignore-errors
      (when mpv-process
        (setq mpv-process (delete-process mpv-process)))))
  (defun mpv-play (uri)
    (mpv-exit)
    (setq mpv-process
          (make-process
           :name "empv-process"
           :buffer nil
           :command `(,mpv-binary ,@empv-mpv-args ,uri))))
  (defun mpv-is-running ()
    mpv-process)

  ;; radio
  (defun radio-switch-backend (&optional backend)
    (interactive)
    (unless backend
      (setq backend
            (completing-read
             "Radio backend select: " '("foobar" "mpv")
             nil t)))
    (when (and (fboundp 'radio-is-running) (radio-is-running))
      (radio-exit))
    (if (equal backend "foobar")
        (progn
          (defalias 'radio-play 'foobar-play)
          (defalias 'radio-exit 'foobar-exit)
          (defalias 'radio-is-running 'foobar-is-running))
      (defalias 'radio-play 'mpv-play)
      (defalias 'radio-exit 'mpv-exit)
      (defalias 'radio-is-running 'mpv-is-running)))

  ;; 音质其实都差不多，mpv下载要快点不卡。mpv支持win7最后版本20230917
  (radio-switch-backend "mpv")

  ;; https://www.radio-browser.info/
  (defvar radio-urls nil)
  (defvar radio-url-current nil)
  (defun radio-urls-read (file)
    "radio_urls.el由radio_urls_get.py生成"
    (with-temp-buffer
      (insert "(\n")
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "\n)")
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        data)))
  (defun radio-select ()
    (interactive)
    (unless radio-urls
      (setq radio-urls
            (radio-urls-read
             (expand-file-name "packages/tools/radio_urls.el"
                               user-emacs-directory)))
      ;; 蜻蜓的音质基本都很差，仅下面这个可以
      (setq radio-urls
            (append
             '(("华语金曲500首"
                "http://ls.qingting.fm/live/3412131.m3u8?bitrate=64"))
             radio-urls)))
    (let* ((completion-ignore-case t)
           (item
            (assoc-string (completing-read "Radio select: " radio-urls
                                           nil t)
                          radio-urls
                          t)))
      (setq radio-url-current item)
      (ignore-errors
        (radio-play (nth 1 item)))))
  (defalias 'radio-stop 'radio-exit)
  ;; 添加mode-line指示，参考`display-time-string'
  (defvar radio-mode-string nil)
  (put 'radio-mode-string 'risky-local-variable t) ;; 必须不然face等没效果
  (defface radio-mode-line-face-playing '((t :foreground "#6ae4b9"))
    "")
  (defface radio-mode-line-face-stopped '((t :foreground "#CC6666"))
    "")
  (defun radio-mode-string-keymap-click (event)
    (interactive "e")
    (when radio-url-current
      (ignore-errors
        (if (radio-is-running)
            (radio-exit)
          (radio-play (nth 1 radio-url-current))))))
  (defun radio-mode-string-update (&optional _)
    (setq radio-mode-string
          (if radio-url-current
              (propertize (concat " ⏯️️" (car radio-url-current))
                          'face
                          (if (radio-is-running)
                              'radio-mode-line-face-playing
                            'radio-mode-line-face-stopped)
                          'mouse-face
                          'mode-line-highlight
                          'local-map
                          radio-mode-string-keymap)
            "")))
  (defvar radio-mode-string-keymap
    (let ((map (make-sparse-keymap)))
      (define-key
       map [mode-line mouse-1] 'radio-mode-string-keymap-click)
      map))
  (advice-add #'foobar-play :after #'radio-mode-string-update)
  (advice-add #'foobar-exit :after #'radio-mode-string-update)
  (advice-add #'mpv-play :after #'radio-mode-string-update)
  (advice-add #'mpv-exit :after #'radio-mode-string-update)
  (setq global-mode-string
        (append global-mode-string '(radio-mode-string))))

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
  (while (window-parameter nil 'window-side)
    ;; 在side window里，切换到其它窗口。other有可能还是side bar所以用了while
    (call-interactively 'other-window)))
(bind-key* (kbd "C-1") 'my-C-1)

;; 这个就是辅助设置`display-buffer-alist'的，设置弹出窗口很方便
(use-package shackle
  :if (bound-and-true-p enable-feature-gui)
  :hook (after-init . shackle-mode)
  :init
  (setq
   shackle-default-size 0.3
   shackle-default-rule nil ;; 最好不设置，只对有限集合设置到`shackle-rules'里
   shackle-default-alignment 'below
   shackle-select-reused-windows t
   shackle-rules
   '((" server log\\*\\'" :noselect t :align 'below :size 0.2) ; dap mode的log窗口
     (magit-status-mode :same t) ;; magit全屏
     (magit-log-mode :same t) ;; magit log全屏
     (vc-git-log-view-mode :same t) ;; vc log全屏
     ("*vc-dir*" :same t) ;; vc-dir-mode不行
     ("\\*SQLite .*"
      :regexp t ;; 默认是普通字符串
      :same t) ;; sqlite-mode全屏
     ("\\*gud-.*" :regexp t :other t :size 0.3)
     (compilation-mode :noselect t :align t :inhibit-window-quit t)
     ("*Help*" :align t :select t)
     ("\\*vc-git : .*" :align t :regexp t)
     ("*Backtrace*" :align t)
     ("*Messages*" :align t)
     ("*Messages*" :align t)
     ("*format-all-errors*" :align t)))
  :config
  (defun is-shacle-popup-buffer (buf)
    (plist-get (shackle-match buf) :align))
  (defun shackle-toogle-popup ()
    "toggle功能"
    (interactive)
    (ignore-errors
      (let ((wcount (length (window-list))))
        ;; 遍历删除shacle buffer的window
        (cl-dolist (w (window-list))
          (when (is-shacle-popup-buffer (window-buffer w))
            (delete-window w)))
        (call-interactively 'keyboard-escape-quit)
        ;; 如果窗口数没变，就可以弹出pop窗口了
        (when (eq wcount (length (window-list)))
          (if (and shackle-last-buffer
                   (buffer-live-p shackle-last-buffer))
              (display-buffer shackle-last-buffer)
            (cl-dolist
             (b (buffer-list))
             (when (is-shacle-popup-buffer b)
               (display-buffer b) ;; 这个会确保:noselect等效果
               (cl-return))))
          (echo-popup-buffers)))))
  (bind-key* (kbd "C-1") 'shackle-toogle-popup)
  (defun shackle-popup-buffer-list ()
    (let (ret)
      (dolist (b (buffer-list))
        (when (is-shacle-popup-buffer b)
          (push b ret)))
      (if (and shackle-last-buffer
               (buffer-live-p shackle-last-buffer))
          (progn
            (setq ret (delete shackle-last-buffer ret))
            (setq ret (nreverse ret))
            (push shackle-last-buffer ret))
        (setq ret (nreverse ret)))
      ret))
  (defface popup-echo-area-buried '((t :inherit shadow))
    "Echo area face for buried popups.")
  (defface popup-echo-area '((t :inverse-video t :weight bold))
    "Echo area face for opened popup.")
  (defun echo-popup-buffers (&rest _app)
    "简单实现poe-echo那样的效果"
    ;; 手动执行C-x C-e message居然没有高亮效果
    (when-let ((buffers (shackle-popup-buffer-list)))
      (message
       (cl-reduce
        #'concat
        (cons
         (propertize (funcall #'identity (buffer-name (car buffers)))
                     'face 'popup-echo-area)
         (cl-mapcar
          (lambda (buf)
            (concat
             (propertize ", " 'face 'popup-echo-area-buried)
             (propertize (funcall #'identity (buffer-name buf))
                         'face 'popup-echo-area-buried)))
          (cdr buffers)))))))
  (when (functionp 'pop-select/pop-select)
    (defun pop-select-shackle-buffer (&optional backward)
      (interactive)
      ;; 检测是否在pop buffer，是的话就切换到其它buffer
      (when (is-shacle-popup-buffer (current-buffer))
        (call-interactively 'other-window))
      (let* ((myswitch-buffer-list (shackle-popup-buffer-list))
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
        (call-interactively 'keyboard-escape-quit)
        (display-buffer (nth sel myswitch-buffer-list)))
      (advice-add
       #'pop-select-shackle-buffer
       :after #'echo-popup-buffers))
    (bind-key* (kbd "M-`") 'pop-select-shackle-buffer))
  (defvar shackle--popup-window-list nil
    "All popup windows.")

  ;; https://github.com/seagle0128/.emacs.d/blob/47c606e43a207922de6b26f03d15827f685b0b3e/lisp/init-window.el#L145
  (define-advice shackle-display-buffer (:after (&rest args) my)
    "关闭tab-line-mode"
    (with-current-buffer (ad-get-argument args 0)
      (tab-line-mode -1))))

;; 修改光标下的数字，这个支持十六进制(shift-number不支持)
(use-package evil-numbers
  :if (bound-and-true-p enable-feature-edit)
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "<C-wheel-up>") 'evil-numbers/dec-at-pt) ;; CTRL+鼠标滚动
  (global-set-key (kbd "<C-wheel-down>") 'evil-numbers/inc-at-pt))

(use-package so-long
  :disabled
  :if (bound-and-true-p enable-feature-builtin)
  :defer 1.3
  :config (global-so-long-mode))

(use-package drag-stuff
  :if (bound-and-true-p enable-feature-edit)
  :commands (drag-stuff-up drag-stuff-down drag-stuff-right drag-stuff-left)
  :init
  (global-set-key (kbd "C-<up>") 'drag-stuff-up)
  (global-set-key (kbd "C-<down>") 'drag-stuff-down)
  ;; (global-set-key (kbd "C-<left>") 'drag-stuff-left) ;; 左右移动貌似没什么用，还是留给winner-mode
  ;; (global-set-key (kbd "C-<right>") 'drag-stuff-right)
  )

(use-package elfeed
  :if (bound-and-true-p enable-feature-tools)
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
  :if (bound-and-true-p enable-feature-prog)
  :commands (csharp-mode)
  :init (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

(use-package simple
  :defer t
  :init
  (setq blink-matching-paren nil) ;; 不然blink-matching-open不显示
  :config
  ;; 很多只读buffer都是继承自`special-mode'
  (define-key special-mode-map (kbd "w") #'scroll-down-command)
  ;; blink-matching-paren为nil的话顺便把这个hook给去掉
  (remove-hook
   'post-self-insert-hook #'blink-paren-post-self-insert-function)
  (remove-hook
   'minibuffer-setup-hook
   #'minibuffer-history-isearch-setup) ;; isarch不需要？
  )

;; 弥补topsy没有timer机制，减少卡顿
(use-package mode-line-idle
  :if (bound-and-true-p enable-feature-mode-line)
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
  :if (bound-and-true-p enable-feature-mode-line)
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

(use-package w32-browser
  :if (bound-and-true-p enable-feature-win32-only)
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
     (or file (buffer-file-name (current-buffer)) default-directory)))
  (global-set-key (kbd "C-x C-d") 'browse-file-in-explorer))

;; 需要删除gud-cdb.elc，不然运行报错
(use-package gud-cdb
  :if (bound-and-true-p enable-feature-win32-only)
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
  (define-advice cdb-command-line-list-source
      (:around (orig-fn &rest args) my)
    "追加-2参数让启动后新开一个命令窗口，-G忽略进程退出的breakpoint, 禁止从网络下载symbol"
    ;; TODO: "-g"忽略初始化breakpoint，目前需要在启动breakpoint时设置断点，还不支持预先设置断点和记忆断点
    (let ((result (apply orig-fn args)))
      (setq result
            (append
             result
             (if cdb-add-g
                 '("-2" "-G" "-netsymsno" "-g")
               '("-2" "-G" "-netsymsno"))))
      result)) ;; cdb /?不对啦(跟.netsyms命令对得上)
  )

(use-package gud
  :if (bound-and-true-p enable-feature-lsp-dap)
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
     (when (bound-and-true-p gud-cdb-history)
       (setq f5-read-command nil))))
  :config
  (define-advice gud-sentinel (:after (&rest args) my)
    "自动关闭Debugger finished的gud buffer，from https://www.reddit.com/r/emacs/comments/ggs0em/autoclose_comint_buffers_on_exit_or_process_end/"
    (let ((process (ad-get-argument args 0)))
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
  (define-advice gud-display-line (:after (&rest args) my)
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
;; lldb需要codelldb，或者自带lldb-vscode
;; gdb则需要cpptools，或者等14版本发布，不过不支持pdb
;; 支持pdb只有codelldb，lldb仅支持clang编译的exe(lldb原版pdb支持有两种，1种自写的pdb解析，2种是用msdia140，测试需要设置变量LLDB_USE_NATIVE_PDB_READER=yes)。
;; 而msvc调试器的dap中转有vsdbg(cpptools里包含)支持，但在dap协议里加了handshake防止其它产品使用
(use-package dape
  :if (bound-and-true-p enable-feature-lsp-dap)
  :defer t
  :init
  (autoload 'dape "lsp/dape" "" t)
  (autoload 'dape-breakpoint-toggle "lsp/dape" "" t)
  (autoload 'dape-step-out "lsp/dape" "" t)
  (autoload 'dape-step-in "lsp/dape" "" t)
  (autoload 'dape-quit "lsp/dape" "" t)
  (global-set-key (kbd "<f9>") 'dape-breakpoint-toggle)
  (global-set-key (kbd "<f10>") 'dape-next)
  (global-set-key (kbd "<f11>") 'dape-step-in)
  (global-set-key (kbd "S-<f11>") 'dape-step-out)
  (global-set-key (kbd "S-<f5>") 'dape-quit)
  (global-set-key (kbd "<f12>") 'dape-watch-dwim)
  (defun dape-auto ()
    (interactive)
    (when (autoloadp (symbol-function 'dape))
      (load "lsp/dape"))
    (if (dape--live-connection 'stopped t)
        (call-interactively 'dape-continue)
      (call-interactively 'dape)))
  :config
  (define-advice dape--overlay-icon (:around (orig-fn &rest args) my)
    "修复断点被diff-hl遮挡问题，使用跟terminal一样的样式"
    (cl-letf (((symbol-function #'window-system)
               (lambda (&rest _) nil)))
      (apply orig-fn args)))
  (define-advice dape--next-like-command (:before (&rest args) my)
    "命令运行前重置布局"
    (run-hook-with-args 'dape-on-start-hooks))
  (define-advice dape--read-config (:around (orig-fn &rest args) my)
    "实现调试开始无须选择命令，C-u F5才需要选择命令"
    (if (or current-prefix-arg (not dape-history))
        (apply orig-fn args)
      (cl-letf (((symbol-function #'read-from-minibuffer)
                 (lambda (&rest _) (car dape-history))))
        (apply orig-fn args))))
  (define-advice dape--add-eldoc-hook (:after (&rest args) my)
    "只能在after开启eldoc-mode"
    (unless (bound-and-true-p eldoc-mode)
      (eldoc-mode 1)))
  (setq dape-autoport 1890)
  (define-advice dape-config-autoport
      (:around (orig-fn &rest args) my)
    "autoport不知道是emacs还是哪里出问题了，之前都没问题"
    (cl-letf (((symbol-function #'process-contact)
               (lambda (&rest _)
                 (list "localhost" dape-autoport)
                 (setq dape-autoport (+ 1 dape-autoport)))))
      (apply orig-fn args)))

  ;; 下载https://github.com/vadimcn/codelldb/releases 文档https://github.com/vadimcn/codelldb/blob/v1.10.0/MANUAL.md
  ;; 测试codelldb命令行程序不能弹窗口，输出也不知道去了哪里。还有就是很慢。
  ;; 因为llvm在PATH里，删除lldb-vscode影响主力debugger
  (assq-delete-all 'lldb-vscode dape-configs)
  ;; cpp, rust https://github.com/microsoft/vscode-cpptools/releases ，解压vsix
  ;; :logging (:engineLogging t) ;; 开启调试消息

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
  (cl-defmethod dape-handle-request
      (_conn (_command (eql handshake)) arguments)
    (list :signature (my-calc (plist-get arguments :value))))
  (define-advice dape-request (:around (orig-fn &rest args) my)
    "stackTrace要求参数startFrame，这是符合标准的 https://microsoft.github.io/debug-adapter-protocol/specification "
    (when (equal (ad-get-argument args 1) "stackTrace")
      (setq args
            (-replace-at
             2
             (append
              (ad-access-argument args 2) (list :startFrame 0))
             args)))
    (apply orig-fn args))

  ;; (setq dape-debug t)
  (defun dape-find-file (&optional default)
    "Read filename without any ignored extensions at project root.
DEFAULT specifies which file to return on empty input."
    (let* ((completion-ignored-extensions nil)
           (default-directory (funcall dape-cwd-fn t))
           (file
            (expand-file-name
             (read-file-name (if default
                                 (format "Program (default %s): "
                                         default)
                               "Program: ")
                             default-directory default t))))
      (if (tramp-tramp-file-p file)
          (tramp-file-name-localname (tramp-dissect-file-name file))
        file)))
  (setq
   dape-cppvsdbg-command
   (expand-file-name
    "~/.emacs.d/.extension/extension/debugAdapters/vsdbg/bin/vsdbg.exe"))
  (add-to-list
   'dape-configs
   `(vsdbg
     modes
     (c-mode c-ts-mode c++-mode c++-ts-mode)
     ensure
     dape-ensure-command
     command-cwd
     dape-command-cwd
     command
     dape-cppvsdbg-command
     command-args
     ["--interpreter=vscode"]
     :type "cppvsdbg" ;; 必须是这个
     :cwd dape-cwd-fn
     :program dape-find-file
     ;; :stopAtEntry t ;; 支持
     ;; :logging (:engineLogging t) ;; 支持，不要随便开，还以为是dape的问题
     )))

(defvar f5-history '()) ;; session只记录list的值
(defun my-shell-switch ()
  "最近buffer和最近shell切换"
  (interactive)
  (let ((to-shell
         (not
          (string-match
           "\*\\(Power\\)?[e]?[Ss]hell\*"
           (buffer-name (current-buffer)))))
        (create-shell t)
        (showed-buffer-list (ep-tabbar-buffer-list)))
    ;; 当前为shell buffer，切换到最近其它buffer
    (cl-dolist
     (b (buffer-list))
     (if (string-match "\*\\(Power\\)?[e]?[Ss]hell\*" (buffer-name b))
         (when to-shell
           (switch-to-buffer b)
           (setq create-shell nil)
           (cl-return))
       (unless to-shell
         (when (memq b showed-buffer-list)
           (switch-to-buffer b)
           (cl-return)))))
    (when (and create-shell to-shell)
      (call-interactively 'shell)))) ;; eshell相比shell更好的是prompt删不掉
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
  :if (bound-and-true-p enable-feature-navigation)
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
  ;; (with-eval-after-load 'view
  ;;   (setq view-mode-map (make-sparse-keymap)))
  (add-hook
   'view-mode-hook
   (lambda ()
     (god-local-mode
      (if buffer-read-only
          +1
        -1))))
  ;; 方便god mode的键绑定
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  :config
  (define-key god-local-mode-map (kbd "i") 'view-mode)
  (define-key god-local-mode-map (kbd "<") 'beginning-of-buffer)
  (define-key god-local-mode-map (kbd ">") 'end-of-buffer)
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
  :if (bound-and-true-p enable-feature-gui)
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
  (define-advice dashboard-insert-recents
      (:around (orig-fn &rest args) my)
    "避免调用`recentf-cleanup'导致首次启动时间长到10秒。新版本已含`dashboard-remove-missing-entry'开关"
    (cl-letf (((symbol-function #'recentf-cleanup)
               (lambda (&rest _))))
      (recentf-mode 1)
      ;; 这里临时缩短`recentf-list'确保处理时间不会太长
      (let ((recentf-list (dashboard-subseq recentf-list 20)))
        (apply orig-fn args))))
  (advice-remove 'require 'dashboard-load-bypass-ffap)
  (use-package ffap
    :commands (ffap-guesser))
  ;; 用god-mode的快捷键
  (define-key
   dashboard-mode-map (kbd "C-r") 'dashboard-jump-to-recents)
  (define-key
   dashboard-mode-map (kbd "C-SPC") 'dashboard-jump-to-projects) ;; P
  (define-key dashboard-mode-map (kbd "C-g") 'revert-buffer)
  (define-key
   dashboard-mode-map (kbd "C-d") 'dashboard-remove-item-under)
  (defun my-god-mode-init ()
    (when (functionp 'god-local-mode)
      (god-local-mode 1))
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
    :if (bound-and-true-p enable-feature-tools)
    :defer t
    :config
    (when (functionp 'god-local-mode)
      (add-hook 'sqlite-mode-hook 'god-local-mode))
    ;; for god-mode
    (define-key sqlite-mode-map (kbd "C-c") 'sqlite-mode-list-columns)
    (define-key sqlite-mode-map (kbd "C-g") 'sqlite-mode-list-tables)
    (define-key sqlite-mode-map (kbd "C-d") 'sqlite-mode-delete)))

(use-package ggtags
  :if (bound-and-true-p enable-feature-navigation)
  :commands (ggtags--xref-backend ggtags--xref-find-tags ggtags-check-project)
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
  (defun ggtag-find-refercense (identifier)
    "TODO: 或许可以添加到`xref-find-references'"
    (interactive (list
                  (xref--read-identifier "Find references of: ")))
    (let ((xref-backend-functions '(ggtags-reference-backend)))
      (xref--find-definitions identifier nil)))

 (defun ggtags-create (root)
   (interactive "DRoot directory: ")
   (let* ((label
           (consult--read '("default" "native" "ctags" "pygments")))
          (default-directory root))
     (async-shell-command
      (concat
       "gtags -q --gtagslabel="
       (if (equal "pygments" label)
           (concat
            label
            " --gtagsconf="
            (expand-file-name "bin/gtags.conf" user-emacs-directory))
         label)))))
  (defun ggtags-update ()
    (interactive)
    (ggtags-check-project)
    (async-shell-command "global -u"))
  :config
  (fmakunbound 'ggtags-find-reference) ;; 删除，避免混淆上面的`ggtag-find-refercense'
  ;; 修改type使支持consult-xref preview
  (define-advice ggtags--xref-collect-tags
      (:around (orig-fn &rest args) my)
    (cl-letf*
        ((org-expand-file-name (symbol-function #'expand-file-name))
         (root (nth 1 args))
         ((symbol-function #'expand-file-name)
          (lambda (&rest args)
            ;; 解决偶尔打开不存在路径的问题，是因为default-directory为空，expand-file-name参数加上root就可以了
            (if (not default-directory)
                (apply org-expand-file-name (list (car args) root))
              (apply org-expand-file-name args)))))
      (let ((result (apply orig-fn args)))
        (dolist (l result)
          ;; slot object可以用array操作
          (aset (aref l 2) 0 'xref-file-location))
        result)))
  (fmakunbound 'ggtags-create-tags) ;; 去掉避免干扰，用我们的async版本
  (fmakunbound 'ggtags-update-tags)
  (define-advice ggtags-global-build-command
      (:around (orig-fn &rest args) my)
    "加上-srax支持成员变量，参考https://github.com/austin-----/code-gnu-global/issues/29"
    (let ((result (apply orig-fn args)))
      (when force-search-variable
        (setq result (concat result " -srax")))
      result))
  (define-advice ggtags--xref-backend
      (:around (orig-fn &rest args) my)
    "不检查`ggtags-completion-table'，避免多启动一次global进程影响速度"
    'ggtags ;; 原版非函数时返回nil(GTAGS里没有成员变量，而GRTAGS里有，用global -sax可以查到)
    )
  (define-advice ggtags--xref-find-tags
      (:around (orig-fn &rest args) my)
    "修复xref无效，问题出在调用`call-process'多了双引号，去掉就可以了"
    (cl-letf (((symbol-function #'shell-quote-argument)
               (lambda (argument &optional posix) argument)))
      (apply orig-fn args))))


;; 这个比grugru要更灵活些，缺点是cursor会变，也没有`grugru-highlight-mode'高亮
(use-package cycle-at-point
  :if (bound-and-true-p enable-feature-edit)
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
  (define-advice cycle-at-point-preset-c-mode
      (:around (orig-fn &rest args) my)
    "追加自定义的"
    (let ((result (apply orig-fn args)))
      (setq result
            (append
             (list
              '(:data ("EXPECT_FALSE" "EXPECT_TRUE") :case-fold t))
             (list
              '(:data ("ASSERT_FALSE" "ASSERT_TRUE") :case-fold t))
             (list
              '(:data ("EXPECT_STREQ" "EXPECT_STRNE") :case-fold t))
             (list
              '(:data ("ASSERT_STREQ" "ASSERT_STRNE") :case-fold t))
             (list
              '(:data
                ("ASSERT_STRCASEEQ" "ASSERT_STRCASENE")
                :case-fold t))
             (list
              '(:data
                ("EXPECT_STRCASEEQ" "EXPECT_STRCASENE")
                :case-fold t))
             (list 'cycle-at-point-find-include) ;; 要排在<>的前面
             result (list 'cycle-at-point-name-convertion)))
      result))
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
    (let
        ((result (list))
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


;; bin下载 https://github.com/lynnux/tree-sitter-module/releases
(use-package treesit
  :if
  (and (fboundp 'treesit-language-available-p)
       (bound-and-true-p enable-feature-prog))
  :commands (treesit-parser-create)
  :defer t
  :init
  ;; M-x -ts-mode提取出来的
  (setq
   treesit-font-lock-level 3 ;; 4最大化高亮
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
     yaml-ts-mode
     lua-ts-mode))
  (cl-dolist
   (ts1 all-ts-mode)
   (let (ts
         dll)
     (pcase ts1
       (`(,ts-mode . ,ts-file) (setq ts ts-mode) (setq dll ts-file))
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
   'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  (add-hook
   'markdown-mode-hook
   #'(lambda () (treesit-parser-create 'markdown)))
  (add-hook 'zig-mode-hook #'(lambda () (treesit-parser-create 'zig)))
  (add-hook
   'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
  (add-hook
   'lisp-data-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  (add-hook
   'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  (with-eval-after-load 'c-ts-mode
    ;; 虽然会根据是否包含c++的头文件来判断是否是cpp，这里直接强制就是cpp
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))))
(use-package fingertip
  :if
  (and (fboundp 'treesit-language-available-p)
       (bound-and-true-p enable-feature-prog))
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
            'lisp-data-mode-hook
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
  (define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
  (define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
  (define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
  (define-key fingertip-mode-map (kbd "M-s") 'fingertip-unwrap)

  ;; 修复cpp`C-k'bug
  ;; 测试for(auto | xxx)在|位置(fingertip-end-of-list-p (point) (line-end-position))执行为nil，而正常返回t(关闭ts-mode就正常了)
  (define-advice fingertip-common-mode-kill
      (:around (orig-fn &rest args) fixcpp)
    "不知道为啥对c/cpp只调用`kill-line'了事？"
    (cl-letf (((symbol-function #'derived-mode-p) (lambda (_) nil)))
      (apply orig-fn args))))
(use-package tree-sitter
  :if
  (and (not (fboundp 'treesit-language-available-p))
       (bound-and-true-p enable-feature-prog))
  :commands (tree-sitter-mode tree-sitter-force-update tree-sitter-setup-timer)
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
   'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
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
    (define-advice tree-sitter--setup (:after (&rest args) my)
      "去掉hook，改为timer模式"
      (remove-hook 'after-change-functions #'tree-sitter--after-change
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
  :if
  (and (not (fboundp 'treesit-language-available-p))
       (bound-and-true-p enable-feature-prog))
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
      (remove-hook 'after-change-functions 'jit-lock-after-change t)))
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
  :if
  (and (not (fboundp 'treesit-language-available-p))
       (bound-and-true-p enable-feature-prog))
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
   grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key
   grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
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
             #'grammatical-edit-backward-delete)))))))

;; 保存当前windows配置为bookmark
(use-package burly
  :if (bound-and-true-p enable-feature-tools)
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

(use-package elisp-autofmt
  :if (bound-and-true-p enable-feature-tools)
  :defer t
  :init
  (autoload 'elisp-autofmt-buffer "tools/elisp-autofmt.el" "" t)
  (autoload 'elisp-autofmt-region "tools/elisp-autofmt.el" "" t)
  (with-eval-after-load 'elisp-mode
    (define-key
     emacs-lisp-mode-map [(meta f8)]
     (lambda ()
       "禁止message提示"
       (interactive)
       (if buffer-read-only
           (signal 'text-read-only nil)
         (let ((inhibit-message t)
               (pos (point)))
           (if (use-region-p)
               (call-interactively 'elisp-autofmt-region)
             (call-interactively 'elisp-autofmt-buffer))
           (goto-char pos))
         (message "elisp format done.")))))
  :config
  ;; 使用内置的use-package格式化会有问题，必须加把use-package加入到`load-path'
  ;; see https://codeberg.org/ideasman42/emacs-elisp-autofmt/issues/4
  (add-to-list
   'load-path "~/.emacs.d/packages/use-package/use-package-master")
  (setq-default elisp-autofmt-load-packages-local '("use-package"))
  (with-eval-after-load 'diff-hl
    ;; 修复格式化后diff-hl消失问题
    (define-advice elisp-autofmt--region (:after (&rest args) my)
      (diff-hl-update))))

(use-package format
  :defer t
  :init
  (setq format-alist nil) ;; `format-decode'会被c函数`insert-file-contents'调用，而里面都是些用不到的文件头，故而可以屏蔽加快启动。
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

(use-package shell
  :if (bound-and-true-p enable-feature-tools)
  :defer t
  :init
  (setq confirm-kill-processes nil)
  (setq comint-input-sender 'my-shell-simple-send)
  (defun my-shell-simple-send (proc command)
    "添加额外命令cls清屏"
    (cond
     ((string-match "^[ \t]*cls[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer))
     ;; Send other commands to the default handler.
     (t
      (comint-simple-send proc command))))
  :config
  (define-key shell-mode-map '[up] 'comint-previous-input)
  (define-key shell-mode-map '[down] 'comint-next-input)

  ;; 在任何位置按M-p/n都可以自动跳到命令输入位置
  (define-advice comint-previous-input (:before (&rest args) my)
    (comint-goto-process-mark))
  (define-advice comint-next-input (:before (&rest args) my)
    (comint-goto-process-mark))

  ;; No confirm kill process: for *shell* and *compilation*
  ;; https://emacs.stackexchange.com/q/24330
  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
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
  (add-hook
   'shell-mode-hook
   (lambda ()
     (make-local-variable 'comint-prompt-read-only)
     (setq comint-prompt-read-only t) ;; 让不删除prompt
     (kill-buffer-on-shell-logout)
     (set-no-process-query-on-exit))))


;; 这个比shell更好的是prompt删不掉，但不支持powershell自身的快捷键
(use-package powershell
  :commands (powershell)
  :config
  (define-advice powershell (:after (&rest args) my)
    "解决kill*powershell*窗口延迟3秒的问题"
    (remove-hook 'kill-buffer-hook 'powershell-delete-process))
  (define-advice powershell--get-max-window-width
      (:around (orig-fn &rest args) my)
    "解决启动延迟3秒问题"
    ;; powershell里得到的是240，但emacs里得到是200，改为240有问题。写死应该是没问题的
    (setq powershell--max-window-width 200)))

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

(use-package cal-china
  :defer 5.5 ;; 不知为何，直接加载会报错
  :init
  (use-package calendar
    :commands (calendar-absolute-from-gregorian calendar-current-date))
  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (setq calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; from https://github.com/xwl/cal-china-x/blob/master/cal-china-x.el
  (defconst cal-china-x-day-name
    ["初一"
     "初二"
     "初三"
     "初四"
     "初五"
     "初六"
     "初七"
     "初八"
     "初九"
     "初十"
     "十一"
     "十二"
     "十三"
     "十四"
     "十五"
     "十六"
     "十七"
     "十八"
     "十九"
     "廿"
     "廿一"
     "廿二"
     "廿三"
     "廿四"
     "廿五"
     "廿六"
     "廿七"
     "廿八"
     "廿九"
     "三十"
     "卅一"
     "卅二"
     "卅三"
     "卅四"
     "卅五"
     "卅六"
     "卅七"
     "卅八"
     "卅九"
     "卅十"])
  (defun my-calendar-chinese-sexagesimal-name (n)
    (format "%s%s"
            (aref calendar-chinese-celestial-stem (% (1- n) 10))
            (aref calendar-chinese-terrestrial-branch (% (1- n) 12))))
  (defun get-chinese-wannianli (&optional date)
    "获取万年历，参考`calendar-chinese-date-string'实现"
    (let* ((a-date
            (calendar-absolute-from-gregorian
             (or date (calendar-current-date))))
           (c-date (calendar-chinese-from-absolute a-date))
           (cycle (car c-date))
           (year (cadr c-date))
           (month (nth 2 c-date))
           (day (nth 3 c-date))
           (cn-month-string
            (concat
             (aref
              calendar-chinese-month-name-array (1- (floor month)))
             (if (integerp month)
                 ""
               "(闰月)")))
           (this-month
            (calendar-chinese-to-absolute (list cycle year month 1)))
           (next-month
            (calendar-chinese-to-absolute
             (list
              (if (= year 60)
                  (1+ cycle)
                cycle)
              (if (= (floor month) 12)
                  (1+ year)
                year)
              ;; Remainder of (1+(floor month))/12, with
              ;; 12 instead of 0.
              (1+ (mod (floor month) 12)) 1))))
      ;; TODO: 月干支有疑问，有些八字排盘按节气月排。目前月干支不是按节气月，跟元享利贞一致。
      (if (not (integerp month))
          ;; 闰月
          (setq month (floor month))
        (if (< 30 (- next-month this-month))
            ;; 闰月的前一个月
            (setq month (1- month))
          ;; 正常月
          (ignore)))
      (format "%s%s日, %s年, %s月, %s日"
              cn-month-string
              (seq-elt cal-china-x-day-name (1- day))
              (my-calendar-chinese-sexagesimal-name year)
              (format "%s"
                      (my-calendar-chinese-sexagesimal-name
                       (+ (* 12 year) month 50)))
              (my-calendar-chinese-sexagesimal-name (+ a-date 15)))))
  ;; (get-chinese-wannianli '(5 4 2020))
  ;; (get-chinese-wannianli '(5 5 2020)) ;; 立夏变更月干支
  ;; (get-chinese-wannianli '(5 22 2020))
  ;; (get-chinese-wannianli '(5 23 2020))
  ;; (get-chinese-wannianli '(7 22 2017))
  ;; (get-chinese-wannianli '(7 23 2017))
  ;; (calendar-chinese-date-string '(5 4 2021))
  ;; (calendar-chinese-date-string '(5 5 2021))
  ;; (calendar-chinese-date-string '(5 1 2021)) ;; 非闰月正常
  ;; (calendar-chinese-date-string '(5 1 2020)) ;; 闰月前一个月的日的干支是错的！
  ;; (calendar-chinese-date-string '(6 1 2020)) ;; 闰月当月的日干支甚至没有！
  :config
  (setq calendar-chinese-month-name-array
        ["正月"
         "二月"
         "三月"
         "四月"
         "五月"
         "六月"
         "七月"
         "八月"
         "九月"
         "十月"
         "冬月"
         "腊月"])
  (setq display-time-string-forms
        '((concat
           (propertize (format-time-string display-time-format now)
                       'face
                       'display-time-date-and-time
                       'help-echo
                       (format-time-string "%a %b %e, %Y" now))
           " " (get-chinese-wannianli))))
  (display-time-update))

;; 这个mode各种问题，将就用就行
(use-package antlr-mode
  :defer t
  :config
  ;; 解决编辑报错，导致`delete-selection-mode'出问题
  (add-hook 'antlr-delayed-mode-hook
            (lambda ()
              (setq-local
               font-lock-extend-after-change-region-function nil))
            50 ;; 一定要在自带hook的后面执行
            ))

;; 代替`eval-expression'M-;
(use-package ielm
  :disabled
  :defer t
  :init
  (setq ielm-header "")
  (setq ielm-prompt
        (propertize (char-to-string #x261B)
                    'face
                    '(foreground-color . "cyan")
                    'display
                    '(raise 0)))
  (defun my-eilm ()
    (interactive)
    (let ((buf (buffer-name (current-buffer))))
      (ielm)
      (ielm-change-working-buffer buf)))
  (bind-key* (kbd "M-:") 'my-eilm)
  :config)

;; 丝滑效果实际就是`while-no-input'循环
(use-package pixel-scroll
  :disabled
  :defer 0.3
  :config
  (setq
   pixel-scroll-precision-interpolate-page
   t ;; 为了`pixel-scroll-interpolate-down'能调用`pixel-scroll-precision-interpolate'
   pixel-scroll-precision-interpolation-total-time 0.1)
  ;; 没必要，全靠`pixel-scroll-precision-interpolate'函数
  ;; (pixel-scroll-precision-mode 1) 
  (defun my-wheel-up ()
    (interactive)
    ;; 鼠标滚动要更快，不然感觉慢
    (let ((pixel-scroll-precision-interpolation-total-time 0.05))
      ;; my-scroll-down-command测试好像有问题
      (my-scroll-up-command -3)))
  (defun my-wheel-down ()
    (interactive)
    ;; 鼠标滚动要更快，不然感觉慢
    (let ((pixel-scroll-precision-interpolation-total-time 0.05))
      (my-scroll-up-command 3)))
  ;; 鼠标滚动感觉有点卡，而且C-n/p有可能造成屏幕移动
  (bind-key* [wheel-up] 'my-wheel-up)
  (bind-key* [wheel-down] 'my-wheel-down)
  (defun my-scroll-up-command (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate
         (* -1 lines (pixel-line-height)))
      (pixel-scroll-interpolate-down)))
  (defun my-scroll-down-command (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate
         (* lines (pixel-line-height))))
    (pixel-scroll-interpolate-up))
  (advice-add #'scroll-up-command :override #'my-scroll-up-command)
  (advice-add
   #'scroll-down-command
   :override #'my-scroll-down-command))

(use-package format-all
  :commands (format-all-region-or-buffer)
  :init
  (defun my-format-all ()
    (interactive)
    (if buffer-read-only
        (signal 'text-read-only nil)
      ;; 其它都用`format-all'
      (call-interactively 'format-all-region-or-buffer)))
  (global-set-key [(meta f8)] 'my-format-all)
  ;; from https://github.com/purcell/inheritenv/blob/main/inheritenv.el
  (defun inheritenv-apply (func &rest args)
    (cl-letf* (((default-value 'process-environment)
                process-environment)
               ((default-value 'exec-path) exec-path))
      (apply func args)))
  (defmacro inheritenv (&rest body)
    `(inheritenv-apply (lambda () ,@body)))
  (provide 'inheritenv)
  (provide 'language-id)
  (defun language-id-buffer ()
    ;; 在`format-all-default-formatters'找对应的字符串
    (or (and (memq major-mode '(c++-mode c++-ts-mode c-mode c-ts-mode)) "C++")
        ;; xml格式化工具 https://github.com/htacg/tidy-html5
        (and (memq major-mode '(nxml-mode)) "XML")
        ;; python格式化工具 pip install black
        (and (memq major-mode '(python-mode python-ts-mode)) "Python")
        (and (memq major-mode '(rust-mode rust-ts-mode)) "Rust")
        (and (memq major-mode '(mhtml-mode)) "HTML")))
  :config
  ;; 设置喜好的format后端
  (setq-default format-all-formatters
                '(("Python" black)
                  ("C++" clang-format) ("HTML" html-tidy)
                  ;; tidy好像不支持设置indent宽度，将就用吧
                  ;; ("XML" (html-tidy "-q --tidy-mark no -i 4 -xml"))
                  ))
  (add-to-list
   'process-coding-system-alist
   '("[tT][iI][dD][yY]" . (utf-8 . utf-8)))
  (add-to-list
   'process-coding-system-alist
   '("[bB][lL][aA][cC][kK]" . (utf-8 . utf-8)))
  (add-to-list
   'process-coding-system-alist
   '("[rR][uU][sS][tT][fF][mM][tT]" . (utf-8 . utf-8))))

;; beacon效果，不过我们只需要某些命令advice就够用了
(when (fboundp 'pop-select/beacon-set-parameters)
  (setq beacon-blink-delay 0.01)
  (setq beacon-blink-duration 0.1)
  (pop-select/beacon-set-parameters 900 20 #x51 #xaf #xef 50)
  (defun my-pulse-momentary-line (&rest _)
    (ignore-errors
      (when (and (not disable-beacon)
                 (frame-visible-p (window-frame)) ;; 可以阻止最小化时弹窗
                 )
        (let*
            ((p (window-absolute-pixel-position))
             (x (car p))
             (h (line-pixel-height))
             (y
              (if header-line-format
                  (- (cdr p) h) ;; 修复开启`header-line-format'时y值不正确
                (cdr p))))
          (when p
            (pop-select/beacon-blink
             x y
             (truncate (* beacon-blink-duration 1000)) ; timer
             (truncate (* beacon-blink-delay 1000)) ; delay
             ))))))
  (dolist (cmd
           '(recenter-top-bottom
             other-window
             switch-to-buffer
             scroll-on-jump-advice--wrapper
             consult--jump
             recenter))
    (advice-add cmd :after #'my-pulse-momentary-line))
  (defvar disable-beacon nil
    "")
  (with-eval-after-load 'consult
    ;; consult preview调用了recenter，这里屏蔽它的beacon效果
    (define-advice consult--jump-preview
        (:around (orig-fn &rest args) my)
      (let ((org-ret (apply orig-fn args)))
        (lambda (action cand)
          (let ((disable-beacon t))
            (funcall org-ret action cand)))))))

(use-package gn-mode
  :commands (gn-mode)
  :init (add-to-list 'auto-mode-alist '("\\.gni?\\'" . gn-mode)))

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
  :if (display-graphic-p)
  :defer t
  :init
  (delay-require-libs
   "~/.emacs.d/themes/themes-master" '(doom-themes))
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
      ;;   (with-eval-after-load 'org-mode
      ;;     (doom-themes-org-config))      
      ))
  ;; (random-load-doom-theme (mapcar 'get-theme (directory-files "~/.emacs.d/themes/themes-master/themes" t "^[a-zA-Z0-9].*.el$")))
  (random-load-doom-theme
   (list
    ;; 'doom-horizon
    ;; 'modus-vivendi
    ;; 'modus-vivendi-tritanopia
    ;; 'doom-snazzy
    ;; 'doom-city-lights
    ;; 'doom-material
    ;; 'doom-tomorrow-night
    ;; 'doom-one ;; 有点太浅了
    ;; 'spacemacs-dark
    'doom-monokai-pro ;; monokai这个系列对比度都还行
    )))

;; (load-theme 'modus-vivendi t)
;; (load-theme 'dracula t)

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
   ((eq th 'doom-monokai-pro)
    (custom-set-faces
     '(vertico-group-title ((t (:foreground "#939293"))))))
   (t))

  ;; fix doom的bug，`(require 'cus-edit)'触发
  (custom-push-theme 'theme-face 'custom-button th 'reset)
  (custom-push-theme 'theme-face 'custom-button-unraised th 'reset)
  (custom-push-theme
   'theme-face 'custom-button-pressed-unraised th 'reset)
  (custom-push-theme 'theme-face 'custom-button-pressed th 'reset)
  (custom-push-theme 'theme-face 'custom-button-mouse th 'reset)

  (when (string-prefix-p "doom" (symbol-name th))
    ;; doom没有处理hi-lock的颜色，`doom-themes--colors'变量含doom内置color
    (custom-set-faces
     `(hi-yellow
       ((t (:foreground "black" :background ,(doom-color 'yellow)))))
     `(hi-pink
       ((t (:foreground "black" :background ,(doom-color 'magenta)))))
     `(hi-green
       ((t (:foreground "black" :background ,(doom-color 'green)))))
     `(hi-blue
       ((t (:foreground "black" :background ,(doom-color 'blue)))))
     `(hi-salmon
       ((t (:foreground "black" :background ,(doom-color 'orange)))))
     `(hi-aquamarine
       ((t (:foreground "black" :background ,(doom-color 'violet)))))
     `(hi-black-b
       ((t (:foreground "white" :background ,(doom-color 'base6)))))
     `(hi-blue-b
       ((t (:foreground "white" :background ,(doom-color 'blue)))))
     `(hi-red-b
       ((t (:foreground "white" :background ,(doom-color 'red)))))
     `(hi-green-b
       ((t (:foreground "white" :background ,(doom-color 'green)))))
     `(hi-black-hb
       ((t
         (:foreground "white" :background ,(doom-color 'orange)))))))
  (when (not (string-prefix-p "doom" (symbol-name th)))
    (set-face-attribute 'doom-themes-visual-bell nil
                        :background "#ff6c6b")))

;; 所有theme共用
(set-face-attribute 'show-paren-match nil :underline t :weight 'bold)
(custom-set-faces
 '(default ((t (:background "#000000")))) ;; 背景色统一用黑色
 '(header-line ((t (:weight bold))))
 '(consult-file ((t (:foreground unspecified))))
 '(consult-bookmark ((t (:foreground unspecified))))
 '(consult-async-running ((t (:inherit mode-line-inactive))))
 '(dashboard-items-face ((t (:weight unspecified))))
 `(org-level-1
   ((t (:weight bold :foreground ,(doom-color 'blue) :height 1.2))))
 `(org-level-2
   ((t
     (:weight bold :foreground ,(doom-color 'magenta) :height 1.15))))
 `(org-level-3
   ((t
     (:weight bold :foreground ,(doom-color 'violet) :height 1.1)))))
;; (set-face-attribute 'hl-line nil :background "#2B2B2B")
;; 当前行加下划线
;; (custom-set-faces
;;  '(vertico-current
;;    ((t
;;      (:inherit
;;       unspecified
;;       :underline t
;;       :background nil
;;       :distant-foreground nil
;;       :foreground nil))))
;;  '(consult-preview-line ((t (:underline t :background nil)))))
;; (set-face-attribute 'idle-highlight nil :inverse-video t) ;; 这个是反色效果，不同的位置颜色可能不同
