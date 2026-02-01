;; -*- coding: utf-8; lexical-binding: t; -*-
;; 说明：
;; 自带的lisp包设置等
;; 自带的不用加require，因为xxx-mode基本上都是autoload！
;; C-x f filecache everything recent-visit/changed find-file-at-point

;; 在这里开启/关闭feature，用于调试问题
(dolist (f
         '(enable-feature-builtin
           enable-feature-minibuffer
           enable-feature-win32-only
           enable-feature-edit
           enable-feature-lsp-dap
           enable-feature-dired
           enable-feature-gui
           enable-feature-navigation
           enable-feature-tools
           enable-feature-prog
           enable-feature-mode-line))
  (eval `(defvar ,f t)))

;; use-package含有bind-keys*，优先级最高，参考`override-global-mode'代码
(unless (symbol-function 'use-package)
  (add-to-list
   'load-path "~/.emacs.d/packages/use-package/use-package-master")
  (condition-case nil
      (require 'use-package)
    (error
     (ignore)
     ;; (update-all-packages)
     )) ;; 首次检查是否解压，之后还是手动更新包吧
  )

(setq history-length 200)

(defun my-kill-region (prefix)
  "解决`The mark is not set now, so there is no region'，自己定义一个`kill-region'就可以了"
  (interactive "p")
  (if (or (use-region-p) (not (called-interactively-p 'any)))
      (funcall 'kill-region (region-beginning) (region-end) 'region)
    (let ((string
           (filter-buffer-substring
            (line-beginning-position) (line-beginning-position 2)
            t)))
      (when (> (length string) 0)
        (put-text-property
         0 (length string) 'yank-handler '(yank-line)
         string))
      (kill-new string nil))))
(bind-key* [remap kill-region] 'my-kill-region)

;;buffer管理，真的太好用了！
(global-set-key (kbd "C-x b") 'bs-show) ;这个更好
(with-eval-after-load 'bs
  (setq bs-default-configuration "files-and-scratch")
  (define-key bs-mode-map "s" 'bs-show-sorted)
  (define-key bs-mode-map "S" 'bs-save)
  (define-key bs-mode-map "<" 'beginning-of-buffer)
  (define-key bs-mode-map ">" 'end-of-buffer))

(use-package server
  :defer 0.5
  :config
  ;; 解决win7上的不安全提示信息
  (and (>= emacs-major-version 23)
       (fset #'server-ensure-safe-dir #'ignore))
  (when (string-equal system-type "windows-nt")
    (server-start))
  ;; 去掉关闭emacsclientw打开的文件的提示
  (remove-hook
   'kill-buffer-query-functions 'server-kill-buffer-query-function))

;; org mode
(use-package org
  :defer t
  :init
 (setq
  org-startup-indented t ; 开启`org-indent'
  org-modules '() ;; 造成org文件打开慢的真凶！
  org-hide-emphasis-markers nil ;; 不显示`org-emphasis-alist'
  org-ellipsis "⤸" ;; ⤵ ▼ ▽ ⌄ ⌵ ⏑ 尽量不用实心的，不然太突兀。显示不出来可以装个unifont
  org-log-done 'time ; 给已完成事项打上时间戳。可选 note，附加注释
  org-startup-folded 'show4levels ; 打开时折叠
  org-confirm-babel-evaluate nil ;; org babel执行不需要warning提示
  org-src-preserve-indentation t ;; for src indent
  )

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t)))
  
  ;; org的C-c C-o居然不走find-file
  (add-to-list 'org-file-apps '("\\.docx?\\'" . default))
  (add-to-list 'org-file-apps '("\\.pcapn?g?\\'" . default))
  (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))

  ;; 给heaedr line添加click keymap，参考 
  ;; https://github.com/sabof/org-bullets/blob/master/org-bullets.el
  ;; https://kitchingroup.cheme.cmu.edu/blog/2017/06/10/Adding-keymaps-to-src-blocks-via-org-font-lock-hook/
  (defvar org-level-click-map '(keymap (mouse-1 . org-cycle))
    "")
  (defun org-add-keymap-to-level (limit)
    (let ((case-fold-search t))
      (while (re-search-forward org-heading-regexp limit t)
        (put-text-property
         (match-beginning 0)
         (match-end 0)
         'keymap
         org-level-click-map))))
  (add-hook 'org-font-lock-hook #'org-add-keymap-to-level)

  ;; 测试英文也是需要空格，所以干脆把emphasis加上零宽空格
  ;; 使汉字的emphasis不需要输入额外的空格，参考https://www.cnblogs.com/apirobot/p/15366984.html
  ;; (dolist (index '(0 1))
  ;;   (setcar
  ;;    (nthcdr index org-emphasis-regexp-components)
  ;;    (string-replace
  ;;     "[:space:]" "[:multibyte:][:space:]"
  ;;     (nth index org-emphasis-regexp-components))))
  ;; (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  ;; (org-set-emph-re
  ;;  'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; 添加markdown的代码标记
  (add-to-list 'org-emphasis-alist '("`" org-code verbatim))
  (setq org-verbatim-re
        (string-replace "[=~]" "[=~`]" org-verbatim-re))
  (define-advice org-do-emphasis-faces
      (:around (orig-fn &rest args) my)
    (cl-letf* ((org-member (symbol-function #'member))
               (org-format (symbol-function #'format))
               ((symbol-function #'format)
                (lambda (&rest args)
                  (if (equal
                       (car args) "\\([%s]\\|^\\)\\([~=*/_+]\\)")
                      (progn
                        (setcar args "\\([%s]\\|^\\)\\([~=*/_+`]\\)")
                        (apply org-format args))
                    (apply org-format args))))
               ((symbol-function #'member)
                (lambda (elt list)
                  (if (equal list '("~" "="))
                      (funcall org-member elt '("~" "=" "`"))
                    (funcall org-member elt list)))))
      (apply orig-fn args)))

  (add-hook
   'org-mode-hook
   (lambda ()
     ;; 禁止<>自动，因为<src什么的还是很常用的
     (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                    (if (char-equal c ?<)
                        t
                      (,electric-pair-inhibit-predicate c))))

     ;; (when (functionp 'tempel-insert)
     ;;   ;; emphasis字符加上零宽空格
     ;;   (defmacro add_zero_space_char (ch)
     ;;     `(lambda ()
     ;;        (interactive)
     ;;        ;; r支持region！但注意有些键`easy-mark'会影响到，如=
     ;;        (tempel-insert (list "" "\x200B" ,ch 'r ,ch "\x200B"))))
     ;;   (local-set-key "*" (add_zero_space_char "*"))
     ;;   (local-set-key "/" (add_zero_space_char "/"))
     ;;   (local-set-key "_" (add_zero_space_char "_"))
     ;;   (local-set-key "=" (add_zero_space_char "="))
     ;;   (local-set-key "~" (add_zero_space_char "~"))
     ;;   (local-set-key "+" (add_zero_space_char "+"))
     ;;   (local-set-key "`" (add_zero_space_char "`")))
     )))

(use-package org-publish
  :defer t
  :init
  (defvar website-org-path nil)
  (defvar website-org-publish-path nil)
  :config
  (when (and website-org-path website-org-publish-path)
    (setq
     org-publish-project-alist
     `(( ;; note ` instead of '
        "org-notes"
        :base-directory ,(format "%s" website-org-path) ;设置存放.org文件位置 
        :base-extension "org" ;仅处理 .org 格式文件
        :publishing-directory ,(format "%s" website-org-publish-path) ;导出html文件位置
        :recursive t
        :publishing-function org-publish-org-to-html
        :headline-levels 4 ;Just the default for this project.
        :auto-preamble t
        :auto-sitemap t ;自动生成 sitemap.org
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
        :publishing-function org-publish-attachment)
       ;;publish 组件
       ("org" :components ("org-notes" "org-static"))))
    (defun html-last-updated ()
      (concat
       "<div id=\"footer\">Last Updated: "
       (format-time-string "%Y-%m-%d %H:%M")
       ". contact: lynnux@qq.com</a></div> "))))

;; 再也不用手动输入tab了
(use-package org-indent
  :diminish
  :defer t)

(use-package org-agenda
  :defer t
  :init
  ;; (setq org-hide-leading-stars t); 只高亮显示最后一个代表层级的 *
  (define-key global-map "\C-ca" 'org-agenda) ;C-c a 进入日程表
  ;; (setq org-capture-bookmark nil) ;; 不需要添加到
  (setq org-bookmark-names-plist
        '(:last-capture
          "org-capture-last-stored"
          ;;:last-refile "org-refile-last-stored"
          ;;:last-capture-marker "org-capture-last-stored-marker"
          ))
  (setq org-capture-templates
        `(("i"
           "Idea"
           entry
           (file+headline , "idea.org" "Index")
           "* IDEA %?\n %i\n %a")
          ("t"
           "Task"
           entry
           (file+headline , "todo.org" "Task")
           "* TODO %i%?\n\n于: %U %a")))
  ;; (setq org-remember-templates
  ;;       '(("Todo" ?t "* TODO %i%?\n\n于: %U %a" "F:/kp/org/remember/TODO.org" "Tasks")
  ;;   	("IDEA" ?i "* IDEA %?\n %i\n %a" "F:/kp/org/remember/Idea.org" "Idea")
  ;;   	))
  (define-key global-map "\C-cr" 'org-capture)
  (define-key global-map "\C-cc" 'org-capture)

  :config
  (add-hook
   'org-agenda-mode-hook (lambda () (setq org-agenda-follow-mode t)))
  (define-advice org-capture-target-buffer
      (:around (orig-fn &rest args) my)
    (let ((tmp-disable-view-mode 2)) ;; 2不恢复只读
      (apply orig-fn args)))
  (define-advice org-archive-subtree (:around (orig-fn &rest args) my)
    (let ((tmp-disable-view-mode 2)) ;; 2不恢复只读
      (apply orig-fn args))))

;; 低版本的`org-capture-target-buffer'在`org-agenda'里？
(use-package org-capture
  :defer t
  :config
  ;; 对org-capture涉及的文件去掉只读
  (define-advice org-capture-target-buffer
      (:around (orig-fn &rest args) my)
    (let ((tmp-disable-view-mode 2)) ;; 2不恢复只读
      (apply orig-fn args))))

(use-package cua-base
  :defer t
  :config
  (setq cua-remap-control-z nil) ;原来是add-hook，所以设置不成功，eval-after-load会在加载cua文件后立即执行。elisp要加强啊！
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
  (setq
   minor-mode-alist
   (append
    `((cua-mode " ") (cua-mode ,cua-mode-line-format)) ;前面一句(cua-mode " ")不是多余的，否则空格也会有蓝色的背景
    (delq (assq 'cua-mode minor-mode-alist) minor-mode-alist)))

  ;; shift + click select region，用shift+鼠标选中，需要开启CUA
  (define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
  (define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
  (put 'mouse-set-point 'CUA 'move)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  ;; (setq cua-keep-region-after-copy t) ;选中复制后保持选中状态  
  )

;; 有些插件如eglot-rename需要临时禁用view-mode，一般用find-file-noselect(fin-file-hook那里做对已经打开的文件无效)，以下是trick
(defun run-with-local-idle-timer (secs repeat function &rest args)
  ""
  (let* ( ;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn
          `(lambda (&rest args)
             (if (not (buffer-live-p ,(current-buffer)))
                 (cancel-timer ,timer)
               (with-current-buffer ,(current-buffer)
                 (apply (function ,function) args))))))
    (fset fns fn)
    fn))

(use-package view
  :defer t
  :init
  (add-hook
   'after-init-hook
   (lambda ()
     (add-hook
      'find-file-hook
      (lambda ()
        ;; 排除git commit的buffer
        (unless (or (string-match
                     "\\(?:COMMIT_EDITMSG\\)$" buffer-file-name)
                    (string-match
                     "\\(?:.dir-locals.el\\)$" buffer-file-name))
          (view-mode 1))))))
  (keyboard-translate ?\C-i ?\H-i) ;把C-I绑定为开关，terminal貌似不起作用
  (global-set-key [?\H-i] 'view-mode)
  (global-unset-key (kbd "C-x C-q")) ;; 偶尔遇到C-i也不能修改就是这个导致的
  (defvar tmp-disable-view-mode nil) ;; 在需要的函数defadvice里设置，2不恢复只读，3禁用
  (defun check-tmp-disable-view-mode (result)
    (unless (eq tmp-disable-view-mode 3)
      (when (and tmp-disable-view-mode (bufferp result))
        (with-current-buffer result
          (when view-mode
            (view-mode -1) ;; 临时禁用
            (cond
             ((eq tmp-disable-view-mode 2)
              ())
             (t
              ;; 2秒后恢复只读，实际上idle可能超过2秒
              (run-with-local-idle-timer
               2 nil
               (lambda () (view-mode 1))))))))))

  ;; 参考org-capture-target-buffer和org-find-base-buffer-visiting，找到下面两个的hook点
  (define-advice find-file-noselect (:around (orig-fn &rest args) my)
    (let ((result (apply orig-fn args)))
      (check-tmp-disable-view-mode result)
      result))
  ;; ivy会调用这个导致wcy加载，不能用ivy-use-virtual-buffers
  (define-advice get-file-buffer (:around (orig-fn &rest args) my)
    (let ((result (apply orig-fn args)))
      (check-tmp-disable-view-mode result)
      result))
  (define-advice find-buffer-visiting
      (:around (orig-fn &rest args) my)
    (let ((result (apply orig-fn args)))
      (check-tmp-disable-view-mode result)
      result))

  :config
  ;; 跟god-mode集成了，原来view单字符的功能全部加上C-，
  (define-key
   view-mode-map (kbd "C-w")
   'View-scroll-page-backward-set-page-size) ;; view-mode显然不能编辑
  (define-key
   view-mode-map (kbd "C-SPC")
   'View-scroll-page-forward-set-page-size) ;; 原C-spc是set-mark
  (define-key
   view-mode-map (kbd "C-v")
   'View-scroll-page-forward-set-page-size) ;; view-mode显然不能编辑
  (define-key view-mode-map (kbd "C-j") nil)
  (define-key
   view-mode-map (kbd "RET")
   (lambda ()
     (interactive)
     (when (eq major-mode 'org-mode)
       (call-interactively 'org-cycle))))
  (defface view-mode-mode-line-face
    '((((type tty pc)) :bold t :background "red" :foreground "white")
      (t (:background "red" :foreground "white")))
    "")
  (defvar view-mode-line-format
    (propertize "View" 'face 'view-mode-mode-line-face)
    "")
  (put 'view-mode-line-format 'risky-local-variable t)
  (setq minor-mode-alist
        (append
         `((view-mode " ") (view-mode ,view-mode-line-format))
         (delq (assq 'view-mode minor-mode-alist) minor-mode-alist))))

(use-package occur
  :defer t
  :init
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
    (interactive (list
                  (read-command "Mode: ") (read-string "Regexp: ")))
    (multi-occur
     (remove-if
      (lambda (buf)
        (set-buffer buf)
        (not (eq major-mode mode)))
      (buffer-list))
     rexp))
  :config
  (define-key occur-mode-map (kbd "p") 'occur-prev)
  (define-key occur-mode-map (kbd "n") 'occur-next))

(setq completion-ignore-case t)

(setq dabbrev-abbrev-char-regexp "[A-Za-z-_]") ;; 不补全中文

(use-package hippie-exp
  :defer t
  :init (bind-key* (kbd "C-'") 'hippie-expand)
  :config
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
          try-complete-lisp-symbol)))

(use-package gdb
  :defer t
  :config
  (add-hook
   'gdb-mode-hook
   #'(lambda ()
       (gdb-many-windows)
       (define-key c-mode-base-map [(f5)] 'gud-go)
       (define-key c-mode-base-map [(f10)] 'gud-step)
       (define-key c-mode-base-map [(f11)] 'gud-next)
       (define-key c-mode-base-map [(f9)] 'gud-break))))

;; autosave 这个会卡
(setq
 auto-save-default nil
 delete-auto-save-files nil)
;; bakup 
(setq make-backup-files nil)
(setq
 auto-save-file-name-transforms
 (quote ((".*" "~/.emacs.d/autosave/" t)))
 backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))

(setq create-lockfiles nil) ;; 禁止创建#.开头的同名文件

;;; inf文件
(add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-windows-mode))

(with-eval-after-load 'ediff
  (setq
   ediff-diff-options "-w" ; turn off whitespace checking
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
  (add-hook 'ediff-suspend-hook 'doom-ediff-restore-wconf-h))

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)
;; 修复read only状态修改后不revert的问题
(defvar my-file-readonly-state nil
  "记录文件的read only状态")
(defun my-save-readonly-state ()
  (setq-local my-file-readonly-state
              (file-attribute-modes
               (file-attributes buffer-file-name))))
(add-hook 'find-file-hook #'my-save-readonly-state)
(with-eval-after-load 'autorevert
  (define-advice auto-revert-handler (:after (&rest args))
    (when buffer-file-name
      (let ((current-state
             (file-attribute-modes
              (file-attributes buffer-file-name)))
            (fn buffer-file-name))
        (unless (equal my-file-readonly-state current-state)
          (setq my-file-readonly-state current-state)
          ;; (call-interactively 'revert-buffer-quick) ;; 这个有bug，添加只读后仍然可以进入编辑模式
          (kill-buffer)
          (if (eq (window-buffer) (current-buffer))
              (find-file fn) ;; find-file会切换到打开的文件buffer
            (find-file-noselect fn) ;; 这个会把buffer置于最后小问题能接受
            ))))))

(setq-default compilation-scroll-output 'first-error)
(electric-indent-mode -1) ;; 貌似没什么用也没有，还占了post-self-insert-hook一席

(remove-hook 'after-save-hook 'rmail-after-save-hook) ;; emacs默认还给你加个这玩意

;; 让M-o支持windows的import生成的头文件
(with-eval-after-load 'find-file
  (add-to-list 'cc-other-file-alist '("\\.tli\\'" (".tlh")))
  (add-to-list 'cc-other-file-alist '("\\.tlh\\'" (".tli"))))

(use-package webjump
  :defer t
  :init
  (defun webjump-auto-query (expr name)
    (let (query)
      (setq query
            (if (region-active-p)
                (buffer-substring-no-properties
                 (region-beginning) (region-end))
              (thing-at-point 'symbol)))
      (unless query
        (setq query (read-string "请输入要搜索的内容：")))
      (when (equal query "")
        (setq query nil)) ;; 空白搜索直接跳首页
      (if query
          (concat
           (aref expr 2) (webjump-url-encode query) (aref expr 3))
        (aref expr 1))))
 (defun search-in-browser ()
   "在浏览器里打开搜索当前symbol(region)，主要参考`xahk-lookup-ahk-ref'"
   (interactive)
   (when (autoloadp (symbol-function 'webjump))
     (require 'webjump))
   (let
       ((webjump-sites
         '(("baidu" .
            [webjump-auto-query
             "https://www.baidu.com"
             "https://www.baidu.com/s?wd="
             ""])
           ("msdn" .
            [webjump-auto-query
             "https://www.baidu.com"
             "https://www.baidu.com/s?wd="
             "%20site%3Amsdn.microsoft.com"])
           ("glib" .
            [webjump-auto-query
             "https://www.google.com"
             "https://www.google.com/search?q="
             "+site%3Adocs.gtk.org"])
           ("google" .
            [webjump-auto-query
             "https://www.google.com"
             "https://www.google.com/search?q="
             ""])
           ("rust winapi" .
            [webjump-auto-query
             "https://docs.rs/winapi/latest/winapi/index.html"
             "https://docs.rs/winapi/latest/winapi/index.html?search="
             ""])
           ("rust windows-sys" .
            [webjump-auto-query
             "https://docs.rs/windows-sys"
             "https://docs.rs/windows-sys/latest/windows_sys/?search="
             ""])
           ("rust windows(COM/WinRT)" .
            [webjump-auto-query
             "https://microsoft.github.io/windows-docs-rs/"
             "https://microsoft.github.io/windows-docs-rs/doc/windows/?search="
             ""])
           ("emacs china" .
            [webjump-auto-query
             "https://emacs-china.org"
             "https://emacs-china.org/search?q="
             ""])
           ("cppreference" .
            [webjump-auto-query
             "https://cppreference.com"
             "https://duckduckgo.com/?sites=cppreference.com&q="
             ""])
           ;; ("everything" . #'consult-everything)
           ;; ("project search" . #'my-project-search)
           )))
     (cl-letf (((symbol-function #'webjump-builtin)
                (lambda (expr name) (webjump-auto-query expr name))))
       (webjump))))
  (global-set-key (kbd "<f1> <f1>") 'search-in-browser) ;; 原命令 `help-for-help'可以按f1 ?
  )

(use-package vc
  :defer t
  :init
  (setq
   vc-log-show-limit 100 ;; 默认2000太多了
   vc-git-diff-switches nil ;; 避免diff时显示为Binary files
   vc-find-revision-no-save t ;; 查看revision时不保存临时文件
   )
  :config
  (define-advice vc-git-command (:around (orig-fn &rest args) my)
    "解决中文路径add问题，注意测试下commit时中文时是否乱码。TODO: 感觉办法不是太好，将就用"
    (let ((vc-git-commits-coding-system 'gbk-dos))
      (apply orig-fn args)))
  (define-advice vc-coding-system-for-diff
      (:around (orig-fn &rest args) my)
    ;; 解决vc diff乱码, TODO: 写死diff编码了，目前只用git没问题
    (let ((coding-system-for-read 'utf-8))
      (apply orig-fn args))))

(use-package vc-dir
  :defer t
  :init
  ;; "让`vc-dir'不选择目录"
  (global-set-key [remap vc-dir] 'project-vc-dir)
  :config
  (define-key vc-dir-mode-map (kbd "k") 'vc-revert)
  (define-key vc-dir-mode-map (kbd "d") 'vc-diff)
  (define-key vc-dir-mode-map (kbd "<tab>") 'vc-diff)
  (define-key vc-dir-mode-map (kbd "s") 'vc-next-action)
  (define-key vc-dir-mode-map (kbd "l") 'vc-print-root-log) ;; 习惯看root而不是单文件的
  (define-key vc-dir-mode-map (kbd "L") 'vc-print-log)
  (define-key vc-dir-mode-map (kbd "F") 'vc-pull)
  (define-key vc-dir-mode-map (kbd "f") 'vc-pull)
  (define-key vc-dir-mode-map (kbd "i") 'vc-dir-ignore)
  (define-advice vc-dir-ignore (:around (orig-fn &rest args) my)
    (let ((tmp-disable-view-mode 2)) ;; 2不恢复只读
      (apply orig-fn args)))
  (defun invoke-vc-dir ()
    (interactive)
    (if (functionp' which-key--show-keymap)
        (progn
          (which-key--show-keymap
           "keymap" vc-dir-mode-map nil nil 'no-paging)
          (set-transient-map vc-dir-mode-map
                             nil
                             'which-key--hide-popup))
      (set-transient-map vc-dir-mode-map nil 'ignore)))
  (define-key vc-dir-mode-map (kbd "?") 'invoke-vc-dir)
  ;; 自动弹出菜单，不能做到transient那样，总比没有要好点
  (add-hook
   'vc-dir-mode-hook
   (lambda () (run-with-local-idle-timer 0.1 nil 'invoke-vc-dir))))

(use-package vc-hooks
  :defer t
  :init (setq vc-handled-backends '(Git))
  ;; bindings.el里直接就把vc-mode写死在`mode-line-format'里了
  :config
  (defface vc-mode-face '((t :foreground "#6ae4b9"))
    "")
  (define-advice vc-call-backend (:around (orig-fn &rest args) my)
    "给`vc-mode'添加颜色，并把Git替换为项目名"
    (let ((result (apply orig-fn args)))
      (if (eq (ad-get-argument args 1) 'mode-line-string)
          (let ((pn
                 (file-name-nondirectory
                  (directory-file-name
                   (project-root (project-current t))))))
            (unless (boundp 'project-mode-line)
              (when pn
                (setq result (string-replace "Git" pn result))))
            (propertize result 'face 'vc-mode-face))
        result)))
  ;; https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key
  ;; 无法取消或者覆盖"C-x v"，只能这样替换成另外的
  (define-key key-translation-map (kbd "C-x v") (kbd "C-c v"))
  (bind-key* (kbd "C-c v") 'invoke-vc)
  (defun invoke-vc ()
    (interactive)
    (if (functionp' which-key--show-keymap)
        (progn
          (which-key--show-keymap
           "keymap" vc-prefix-map nil nil 'no-paging)
          (set-transient-map vc-prefix-map
                             nil
                             'which-key--hide-popup))
      (set-transient-map vc-prefix-map nil 'ignore))))

(use-package log-view
  :defer t
  :config
  (defun log-view-show-diff (&rest args)
    "让log窗口按RET显示diff，跟magit的log RET一致"
    (interactive)
    (call-interactively 'log-view-diff))
  (setq log-view-expanded-log-entry-function 'log-view-show-diff)
  (define-advice magit-log-current (:around (orig-fn &rest args) my)
    "调用`vc-print-root-log'还弹窗"
    (vc-print-log-internal
     'Git
     (list (project-root (project-current t)))
     nil
     nil
     vc-log-show-limit
     nil))
  (with-eval-after-load 'vc-git
    (add-to-list
     'vc-git-log-view-mode-hook
     (lambda ()
       (setq-local log-view-expanded-log-entry-function
                   'log-view-show-diff)))))

(use-package diff
  :defer t
  :init
  ;; 设置了`diff-command'后diff refine就正常了，让它仅diff buffer里p/n才显示
  (setq diff-refine 'nil)
  :config
  ;; 现在不加git PATH了(跟vc环境冲突)，所以需要单独设置
  (setq diff-command
        (concat
         (file-name-parent-directory
          (file-name-directory (executable-find "git")))
         "usr/bin/diff.exe")))

(use-package diff-mode
  :defer t
  :config
  (define-advice diff-goto-source (:around (orig-fn &rest args) my)
    "实现打开对应的revision文件"
    (let ((rev1 (car-safe diff-vc-revisions))
          (rev2 (cdr-safe diff-vc-revisions))
          rev-at-point)
      (if (and diff-vc-backend rev1 rev2)
          (progn
            (when (memq
                   'diff-added
                   (idle-highlight--faces-at-point (point)))
              (setq rev-at-point rev1))
            (when (memq
                   'diff-removed
                   (idle-highlight--faces-at-point (point)))
              (setq rev-at-point rev2))
            (message "rev:%S" rev-at-point))
        ;; revision为nil调用原函数
        (apply orig-fn args)))))

(use-package text-mode
  :defer t
  :config
  ;; 解决vc commit时corfu报错
  (add-hook
   'text-mode-hook
   (lambda ()
     (remove-hook
      'completion-at-point-functions #'ispell-completion-at-point
      t))))

(use-package help-fns
  :defer t
  :init
  ;; 加快F1 f/v，禁止根据输入加载内置包
  (setq help-enable-completion-autoload nil))

(use-package goto-addr
  :if (bound-and-true-p enable-feature-gui)
  :defer 0.8
  :init
  (global-set-key (kbd "C-c C-o") 'goto-address-at-point) ;; 跟org快捷键一致
  (setq goto-address-uri-schemes-ignored '("info:" "mailto:" "data:")) ;; 去掉info:
  :config
  (global-goto-address-mode 1))

;; from tabbar-ruler
(setq EmacsPortable-included-buffers
      '("*scratch*"
        "*rg*"
        "*eww*"
        "*xref*"
        "*shell*"
        "*eshell*"
        "*PowerShell*"
        "*org-roam*"
        "*elfeed-entry*"
        "*elfeed-search*"
        "*dashboard*"
        "*vc-dir*"
        "*devdocs*"))
(defun tab-show-buffer-p (b)
  (cond
   ;; Always include the current buffer.
   ((eq (current-buffer) b)
    b)
   ((string-match "^TAGS\\(<.*>\\)?$" (format "%s" (buffer-name b)))
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
(defun ep-tabbar-buffer-list ()
  (delq nil (mapcar #'tab-show-buffer-p (buffer-list))))


;; 27.1自带tab-bar-mode和tab-line-mode，试了下tab-line跟tabbar-ruler是一样的效果
(use-package tab-line
  :if
  (and (functionp 'global-tab-line-mode)
       (bound-and-true-p enable-feature-gui))
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
         ;; (propertize 
         ;;             'face '(foreground-color . "cyan"))
         (format " %s "
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
                    (nth (mod (random t) (length tl)) tl))))
       " | ") ;; 这个比close button好看 
     ))
  :config
  (define-advice volatile-kill-buffer (:before (&rest args) my)
    "让C-2跳过那些tab不可见的buffer"
    (cl-dolist
        (buf (buffer-list))
      (when (not (tab-show-buffer-p buf))
        ;; (message "bury-buffer:%S" buf)
        (bury-buffer buf) ;; 将buffer放到最后去
        (cl-return))))
  (global-tab-line-mode 1)
  (add-to-list 'tab-line-exclude-modes 'speedbar-mode)
  (add-to-list 'tab-line-exclude-modes 'dired-sidebar-mode)
  (add-to-list 'tab-line-exclude-modes 'dape-info-breakpoints-mode)
  (add-to-list 'tab-line-exclude-modes 'dape-info-stack-mode)
  (add-to-list 'tab-line-exclude-modes 'dape-info-scope-mode)
  (add-to-list 'tab-line-exclude-modes 'dape-repl-mode)
  (add-to-list 'tab-line-exclude-modes 'dape-info-threads-mode)
  (add-to-list 'tab-line-exclude-modes 'dape-info-watch-mode))


(use-package python
 :defer t
 :config
 ;; python -m venv创建的居然没有python3.exe！
 (setq python-shell-interpreter "python")
 (define-advice python-shell-send-region (:before (&rest args) my)
   "自动滚动到最低(仅当光标在最后面时) https://github.com/jorgenschaefer/elpy/issues/1641"
   (let ((curbuf (current-buffer)))
     (python-shell-switch-to-shell)
     (goto-char (point-max))
     (recenter -10)
     (pop-to-buffer curbuf))))


(use-package eldoc
  :if (bound-and-true-p enable-feature-builtin)
  :defer t
  :diminish (eldoc-mode)
  :init
  (setq eldoc-echo-area-use-multiline-p nil) ;; 不要多行显示
  :config
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore) ;; 在pre-command-hook里影响性能

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
           0))))))

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

(use-package so-long
  :disabled
  :if (bound-and-true-p enable-feature-builtin)
  :defer 1.3
  :config (global-so-long-mode))

(use-package hl-line
  :disabled
  :if (and (display-graphic-p) (bound-and-true-p enable-feature-gui))
  :defer 0.6
  :config (global-hl-line-mode t))
