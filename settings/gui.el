;; 界面相关的

(custom-set-variables
 '(blink-cursor-mode nil) ;光标是否闪烁
 '(column-number-mode t) ;状态栏里显示行号和列号
 '(line-number-mode t)
 '(mode-line-percent-position nil) ;; 不显示位置百分比
 ;; '(size-indication-mode t) ; 显示文件大小
 '(display-time-mode t) ;显示时间
 '(inhibit-startup-screen t) ;禁止显示启动画面
 '(show-paren-mode t) ;()匹配提示
 '(tooltip-mode nil) ;windows会卡，不用
 '(tool-bar-mode nil) ;不显示toolbar
 '(menu-bar-mode nil) ; win10黑色模块就这个是白的，所幸去掉算了，命令tmm-menubar能在minibuffer里显示
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp)))
 '(minibuffer-prompt-properties ;; 禁止光标移动到在minibuffer的prompt里去，不然输入会提示Text readonly
   (quote (read-only t cursor-intangible t face minibuffer-prompt)))
 '(auto-compression-mode nil)
 '(auto-encryption-mode nil)
 '(auto-composition-mode nil)
 '(file-name-shadow-mode nil))

;; 位置百分比变为总行数
(add-hook
 'after-init-hook
 (lambda ()
   (setq mode-line-percent-position
         '(:eval
           (format "all:%d" (line-number-at-pos (point-max)))))))

(setq display-time-24hr-format t) ; 24小时格式
;; (setq display-time-day-and-date t) ; 显示日期
(setq display-time-format " %a %H:%M %Y-%m-%d")
(setq display-time-default-load-average nil)
(custom-set-faces
 '(display-time-date-and-time ((t (:foreground "#f78fe7")))))
(setq frame-title-format
      '("%f" (:eval
         (if (buffer-modified-p)
             " *"
           ""))))

(setq confirm-nonexistent-file-or-buffer nil)
(setq
 show-paren-when-point-inside-paren t
 show-paren-when-point-in-periphery t
 ;;show-paren-context-when-offscreen t ;; 29新功能，显示屏幕外的匹配项
 )
(setq
 indicate-buffer-boundaries nil
 indicate-empty-lines nil)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
;; (setq-default line-spacing 1) ; 这个设置会导致F3到下一个时滚动出问题

;; (global-visual-line-mode 1); 折行但是不显示两边难看的小符号
(setq-default word-wrap t) ; 换行不打断单词
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
;; (add-hook 'text-mode-hook #'visual-line-mode)

(set-scroll-bar-mode 'right) ; 滚动条在右侧(ubuntu)

;; 抄doom的，感觉没多大作用
(setq
 hscroll-margin 2
 hscroll-step 1
 scroll-margin 2 ;; 最高/低总是保留几层光标碰不到
 scroll-conservatively 101 ;; 当光标移动到未显示区域时recenter，好像除了0其它没什么效果？
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
 mouse-wheel-scroll-amount-horizontal 2)

;; 鼠标滚轮，默认的滚动太快，这里改为3行
(defun up-slightly ()
  (interactive)
  (scroll-up 4))
(defun down-slightly ()
  (interactive)
  (scroll-down 4))
(setq scroll-error-top-bottom t) ;; 当到达顶或底时自动跳到第一行或最后一行
(if (string-equal system-type "windows-nt")
    (progn ;windows，难怪以前设置没效果
      (global-set-key [wheel-up] 'down-slightly)
      (global-set-key [wheel-down] 'up-slightly))
  (progn ;linux
    (global-set-key [mouse-4] 'down-slightly)
    (global-set-key [mouse-5] 'up-slightly)))

;;窗口按键设置
(defun volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))
(global-set-key (kbd "C-2") 'volatile-kill-buffer)
(global-set-key "\M-r" 'replace-string)

;; 使用`cnfonts-edit-profile'确定字号
(when (display-graphic-p)
  (require 'cl) ;; find-if
  (defun qiang-font-existsp (font)
    (find-font (font-spec :name font)))
  (defun qiang-set-font
      (english-fonts
       english-font-size &optional chinese-fonts chinese-font-size)
    (let ((en-font
           (font-spec
            :name
            (find-if #'qiang-font-existsp english-fonts)
            :size english-font-size))
          (zh-font
           (font-spec
            :family
            (find-if #'qiang-font-existsp chinese-fonts)
            :size chinese-font-size)))
      (set-face-attribute 'default nil :font en-font)
      (when chinese-fonts
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font
           (frame-parameter nil 'font) charset zh-font)))))

  ;; https://www.programmingfonts.org 在线看字体效果
  ;; https://www.codingfont.com/ 正是它发现Roboto Mono的
  ;; 很多字体要实际安装在emacs看才真正知道效果，网页只能看个大概
  (qiang-set-font
   '("Iosevka"
     "Roboto Mono"
     "Cousine"
     "Source Code Pro"
     "Hack"
     "Bitstream Vera Sans Mono"
     "Office Code Pro"
     "JetBrains Mono"
     "Fira Code"
     "Hack Regular"
     "Fixedsys")
   11.5
   '("新宋体" ;; org跟Iosevka table自动对齐
     "微软雅黑"
     "宋体"
     "新宋体"
     "Microsoft Yahei"
     "黑体"
     "WenQuanYi Bitmap Song"
     "文泉驿等宽微米黑")
   ;; 11.5 ;; 不能指定中文大小，不然org face的height有问题 其它方法见https://emacs-china.org/t/face-charset-size/16916
   ))
;; 要能区分0O和1lI(光l1就能排除很多了)，单看单词时要好看，--__能区分出来，粗体要好看，()要好看(感觉小点好看)
(set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend) ;; 修复部分字符显示问题 https://emacs-china.org/t/emacs/15676/10

(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

(setq enable-local-variables :all) ;; 关闭打开一些文件时risk local variable提示
(setq
 ring-bell-function #'ignore
 visible-bell t) ;; ** 关闭 beep ; blink, don't bark

(setq scroll-preserve-screen-position t) ;; 滚动时鼠标保持位置，聊胜于无吧，原来是直接到行首

;; 目标是本窗口，将就用。也可以用鼠标去调整split
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)

;; 高亮TODO等
(defface my-todo-face '((t :foreground "#cc6666" :inverse-video t))
  "")
(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
       1
       'my-todo-face
       t)))))
