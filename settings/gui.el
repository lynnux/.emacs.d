;; 界面相关的

(custom-set-variables
 '(blink-cursor-mode nil)		;光标是否闪烁
 '(column-number-mode t)		;状态栏里显示行号和列号
 '(line-number-mode t)
 '(size-indication-mode t)          ; 显示文件大小
 '(display-time-mode t) 		;显示时间
 '(inhibit-startup-screen t)		;禁止显示启动画面
 '(show-paren-mode t)			;()匹配提示
 '(tooltip-mode nil)			;windows会卡，不用
 '(tool-bar-mode nil)          		;不显示toolbar
 '(menu-bar-mode nil)          ; win10黑色模块就这个是白的，所幸去掉算了，命令tmm-menubar能在minibuffer里显示
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp)))
 )

(setq confirm-nonexistent-file-or-buffer nil)
(setq blink-matching-paren nil)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
;; (setq-default line-spacing 1) ; 这个设置会导致F3到下一个时滚动出问题

;; (global-visual-line-mode 1); 折行但是不显示两边难看的小符号
(setq-default word-wrap t)	    ; 换行不打断单词
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
;; (add-hook 'text-mode-hook #'visual-line-mode)

(set-scroll-bar-mode 'right); 滚动条在右侧(ubuntu)

;; 抄doom的，感觉没多大作用
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; 鼠标滚轮，默认的滚动太快，这里改为3行
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))

(if (string-equal system-type "windows-nt")
    (progn				;windows，难怪以前设置没效果
      (global-set-key [wheel-up] 'down-slightly)
      (global-set-key [wheel-down] 'up-slightly) 
      )
  (progn				;linux
    (global-set-key [mouse-4] 'down-slightly)
    (global-set-key [mouse-5] 'up-slightly) 
    )
  )


;;窗口按键设置
(global-set-key (kbd "C-1") 'delete-other-windows) ; Alt-1 关闭其它窗口
(defun volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))
(global-set-key (kbd "C-2") 'volatile-kill-buffer)
(global-set-key "\M-r" 'replace-string)
;; (defun display-buffer-name ()
;;   (interactive)
;;   (message (buffer-file-name (current-buffer))))
;; (global-set-key (kbd "C-5") 'display-buffer-name);Alt-5 显示buffer文件名

(setq display-time-24hr-format t) ; 24小时格式
(setq display-time-day-and-date t) ; 显示日期
;(mouse-avoidance-mode 'animate) ; 光标移动到鼠标下时，鼠标自动弹开
;(setq frame-title-format "%f")		; 显示当前编辑的文档
; toobar-ruler因为24.3是bug对修改的是buffer不加粗显示了
(setq frame-title-format '("%f" (:eval (if (buffer-modified-p) " *" ""))))


(when (display-graphic-p)
  ;; 字体设置，下载Consolas字体，很好看，据说是ms专门给vs studio用的
  (defun qiang-font-existsp (font)
    (find-font (font-spec :name font)))
  (defun qiang-make-font-string (font-name font-size)
    (if (and (stringp font-size) 
	     (equal ":" (string (elt font-size 0))))
	(format "%s%s" font-name font-size)
      (format "%s %s" font-name font-size)))
  (defun qiang-set-font (english-fonts
			 english-font-size
			 chinese-fonts
			 &optional chinese-font-size)
    "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
    (require 'cl) ; for find if
    (let ((en-font (qiang-make-font-string
		    (find-if #'qiang-font-existsp english-fonts)
		    english-font-size))
	  (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
			      :size chinese-font-size)))

      ;; Set the default English font
      ;; 
      ;; The following 2 method cannot make the font settig work in new frames.
      ;; (set-default-font "Consolas:pixelsize=18")
      ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
      ;; We have to use set-face-attribute
      (message "Set English Font to %s" en-font)
      (set-face-attribute
       'default nil :font en-font)

      ;; Set Chinese font 
      ;; Do not use 'unicode charset, it will cause the english font setting invalid
      (message "Set Chinese Font to %s" zh-font)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font)
			  charset
			  zh-font))))

  ;; https://www.programmingfonts.org在线看字体效果
  ;; https://www.codingfont.com/ 正是它发现Roboto Mono的
  ;; 很多字体要实际安装在emacs看才真正知道效果，网页只能看个大概
  (qiang-set-font
   '("Roboto Mono" "Cousine" "Source Code Pro" "Hack" "Bitstream Vera Sans Mono" "Office Code Pro" "JetBrains Mono" "Fira Code" "Hack Regular" "Fixedsys") ":pixelsize=14"
   '("微软雅黑" "宋体" "新宋体" "Microsoft Yahei" "黑体" "WenQuanYi Bitmap Song" "文泉驿等宽微米黑") 14)
  )
;; 要能区分0O和1lI(光l1就能排除很多了)，单看单词时要好看，--__能区分出来，粗体要好看，()要好看(感觉小点好看)

;; 下面这两个看特殊字符时不卡，按上面的说法`set-default-font'不支持new frames，这个我暂时会用到
;;(set-default-font "Consolas 11") ;; for 英文     
;;(set-fontset-font "fontset-default" 'unicode "微软雅黑 11") ;; for 中文

;; ctrl+鼠标滚轮调整文字大小
(if (string-equal system-type "windows-nt")
    (progn 
      ;; For Windows
      (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
      (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))
  (progn
    (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
    ))

;; 设置颜色，根据theme来设置，M-X list-colors-display 可以显示更多的颜色
;; (set-face-background 'show-paren-match-face "slate blue") ;; 27无效
;;(set-face-foreground 'show-paren-match-face "gray15")

;; 当光标位于括号上时，类似选中的效果
;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;; 				  (make-local-variable 'show-paren-style)
;; 				  (setq show-paren-style 'expression)
;; 				  ;(face-remap-add-relative 'show-paren-match '((:background "#2B2B2B"))) ; elisp单独一个颜色, 为zenburn-bg-1
;; 				  ))

(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S lynnux")

;;; 窗口最大化 24.5没效果？
(when (display-graphic-p)
  (when (and (>= emacs-major-version 25) (string-equal system-type "windows-nt"))
    (w32-send-sys-command 61488)))

(setq enable-local-variables :safe) ;; 关闭打开一些文件时risk local variable提示
(setq ring-bell-function #'ignore
      visible-bell t) ;; ** 关闭 beep ; blink, don't bark

(setq scroll-preserve-screen-position t) ;; 滚动时鼠标保持位置，聊胜于无吧，原来是直接到行首
