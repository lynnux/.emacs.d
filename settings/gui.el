;; Time-stamp: <2017-07-25 11:26:06 lynnux>
;; 界面相关的

(custom-set-variables
 '(blink-cursor-mode nil)		;光标是否闪烁
 '(column-number-mode t)		;状态栏里显示行号和列号
 '(line-number-mode t)
 '(display-time-mode t) 		;显示时间
 '(inhibit-startup-screen t)		;禁止显示启动画面
 '(show-paren-mode t)			;()匹配提示
 '(tooltip-mode nil)			;windows会卡，不用
 '(tool-bar-mode nil)          		;不显示toolbar
 )

(setq-default line-spacing 1)

;; (global-visual-line-mode 1); 折行但是不显示两边难看的小符号
(set-scroll-bar-mode 'right); 滚动条在右侧(ubuntu)

(setq scroll-step 1
      ;scroll-margin 3 ; 这个有点小影响highlight-symbol-scroll-out
      scroll-conservatively 10000) ; 滚动页面时比较舒服，不要整页的滚动
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
(global-set-key (kbd "M-1") 'other-window)
(defun volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))
(global-set-key (kbd "C-2") 'volatile-kill-buffer)
(global-set-key (kbd "C-4") 'copy-buffer-file-name-as-kill) ;/路径
(global-set-key (kbd "C-3") 'copy-buffer-file-name-as-kill-windows) ; \路径
(global-set-key "\M-r" 'replace-string)
(defun copy-buffer-file-name-as-kill(choice &optional name)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name : [f]ull, [d]irectory, n[a]me?")
  (let ((new-kill-string)
        )
    (setq name (or name (if (eq major-mode 'dired-mode)
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
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))
(defun copy-buffer-file-name-as-kill-windows(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name : [f]ull, [d]irectory, n[a]me?")
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
      
      (let ((win-path (replace-regexp-in-string "/" "\\\\" new-kill-string)))
	(message "%s copied" win-path)
	(kill-new win-path))
      )))
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

;; 字体设置，下载Consolas字体，很好看，据说是ms专门给vs studio用的
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))
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

;; 设置字体,Fixedsys要用修正过的，为了保证org的table显示正常，字体大小应该设置成一样大，不过这样感觉汉字大多了，大也好，慢慢适应吧
;;; 25上用Fixedsys Excelsior 3.01-L会卡死，可以用Fixedsys(24不能用)，但发现字体有发虚。最后发现是因为字体名带-，用Hxd替换-为_就行了(同时替换unicode)
(qiang-set-font
 '("Fixedsys Excelsior 3.01_L" "Consolas" "Source Code Pro" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=16"
 '("WenQuanYi Bitmap Song" "宋体" "Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体") 16)

;; 下面这两个看特殊字符时不卡，按上面的说法`set-default-font'不支持new frames，这个我暂时会用到
;(set-default-font "Consolas 11") ;; for 英文     
;(set-fontset-font "fontset-default" 'unicode "微软雅黑 11") ;; for 中文

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
(set-face-background 'show-paren-match-face "slate blue")
;(set-face-foreground 'show-paren-match-face "gray15")

;; 当光标位于括号上时，类似选中的效果
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (make-local-variable 'show-paren-style)
				  (setq show-paren-style 'expression)
				  ;(face-remap-add-relative 'show-paren-match '((:background "black"))) ; elisp单独一个颜色
				  ))

(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S lynnux")

;;; 窗口最大化 24.5没效果？
(when (and (>= emacs-major-version 25) (string-equal system-type "windows-nt"))
   (w32-send-sys-command 61488))
