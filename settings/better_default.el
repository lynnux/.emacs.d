;; gui相关设置在set_gui.el中 -*- lexical-binding: t -*-
;; 内置plugin设置在plugin_basic.el中,非官方的在plugin_extra.el中

(require 'server)
;; 解决win7上的不安全提示信息
(and (>= emacs-major-version 23) (defun server-ensure-safe-dir (dir) "Noop" t))
(when (string-equal system-type "windows-nt")
  (server-start))
(with-eval-after-load 'server (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)) ; 去掉关闭emacsclientw打开的文件的提示
(define-key global-map "\C-r" 'kill-ring-save); M-w经常不起作用
(define-key global-map (kbd "C-x SPC") (lambda () (interactive)
                                         (switch-to-buffer (other-buffer (current-buffer) 1)))) ; 最近buffer切换
(global-set-key (kbd "C-v") 'yank)	; 翻页基本不用
(delete-selection-mode 1);; 选中替换模式，比较方便，但是据说有副作用，先用用再说
(global-set-key [?\C-h] 'delete-backward-char) ;C-H当删除很好用！
(global-set-key [?\M-h] 'backward-kill-word) ;M-H顺便也弄上
(setq x-select-enable-clipboard t);; 支持emacs和外部程序的粘(ubuntu)
;; (icomplete-mode 1);; 用M-x执行某个命令的时候，在输入的同时给出可选的命令名提示，跟swiper冲突

;; 不创建~和#文件
(global-set-key [(meta f8)] (lambda () (interactive) (unless (use-region-p) (mark-page))
			      (call-interactively 'indent-region)))
(setq default-major-mode 'text-mode); 默认text模式

(setq gdb-non-stop-setting nil)
(put 'narrow-to-region 'disabled nil) ; C-x n n和C-x n w，只操作部分buffer

(fset 'yes-or-no-p 'y-or-n-p) ; 将yes/no替换为y/n
(setq use-short-answers t)

;; insert-date
(defun insert-date () ;
  "Insert date at point." ;
  (interactive) ;
  (insert (format-time-string "%Y年%m月%e日 %l:%M %a %p"))) ;

;; Alt;是添加注释，很好用，这里是加强它的功能:
;; 没有激活的区域，就注释/反注释当前行，仅当在行尾的时候才在行尾加注释
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
		    ;;                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
		  ;;                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
;; (global-set-key (kbd "C-c C-c") 'comment-eclipse)

(setq kill-do-not-save-duplicates t)
;; 每次拷贝其他程序的文字来替换时常常会删除，这样剪切板里是内容就没有了
(when (string-equal system-type "windows-nt")
  (defadvice kill-region (before save-clip activate)
    (let* ((clip-str (w32-get-clipboard-data)))
      (and clip-str
	   (unless (equal (nth 0 kill-ring) clip-str)
	     (kill-new clip-str))))))

;; 参考https://www.emacswiki.org/emacs/WholeLineOrRegion和whole-line-or-region.el改进(kill-new最新版本没有yank-handler参数了)
(defadvice kill-ring-save (around slick-copy activate)
  "When called interactively with no active region, copy a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'any)))
      ad-do-it
    (let ((string (buffer-substring (line-beginning-position)
				    (line-beginning-position 2))))
      (when (> (length string) 0)
	(put-text-property 0 (length string)
                           'yank-handler '(yank-line) string))
      (kill-new string nil)
      )
    (message "Copied line")))

(defadvice kill-region (around slick-copy activate)
  "When called interactively with no active region, kill a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'any)))
      ad-do-it
    (let ((string (filter-buffer-substring (line-beginning-position)
					   (line-beginning-position 2) t)))
      (when (> (length string) 0)
	(put-text-property 0 (length string)
                           'yank-handler '(yank-line) string))
      (kill-new string nil)
      )))

(defun yank-line (string)
  "Insert STRING above the current line."
  (save-excursion
    (beginning-of-line)
    (unless (= (elt string (1- (length string))) ?\n)
      (save-excursion (insert "\n")))
    (let ((beg (point-marker)))
      (insert string)
      ;; 必须要删除yank-handler，不然还会遗留在string里 
      (remove-yank-excluded-properties beg (point))
      (yank-advised-indent-function beg (point))   ; 顺便indent一下
      )
    ))

;; 不要agressive之类的auto indent了，但是yank还是需要自动indent的
;; https://github.com/magnars/.emacs.d/blob/cbc1c97756a5bdc19bb386c3de904e83b7be7406/defuns/editing-defuns.el#L99-L124
(defvar yank-indent-modes '(prog-mode
                            sgml-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")
(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  ;; (if (<= (- end beg) yank-advised-indent-threshold)
  ;;     (indent-region beg end nil))
  (indent-region beg end nil)
  )
(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (cl-find-if 'derived-mode-p yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))
(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (cl-find-if 'derived-mode-p yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; Copy line from point to the end, exclude the line break
(defun qiang-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
		  (line-end-position))
  ;; (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key (kbd "M-k") 'qiang-copy-line)

(put 'downcase-region 'disabled nil);; 选中区域 C-X C-L 
(put 'upcase-region 'disabled nil);; 选中区域 C-X C-U

;; C-t 设置标记，原键用c-x t代替，用colemak后，t在食指太容易按到
(global-set-key (kbd "C-q") 'set-mark-command)
;;(global-set-key (kbd "\C-xt") 'transpose-chars)

(setq user-full-name "lynnux")
(setq user-mail-address "lynnux@qq.com")

;;;###autoload
(defun my-insert-char-next-line (arg)
  "insert char below the cursor"
  (interactive "p")
  (let ((col (current-column))
        char)
    (setq char
          (save-excursion
            (forward-line arg)
            (move-to-column col)
            (if (= (current-column) col)
                (char-after))))
    (if char
        (insert-char char 1)
      (message (concat "Can't get charactor in "
                       (if  (< arg 0)
                           "previous"
                         "next")
                       (progn (setq arg (abs arg))
                              (if (= arg 1) ""
                                (concat " " (number-to-string arg))))
                       " line.")))))

;;;###autoload
(defun my-insert-char-prev-line (arg)
  "insert char above the cursor"
  (interactive "p")
  (my-insert-char-next-line (- arg)))

(global-set-key (kbd "C-j") 'my-insert-char-prev-line) ;原想用C-I的，结果会影响TAB键
(global-set-key (kbd "C-S-j") 'my-insert-char-next-line) ;

;;; m-o切换h/cpp文件
(global-set-key (kbd "M-o") 'ff-find-other-file)

(global-set-key (kbd "<f4>") 'next-error)
(global-set-key (kbd "S-<f4>") 'previous-error)

;; 禁止narrow功能
(global-set-key (kbd "C-x n-") nil)
(global-set-key (kbd "C-x n") 'next-line)

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "M-i") 'other-window) ; m-o还是给h/cpp切换吧

(global-unset-key (kbd "C-x C-z"))

(setq delete-by-moving-to-trash t)     ; move files to trash instead of deleting

(setq-default tab-width 4
	      indent-tabs-mode nil)

;; newline-and-indent对{|RET}的}没有处理到
;; https://github.com/magnars/.emacs.d/blob/cbc1c97756a5bdc19bb386c3de904e83b7be7406/defuns/editing-defuns.el#L24-L35
(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-according-to-mode)))
    (indent-according-to-mode))) ;; indent-for-tab-command在Text Mode有问题
(global-set-key (kbd "RET") 'new-line-dwim)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line) ;; shift RET直接新开一行！

;; C-m-f C-m-b不好按，用下面代替
(global-set-key (kbd "M-e") 'forward-sexp)
(global-set-key (kbd "M-a") 'backward-sexp)

;; 各种尝试优化emacs速度，好像有点效果^_^
;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
;; 下面抄自doom，略有修改(spacemacs和purcell都没有)
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right) 
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
;; (setq highlight-nonselected-windows nil) 有些不自然，比如diff的时候
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)
(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
      )
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq read-process-output-max (* 64 1024))
(setq redisplay-skip-fontification-on-input t)
(setq command-line-ns-option-alist nil)

(setq w32-get-true-file-attributes nil   ; decrease file IO workload
      w32-pipe-read-delay 0              ; faster ipc
      w32-pipe-buffer-size (* 64 1024))
(setq
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil)

;; 恢复gc-cons-threshold
(defconst dotspacemacs-gc-cons '(100000000 0.1))
(add-hook 'emacs-startup-hook
          (lambda ()
	    (setq gc-cons-threshold (car dotspacemacs-gc-cons)
                  gc-cons-percentage (cadr dotspacemacs-gc-cons)
                  file-name-handler-alist default-file-name-handler-alist)
            (makunbound 'default-file-name-handler-alist)))
(setq auto-save-list-file-prefix nil)

(setq next-error-recenter (quote (4)))
(setq w32-ignore-modifiers-on-IME-input nil) ;; 有输入法时屏幕C M等
;; revert-buffer不提示，因为有auto save
(setq-default revert-buffer-function (lambda (ignore-auto noconfirm)
                                       (revert-buffer--default ignore-auto t)))

(setq large-file-warning-threshold 100000000) ; 大文件询问，原来10M左右，调整为100M，避免加载TAGS时询问
