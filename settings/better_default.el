;; Time-stamp: <2021-11-24 21:03:06 lynnux>
;; gui相关设置在set_gui.el中
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
(global-set-key (kbd "C-S-v") 'popup-kill-ring)
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
(global-set-key "\M-;" 'qiang-comment-dwim-line)
(global-set-key [(meta ?/)] 'comment-line)

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
      (remove-yank-excluded-properties beg (point)))
    ))

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

;; 时间戳用法：Time-stamp: <>或者Time-stamp: " "，只会更新第一个时间戳
(add-hook 'before-save-hook 'time-stamp)

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

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key (kbd "C-<up>") 'move-text-up)
(global-set-key (kbd "C-<down>") 'move-text-down)

;;; m-o切换h/cpp文件
(global-set-key (kbd "M-o") 'ff-find-other-file)

(global-set-key (kbd "<f4>") 'next-error)
(global-set-key (kbd "S-<f4>") 'previous-error)

;; 禁止narrow功能
(global-set-key (kbd "C-x n-") nil)
(global-set-key (kbd "C-x n") 'next-line)

(global-set-key (kbd "C-x C-o") 'other-window)

(global-unset-key (kbd "C-x C-z"))

(setq delete-by-moving-to-trash t)     ; move files to trash instead of deleting

(setq tab-width 4
      indent-tabs-mode nil)
