(setq byte-compile-warnings '(cl-functions)) ; disable "Package cl is deprecated"
(setq load-prefer-newer t) ; since 24.4 不加载过期elc文件，但IO会检查文件时间，如果长期不更新可以用(setq load-prefer-newer noninteractive)
(setq native-comp-async-report-warnings-errors 'silent)
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6) ; 参考spacemacs，据说可以加快启动时间，在startup-hook里恢复

;; https://www.reddit.com/r/emacs/comments/msll0j/do_any_of_you_have_some_tips_on_speeding_up_emacs/guxj18c/?context=3
(setq
 default-file-name-handler-alist
 file-name-handler-alist
 file-name-handler-alist nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; 窗口最大化

;; 参考doom屏蔽启动时的ugly flash并加快一点速度
(setq-default
 inhibit-redisplay t
 inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
