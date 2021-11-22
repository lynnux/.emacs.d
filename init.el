;; compile all: C-u 0 M-x byte-recompile-directory
;; 唯一不能用elc的只有packages\tabbar\tabbar.elc，删除之tabbar就正常了
;; 包不能放下面个目录，应该放plugin目录，否则require的时候找不到
(mapc 'load (directory-files "~/.emacs.d/settings" t "^[a-zA-Z0-9].*.el$"))

;; .emacs主要放针对不同机子要改变的数据！
;; 现在改进为根据登录用户名和系统版本来分别配置
(defun add-path-to-execute-path (path)
  (setenv "PATH" (concat (getenv "PATH") path))
  (setq exec-path (append exec-path (list path))))
(cond 
 ;; home
 ((and
   (string-equal system-type "windows-nt")
   (string-equal "20100910-1853" (system-name)) ; computer name
   (equal (user-real-login-name) "Administrator")
   (equal (list 5 1 2600) (w32-version))	; xp
   )
  ) ;; end home
 
 ;; work
 ((and 
   (string-equal system-type "windows-nt")
   (string-equal "WIN7-2021QVBZQE" (system-name))
   (equal (list 6 1 7601) (w32-version)) ;; win7 64
   )
  (setq
   org-directory "D:/autosync/autosync/ts/org"
   org-agenda-files (list "D:/autosync/autosync/ts/org/idea.org" "D:/autosync/autosync/ts/org/todo.org")
   )
  (setq org-default-notes-file "D:/autosync/autosync/ts/org/default.org")
  (setq tfs/tf-exe "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\IDE\\TF.exe")

  ;; ls，xargs都用git里的，但git有多个bin目录，没设置/usr/bin到全局PATH，因为exe太多怕影响vs等程序(如含有link.exe)
  ;; https://emacs.stackexchange.com/questions/27326/gui-emacs-sets-the-exec-path-only-from-windows-environment-variable-but-not-from
  (add-to-list 'exec-path "F:/Git/usr/bin")
  (setenv "PATH" (mapconcat #'identity exec-path path-separator))
  )

 ;; 
 (t

  t)
 )
