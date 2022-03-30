;; compile all: C-u 0 M-x byte-recompile-directory
;; 唯一不能用elc的只有packages\tabbar\tabbar.elc，删除之tabbar就正常了
;; 包不能放下面个目录，应该放plugin目录，否则require的时候找不到
(mapc 'load (directory-files "~/.emacs.d/settings" t "^[a-zA-Z0-9].*.el$"))

;; .emacs主要放针对不同机子要改变的数据！
;; 现在改进为根据登录用户名和系统版本来分别配置
(defun add-path-to-execute-path (path)
  ;;  (setenv "PATH" (concat (getenv "PATH") path))
  ;;  (setq exec-path (append exec-path (list path)))
  (add-to-list 'exec-path path)
  (setenv "PATH" (mapconcat #'identity exec-path path-separator))
  )

(add-path-to-execute-path (expand-file-name "bin" user-emacs-directory))

(cond 
 ;; home
 ((and 
   (string-equal system-type "windows-nt")
   (string-equal "WIN-6PRFQIFSB6O" (system-name))
   (equal (list 6 1 7601) (w32-version)) ;; win7 64
   )
  (add-path-to-execute-path "C:/Git/usr/bin")
  )
 
 ;; work
 ((and 
   (string-equal system-type "windows-nt")
   (string-equal "DESKTOP-9JD2LQI" (system-name))
   (equal (list 10 0 19044) (w32-version))
   )
  (setq
   org-directory "D:/autosync/autosync/ts/org"
   org-agenda-files (list "D:/autosync/autosync/ts/org/idea.org" "D:/autosync/autosync/ts/org/todo.org")
   )
  (setq org-default-notes-file "D:/autosync/autosync/ts/org/default.org")
  (setq tfs/tf-exe "C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\TF.exe")

  ;; ls，xargs都用git里的，但git有多个bin目录，没设置/usr/bin到全局PATH，因为exe太多怕影响vs等程序(如含有link.exe)
  ;; https://emacs.stackexchange.com/questions/27326/gui-emacs-sets-the-exec-path-only-from-windows-environment-variable-but-not-from
  (add-path-to-execute-path "C:/Git/usr/bin")
  )

 ;; 
 (t

  t)
 )
