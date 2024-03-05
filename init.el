;; compile all: C-u 0 M-x byte-recompile-directory
;; 唯一不能用elc的只有packages\tabbar\tabbar.elc，删除之tabbar就正常了
;; 包不能放下面个目录，应该放plugin目录，否则require的时候找不到
;; (mapc 'load (directory-files "~/.emacs.d/settings" t "^[a-zA-Z0-9].*.el$"))
(load "~/.emacs.d/settings/better_default.el")
(load "~/.emacs.d/settings/gui.el")
(load "~/.emacs.d/settings/package_builtin.el")
(load "~/.emacs.d/settings/package_extra.el")

;; .emacs主要放针对不同机子要改变的数据！
;; 现在改进为根据登录用户名和系统版本来分别配置
(defun add-path-to-execute-path (path)
  ;;  (setenv "PATH" (concat (getenv "PATH") path))
  ;;  (setq exec-path (append exec-path (list path)))
  (add-to-list 'exec-path path)
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

(add-path-to-execute-path
 (expand-file-name "bin" user-emacs-directory))

(cond
 ;; home
 ((and (string-equal system-type "windows-nt")
       (string-equal "WIN-6PRFQIFSB6O" (system-name))
       (equal (list 6 1 7601) (w32-version)) ;; win7 64
       )
  (setq
   org-directory "D:/autosync/autosync/org"
   org-agenda-files
   (list
    "D:/autosync/autosync/org/idea.org"
    "D:/autosync/autosync/org/todo.org")
   org-default-notes-file "D:/autosync/autosync/org/default.org"
   org-roam-directory (file-truename "F:/doc/mynote"))
  (add-path-to-execute-path "C:/Program Files/7-Zip") ;; for dired，and `update-all-packages'
  (setq foobar-binary "c:/green/foobar2000/foobar2000.exe")
  (setq mpv-binary "C:/green/mpv/mpv.exe"))

 ;; work
 ((and (string-equal system-type "windows-nt")
       (string-equal "DESKTOP-9JD2LQI" (system-name))
       (equal (list 10 0 19044) (w32-version)))
  (setq
   org-directory "H:/doc/autosync/autosync/org"
   org-agenda-files
   (list
    "H:/doc/autosync/autosync/org/idea.org"
    "H:/doc/autosync/autosync/org/todo.org")
   org-default-notes-file "H:/doc/autosync/autosync/org/default.org"
   org-roam-directory (file-truename "G:/doc/mynote"))
  (setq
   tfs/tf-exe
   "C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\TF.exe")
  (add-path-to-execute-path "C:/Program Files/7-Zip") ;; for dired，and `update-all-packages'
  (setq foobar-binary "H:/green/foobar2000/foobar2000.exe")
  (setq mpv-binary "H:/green/mpv/mpv.exe"))

 ;; 
 (t
  (when (executable-find "git")
    (let ((git-bin-dir
           (concat
            (file-name-parent-directory
             (file-name-directory (executable-find "git")))
            "usr/bin")))
      (when (file-exists-p git-bin-dir)
        (add-path-to-execute-path git-bin-dir))))))
