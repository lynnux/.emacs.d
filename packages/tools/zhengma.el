(require 'json)
(defvar zhengma:data nil
  "")
(setq zhengma:data
      (json-read-file
       (concat
        (file-name-directory load-file-name) "zhengma/zhengma.json")))
(setq zhengma:data (mapcar #'identity zhengma:data))

(defcustom zhengma:host "127.0.0.1"
  "Preview http host."
  :type 'string
  :group 'zhengma)

(defcustom zhengma:port 8882
  "Preview http port."
  :type 'integer
  :group 'zhengma)
(defvar zhengma:web-index nil)
(defvar zhengma:home-path (file-name-directory load-file-name))
(defvar zhengma:preview-file
  (concat zhengma:home-path "zhengma.html"))
(defvar zhengma:current nil
  "")

(defun zhengma:preview-template ()
  "Template."
  (with-temp-buffer
    (insert-file-contents zhengma:preview-file)
    (buffer-string)))
(defun zhengma:open-browser ()
  "Open browser."
  (browse-url (format "http://%s:%s" zhengma:host zhengma:port)))

(defun zhengma:send-preview (num)
  (emacs-preview-rs/web-server-set-content
   zhengma:web-index
   "/get_content"
   (format "<img src=\"%s.png\"/>" num)))

(defun zhengma:start ()
  "Preview init."
  (interactive)
  (unless zhengma:web-index
    (let ((ret
           (emacs-preview-rs/web-server-start
            zhengma:host zhengma:port)))
      (when (> ret 0)
        (setq zhengma:web-index ret)
        (emacs-preview-rs/web-server-set-root
         zhengma:web-index
         (vconcat [] (list (concat zhengma:home-path "zhengma"))))
        (emacs-preview-rs/web-server-set-content
         zhengma:web-index "/" (zhengma:preview-template)))))
  (zhengma:open-browser)
  (zhengma:next-)
  (call-interactively 'zhengma:next))

(defun zhengma:next- ()
  ;; TODO: 跳过376及以后
  (setq zhengma:current
        (nth (mod (random t) (length zhengma:data)) zhengma:data))
  (zhengma:send-preview (cdr (assoc 'index zhengma:current))))

(defun zhengma:next (&optional input)
  (interactive "s请输入图示字根: ")
  (if (string-equal-ignore-case
       input
       (or (cdr
            (assoc (cdr (assoc 'index zhengma:current)) zhengma:gen))
           ""))
      (zhengma:next-)
    (when (functionp 'doom-themes-visual-bell-fn)
      (doom-themes-visual-bell-fn)))
  (call-interactively 'zhengma:next))

;; 376开始，后面几个都是至至郑码新增的字根
(defvar zhengma:gen
  '(("0" . "a") ("1" . "a") ("57" . "du") ("57" . "du"))
  "")

(defun zhengma:stop ()
  (interactive)
  (when zhengma:web-index
    (emacs-preview-rs/web-server-stop zhengma:web-index)
    (setq zhengma:web-index nil)))

(provide 'zhengma)
