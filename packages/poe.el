;;; poe.el --- A popup & window manager -*- lexical-binding: t; -*-
;;
;; A popup and window management library mostly forked from
;; shackle:
;;
;;   https://depp.brause.cc/shackle/
;;
;; With added popup management borrowing from doom's popup module:
;;
;;   https://github.com/doomemacs/doomemacs/blob/master/modules/ui/popup/
;;
;; As opposed to doom popups, in poe there will only ever be one
;; popup shown at a time. To support a fast work flow, many
;; commands to deal with popup exist, such as cycling backwards
;; and forwards, "remote" killing of popups from other buffers,
;; and more.
;;
(require 'cl-extra)
(require 'cl-seq)

(defgroup poe nil
  "A window and popup manager."
  :group 'convenience)

(defcustom poe-popup-slot -1911
  "Slot number to use for popup windows."
  :group 'poe
  :type 'integer)

(defcustom poe-default-size 0.5
  "Default size of aligned windows.

A floating point number between 0 and 1 is interpreted as a
ratio.  An integer equal or greater than 1 is interpreted as a
number of lines. If a function is specified, it is called with
zero arguments and must return a number of the above two types."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Number of lines (ratio)")
                 (function :tag "Custom"))
  :group 'poe)

(defcustom poe-default-alignment 'below
  "Default alignment of aligned windows.
It may be one of the following values:

above: Align above the currently selected window.

below: Align below the currently selected window.

left: Align on the left side of the currently selected window.

right: Align on the right side of the currently selected
window.

<function>: Call the specified function with no arguments to
determine side, must return one of the above four values."
  :type '(choice (const :tag "Above" above)
                 (const :tag "Below" below)
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'poe)

(defcustom poe-remove-fringes-from-popups t
  "Remove fringes from popup windows."
  :group 'poe
  :type 'boolean)

(defcustom poe-dim-popups t
  "Change (\"dim\") popup windows with a different background
color.

See: `poe-popup-dimmed-face'."
  :group 'poe
  :type 'boolean)

(defface poe-popup-dimmed-face
  `((t (:background "#151617")))
  "Face used to \"dim\" popup windows.

Only the background property is used."
  :group 'poe)

(defconst poe-rules-custom-type
  '(plist :options
          (((const :tag "Regexp" :regexp) boolean)
           ((const :tag "Same" :same) boolean)
           ((const :tag "Frame" :frame) boolean)
           ((const :tag "Popup" :popup) boolean)
           ((const :tag "Select" :select) boolean)
           ((const :tag "Shrink" :shrink) boolean)
           ((const :tag "Inhibit window quit" :inhibit-window-quit) boolean)
           ((const :tag "Align" :align)
            (choice :tag "Align" :value nil
                    (const :tag "Default" nil)
                    (const :tag "Above" above)
                    (const :tag "Below" below)
                    (const :tag "Left" left)
                    (const :tag "Right" right)
                    (function :tag "Function")))
           ((const :tag "Size" :size) number)))
  "Shared custom :type fields for rules.")

(defcustom poe-rules nil
  "Window display rules."
  :group 'poe
  :type '(alist :key-type (choice :tag "Condition"
                                  (symbol :tag "Major mode")
                                  (string :tag "Buffer name"))
                :value-type poe-rules-custom-type))

(defcustom poe-default-rule nil
  "Default rules to include for all windows.

Supported rules are the same as for `poe-rules'."
  :group 'poe
  :type poe-rules-custom-type)

(defcustom poe-popup-default-rule '(:align below
                                    :size 0.25
                                    :popup t)
  "Default rules to include for popup windows.

Supported rules are the same as for `poe-rules', however, the
\":popup\" rule has no effect."
  :group 'poe
  :type poe-rules-custom-type)


;; Implementation

(defvar poe--popup-buffer-list nil
  "List of popup buffers in the order they were opened in.

The `car' of this list will be the most recently visible popup.
Used for cycling popup buffers with `poe-popup-next' and
`poe-popup-prev'.")

(defun poe--alist-merge (&rest alists)
  "Create a single association list from all alists in ALISTS.
The process starts by copying the first list, and then setting
associations from the other lists.  Settings in the last list
are the most significant ones and overrule settings in the
other lists.

Adapted from `org-combine-plists' for association lists."
  (let ((rtn (copy-sequence (pop alists)))
        ls)
    (while alists
      (setq ls (pop alists))
      (dolist (x ls)
        (if (null (assoc (car x) rtn))
            (setq rtn (nconc rtn (list x)))
          (setcdr (assq (car x) rtn) (cdr x)))))
    rtn))

(defun poe--plist-merge (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting
properties from the other lists.  Settings in the last list
are the most significant ones and overrule settings in the
other lists.

This is coming from `org-mode' (`org-combine-plists'). Requiring
`org-mode' loads a 24k+ line Emacs Lisp file, which introduces
significant overhead when used as a utility library, hence it
has been extracted."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun poe--transform-side (split-side)
  "Transforms SPLIT-SIDE argument for `split-window' into a
\"side\" suitable for for `display-buffer-alist' alists."
  (pcase split-side
    ('above 'top)
    ('below 'bottom)
    (_ split-side)))

(defun poe--alist (alist rule)
  "Transforms a poe rule into an alist in the format expected by
display buffer actions."
  (poe--alist-merge
   alist
   `((actions         . ,(plist-get rule :actions))
     (size            . ,(plist-get rule :size))
     (side            . ,(poe--transform-side (plist-get rule :size)))
     (slot            . ,(plist-get rule :slot)))))

(defvar poe--inhibit-match nil
  "Variable that can be set to inhibit `display-buffer' to select
the poe display functions.")

(defvar-local poe--popup-buffer-override nil
  "Buffer-local variable that holds state overriding poe's matching.

`poe-popup-raise' and `poe-popup-lower' may set this to override
the matching behaviour in `poe--match' and `poe--popup-buffer-p'.")

(put 'poe--popup-buffer-override 'permanent-local t)

(defun poe--popup-raised-p (&optional buffer)
  "Predicate function that indicates whether BUFFER is raised.

Defaults to the current buffer"
  (let ((buffer (or buffer
                    (current-buffer))))
    (eq (buffer-local-value 'poe--popup-buffer-override buffer)
        'raised)))

(defun poe--popup-lowered-p (&optional buffer)
  "Predicate function that indicates whether BUFFER is lowered.

Defaults to the current buffer"
  (let ((buffer (or buffer
                    (current-buffer))))
    (eq (buffer-local-value 'poe--popup-buffer-override buffer)
        'lowered)))

(defun poe--popup-raise (&optional buffer)
  "Raises the given popup buffer to a regular buffer.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (unless (poe--popup-match buffer)
      (user-error "Cannot raise a non-popup buffer"))
    (with-current-buffer buffer
      (when poe-dim-popups
        (buffer-face-set 'default))
      (poe-popup-mode -1)
      (if (poe--popup-lowered-p buffer)
          (setq poe--popup-buffer-override nil)
        (setq poe--popup-buffer-override 'raised)))
    (poe--popup-remove-from-list buffer)))

(defun poe--popup-lower (&optional buffer)
  "Lowers the given buffer to a popup buffer.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (if (poe--popup-raised-p buffer)
        (with-current-buffer buffer
          (setq poe--popup-buffer-override nil)
          (poe-popup-mode t))
      (with-current-buffer buffer
        (setq poe--popup-buffer-override 'lowered)
        (poe-popup-mode t)))))

(defun poe--match (buffer-or-name)
  "Match BUFFER-OR-NAME against any conditions defined in
`poe-rules' and, if found, returns the rule plist."
  (unless poe--inhibit-match
    (when-let* ((buffer (get-buffer buffer-or-name))
                (buffer-major-mode (buffer-local-value 'major-mode buffer))
                (buffer-name (buffer-name buffer)))
      (cond
       ((poe--popup-lowered-p buffer)
        poe-popup-default-rule)
       ((poe--popup-raised-p buffer)
        nil)
       (t
        (cl-loop
         for (condition . plist) in poe-rules
         when (or
               ;; Symbol, compare to major-mode
               (and (symbolp condition)
                    (eq condition buffer-major-mode))
               ;; String, compare to buffer name or match against it if
               ;; the :regexp rule is set.
               (and (stringp condition)
                    (or (string= condition buffer-name)
                        (and (plist-get plist :regexp)
                             (string-match condition
                                           buffer-name)))))
         return plist))))))

(defun poe--popup-match (buffer-or-name)
  "Match BUFFER-OR-NAME against any conditions defined in
`poe-rules' and, if found and a popup, returns the rule plist."
  (let ((rule (poe--match buffer-or-name)))
    (when (plist-get rule :popup)
      rule)))

(defun poe--popup-split-window (window size side)
  "Ensure a non-dedicated/popup window is selected when splitting
a window."
  (cl-loop for win
           in (cons (or window (selected-window))
                    (window-list nil 0 window))
           unless (poe--popup-window-p win)
           return (setq window win))
  (let ((ignore-window-parameters t))
    (split-window window size side)))

(defun poe--popup-remove-from-list (&optional buffer)
  "Remove BUFFER from the popup buffer list.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (setq poe--popup-buffer-list
          (remove buffer poe--popup-buffer-list))))

(defvar poe--popup-inhibit-select-buffer nil
  "Variable that can be set to inhibit `display-buffer' to select
a new popup that has the :select rule when the popup buffer is not
already focused.")


(defvar poe--popup-inhibit-kill-buffer nil
  "Variable that can be set during a delete-window cycle to inhibit
any `kill-buffer' calls issued for buffers with the :ephemeral
rule.")

(defvar poe--popup-force-kill-buffer nil
  "Variable that can be set during a delete-window cycle to force
a subsequent `kill-buffer', irrespective of the :ephemeral rule.")

(defun poe--popup-kill-buffer (buffer)
  "Kill the popup-buffer BUFFER after killing any associated
buffer-process. "
  (let ((inhibit-quit t))
    (cond
     ;; Buffer isn't live anymore, no need to kill it.
     ((not (buffer-live-p buffer)))
     ((not (get-buffer-window buffer t))
      (with-demoted-errors "Error killing ephemeral buffer: %s"
        (with-current-buffer buffer
          (let (confirm-kill-processes)
            ;; Don't ask to kill processes.
            (when-let (process (get-buffer-process buffer))
              (when (eq (process-type process) 'real)
                (kill-process process)))
            (let (kill-buffer-query-functions)
              ;; HACK The debugger backtrace buffer, when killed, called
              ;;      `top-level'. This causes jumpiness when the popup
              ;;      manager tries to clean it up.
              (cl-letf (((symbol-function #'top-level) #'ignore))
                (kill-buffer buffer))))))))))

(defun poe--popup-delete-window (window)
  "A `delete-window' window-parameter function for popup buffers.

Will ask to save if the popup is file backed and modified, restore
the window-configuration and kill the popup buffer if it's marked
as :ephemeral,unless the window is still visiable after restoring
the previous window configuration."
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    ;; If the window buffer is file-backed and has been modified, ask if we
    ;; want to save it.
    (and (or (buffer-file-name buffer)
             (if-let (base-buffer (buffer-base-buffer buffer))
                 (buffer-file-name base-buffer)))
         (buffer-modified-p buffer)
         (y-or-n-p "Popup buffer is modified. Save it?")
         (with-current-buffer buffer (save-buffer)))
    ;; Delete window.
    (let ((ignore-window-parameters t))
      (delete-window window))
    ;; Kill the buffer unless it's still a live-buffer.
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (poe-popup-mode -1)
        (when (or poe--popup-force-kill-buffer
                  (and (not poe--popup-inhibit-kill-buffer)
                       (plist-get (window-parameter window 'poe-rule)
                                  :ephemeral)))
          (poe--popup-remove-from-list buffer)
          (poe--popup-kill-buffer buffer))))))

(defun poe--frame-splittable-p (frame)
  "Return FRAME if it is splittable."
  (when (and (window--frame-usable-p frame)
             (not (frame-parameter frame 'unsplittable)))
    frame))

(defun poe--splittable-frame ()
  "Return a splittable frame to work on.

This can be either the selected frame or the last frame that's
not displaying a lone minibuffer."
  (let ((selected-frame (selected-frame))
        (last-non-minibuffer-frame (last-nonminibuffer-frame)))
    (or (poe--frame-splittable-p selected-frame)
        (poe--frame-splittable-p last-non-minibuffer-frame))))

(defun poe--find-window-by-slot (slot)
  "Find window with window-parameter \"window-slot\" matching SLOT.

If found, returns the existing window."
  (when slot
    (let ((this-slot)
          (this-window))
      (catch 'found
        (dolist (window (window-list))
          (setq this-slot (window-parameter window 'window-slot))
          (cond
           ((not (numberp this-slot)))
           ((= this-slot slot)
	    ;; A window with a matching slot has been found.
	    (setq this-window window)
	    (throw 'found t)))))
      this-window)))

(defun poe--display-buffer-frame (buffer alist plist)
  "Display BUFFER in a popped up frame.

ALIST is passed to `window--display-buffer' internally.
If PLIST contains the :other key with t as value, reuse the next
available frame if possible, otherwise pop up a new frame."
  (let* ((params (cdr (assq 'pop-up-frame-parameters alist)))
         (pop-up-frame-alist (append params pop-up-frame-alist))
         (fun pop-up-frame-function))
    (when fun
      (let* ((frame (if (and (plist-get plist :other)
                             (> (length (frames-on-display-list)) 1))
                        (next-frame nil 'visible)
                      (funcall fun)))
             (window (frame-selected-window frame)))
        (prog1 (window--display-buffer buffer window 'frame alist)
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame frame)))))))

(defun poe--display-buffer-aligned-window (buffer alist plist)
  "Display BUFFER in an aligned window.

ALIST is passed to `window--display-buffer' internally.
Optionally use a different alignment and/or size if PLIST
contains the :alignment key with an alignment different than the
default one in `poe-default-alignment' and/or PLIST contains
the :size key with a number value."
  (let ((frame (poe--splittable-frame)))
    (when frame
      (let* ((alignment-argument (plist-get plist :align))
             (alignments '(above below left right))
             (alignment (cond
                         ((functionp alignment-argument)
                          (funcall alignment-argument))
                         ((memq alignment-argument alignments)
                          alignment-argument)
                         ((functionp poe-default-alignment)
                          (funcall poe-default-alignment))
                         (t poe-default-alignment)))
             (horizontal (when (memq alignment '(left right)) t))
             (old-size (window-size (frame-root-window) horizontal))
             (size (or (plist-get plist :size)
                       (if (functionp poe-default-size)
                           (funcall poe-default-size)
                         poe-default-size)))
             (new-size (round (if (>= size 1)
                                  (- old-size size)
                                (* (- 1 size) old-size)))))
        (if (or (< new-size (if horizontal window-min-width window-min-height))
                (> new-size (- old-size (if horizontal window-min-width
                                          window-min-height))))
            (error "Invalid aligned window size %s, aborting" new-size)
          (let ((slot (plist-get plist :slot)))
            ;; Ok, this is a nasty work-around. If we have a window with a
            ;; matching slot and try to reuse the window, we have to deal with
            ;; calculating deltas to resize. Also, the window may not be
            ;; resizable. So in the end it's easier to just delete the window
            ;; and create a new one so popups also end up where they're
            ;; expected.
            (when-let ((existing-window (poe--find-window-by-slot slot)))
              ;; Edge case: In some cases it may not be safe to delete the
              ;; window, so we just ignore that one already exists and spawn a
              ;; new one anyway.
              (when (window-deletable-p existing-window)
                (delete-window existing-window)))
            (let ((window (split-window (frame-root-window frame)
                                        new-size alignment)))
              (prog1 (window--display-buffer buffer window 'window alist)
                (set-window-parameter window 'window-slot slot)
                (unless (cdr (assq 'inhibit-switch-frame alist))
                  (window--maybe-raise-frame frame))))))))))

(defun poe--display-buffer-reuse-window (buffer alist _plist)
  "Wrapper for `display-buffer-reuse-window' for `poe-mode'."
  (display-buffer-reuse-window buffer alist))

(defun poe--display-buffer-same-window (buffer alist _plist)
  "Wrapper for `display-buffer-same-window' for `poe-mode'."
  (display-buffer-same-window buffer alist))

(defun poe--display-buffer-fallback (buffer alist _plist)
  "Wrapper for `display-buffer' fallback for `poe-mode'."
  (cl-loop for func in (car display-buffer-fallback-action)
           if (funcall func buffer alist)
           return it))

(defun poe--popup-delete-other-windows (&rest _)
  "Handler for `delete-other-windows' window-parameter for
poe-popup-mode buffers."
  (error "Cannot make popup window the only window"))

(defun poe--display-buffer-actions (alist rule)
  "Return display-buffer actions to be used for displaying a buffer
with ALIST and poe rules RULE."
  (or (cdr (assq 'actions alist))
      (cond
       ;; When :popup or :side is set, display it as an aligned window.
       ((or (plist-get rule :popup)
            (plist-get rule :side))
        '(poe--display-buffer-reuse-window
          poe--display-buffer-aligned-window))
       ;; Use same window if :same is set.
       ((and (plist-get rule :same)
             ;; There is `display-buffer--same-window-action' which things
             ;; like `info' use to reuse the currently selected window, it
             ;; happens to be of the (inhibit-same-window . nil) form and
             ;; should be permitted unless a popup is requested
             (and (assq 'inhibit-same-window alist)
                  (not (cdr (assq 'inhibit-same-window alist)))))
        '(poe--display-buffer-reuse-window
          poe--display-buffer-same-window))
       ;; When :frame, display it as an aligned window.
       ((plist-get rule :frame)
        '(poe--display-buffer-reuse-window
          poe--display-buffer-frame))
       ;; Default to `display-buffer-fallback-action'.
       (t
        '(poe--display-buffer-fallback)))))

(defun poe--init-popup (window)
  "Initialize `poe-popup-mode' and popup-specific
window-parameters for WINDOW."
  (with-selected-window window
    (set-window-dedicated-p window t)
    (set-window-parameter window
                          'split-window
                          #'poe--popup-split-window)
    (set-window-parameter window
                          'delete-window
                          #'poe--popup-delete-window)
    (set-window-parameter window
                          'delete-other-windows
                          #'poe--popup-delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (poe-popup-mode t)))

(defun poe--normalize-rule (rule)
  "Normalize RULE by merging with `poe-default-rule'.

If rule has :popup set to t, will also merge RULE with
`poe-popup-default-rule' and force the slot to `poe-popup-slot'."
  (if (plist-get rule :popup)
      (poe--plist-merge poe-default-rule
                        poe-popup-default-rule
                        rule
                        `(:slot ,poe-popup-slot))
    (poe--plist-merge poe-default-rule
                      rule)))

(defun poe--display-buffer (buffer alist rule)
  "Handles displaying of poe-managed buffers."
  (let* ((origin (selected-window))
         (origin-was-popup (poe--popup-window-p origin))
         (rule (poe--normalize-rule rule))
         (alist (poe--alist alist rule))
         (actions (poe--display-buffer-actions alist rule)))
    ;; Call all the display-buffer actions until we find one that works.
    (when-let (window (cl-loop for func in actions
                               if (funcall func buffer alist rule)
                               return it))
      (set-window-parameter window 'poe-rule rule)
      (when (plist-get rule :inhibit-window-quit)
        (set-window-parameter window 'quit-restore nil))
      (when (plist-get rule :popup)
        (poe--init-popup window))
      (when (plist-get rule :shrink)
        (poe--shrink-to-fit window))
      ;; Select the new popup if the :select rule is set. When the origin
      ;; window was a popup, mainain focus irrespective of :select.
      (if (or origin-was-popup
              (and (not poe--popup-inhibit-select-buffer)
                   (plist-get rule :select)))
          (select-window window)
        ;; Handle edge case where we spawn a popup without selecting it and
        ;; the currently highlighted line in `hl-line-mode' buffers with
        ;; `hl-line-sticky-flag' set to nil would disappear until the cursor
        ;; is moved.
        (with-selected-window origin
          (when (and (bound-and-true-p hl-line-mode)
                     (fboundp 'hl-line-highlight))
            (hl-line-highlight))))
      window)))

(defun poe--display-buffer-condition (buffer _action)
  "Condition function for `display-buffer-alist'."
  (poe--match buffer))

(defun poe--display-buffer-action (buffer alist)
  "Action function for `display-buffer-alist'."
  (poe--display-buffer buffer alist (poe--match buffer)))

(defun poe--shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.
Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

(defun poe--popup-buffer-p (&optional buffer)
  "Return BUFFER if the buffer is a popup buffer, `nil' otherwise.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (and (bufferp buffer)
         (buffer-live-p buffer)
         (buffer-local-value 'poe-popup-mode buffer)
         (not (poe--popup-raised-p buffer))
         buffer)))

(defun poe--regular-buffer-p (&optional buffer)
  "Return BUFFER if the buffer is a regular buffer, `nil' otherwise.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (when (not (poe--popup-buffer-p buffer))
      buffer)))

(defun poe--popup-window-p (&optional window)
  "Return WINDOW if the window's buffer is a popup buffer, `nil'
otherwise.

Defaults to the currently selected window."
  (let ((window (or window (selected-window))))
    (and (poe--popup-buffer-p (window-buffer window))
         window)))

(defsubst poe--move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))

(defun poe--popup-all-buffers ()
  "Returns a list of open popup buffers, including buffers that
would be handled as popups but have not been displayed yet."
  (seq-filter #'poe--popup-match (buffer-list)))

(defun poe--popup-buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'poe--popup-buffer-p (buffer-list)))

(defun poe--popup-windows ()
  "Returns a list of open popup windows."
  (seq-filter #'poe--popup-window-p (window-list)))

(defun poe--switch-to-prev-buffer-skip (window new-buffer _bury-or-kill)
  "Handler for `switch-to-prev-buffer-skip'.

Ensudes that `switch-to-prev-buffer' won't replace non-popup
buffers with popups and vice versa."
  (let ((window-is-popup (poe--popup-window-p window))
        (new-buffer-is-popup (poe--popup-match new-buffer)))
    (or (and (not window-is-popup)
             new-buffer-is-popup)
        (and window-is-popup
             (not new-buffer-is-popup)))))


;; Advice functions

(defun poe--override-display-buffer-alist-a (orig-fun &rest args)
  "Advice for functions like `switch-to-buffer-other-frame' to respect their
passed actions and bypass `display-buffer-alist'. "
  (let ((poe--inhibit-match t))
    (apply orig-fun args)))


;; Hook functions

(defun poe--popup-update-buffer-list-h ()
  "Hook function called from `window-configuration-change-hook' to
update `ef-popup--buffer-list' with any changes."
  (dolist (buf (cl-set-difference (poe--popup-all-buffers)
                                  poe--popup-buffer-list))
    (setq poe--popup-buffer-list
          (poe--move-to-front buf poe--popup-buffer-list))))

(defun poe--popup-dim-h ()
  "Hook function to dim buffer background with the :background
property from `poe-popup-dimmed-face'.

Called in `poe-popup-buffer-mode-hook'"
  (when (and (bound-and-true-p poe-popup-mode)
             poe-dim-popups)
    (if (poe--popup-buffer-p)
        (buffer-face-set
         `(:background ,(face-background 'poe-popup-dimmed-face)))
      (buffer-face-set 'default))))

(defun poe--popup-remove-fringes-h ()
  "Hook function to remove finges from popup buffers.

Called in `poe-popup-buffer-mode-hook'"
  (when (and (bound-and-true-p poe-popup-mode)
             poe-remove-fringes-from-popups)
    (let ((f (if (poe--popup-buffer-p) 0)))
      (set-window-fringes nil f f fringes-outside-margins))))

(defun poe--popup-run-hooks-maybe-h ()
  "Hook function called from `after-change-major-mode-hook' to re-run
`poe-popup-mode-hook' hooks if the buffer still matches the conditions.

Some packages set major-modes after displaying the window, which will disable
`poe-popup-mode' again. While we keep the variable `poe-popup-mode' as
permanent-local, we have to re-run the hooks again."
  (when (and (bound-and-true-p poe-mode)
             (bound-and-true-p poe-popup-mode)
             (poe--popup-match (current-buffer)))
    (run-hooks 'poe-popup-mode-hook)))


;; Public

;;;###autoload
(defun poe-popup-save-a (orig-fun &rest args)
  "Sets aside popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (if (bound-and-true-p poe-mode)
      (let ((poe--popup-inhibit-kill-buffer t))
        (if (poe--popup-windows)
            (prog2
                (poe-popup-close)
                (apply orig-fun args)
              (poe-popup-open))
          (apply orig-fun args)))
    (apply orig-fun args)))

;;;###autoload
(defun poe-rule (key &rest plist)
  "Add a new display-buffer rule to `poe-rules'."
  ;; Avoid having duplicate rules for a condition.
  (setq poe-rules (cl-delete key poe-rules :key #'car :test #'equal))
  (push (cons key plist) poe-rules))

;;;###autoload
(defun poe-popup (key &rest plist)
  "Add a new display-buffer rule for a popup window to `poe-rules'."
  (setq poe-rules (cl-delete key poe-rules :key #'car :test #'equal))
  (push (cons key (plist-put plist :popup t)) poe-rules))

;;;###autoload
(defun poe-popup-discard ()
  "Kills the currently visible popup, if any. "
  (interactive)
  (when-let ((win (car (poe--popup-windows))))
    (let ((poe--popup-force-kill-buffer t))
      (delete-window win))))

;;;###autoload
(defun poe-popup-kill ()
  "Kills the currently visible popup, if any.

If there is more than one buffer in the buffer-list that is managed
as a popup, display that buffer."
  (interactive)
  (when-let ((win (car (poe--popup-windows))))
    (let ((poe--popup-force-kill-buffer t))
      (delete-window win))
    (when-let ((buf (car poe--popup-buffer-list)))
      (display-buffer buf))))

;;;###autoload
(defun poe-popup-close ()
  "Close the open popup window, if any."
  (interactive)
  (when-let ((win (car (poe--popup-windows))))
    (delete-window win)
    t))

;;;###autoload
(defun poe-popup-open ()
  "Open the last opened popup window.

Will select the popup window if the popup rule specifies :select."
  (interactive)
  (when-let ((buf (car poe--popup-buffer-list)))
    (display-buffer buf)))

;;;###autoload
(defun poe-popup-toggle ()
  "Toggle visibility of the last opened popup window.

Will select the popup window if the popup rule specifies :select."
  (interactive)
  (or (poe-popup-close)
      (poe-popup-open)))

;;;###autoload
(defun poe-popup-raise (window &optional arg)
  "Raise a popup to a regular window.

If the popup is a previously lowered regular buffer, will re-open
it in a regular window.

Defaults to the currently selected window."
  (interactive
   (list (selected-window) current-prefix-arg))
  (cl-check-type window window)
  (unless (poe--popup-window-p window)
    (user-error "Cannot raise a non-popup window"))
  (let ((buffer (window-buffer window))
        (poe--popup-inhibit-kill-buffer t)
        (inhibit-quit t))
    (poe--popup-raise buffer)
    (delete-window window)
    (if arg
        (pop-to-buffer buffer)
      (display-buffer buffer))
    (selected-window)))

(defun poe-popup-lower (window)
  "Lowers a window to a popup window.

If the window is a previously raised popup buffer, will re-open
it in a popup window.

Defaults to the currently selected window."
  (interactive (list (selected-window)))
  (cl-check-type window window)
  (if (poe--popup-window-p window)
      (user-error "Cannot lower a popup window"))
  (let ((buffer (window-buffer window))
        (poe--popup-inhibit-kill-buffer t)
        (inhibit-quit t))
    (unless (switch-to-prev-buffer window)
      (user-error "No regular buffer to replace lowered buffer with"))
    (poe-popup-close)
    (poe--popup-lower buffer)
    (display-buffer buffer)
    (selected-window)))

;;;###autoload
(defun poe-popup-next ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (let ((poe--popup-inhibit-select-buffer t))
    (if (= 0 (length (poe--popup-windows)))
        (poe-popup-open)
      (when poe--popup-buffer-list
        (setq poe--popup-buffer-list
              (cons (car (last poe--popup-buffer-list))
                    (butlast poe--popup-buffer-list)))
        (display-buffer (car poe--popup-buffer-list))))))

;;;###autoload
(defun poe-popup-prev ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (let ((poe--popup-inhibit-select-buffer t))
    (if (= 0 (length (poe--popup-windows)))
        (poe-popup-open)
      (when poe--popup-buffer-list
        (setq poe--popup-buffer-list
              (append (cdr poe--popup-buffer-list)
                      (list (car poe--popup-buffer-list))))
        (display-buffer (car poe--popup-buffer-list))))))


;; Consult source

(defun poe--consult-source-predicate (buffer)
  "Predicate function for `consult'.

If the currently selected buffer is a popup, returns t if BUFFER is
a popup, otherwise returns t if BUFFER is a regular buffer. "
  (if (poe--popup-buffer-p)
      (when-let ((rule (poe--match buffer)))
        (plist-get rule :popup))
    (if-let ((rule (poe--match buffer)))
        (not (plist-get rule :popup))
      t)))

(defun poe--consult-source-query ()
  "Query function for `consult'.

Uses `poe--consult-source-predicate' to provide popup context
aware buffer lists."
  (consult--buffer-query :sort 'visibility
                         :predicate #'poe--consult-source-predicate
                         :as #'buffer-name))

(with-eval-after-load 'consult
  (declare-function consult--buffer-state "ext:consult")
  (declare-function consult--buffer-query "ext:consult")

  (defvar poe-consult-source
    (list :name     "Poe Buffers"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items #'poe--consult-source-query)
    "A `consult' source for `poe-mode'.

Provides buffer lists based on the currently selected buffer. If a
popup is selected, will give a list of popups, otherwise gives a
list of regular, non-popup buffers."))


;; Modes

(defvar poe-popup-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows.

See `poe-popup-mode'.")

;;;###autoload
(define-minor-mode poe-popup-mode
  "Minor mode for `poe-mode' buffers shown in popup windows."
  :group 'poe
  :lighter nil
  :interactive nil
  :init-value nil
  :keymap poe-popup-mode-map)

(put 'poe-popup-mode 'permanent-local t)
(put 'poe-popup-mode 'permanent-local-hook t)

(defvar poe-mode-map (make-sparse-keymap)
  "Global keymap for `poe-mode'.")

(defvar poe--old-switch-to-buffer-obey-display-actions
  switch-to-buffer-obey-display-actions
  "Placeholder for `switch-to-buffer-obey-display-actions' value
 before enabling `'poe-mode'")

(defvar poe--old-switch-to-prev-buffer-skip
  switch-to-prev-buffer-skip
  "Placeholder for `switch-to-prev-buffer-skip' value before
enabling `'poe-mode'")

;;;###autoload
(define-minor-mode poe-mode
  "Toggle `poe' on or off."
  :group 'poe
  :global t
  :lighter nil
  :keymap poe-mode-map
  (if poe-mode
      ;; Mode enabled
      (progn
        (setq poe--old-switch-to-prev-buffer-skip switch-to-prev-buffer-skip)
        (setq switch-to-prev-buffer-skip #'poe--switch-to-prev-buffer-skip)
        (setq poe--old-switch-to-buffer-obey-display-actions
              switch-to-buffer-obey-display-actions)
        (setq switch-to-buffer-obey-display-actions t)
        (add-hook 'after-change-major-mode-hook #'poe--popup-run-hooks-maybe-h)
        (add-hook 'window-configuration-change-hook
                  #'poe--popup-update-buffer-list-h)
        (add-hook 'kill-buffer-hook #'poe--popup-remove-from-list)
        (setq display-buffer-alist
              (cons '(poe--display-buffer-condition poe--display-buffer-action)
                    display-buffer-alist))
        (advice-add 'balance-windows :around #'poe-popup-save-a)
        (advice-add 'display-buffer-other-frame
                    :around #'poe--override-display-buffer-alist-a)
        (advice-add 'switch-to-buffer-other-frame
                    :around #'poe--override-display-buffer-alist-a)
        (advice-add 'switch-to-buffer-other-window
                    :around #'poe--override-display-buffer-alist-a)
        (advice-add 'switch-to-buffer-other-tab
                    :around #'poe--override-display-buffer-alist-a)
        (poe--popup-update-buffer-list-h))
    ;; Mode disabled
    (setq switch-to-prev-buffer-skip poe--old-switch-to-prev-buffer-skip)
    (setq poe--old-switch-to-prev-buffer-skip nil)
    (setq switch-to-buffer-obey-display-actions
          poe--old-switch-to-buffer-obey-display-actions)
    (setq poe--old-switch-to-buffer-obey-display-actions nil)
    (remove-hook 'after-change-major-mode-hook #'poe--popup-run-hooks-maybe-h)
    (remove-hook 'kill-buffer-hook #'poe--popup-remove-from-list)
    (remove-hook 'window-configuration-change-hook
                 #'poe--popup-update-buffer-list-h)
    (setq display-buffer-alist
          (remove '(poe--display-buffer-condition poe--display-buffer-action)
                  display-buffer-alist))
    (advice-remove 'balance-windows #'poe-popup-save-a)
    (advice-remove 'display-buffer-other-frame
                   #'poe--override-display-buffer-alist-a)
    (advice-remove 'switch-to-buffer-other-frame
                   #'poe--override-display-buffer-alist-a)
    (advice-remove 'switch-to-buffer-other-window
                   #'poe--override-display-buffer-alist-a)
    (advice-remove 'switch-to-buffer-other-tab
                   #'poe--override-display-buffer-alist-a)))

(add-hook 'poe-popup-mode-hook #'poe--popup-dim-h)
(add-hook 'poe-popup-mode-hook #'poe--popup-remove-fringes-h)

(provide 'poe)
