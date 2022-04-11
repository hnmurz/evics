(defvar evics-visual-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") 'kill-region)
    map)
  "Evics visual transient mode keymap, this keymap will be active
  whenever the mark is active. Use this keymap to bind any custom
  selection functions that you want to write. By default this map
  will not have any new behaviour since this would not be
  included in vanilla emacs or vim. But, feel free to add your
  own custom functions.")

(defvar evics-visual-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "d") 'evics-visual-kill-line)
    (define-key map (kbd "<escape>") 'evics-visual-goto-normal-mode)
    (define-key map (kbd "M-c") 'evics-goto-normal-mode)
    (define-key map "n" 'isearch-repeat-forward)
    (define-key map "N" 'isearch-repeat-backward)
    map)
  "Evics visual mode keymap")

(defun evics-visual-goto-normal-mode ()
  "Switch from visual evics mode to evics normal mode"
  (interactive)
  (evics-visual-mode -1)
  (evics-normal-mode t)
  (evics-left-char-same-line)
  (keyboard-quit) ;; Seems to clobber the message call below
  (message "-- NORMAL --"))

(defun evics-visual-kill-line (start end)
  "Kill selected region and save to ring buffer"
  (interactive "r")
  ;; (delete-forward-char 1 t)
  (kill-region start end)
  (evics-goto-normal-mode))

(defvar evics-toggle-transient-visual-callback nil
  "Callback to disable the transient
  evics-toggle-transient-visual-map that we enable when setting
  the mark in a buffer")
(make-variable-buffer-local 'evics-toggle-transient-visual-callback)

(defun evics-visual-pre-command ()
  "Check the current position vs evics--region-position and move
  mark accordingly to emulate vim line mode highlighting"
  (setq evics--previous-line-number (line-number-at-pos)))

(defun evics-visual-post-command ()
  "Check the current position vs evics--region-position and move
  mark accordingly to emulate vim line mode highlighting"
  (if (and (boundp evics-visual-mode)
           evics-visual-mode)
      (let ((line-number (line-number-at-pos)))
        (cond ((<= evics--previous-line-number line-number)
               (cond ((= (+ 1 evics--region-position) line-number)
                      (goto-line evics--region-position)
                      (set-mark (point))
                      (goto-line (+ 1 line-number)))
                     ((< evics--region-position line-number)
                      (goto-line evics--region-position)
                      (set-mark (point))
                      (goto-line line-number))))
              ((and (= evics--region-position line-number)
                    (> evics--previous-line-number evics--region-position))
               (call-interactively 'evics-select-line)
               (forward-line -1))))))

(defun evics-disable-transient-visual-map ()
  "Enable evics visual keybindings when we set or unset the
mark."
  (when evics-toggle-transient-visual-callback
    (funcall evics-toggle-transient-visual-callback)
    (setq evics-toggle-transient-visual-callback nil)))

(defun evics-enable-transient-visual-map ()
  "Disable evics visual keybindings when we unset the mark."
  (if (not evics-toggle-transient-visual-callback)
      (setq evics-toggle-transient-visual-callback
            (set-transient-map
             evics-visual-transient-mode-map
             'evics-keep-pred-cb))))

(add-hook 'pre-command-hook 'evics-visual-pre-command)
(add-hook 'post-command-hook 'evics-visual-post-command)
(add-hook 'activate-mark-hook 'evics-enable-transient-visual-map)
(add-hook 'deactivate-mark-hook 'evics-disable-transient-visual-map)

(define-minor-mode evics-visual-mode
  "Toggle evics visual mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <V>"
  ;; The minor mode bindings.
  :keymap evics-visual-mode-map
  :group 'evics-insert
  (setq cursor-type 'bar)
  (setq line-move-visual (not evics-visual-mode)))

(provide 'evics-visual)
