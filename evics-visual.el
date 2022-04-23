(defvar evics-mark-active-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-c") 'evics-goto-normal-mode)
    ;; Using kill-region interactively will handle rectangle
    ;; selections nicely
    (define-key map (kbd "x") 'kill-region)
    (define-key map (kbd "d") 'kill-region)
    (define-key map (kbd "<escape>") 'evics-visual-goto-normal-mode)
    map)
  "Evics visual transient mode keymap, this keymap will be active
  whenever the mark is active. Use this keymap to bind any custom
  selection functions that you want to write. By default this map
  will not have any new behaviour since this would not be
  included in vanilla emacs or vim. But, feel free to add your
  own custom functions.")

(defun evics-visual-goto-normal-mode ()
  "Switch from visual evics mode to evics normal mode"
  (interactive)
  (evics-visual-mode -1)
  (evics-normal-mode t)
  (evics-left-char-same-line)
  (keyboard-quit) ;; Seems to clobber the message call below
  (message "-- NORMAL --"))

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

(add-hook 'pre-command-hook 'evics-visual-pre-command)
(add-hook 'post-command-hook 'evics-visual-post-command)
(add-hook 'activate-mark-hook '(lambda () (setq cursor-type 'bar)))
(add-hook 'deactivate-mark-hook 'evics-goto-normal-mode)

(define-minor-mode evics-visual-mode
  "Toggle evics visual mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <V>"
  :group 'evics-insert
  (setq cursor-type 'bar)
  (setq line-move-visual (not evics-visual-mode)))

(provide 'evics-visual)
