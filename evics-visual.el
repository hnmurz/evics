(defvar evics-mark-active-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-c") 'evics-visual-goto-normal-mode)
    ;; Using `kill-region' interactively will handle rectangle
    ;; selections nicely
    (define-key map (kbd "x") 'kill-region)
    (define-key map (kbd "d") 'kill-region)
    (define-key map (kbd "<escape>") 'evics-visual-goto-normal-mode)
    map)
  "This keymap will be active whenever the mark is active. Use
this keymap to bind any custom selection functions that you
want to write. Example:

(define-key evics-mark-active-mode-map (kbd \"f\") \'mark-defun)

It is recommended to be wise with whatever keybindings you add
here since they could clobber keybindings in other modes. By
default this map will not have any new behaviour since this would
not be included in vanilla emacs or vim. But, feel free to add
your own custom functions.")

(defun evics-visual-goto-normal-mode ()
  "Switch from visual evics mode to evics normal mode"
  (interactive)
  (evics-visual-mode -1)
  (if (not evics-mini-mode)
      (evics-normal-mode t))
  (keyboard-quit) ;; Seems to clobber the message call below
  (message "-- NORMAL --"))

(defun evics-visual-pre-command ()
  "Check the current position vs `evics--region-position' and move
mark accordingly to emulate vim line mode highlighting"
  (setq evics--previous-line-number (line-number-at-pos)))

(defun evics-visual-post-command ()
  "Check `line-number-at-pos' vs `evics--region-position' and
move mark accordingly to emulate vim line mode highlighting.

When marking just one line, we set the mark below the line and
move the cursor to the current line. This has issues obviously if
we are moving the cursor. So this function runs after every
command, and if we are in visual line mode it will move the
cursor and or the mark accordingly to emulate selecting by whole
lines."
  (if (and (boundp evics-visual-mode)
           evics-visual-mode)
      (let ((line-number (line-number-at-pos)))
        (cond
         ;; This catches the case where we are moving down and are
         ;; wanting to higlight the originally marked line and the
         ;; next line. Remember, when marking the original line, we
         ;; set the mark on the line below then move the cursor up, to
         ;; highlight the current line. If we just move down normally,
         ;; then we would have no selection since point ==
         ;; mark. Therefore, we move the point to the originally
         ;; marked line, set the mark, and then move the point 2 lines
         ;; down, thus selecting the original line and the next one
         ((<= evics--previous-line-number line-number)
          ;; Down movement scenario explained above
          (cond ((= (+ 1 evics--region-position) line-number)
                 (goto-line evics--region-position)
                 (set-mark (point))
                 (goto-line (+ 1 line-number)))
                ;; This captures the scenario where we were moving
                ;; more than 1 line at a time. In this scenario we
                ;; don't move 2 lines down. We could, but we don't,
                ;; since for certain commands this could cause the
                ;; screen to inadvertently move
                ;; i.e. `evics-bottom-of-screen'.
                ((< evics--region-position line-number)
                 (goto-line evics--region-position)
                 (set-mark (point))
                 (goto-line line-number))))
         ;; This captures the scenario where the cursor was past the
         ;; originally selected line (the above comment explains what
         ;; happens). So for this scenario we are just wanting to
         ;; restore the original line selection.
         ((and (= evics--region-position line-number)
               (> evics--previous-line-number evics--region-position))
          (call-interactively 'evics-select-line)
          (forward-line -1))))))

(defun evics-visual-deactivate-mark-hook ()
  "This function is invoked when we deactivate the mark. It's
purpose is to simply disable `evics-visual-mode'"
  (interactive)
  (evics-visual-mode -1)
  (if (not evics-mini-mode)
      (evics-normal-mode t)))

(add-hook 'pre-command-hook 'evics-visual-pre-command)
(add-hook 'post-command-hook 'evics-visual-post-command)
(add-hook 'activate-mark-hook #'(lambda () (setq cursor-type 'bar)))
(add-hook 'deactivate-mark-hook 'evics-visual-deactivate-mark-hook)

(define-minor-mode evics-visual-mode
  "Toggle evics visual mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <V>"
  :keymap nil
  :group 'evics-insert
  (when evics-visual-mode
    (setq cursor-type 'bar))
  (setq line-move-visual (not evics-visual-mode)))

(provide 'evics-visual)
