(defvar evics-insert-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "t") 'self-insert-command)
    (define-key map (kbd "M-c") 'evics-goto-normal-mode)
    (define-key map (kbd "<escape>") 'evics-goto-normal-mode)
    map)
  "Evics visual insert mode overriding keymap. The user can add
their prefix keys with evics-define-prefix key. This is so evics
can handle prefix keys being attached to keybindings that would
interfere with `evics-insert-mode'. If we are not dealing with
prefix keys, the user should simply define their binding with
`evics-define-key'.")

(define-minor-mode evics-insert-mode
  "Toggle evics normal mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <I>"
  :keymap nil
  :group 'evics-insert
  (setq cursor-type 'bar)
  (when (and (boundp 'evics-visual-insert-callback)
             evics-visual-insert-callback)
    (evics-visual-insert-callback)
    (setq evics-visual-insert-callback nil)))

(provide 'evics-insert)
