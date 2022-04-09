(defvar evics-insert-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "M-c") 'evics-goto-normal-mode)
    (define-key map (kbd "<escape>") 'evics-goto-normal-mode)
    map)
  "Evics visual insert mode overriding keymap")

(define-minor-mode evics-insert-mode
  "Toggle evics normal mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <I>"
  ;; The minor mode bindings.
  :keymap evics-insert-mode-map
  :group 'evics-insert
  (setq cursor-type 'bar)
  (when (and (boundp 'evics-visual-insert-callback)
             evics-visual-insert-callback)
    (evics-visual-insert-callback)
    (setq evics-visual-insert-callback nil)))

(provide 'evics-insert)
