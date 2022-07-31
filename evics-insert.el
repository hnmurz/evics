(defvar evics-insert-mode-map
  (let ((map (make-keymap)))
    ;; Cannot inherit global map since we now override any insert mode
    ;; modifications i.e. in c-mode ; will cause line to indent
    ;; etc. Correct solution is to define a new variable for each
    ;; minor mode we have to keep track of.
    (define-key map (kbd "M-c") 'evics-goto-normal-mode)
    (define-key map (kbd "<escape>") 'evics-goto-normal-mode)

    (define-key map (kbd "q") 'self-insert-command)
    (define-key map (kbd "w") 'self-insert-command)
    (define-key map (kbd "e") 'self-insert-command)
    (define-key map (kbd "r") 'self-insert-command)
    (define-key map (kbd "t") 'self-insert-command)
    (define-key map (kbd "y") 'self-insert-command)
    (define-key map (kbd "u") 'self-insert-command)
    (define-key map (kbd "i") 'self-insert-command)
    (define-key map (kbd "o") 'self-insert-command)
    (define-key map (kbd "p") 'self-insert-command)
    (define-key map (kbd "[") 'self-insert-command)
    (define-key map (kbd "]") 'self-insert-command)
    (define-key map (kbd "\\") 'self-insert-command)

    (define-key map (kbd "`") 'self-insert-command)
    (define-key map (kbd "1") 'self-insert-command)
    (define-key map (kbd "2") 'self-insert-command)
    (define-key map (kbd "3") 'self-insert-command)
    (define-key map (kbd "4") 'self-insert-command)
    (define-key map (kbd "5") 'self-insert-command)
    (define-key map (kbd "6") 'self-insert-command)
    (define-key map (kbd "7") 'self-insert-command)
    (define-key map (kbd "8") 'self-insert-command)
    (define-key map (kbd "9") 'self-insert-command)
    (define-key map (kbd "0") 'self-insert-command)
    (define-key map (kbd "-") 'self-insert-command)
    (define-key map (kbd "=") 'self-insert-command)

    (define-key map (kbd "z") 'self-insert-command)
    (define-key map (kbd "x") 'self-insert-command)
    (define-key map (kbd "c") 'self-insert-command)
    (define-key map (kbd "v") 'self-insert-command)
    (define-key map (kbd "b") 'self-insert-command)
    (define-key map (kbd "n") 'self-insert-command)
    (define-key map (kbd "m") 'self-insert-command)
    (define-key map (kbd ",") 'self-insert-command)
    (define-key map (kbd ".") 'self-insert-command)
    (define-key map (kbd "/") 'self-insert-command)
    map)
  "Evics visual insert mode overriding keymap")

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
