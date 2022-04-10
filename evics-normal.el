;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;         Normal Mode            ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evics-goto-insert-mode ()
  "Switch from whatever evics mode to insert"
  (interactive)
  (evics-normal-mode -1)
  (evics-insert-mode t)
  (message "-- INSERT --"))

(defun evics-goto-Insert-mode ()
  "Switch from whatever evics mode to insert"
  (interactive)
  (evics-normal-mode -1)
  (evics-insert-mode t)
  (exchange-point-and-mark)
  (message "-- INSERT --"))

(defun evics-kill-ring-save ()
  "Call kill ring save and force us out of visual mode"
  (interactive)
  (call-interactively 'kill-ring-save)
  (evics-visual-mode -1)
  (evics-normal-mode 1))

(defun evics-redo ()
  "If undotree is present call that, else no-op"
  (interactive)
  (if (and (locate-library "undo-tree")
           (boundp 'undo-tree-mode)
           undo-tree-mode)
      (undo-tree-redo)))

(defun evics-join-line ()
  "Join line of text with next one"
  (interactive)
  (save-excursion
    (join-line t)))

(defun evics-backward-WORD ()
  "Skip backwards over a WORD. evics-WORD was defined with
define-thing-chars, and this macro does not seem to assign a
forward op. So this method uses a makeshift forward op."
  (interactive)
  (while
      (not
       (and (thing-at-point 'evics-WORD)
            (and (> (point)
                    (car (bounds-of-thing-at-point 'word)))
                 (<= (point)
                     (cdr (bounds-of-thing-at-point 'word))))))
    (left-char))
  (beginning-of-thing 'evics-WORD))

(defun evics-forward-WORD ()
  "Skip over a WORD. evics-WORD was defined with
define-thing-chars, and this macro does not seem to assign a
forward op. So this method uses a makeshift forward op."
  (interactive)
  (while
      (not
       (and (thing-at-point 'evics-WORD)
            (and (>= (point)
                     (car (bounds-of-thing-at-point 'word)))
                 (< (point)
                    (cdr (bounds-of-thing-at-point 'word))))))
    (right-char))
  (end-of-thing 'evics-WORD))

(defun evics-kill-whole-line ()
  "Kill line of text"
  (interactive)
  (move-beginning-of-line nil)
  (kill-whole-line))

(defun evics-kill-whole-word ()
  "Kill line of text"
  (interactive)
  ;; Hack to prevent us from going to the previous word if we are at
  ;; the start of the word
  (forward-char)
  (backward-word)
  (kill-word 1))

(defun evics-kill-whole-word-insert ()
  "Kill line of text"
  (interactive)
  (forward-char)
  (backward-word)
  (kill-word 1)
  (evics-goto-insert-mode))

(defun evics-kill-word-insert ()
  "Kill line of text"
  (interactive)
  (kill-word 1)
  (evics-goto-insert-mode))

(defun evics--kill-line-or-whitespace ()
  "Kill-line functionality that does not kill through the
newline."
  (if (looking-at-p "[[:blank:]]*$")
      (progn (re-search-forward "[[:blank:]]*")
             (replace-match ""))
    (kill-line)))

(defun evics-kill-whole-line-insert ()
  "Kill line of text"
  (interactive)
  (move-beginning-of-line nil)
  (evics--kill-line-or-whitespace)
  (evics-goto-insert-mode))

(defun evics-kill-line-insert ()
  "Kill line of text"
  (interactive)
  (evics--kill-line-or-whitespace)
  (evics-goto-insert-mode))

(defun evics-newline-above ()
  "Insert newline above and enter evics insert mode"
  (interactive)
  (beginning-of-line)
  ;; Currently calling this interactively so it indents
  (call-interactively 'newline)
  (forward-line -1)
  (evics-goto-insert-mode))

(defun evics-newline-below ()
  "Insert newline below and enter evics insert mode"
  (interactive)
  (move-end-of-line nil)
  ;; Currently calling this interactively so it indents
  (call-interactively 'newline)
  (evics-goto-insert-mode))

(defun evics-append ()
  "Goto end of line and enter insert mode"
  (interactive)
  (forward-char)
  (evics-goto-insert-mode))

(defun evics-append-line ()
  "Goto end of line and enter insert mode"
  (interactive)
  (end-of-line)
  (evics-goto-insert-mode))

(defun evics-replace-char ()
  "Delete character under string and replace it with next the
next character the user types"
  (interactive)
  (let ((char (read-key)))
    (delete-forward-char 1)
    (insert char)
    (backward-char)))

(defun evics-regex-replace ()
  "Check if we have any desirable backend like visual regexp or
anzu installed. If not then invoke regexp-replace"
  (interactive)
  ;; Using cond here instead of if incase in the future we want to add
  ;; another possible backend, perhaps anzu.
  (cond ((require 'visual-regexp nil t) (call-interactively 'vr/replace))
        (t (call-interactively 'replace-regexp))))

(defun evics-esc (map)
  "Catch \\e on TTY and translate to escape if there is no other
action after timeout"
  (if (and (equal (this-single-command-keys) [?\e])
           (sit-for 0.1))
      [escape] map))

(defun evics-init-esc ()
  "If we are in tty then we will have to translate \\e to escape
under certain conditions. This is taken from viper mode."
  (when (terminal-live-p (frame-terminal))
    (let ((default-esc (lookup-key input-decode-map [?\e])))
      (define-key input-decode-map [?\e] `(menu-item "" ,default-esc :filter evics-esc)))))

(defun evics-toggle-kbd-macro ()
  "Start or end kbd macro recording."
  (interactive)
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        (kmacro-to-register evics-normal--kbd-macro-register))
    (progn
      (setq evics-normal--kbd-macro-register (read-char "Register: "))
      (start-kbd-macro nil))))

;(defun evics-command ()
;  "Read command from minibuffer and perform the action specified
;in evics-command-mode-map"
;  (interactive)
;  ;; We can specify a keymap and bind tab to a completion
;  ;; function... look at evil implementation
;  (let ((input (read-from-minibuffer ":"))
;        (command nil))
;    (setq command (car (cdr (assoc input evics-command-mode-alist))))
;    (if command
;        ;; Perhaps switch to funcall in the future
;        (call-interactively command)
;      ((mapc (lambda (x)
;               (call-interactively
;                (car (cdr (assoc (key-description (list x)) evics-command-mode-alist)))))
;             input)))))

(defun evics-command ()
  "Read command from minibuffer and perform the action specified
in evics-command-mode-map"
  (interactive)
  ;; We can specify a keymap and bind tab to a completion
  ;; function... look at evil implementation
  (let ((input (read-from-minibuffer ":"))
        (command nil))
    (setq command (car (cdr (assoc input evics-command-mode-alist))))
    (if command
        ;; Perhaps switch to funcall in the future
        (call-interactively command)
      ((mapc (lambda (x)
               (call-interactively
                (car (cdr (assoc (key-description (list x)) evics-command-mode-alist)))))
             input)))))

;;(defun evics-command ()
;;  "Read command from minibuffer and perform the action specified
;;in evics-command-mode-map"
;;  (interactive)
;;  ;; We can specify a keymap and bind tab to a completion
;;  ;; function... look at evil implementation
;;  (let ((input (read-from-minibuffer ":")))
;;    (mapc (lambda (x)
;;            ;; Want to switch to funcall.. not sure how to handle
;;            ;; find-file
;;            (call-interactively
;;             (lookup-key evics-command-mode-map
;;                         (key-description (list x)))))
;;          input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;         Keymap            ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rect)
(defvar evics-normal-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    
    (define-key map "@" 'jump-to-register)
    (define-key map "$" 'move-end-of-line)
    (define-key map "/" 'isearch-forward)
    (define-key map "?" 'isearch-backward)
    (define-key map "_" 'beginning-of-line-text)
    ;; Kind of a cheat to bind this to keyboard-quit instead of
    ;; keyboard escape quit.
    (define-key map (kbd "<escape>") 'keyboard-quit)
    (define-key map ":" 'evics-command)
    (define-key map ";" 'ignore)
    (define-key map "," 'jump-to-register)
    (define-key map "=" 'indent-region)

    ;; If 0 is pressed without any other digit args before it, then we
    ;; goto the beginning of the line.
    (evics-key-prefix-argument-overload map "0" 'digit-argument 'move-beginning-of-line)
    (define-key map "1" 'digit-argument)
    (define-key map "2" 'digit-argument)
    (define-key map "3" 'digit-argument)
    (define-key map "4" 'digit-argument)
    (define-key map "5" 'digit-argument)
    (define-key map "6" 'digit-argument)
    (define-key map "7" 'digit-argument)
    (define-key map "8" 'digit-argument)
    (define-key map "9" 'digit-argument)

    (define-key map "a" 'evics-append)
    (define-key map "A" 'evics-append-line)
    (define-key map "b" 'backward-word)
    (define-key map "B" 'evics-backward-WORD)
    (define-key map (kbd "c i w") 'evics-kill-whole-word-insert)
    (define-key map (kbd "c w") 'evics-kill-word-insert)
    (define-key map (kbd "c c") 'evics-kill-whole-line-insert)
    (define-key map "C" 'evics-kill-line-insert)
    (define-key map (kbd "d w") 'evics-kill-whole-word)
    (define-key map (kbd "d w") 'kill-word)
    (define-key map (kbd "d d") 'evics-kill-whole-line)
    (define-key map "D" 'kill-line)
    (define-key map "e" 'forward-word)
    (define-key map "E" 'evics-forward-WORD)
    (define-key map (kbd "g g") 'beginning-of-buffer)
    (evics-key-prefix-argument-overload map "G" 'goto-line 'end-of-buffer)
    (define-key map "h" 'left-char)
    (define-key map "H" 'backward-list)
    (define-key map "i" 'evics-goto-insert-mode)
    (define-key map "I" 'evics-goto-Insert-mode)
    (define-key map "j" 'next-line)
    (define-key map "J" 'down-list)
    (define-key map "k" 'previous-line)
    (define-key map "K" 'backward-up-list)
    (define-key map "l" 'right-char)
    (define-key map "L" 'forward-list)
    (define-key map "m" 'point-to-register)
    (define-key map "M" (lambda () (interactive)(print (current-minor-mode-maps))))
    (define-key map "n" 'isearch-repeat-forward)
    (define-key map "N" 'isearch-repeat-backward)
    (define-key map "o" 'evics-newline-below)
    (define-key map "O" 'evics-newline-above)
    (define-key map "p" 'yank) ; Note: We have delete-selection-mode enabled.
    (define-key map "P" 'yank) ; Note: We have delete-selection-mode enabled.
    (define-key map "q" 'evics-toggle-kbd-macro)
    (define-key map "r" 'evics-replace-char)
    (define-key map "u" 'undo)
    (define-key map "v" 'set-mark-command)
    (define-key map "V" 'evics-select-line)
    (define-key map "x" 'delete-forward-char)
    (define-key map "y" 'evics-kill-ring-save)
    (define-key map "z" 'eval-defun)

    ;; Will need to remove undo-tree dependency in the future
    (define-key map (kbd "C-f") 'scroll-up-command)
    (define-key map (kbd "C-j") 'evics-join-line)
    (define-key map (kbd "C-r") 'evics-redo)
    (define-key map (kbd "C-v") 'rectangle-mark-mode)
    (define-key map (kbd "C-=") 'align)
    (define-key map (kbd "C-b") 'scroll-down-command)
    (define-key map (kbd "DEL") 'left-char)
    (define-key map (kbd "RET") 'isearch-exit)
    map)
  "Evics normal mode keymap")

(defvar evics-mini-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map ":" 'evics-command)
    (define-key map "/" 'isearch-forward)
    (define-key map "?" 'isearch-backward)
    (define-key map "$" 'move-end-of-line)
    (define-key map "B" 'evics-backward-WORD)
    (define-key map "b" 'backward-word)
    (define-key map "e" 'forward-word)
    (define-key map "E" 'evics-forward-WORD)
    (define-key map (kbd "g g") 'beginning-of-buffer)
    (define-key map (kbd "G") 'end-of-buffer)
    (define-key map "h" 'left-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'right-char)
    (define-key map "v" 'set-mark-command)
    (define-key map "V" 'evics-select-line)
    (define-key map (kbd "C-f") 'scroll-up-command)
    (define-key map (kbd "C-b") 'scroll-down-command)
    (define-key map (kbd "<escape>") 'keyboard-quit)
    map)
  "Minimal keymap for navigation. This is used to override
  special modes keybindings.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;             commands              ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evics-command-mode-alist
      '(("w" save-buffer)
        ("W" save-buffer)
        ("q" kill-buffer)
        ("Q" save-buffers-kill-terminal)
        ("o" delete-other-windows)
        ("e" find-file)
        ("vsplit" split-window-right)
        ("split" split-window-below)
        ("s" evics-regex-replace)))
(defvar evics-command-mode-map (make-sparse-keymap) "Evics command mode keymap")
(define-key evics-command-mode-map (kbd "w") 'save-buffer)
(define-key evics-command-mode-map (kbd "W") 'save-buffer)
(define-key evics-command-mode-map (kbd "q") 'kill-buffer)
(define-key evics-command-mode-map (kbd "Q") 'save-buffers-kill-terminal)
(define-key evics-command-mode-map (kbd "o") 'delete-other-windows)
(define-key evics-command-mode-map (kbd "e") 'find-file)
(define-key evics-command-mode-map (kbd "s") 'evics-regex-replace)
;; (define-key evics-command-mode-map (kbd "split") 'split-window)

(define-minor-mode evics-normal-mode
  "Toggle evics normal mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <N>"
  ;; The minor mode bindings.
  :keymap evics-normal-mode-map
  :group 'evics-normal
  (setq cursor-type 'box))

(define-minor-mode evics-mini-mode
  "Toggle evics normal mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <N>"
  ;; The minor mode bindings.
  :keymap evics-mini-mode-map
  :group 'evics-mini
  (evics-disable-all-modes)
  (setq cursor-type 'box))


(provide 'evics-normal)
