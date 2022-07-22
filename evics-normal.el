;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;         Normal Mode            ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evics-goto-insert-mode ()
  "Switch from whatever evics mode to insert"
  (interactive)
  (evics-normal-mode -1)
  (evics-insert-mode t)
  (message "-- INSERT --"))

(defun evics-yank ()
  "We have to disable evics-visual-mode here. We can't use the
`deactivate-mark-hook' since this will be called after the mark
is disabled which seems to happen after `post-command-hook'. So
`post-command-hook' will move the cursor."
  (interactive)
  ;; Note: We have delete-selection-mode enabled.
  (evics-visual-mode -1)
  (delete-selection-mode t)
  (delete-selection-helper 'yank)
  (evics-normal-mode t))

(defun evics-goto-Insert-mode ()
  "Switch from whatever evics mode to insert"
  (interactive)
  (evics-normal-mode -1)
  (evics-insert-mode t)
  (exchange-point-and-mark)
  (message "-- INSERT --"))

;; If you are curious about how this works, read about "Syntax Table
;; Internals" in the emacs manual
(defun evics-goto-matching-paren ()
  "Go to the matching paren, in this case meaning end or
beginning of sexp"
  (interactive)
  (let ((pos (point)))
    (cond ((eq (syntax-class (syntax-after pos)) 4)
           (forward-sexp))
          ((eq (syntax-class (syntax-after (- pos 1))) 5)
           (backward-sexp))
          ((eq (syntax-class (syntax-after pos)) 5)
           (right-char)
           (backward-sexp)))))

(defun evics-scroll-up-line ()
  "DOCSTRING"
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (scroll-up 1)))

(defun evics-scroll-down-line ()
  "DOCSTRING"
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (scroll-down 1)))

(defun evics-kill-ring-save ()
  "Call kill ring save and force us out of visual mode"
  (interactive)
  (exchange-point-and-mark)
  (call-interactively 'kill-ring-save)
  (evics-visual-mode -1)
  (if (not evics-mini-mode)
      (evics-normal-mode 1)))

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
`define-thing-chars', and this macro does not seem to assign a
forward op. So this method uses a makeshift forward op."
  (interactive)
  (while
      (not
       (and (thing-at-point 'evics-WORD)
            (car (bounds-of-thing-at-point 'word))
            (and (> (point)
                    (car (bounds-of-thing-at-point 'word)))
                 (<= (point)
                     (cdr (bounds-of-thing-at-point 'word))))))
    (left-char))
  (beginning-of-thing 'evics-WORD))

(defun evics-forward-WORD ()
  "Skip over a WORD. evics-WORD was defined with
`define-thing-chars', and this macro does not seem to assign a
forward op. So this method uses a makeshift forward op."
  (interactive)
  (while
      (not
       (and (thing-at-point 'evics-WORD)
            (car (bounds-of-thing-at-point 'word))
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
  ;;
  ;; In the future I should update this function to use
  ;; bounds-of-thing-at-point
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

(defun evics-middle-of-screen ()
  "DOCSTRING"
  (interactive)
  (move-to-window-line 'nil))

(defun evics-bottom-of-screen ()
  "DOCSTRING"
  (interactive)
  (move-to-window-line -1))

(defun evics-top-of-screen ()
  "DOCSTRING"
  (interactive)
  (move-to-window-line 0))

(defun evics--kill-line-or-whitespace (&optional dont-kill)
  "Kill-line functionality that does not kill through the
newline."
  (if (looking-at-p "[[:blank:]]*$")
      (progn (re-search-forward "[[:blank:]]*")
             (replace-match ""))
    (if (not dont-kill)
        (kill-line))))

(defun evics-kill-whole-line-insert ()
  "Kill line of text"
  (interactive)
  (move-beginning-of-line nil)
  (evics--kill-line-or-whitespace)
  (funcall indent-line-function)
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
anzu installed. If not then invoke `regexp-replace'"
  (interactive)
  ;; Using cond here instead of if incase in the future we want to add
  ;; another possible backend, perhaps anzu.
  (cond ((require 'visual-regexp nil t) (call-interactively 'vr/replace))
        (t (call-interactively 'replace-regexp))))

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

(defun evics-disable-all-modes ()
  "Disable all evics modes. This is used for specific
scenearios (help buffers etc), so we don't clobber keybindings
for other minor modes."
  (evics-normal-mode -1)
  (evics-visual-mode -1)
  (evics-insert-mode -1)
  (setq cursor-type 'box))

(defun evics-command ()
  "Read command from minibuffer and perform the action specified
in evics-command-mode-map"
  (interactive)
  ;; We can specify a keymap and bind tab to a completion
  ;; function... look at evil implementation
  (let* ((input (read-from-minibuffer ":"))
         (input-symbol (read input))
         (command nil))
    (setq command (cadr (assoc input evics-command-mode-alist)))
    (cond (command
           (call-interactively command))
          ((functionp input-symbol)
           (call-interactively input-symbol))
          (t
           (message (concat "Unknown command: " input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;         Keymaps           ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar evics-user-normal-map (make-keymap)
  "Since we have a mini and normal keymap. Defining keys in
evics-normal-mode-map doesnt necessarily carry over to buffers
that are only using the mini mode. To remedy this, please add
keybindings to this keymap.

In the future, the plan is to have some scheme to easily allow
per minor mode overrides. But, perhaps just increasing the
priority of these minor mode maps is the better solution. The
only issue with this is that these defined minor modes have a lot
of unused default keybindings that might clobber evics bindings.")

(require 'rect)
(defvar evics-mini-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map ":" 'evics-command)
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
    (define-key map "n" 'isearch-repeat-forward)
    (define-key map "N" 'isearch-repeat-backward)
    (define-key map "y" 'evics-kill-ring-save)
    (define-key map "v" 'set-mark-command)
    (define-key map "V" 'evics-select-line)
    (define-key map (kbd "C-b") 'scroll-down-command)
    (define-key map (kbd "C-e") 'evics-scroll-up-line)
    (define-key map (kbd "C-f") 'scroll-up-command)
    (define-key map (kbd "C-y") 'evics-scroll-down-line)
    (define-key map (kbd "<escape>") 'keyboard-quit)
    (define-key map (kbd "<down-mouse-1>") 'push-button)
    (define-key map (kbd "<drag-mouse-1>") 'nil)
    map)
  "Minimal keymap for navigation. This is used to override
  special modes keybindings.")

(defvar evics-normal-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map evics-mini-mode-map)

    (define-key map "@" 'jump-to-register)
    (define-key map "%" 'evics-goto-matching-paren)
    (define-key map "_" 'beginning-of-line-text)
    ;; Kind of a cheat to bind this to keyboard-quit instead of
    ;; keyboard escape quit.
    (define-key map (kbd "<escape>") 'keyboard-quit)
    (define-key map ";" 'ignore)
    (define-key map "#" 'ignore)
    (define-key map "'" 'jump-to-register)
    (define-key map "=" 'indent-region)
    (define-key map "/" 'isearch-forward)
    (define-key map "?" 'isearch-backward)

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
    (define-key map (kbd "c i w") 'evics-kill-whole-word-insert)
    (define-key map (kbd "c w") 'evics-kill-word-insert)
    (define-key map (kbd "c c") 'evics-kill-whole-line-insert)
    (define-key map "C" 'evics-kill-line-insert)
    (define-key map (kbd "d w") 'evics-kill-whole-word)
    (define-key map (kbd "d w") 'kill-word)
    (define-key map (kbd "d d") 'evics-kill-whole-line)
    (define-key map "D" 'kill-line)
    (evics-key-prefix-argument-overload map "G" 'goto-line 'end-of-buffer)
    (define-key map "H" 'evics-top-of-screen)
    (define-key map "i" 'evics-goto-insert-mode)
    (define-key map "I" 'evics-goto-Insert-mode)
    (define-key map "J" 'evics-join-line)
    (define-key map "K" 'man)
    (define-key map "L" 'evics-bottom-of-screen)
    (define-key map "m" 'point-to-register)
    (define-key map "M" 'evics-middle-of-screen)
    (define-key map "o" 'evics-newline-below)
    (define-key map "O" 'evics-newline-above)
    (define-key map "p" 'yank)  ;; Note: We have delete-selection-mode enabled.
    (define-key map "P" 'yank)
    (define-key map "q" 'evics-toggle-kbd-macro)
    (define-key map "r" 'evics-replace-char)
    (define-key map "u" 'undo)
    (define-key map "x" 'delete-forward-char)
    (define-key map "z" 'eval-defun)

    ;; Will need to remove undo-tree dependency in the future
    (define-key map (kbd "C-r") 'evics-redo)
    (define-key map (kbd "C-v") 'rectangle-mark-mode)
    (define-key map (kbd "C-=") 'align)
    (define-key map (kbd "DEL") 'left-char)
    (define-key map (kbd "RET") 'isearch-exit)
    map)
  "Evics normal mode keymap")

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

(define-minor-mode evics-normal-mode
  "Toggle evics normal mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <N>"
  :keymap nil
  :group 'evics-normal
  (setq cursor-type 'box)
  (evics-init-esc)
  (when (and evics-mini-mode evics-normal-mode)
    (evics-normal-mode -1)))

(define-minor-mode evics-mini-mode
  "Toggle evics normal mode."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " <M>"
  :keymap nil
  :group 'evics-mini
  (evics-disable-all-modes)
  (setq cursor-type 'box))


(provide 'evics-normal)

