;; EVICS
;; - Get rid of evics-mini-mode. Just use a variable to key off if we want to use the
;; minikeymap or not.
;; - When pasting over region, mark remains set afterwards, look into fixing evics-yank
;; - Add logic to push mark before calling anything in evics-visual-transient-mode-map
;; Afterwards we can restore the mark.
;; - Make a special keybinding for "(" to enclose brackets around next sexp
;; - When looking at a directory file, return doesnt open the file
;; - Probably have to change evics-command-mode-map to be an alist instead of keymap to handle
;; string arguments
;; - Show current mode on modeline
;; - For regex replace, see if we can do global replace i.e. s/<pat>/<pat>/g
;; - Make evics commands handle prefix values
;;

(defvar evics-normal--kbd-macro-register nil
  "Register to record current macro to when it is done")
(make-variable-buffer-local 'evics-normal--kbd-macro-register)

(defvar evics--region-position nil
  "Position of the start of the region. To properly emulate
selecting lines we need to sometimes skip over 2 lines. This
variable is used in conjunction with evics-previous-line-number
to control this behaviour")
(make-variable-buffer-local 'evics--region-position)

(defvar evics--previous-position nil
  "Position of the point before executing the previous
command. This variable is set in the pre-command-hook.")
(make-variable-buffer-local 'evics--previous-position)

(defvar evics-use-mini-mode nil
  "Use our mini keybindings to reduce keybinding cloberring in
specific modes.")

(defvar evics-esc-timeout 0.01
  "How long to wait before registering an ESC keypress as escape
vs using it as meta. This is only used for emacs in TTY mode.")

;; See example: evil-redirect-digit-argument
;; also see:    https://stackoverflow.com/questions/29956644/elisp-defmacro-with-lambda
(defmacro evics-key-prefix-argument-overload (map key cb1 cb2)
  "Overload function key with the specified cbs. This is useful
when we want different behaviour in the lack of prefix args.
This function will call cb1 if `current-prefix-arg' is defined,
else it will call cb2"
  `(define-key ,map (kbd ,key)
     #'(lambda () (interactive)
         (cond (current-prefix-arg
                (setq this-command ,cb1)
                (call-interactively ,cb1))
               (t
                (setq this-command ,cb2)
                (call-interactively ,cb2))))))

(require 'thingatpt)
(define-thing-chars evics-WORD "[:alnum:]_-")

(defun evics-left-char-same-line ()
  "Only go as far left as column 0"
  (interactive)
  (if (not (= 0 (current-column)))
      (left-char)))

(defun evics-goto-normal-mode ()
  "Switch from whatever evics mode to evics normal mode"
  (interactive)
  (evics-insert-mode -1)
  (evics-visual-mode -1)
  ;; This function is called from `mark-deactivate-hook', so we only
  ;; want to enable `evics-normal-mode' if we are not in mini-mode. A
  ;; specific example of this would be if we use C-s (`helm-occur') in
  ;; a buffer in mini mode, we will go into `evics-normal-mode'
  ;; afterwards
  (when (not evics-mini-mode)
    (evics-normal-mode t))
  ;; We dont want to leave blank lines with whitespace.
  (evics-left-char-same-line)
  (save-excursion
    (move-beginning-of-line nil)
    (evics--kill-line-or-whitespace t))
  ;; (keyboard-quit) ;; For now keep this disabled... seems to clobber the message call below
  (message "-- NORMAL --"))

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2010-12/msg01183.html
(defun evics-select-line ()
  "Select whole line, by setting the mark at the start of the line"
  (interactive)
  (setq evics--region-position (line-number-at-pos))
  ;; We will handle the last line specially since sometimes there is
  ;; no newline at the end of the last line.
  (if (= (line-number-at-pos) (line-number-at-pos (point-max)))
      (progn
        (end-of-line)
        (set-mark (point))
        (beginning-of-line))
    (progn
      (forward-line 1)
      (move-beginning-of-line nil)
      (set-mark (point))
      (forward-line -1)))
  (evics-visual-mode 1))

(defun evics-esc (map)
  "Catch \\e on TTY and translate to escape if there is no other
action after timeout. One may ask, why do we do this for TTY's?
Using `showkey -a' can reveal the answer to us:

showkey -a
<pressing escape key>
^[       27 0033 0x1b

<pressing Meta+x
^[x      27 0033 0x1b
        120 0170 0x78

We can see that the same key sequence appears when we press the
escape button and when we press a meta key
combination. Generally, someone will press escape on it's own, so
using the timeout method is a good heuristic. Both viper and evil
have similar functions, evics got the idea from their
implementations."
  (if (and (equal (this-single-command-keys) [?\e])
           (sit-for evics-esc-timeout))
      [escape] map))

(defun evics-init-esc (&optional frame)
  "If we are in tty then we will have to translate \\e to escape
under certain conditions. This is taken from viper mode."
  (when (terminal-live-p (frame-terminal frame))
    (let ((default-esc (lookup-key input-decode-map [?\e])))
      (define-key input-decode-map [?\e] `(menu-item "" ,default-esc :filter evics-esc)))))

(require 'evics-normal)
(require 'evics-insert)
(require 'evics-visual)

(defun evics-enable-normal-mode ()
  "Determine if we want to enable evics. As of now the only
exclusion is being inside the minibuffer."
  (if (not (minibufferp (current-buffer)))
      (evics-normal-mode 1)))
(define-globalized-minor-mode evics-global-mode evics-normal-mode evics-enable-normal-mode)
(evics-init-esc)

(defun reload-evics ()
  "Forcibly reload evics, this is useful if you are changing
keybindings on the fly and noticing they are not taking effect."
  (interactive)
  (unload-feature 'evics t)
  (unload-feature 'evics-visual t)
  (unload-feature 'evics-normal t)
  (unload-feature 'evics-insert t)
  (require 'evics)
  (evics-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non evics related config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This will cause us to replace selected text when pasting etc...
(delete-selection-mode 1)

;; Making window moving like in vim
(require 'winner)
(define-key winner-mode-map (kbd "C-a h") 'windmove-left)
(define-key winner-mode-map (kbd "C-a l") 'windmove-right)
(define-key winner-mode-map (kbd "C-a j") 'windmove-down)
(define-key winner-mode-map (kbd "C-a k") 'windmove-up)
(define-key winner-mode-map (kbd "C-a c") 'delete-window)
(winner-mode t)

(define-key rectangle-mark-mode-map (kbd "I") 'string-rectangle)
(define-key rectangle-mark-mode-map (kbd "r") 'replace-rectangle)

;; These modes operate on read only buffers, as such, we don't want to
;; clobber their keybindings, so we try and use a minimalist evics
;; keymap that will just have navigation commands enabled. This
;; approach seems a little more pragmatic than dealing with keymap
;; priority.
(add-hook 'special-mode-hook         'evics-mini-mode)
(add-hook 'Info-mode-hook            'evics-mini-mode)
(add-hook 'compilation-mode-hook     'evics-mini-mode)
(add-hook 'diff-mode-hook            'evics-mini-mode)
(add-hook 'debugger-mode-hook        'evics-mini-mode)
(add-hook 'messages-buffer-mode-hook 'evics-mini-mode)
(add-hook 'Man-mode-hook             'evics-mini-mode)
(add-hook 'ediff-mode-hook           'evics-mini-mode)
(add-hook 'edit-abbrevs-mode-hook    'evics-mini-mode)

;; This does not seem to work.. for now I init the escape key
;; conditionaliy when entering evics normal mode
;; (add-to-list 'after-make-frame-functions #'evics-init-esc)


;; Need to find a way to nicely handle defining evics overriding keys
;; for major modes

;; Entries without an order always appear at end
(defvar evics--emulation-maps
  (list
   (cons 'mark-active         evics-mark-active-mode-map)
   (cons 'evics-mini-mode     evics-user-normal-map)
   (cons 'evics-mini-mode     evics-mini-mode-map)
   ;; This approach is easier than maintaining a new evics-user-mode variable.
   (cons 'evics-normal-mode evics-user-normal-map)
   (cons 'evics-normal-mode evics-normal-mode-map)
   ;; (cons 'evics-insert-mode evics-insert-mode-map)
   )
  "List of keymaps that evics is using. The order of the keymaps
is important since it sets the precendence.")
(add-to-ordered-list 'evics--emulation-maps (cons 'rectangle-mark-mode rectangle-mark-mode-map) 0)
(add-to-ordered-list 'evics--emulation-maps (cons 'mark-active evics-mark-active-mode-map) 1)
(add-to-ordered-list 'evics--emulation-maps (cons 'evics-insert-mode evics-insert-mode-map) 2)

(defun evics-define-key (mode key func &optional prefix)
  "Bind FUNC to KEY in MODE's map.

This keybinding will only be available if evics-normal-mode is
enabled. So it is mainly used for keybindings that would
interfere with insert mode. I.e. without any leader keys:
   (kbd \"k\")
If the user is added a key combination with a prefix, they should
just add the key combination normally with `define-key' and
afterwards they can invoke `evics-define-prefix-key' with the
prefix they have used."
  ;; Not gonna deal with defun since it's a macro, and the macro
  ;; expansion was proving to be a little annoying. I'll just call
  ;; fset directly. This will define the evics wrapper for FUNC
  (fset (intern (concat "evics--" (symbol-name func)))
        `(lambda ()
           (interactive)
           (if evics-normal-mode
               (call-interactively ',func)
             (call-interactively 'self-insert-command))))
  (define-key
    (symbol-value (intern-soft (concat (symbol-name mode) "-map")))
    key
    (intern-soft (concat "evics--" (symbol-name func)))))

(defun evics-define-prefix-key (key)
  "If we are using a key without modifiers (i.e. a key that would
conflict with insert mode), then we have to add the prefix key to
insert mode to override the prefix key we've defined. Remember,
`evics-insert-mode-map' is one of evics' high priority maps,
users should not put any keymaps with a higher precedence than
this map."
  (when (not (lookup-key evics-insert-mode-map key))
    (define-key evics-insert-mode-map key 'self-insert-command)))

(defun evics-add-to-emulation-map (arg index)
  "Arg is expected to represent the element form expected for
add-to-ordered-list. See examples in this file for what this form
looks like. This function adds 5 to the supplied index since the
first 5 indices are reserved for evics."
  (setq index (+ index 5))
  (add-to-ordered-list 'evics--emulation-maps arg index))

(add-to-list 'emulation-mode-map-alists 'evics--emulation-maps)

(provide 'evics)
