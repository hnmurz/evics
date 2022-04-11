;; EVICS
;; - Investigate keybindings prio in vc-diffxOB
;; - Make a special keybinding for "(" to enclose brackets around next sexp
;; - When looking at a directory file, return doesnt open the file
;; - BUG: Mark like at top of file "G" then paste, seems to cut out the first line
;; - Make yank (pasting) more like vim
;; - Probably have to change evics-command-mode-map to be an alist instead of keymap to handle
;; string arguments
;; - Show current mode on modeline
;; - highlight under cursor when marking region
;; - maybe use previous-logical-line
;; - For regex replace, see if we can do global replace i.e. s/<pat>/<pat>/g
;; - Record kbd macros with q (see keyboard macro registers in manual)
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

(defvar evics--previous-line-number nil
  "Position of the point before executing the previous
  command. This variable is set in the pre-command-hook.")
(make-variable-buffer-local 'evics--previous-line-number)

(defvar evics-visual-block-callback nil
  "Callback to disable the transient rectangle-mark-mode-map that
  we enable when selecting rectangles in rectangle-mark-mode")
(make-variable-buffer-local 'evics-visual-block-callback)

(defvar evics-use-mini-mode nil
  "Use our mini keybindings to reduce keybinding cloberring in
  specific modes.")

;; See example: evil-redirect-digit-argument
;; also see:    https://stackoverflow.com/questions/29956644/elisp-defmacro-with-lambda
(defmacro evics-key-prefix-argument-overload (map key cb1 cb2)
  "Overload function key with the specified cbs. This is useful 
when we want different behaviour in the lack of prefix args. 
This function will call cb1 if `current-prefix-arg' is defined,
else it will call cb2"
  `(define-key ,map (kbd ,key) 
    '(lambda () (interactive)
       (cond (current-prefix-arg
              (setq this-command ,cb1)
              (call-interactively ,cb1))
             (t
              (setq this-command ,cb2)
              (call-interactively ,cb2))))))

(require 'thingatpt)
(define-thing-chars evics-WORD "[:alnum:]_-")

(defun evics-keep-pred-cb ()
  "Callback to supply to set-transient-map"
  (interactive)
  t)

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
  (evics-normal-mode t)
  (evics-left-char-same-line)
  ;; (keyboard-quit) ;; For now keep this disabled... seems to clobber the message call below
  (message "-- NORMAL --"))

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2010-12/msg01183.html
(defun evics-select-line ()
  "Select whole line, by setting the mark at the start of the line"
  (interactive)
  (setq evics--region-position (line-number-at-pos))
  (forward-line 1)
  (move-beginning-of-line nil)
  (set-mark (point))
  (forward-line -1)
  (evics-visual-mode 1))

(defun evics-esc (map)
  "Catch \\e on TTY and translate to escape if there is no other
action after timeout"
  (if (and (equal (this-single-command-keys) [?\e])
           (sit-for 0.1))
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

(defun evics-disable-all-modes ()
  "Disable all evics modes. This is used for specific
scenearios (help buffers etc), so we don't clobber keybindings
for other minor modes."
  (evics-normal-mode -1)
  (evics-visual-mode -1)
  (evics-insert-mode -1)
  (setq cursor-type 'box))

(defun evics-visual-pre-command ()
  "Check the current position vs evics--region-position and move
  mark accordingly to emulate vim line mode highlighting"
  (setq evics--previous-line-number (line-number-at-pos)))
(add-hook 'pre-command-hook 'evics-visual-pre-command)

(defun evics-visual-post-command ()
  "Check the current position vs evics--region-position and move
  mark accordingly to emulate vim line mode highlighting"
  (if (and (boundp evics-visual-mode)
           evics-visual-mode)
      (let ((line-number (line-number-at-pos)))
        (cond ((and (<= evics--previous-line-number line-number)
                    (= (+ 1 evics--region-position) line-number))
               (progn
                 (goto-line evics--region-position)
                 (set-mark (point))
                 (goto-line (+ 1 line-number))))
              ((and (= evics--region-position line-number)
                    (> evics--previous-line-number evics--region-position))
               (progn
                 (call-interactively 'evics-select-line)
                 (forward-line -1)))))))
(add-hook 'post-command-hook 'evics-visual-post-command)

(defun evics-enable-normal-mode ()
  "Function that will determine if we want to enable evics"
  (if (not (minibufferp (current-buffer)))
      (evics-normal-mode 1)))
(define-globalized-minor-mode evics-global-mode evics-normal-mode evics-enable-normal-mode)
(evics-init-esc)

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

(define-key emacs-lisp-mode-map (kbd "M-t") 'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-<") 'xref-pop-marker-stack)

;; Configure rectangle marking
(defun evics-toggle-transient-rectangle-map ()
  "DOCSTRING"
  (if rectangle-mark-mode
      (progn (setq evics-visual-block-callback
                   (set-transient-map
                    rectangle-mark-mode-map 'evics-keep-pred-cb)))
    (when evics-visual-block-callback
      (funcall evics-visual-block-callback)
      (setq evics-visual-block-callback nil))))
(define-key rectangle-mark-mode-map (kbd "I") 'string-insert-rectangle)
(add-hook 'rectangle-mark-mode-hook 'evics-toggle-transient-rectangle-map)

;; These modes operate on read only buffers, as such, we don't want to
;; clobber their keybindings, so we try and use a minimalist evics
;; keymap that will just have navigation commands enabled. This
;; approach seems a little more pragmatic than dealing with keymap
;; priority.
(add-hook 'special-mode-hook 'evics-mini-mode)
(add-hook 'Info-mode-hook 'evics-mini-mode)
(add-hook 'compilation-mode-hook 'evics-mini-mode)
;; This does not seem to work.. for now I init the escape key
;; conditionaliy when entering evics normal mode
;; (add-to-list 'after-make-frame-functions #'evics-init-esc)

(provide 'evics)
