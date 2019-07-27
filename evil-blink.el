;;; evil-blink.el --- posframe preview for evil's register and mark commands -*- lexical-binding: t -*-

;; Copyright (C) 2019 Daniel Phan

;; Author: Daniel Phan <daniel.phan36@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (evil "1.2.13") (posframe "0.5.0"))
;; Homepage: https://github.com/mamapanda/evil-blink
;; Keywords: emulations, evil, visual

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; TODO: commentary section

;;; Code:

;; * Setup
(require 'cl-lib)
(require 'evil)
(require 'posframe)

(defgroup evil-blink nil
  "Register and mark preview popups."
  :prefix 'evil-blink
  :group 'evil)

;; * User Options
;; ** Variables
(defcustom evil-blink-register-group-alist
  `(("Named"     . ,(cl-loop for c from ?a to ?z collect c))
    ("Numbered"  . ,(cl-loop for c from ?0 to ?9 collect c))
    ("Special"   . (?\" ?* ?+ ?-))
    ("Read-only" . (?% ?# ?/ ?: ?.)))
  "An alist of register group names to registers.
Groups and registers will be displayed in the same order they appear
in this variable."
  :type '(alist :key string :value (repeat character)))

(defcustom evil-blink-header-format "%s\n"
  "The format for a section header."
  :type 'string)

(defcustom evil-blink-entry-format "%s: %s\n"
  "The format for an entry.
There should be two string placeholders: one for the mark or register
and another for the contents."
  :type 'string)

(defcustom evil-blink-separator "\n"
  "The separator string to place between sections."
  :type 'string)

(defcustom evil-blink-extra-posframe-args nil
  "Extra arguments to pass to `posframe-show'."
  :type 'list)

(defcustom evil-blink-idle-delay 1
  "The idle delay, in seconds, before the popup appears."
  :type 'float)

(defcustom evil-blink-register-char-limit nil
  "Maximum number of characters to consider in a string register."
  :type 'integer)

(defcustom evil-blink-lighter " blink"
  "Lighter for `evil-blink-mode'."
  :type 'string)

;; ** Faces
(defface evil-blink-group-name nil
  "The face for group names.")

(defface evil-blink-entry-name nil
  "The face for marks and registers.")

;; * Display Strings
;; ** Common
(defun evil-blink--section-string (group entry-names describe-fn)
  (let ((header (format evil-blink-header-format
                        (propertize group 'face 'evil-blink-group-name)))
        ;; TODO: can we make this cleaner?
        (body (cl-loop for entry-name in entry-names
                       for description = (funcall describe-fn entry-name)
                       when (cl-plusp (length description))
                       concat (format evil-blink-entry-format
                                      (propertize (char-to-string entry-name)
                                                  'face
                                                  'evil-blink-entry-name)
                                      description))))
    (concat header body evil-blink-separator)))

(defun evil-blink--display-string (group-alist describe-fn)
  ;; TODO: this could probably be a lot cleaner with `mapconcat'
  ;; instead of using `string-remove-suffix'
  (string-remove-suffix
   evil-blink-separator
   (cl-loop for (group . entry-names) in group-alist
            concat (evil-blink--section-string group entry-names describe-fn))))

;; ** Registers
(defun evil-blink--get-register (reg)
  "Get the contents of REG as a string."
  (when-let ((contents (evil-get-register reg t)))
    (cond
     ((stringp contents)
      (when (and evil-blink-register-char-limit
                 (> (length contents) evil-blink-register-char-limit))
        (setq contents (substring contents 0 evil-blink-register-char-limit)))
      (replace-regexp-in-string "\n" "^J" contents))
     ((vectorp contents)
      (key-description contents)))))

(defun evil-blink--registers-string ()
  "Obtain the display string for registers."
  (evil-blink--display-string evil-blink-register-group-alist
                              #'evil-blink--get-register))

;; * Posframe
;; ** Show / Hide
(defconst evil-blink--buffer " *evil-blink*"
  "The buffer name for the popup.")

(defvar evil-blink--timer nil
  "The timer for the popup.")

(defun evil-blink--show (string)
  (when (posframe-workable-p)
    (apply #'posframe-show
           evil-blink--buffer
           :string string
           :position (point)
           evil-blink-extra-posframe-args)
    ;; match :reg's behavior
    (posframe-funcall evil-blink--buffer
                      (lambda () (setq-local truncate-lines t)))))

(defun evil-blink--idle-show (string)
  (setq evil-blink--timer
        (run-at-time evil-blink-idle-delay nil #'evil-blink--show string)))

(defun evil-blink--hide ()
  (when evil-blink--timer
    (cancel-timer evil-blink--timer)
    (setq evil-blink--timer nil))
  (posframe-delete evil-blink--buffer))

;; ** Keybindings
(defvar evil-blink-popup-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") #'keyboard-quit)
    (define-key map (kbd "C-g") #'keyboard-quit)
    ;; TODO: these should be wrapped with `posframe-funcall' and `condition-case'
    (define-key map (kbd "C-b") #'evil-blink-scroll-popup-up)
    (define-key map (kbd "C-f") #'evil-blink-scroll-popup-down)
    map)
  "Keymap applied when the popup is active.")

(defun evil-blink-scroll-popup-up ()
  "Scroll the popup up one page."
  (interactive)
  (condition-case nil
      (posframe-funcall evil-blink--buffer #'scroll-down)
    (beginning-of-buffer nil)))

(defun evil-blink-scroll-popup-down ()
  "Scroll the popup down one page."
  (interactive)
  (condition-case nil
      (posframe-funcall evil-blink--buffer #'scroll-up)
    (end-of-buffer nil)))

(defmacro evil-blink--with-popup-map (&rest body)
  "Execute BODY with `evil-blink-popup-map' as the sole keymap."
  (declare (indent 0))
  (let ((current-global-map (gensym "current-global-map")))
    `(let ((overriding-terminal-local-map nil)
           (overriding-local-map evil-blink-popup-map)
           (,current-global-map (current-global-map)))
       (unwind-protect
           (progn
             (use-global-map (make-sparse-keymap))
             ,@body)
         (use-global-map ,current-global-map)))))

;; TODO: probably reorganize this
;; TODO: when register contents is "scroll", there's an error with @
;;       >> execute macro was passing args?
;; TODO: "scroll" failed because `evil-snipe-def' uses `evil-read-key'
;;        this means we can't use `cl-letf', since other commands
;;        might use `evil-read-key' and `read-char' when using `evil-execute-macro'
;; TODO: verify that our thing works with macros
;;       hint: it doesn't
;; NOTE: a possible solution is to obtain the interactive form of the
;;       evil commands and execute just those with `cl-letf'
;;          - but is it possible to obtain an interactive form?
;;       --> `interactive-form'?
;; (interactive-form #'evil-execute-macro)
;; (interactive-form #'evil-backward-arg)
;; TODO: but what it the forms are string codes?
;;       luckily, the relevant evil functions don't use built-in string codes, so we're fine
;; NOTE: could we just grab the interactive form and paste it as the
;; wrapper's interactive form, then somehow use `cl-letf' or something
;; to replace `read-char' and `evil-read-key' calls?
;;        we could inject a `cl-letf' in the form?
;; (advice-eval-interactive-spec (cl-second (interactive-form #'evil-backward-arg)))
;; TODO: check if this conflicts with macros
(defun evil-blink--read-register-or-mark (&rest _)
  "Read a register or mark character.
This function allows executing commands in `evil-blink-popup-map', and
the keys of such commands will not be read."
  (evil-blink--with-popup-map
    (catch 'char
      (while t
        (let* ((keys (read-key-sequence nil t))
               (cmd (key-binding keys)))
          (cond
           (cmd
            (call-interactively cmd))
           ((and (stringp keys) (= (length keys) 1))
            (throw 'char (aref keys 0)))
           (t
            ;; Keys that are represented with vectors and not
            ;; strings (e.g. delete and f3) are not valid registers
            ;; or marks.
            (user-error "%s is undefined" (key-description keys)))))))))

;; * Minor Mode
;; TODO: Maybe keyword args just to be more readable (:wrap :display)
(defmacro evil-blink--define-wrapper (name wrapped-fn string-fn)
  ;; TODO: use `evil-define-command' and copy with evil's command properties
  ;;       - `evil-get-command-properties' `evil-add-command-properties'
  (cl-assert (symbolp name))
  (cl-assert (commandp wrapped-fn))
  (cl-assert (functionp string-fn))
  `(defun ,name ()
     ,(format "Wrapper function for `%s' that shows a posframe preview." wrapped-fn)
     (interactive)
     (setq this-command #',wrapped-fn)
     (apply
      #',wrapped-fn
      (unwind-protect
          (progn
            (evil-blink--idle-show (,string-fn))
            (cl-letf (((symbol-function 'evil-read-key) #'evil-blink--read-register-or-mark)
                      ((symbol-function 'read-char) #'evil-blink--read-register-or-mark))
              (advice-eval-interactive-spec (cl-second (interactive-form #',wrapped-fn)))))
        (evil-blink--hide)))))

(evil-blink--define-wrapper evil-blink-use-register
                            evil-use-register
                            evil-blink--registers-string)

(evil-blink--define-wrapper evil-blink-execute-macro
                            evil-execute-macro
                            evil-blink--registers-string)

(evil-blink--define-wrapper evil-blink-record-macro
                            evil-record-macro
                            evil-blink--registers-string)

(evil-blink--define-wrapper evil-blink-paste-from-register
                            evil-paste-from-register
                            evil-blink--registers-string)

(define-minor-mode evil-blink-mode
  "A minor mode to preview marks and registers before using them."
  :global t
  :lighter evil-blink-lighter
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key*
             'normal map
             [remap evil-record-macro] #'evil-blink-record-macro
             [remap evil-execute-macro] #'evil-blink-execute-macro
             [remap evil-use-register] #'evil-blink-use-register)
            (evil-define-key*
             'insert map
             [remap evil-paste-from-register] #'evil-blink-paste-from-register)
            map))

;; * scratch
(unless t                               ; NOTE: current attempt

  ;; for testing if longer keys work
  (general-def evil-blink-popup-map "C-x x x" (lambda () (interactive) (message "hi")))
  (general-def evil-blink-popup-map "<f3>" (lambda () (interactive) (message "f3")))

  ;; could this be helpful?
  (lookup-key function-key-map (kbd "<return>"))
  (lookup-key function-key-map (kbd "<backspace>"))
  (lookup-key function-key-map (kbd "<delete>")) ;; => [127]
  (lookup-key local-function-key-map (kbd "<return>"))
  (lookup-key local-function-key-map (kbd "<delete>")) ;; => [deletechar]
  (lookup-key local-function-key-map (kbd "<deletechar>")) ;; => nil
  (lookup-key local-function-key-map (kbd "<backspace>"))


  ;; NOTE: despite its name, `evil-read-key' actually returns a character
  ;;       it's main advantage over `read-char' is that it implements a custom keymap,
  ;;       which we don't really need for marks and registers.
  ;; so it looks like the below isn't actually needed

  ;; TODO: do we have to write a replacement for `evil-read-key'?
  ;;       can't we just reuse `evil-blink--read-register-or-mark'?
  ;; `evil-read-key' should just return a character for valid register and mark values anyways.
  (defun evil-blink--read-key-advice (_)
    "Advice for scroll commands to be compatible with `evil-read-key'."
    ;; `evil-read-key' expects all commands in `evil-read-key-map' to
    ;; return keys. Things work out if we recursively call
    ;; `evil-read-key'.
    (evil-read-key))

  ;; TODO: it might be less risky to read the key and then pass it to evil's functions
  ;;       so they aren't interactively called
  ;;       but this is kind of stupid, since some of them implement custom interactive logic
  ;;       that i'd like to replicate.
  ;;       - Also, they each only call `evil-read-key' or `read-char' only once anyways, so
  ;;         there's little chance of conflict.
  (defun evil-blink--read-key ()
    "A drop-in for `evil-read-key' that enables `evil-blink-popup-map'."
    ;; TODO: the blink commands must return keys.
    (let ((evil-read-key-map (copy-keymap evil-read-key-map))))
    ;; TODO: this doesn't iterate through all bindings
    (let (a)
      (map-keymap (lambda (key def) (push key a)) yas-minor-mode-map)
      a)
    ))

;; (unless t
;;   (defvar evil-blink-popup-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "C-b") #'evil-scroll-up)
;;       (define-key map (kbd "C-f") #'evil-scroll-down)
;;       map)
;;     "Keymap applied when the popup is active.")
;;   (defun evil-blink--use-popup-map ()
;;     "Execute key sequences in `evil-blink-popup-map'.
;; This function terminates when a key is not in the map, pushing said
;; key into `unread-command-events'."
;;     (let ((overriding-terminal-local-map nil)
;;           (overriding-local-map evil-blink-popup-map)
;;           (current-global-map (current-global-map))
;;           keys
;;           cmd)
;;       (unwind-protect
;;           (progn ; TODO: is there a less hacky way to do this?
;;             (use-global-map (make-sparse-keymap))
;;             (setq keys (read-key-sequence nil nil t))
;;             (while (setq cmd (key-binding keys))
;;               (call-interactively cmd)
;;               (setq keys (read-key-sequence nil nil t)))
;;             (setq unread-command-events
;;                   (nconc (listify-key-sequence keys) unread-command-events)))
;;         (use-global-map current-global-map))))

;;   ;; NOTE: `read-key-sequence' always returns the string form?
;;   ;;      actually, we can just use the same trick as evil maybe

;;   ;; TODO: if we give up, we can do the same thing as before, with the
;;   ;; hacked `evil-read-key-map', and we can also have our own custom `read-char' function.
;;   ;; - the above can be modified to be the new `read-char' function
;;   ;; - we'd have two functions to `cl-letf' in that case
;;   ;;   but `cl-letf'ing `read-char' seems like such an unnecessary hack though
;;   ;;   - NOTE BUT our argument could be that we're "advising" the input functions
;;   ;;          to allowing scrolling before the input is actually read
;;   ;; - let's take advantage of the fact that the relevant evil functions show no prompt
;;   (advice-add #'read-char :after (lambda (&rest _) (message "hi") (sleep-for 1)))
;;   (read-char)

;;   ;; TODO: then we need to use `discard-input' to clear unknown keys maybe?
;;   ;;       - If the key sequence is longer than 1 key, there's no
;;   ;;         guarantee that evil will expect only one key. For
;;   ;;         example, the user might bind a multi-sequence key in
;;   ;;         `evil-read-key-map'.
;;   ;;         - so we should probably clear it AFTER evil's functions execute
;;   ;;           - none of the relevant evil functions will leave unread keys lying around

;;   ;; TODO: do we need to add this into the function?
;;   ;; (mapc 'store-kbd-macro-event char-or-events)
;;   ;; see `isearch-unread'

;;   (defun test ()
;;     (evil-blink--use-popup-map)
;;     ;; TODO: seems like it works as long as it's in the same function call?
;;     ;;       but an extra "l" is added to the macro. see :reg
;;     (setq unread-command-events (nconc (listify-key-sequence "l") unread-command-events))
;;     (insert "%s" (read-char)))
;;   (test) ; qq SPC ; , e l => the macro does an extra l
;;   ;;     is it possible that setting unread-command-events adds to the macro?
;;   ;;          it does add it
;;   (progn
;;     (setq unread-command-events (nconc (listify-key-sequence "l") unread-command-events))
;;     (message "%s" (read-char))
;;     (setq unread-command-events (nconc (listify-key-sequence "l") unread-command-events))
;;     (message "%s" (read-char))
;;     (setq unread-command-events (nconc (listify-key-sequence "l") unread-command-events))
;;     (message "%s" (read-char))
;;     (setq unread-command-events (nconc (listify-key-sequence "l") unread-command-events))
;;     (message "%s" (read-char))
;;     (setq unread-command-events (nconc (listify-key-sequence "l") unread-command-events))
;;     (message "%s" (read-char))
;;     ;; (message "%s" (read-key-sequence ""))
;;     ;; TODO: see SPC ; , e l on this snippet. it gives a weird error when recording a macro
;;     ;; instead of messaging, the key is actually executed
;;     ;;     looks like the (message "%s" (read-char)) is skipped in the macro
;;     ;; putting an (insert (read-char)) into the defun, it seems that the key will be read in the function
;;     ;; it seems unread-command-events doesn't work with macros? or at least there's something weird
;;     ;;    - there might be an extra key being added
;;     ;; TODO: looks like the unread-command-events are added to the kbd macro
;;     ;; TODO: do NOT use `discard-input', since it'll cancel any kbd macros being recorded
;;     ;;       we could (setq unread-command-events old-unread-command-events) or something instead
;;     )
;;   )

;; (unless t
;;   (defvar evil-blink-popup-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "C-b") #'evil-scroll-up)
;;       (define-key map (kbd "C-f") #'evil-scroll-down)
;;       map)
;;     "Keymap applied when the popup is active.")
;;   ;; We need to be wary of any extra keypresses generated that won't
;;   ;; be read by evil, such as `evil-execute-macro'.
;;   (defun evil-blink--use-popup-map ()
;;     "Execute key sequences in `evil-blink-popup-map'.
;; This function terminates when a key is not in the map."
;;     ;; TODO: will this work with `evil-execute-macro'? It uses `read-char'.
;;     ;;       we might be able to use `discard-input'
;;     (cl-loop for input = (read-key-sequence "") ;; TODO: fails if key isn't in the map
;;              for cmd = (lookup-key-ignore-too-long evil-blink-popup-map input)
;;              while cmd
;;              do (call-interactively cmd)
;;              finally (setq unread-command-events
;;                            (nconc (listify-key-sequence input)
;;                                   unread-command-events))))
;;   (progn
;;     (evil-blink--use-popup-map)
;;     ;; TODO: fails. compare SPC ; , e with l
;;     ;; TODO: why does it work all of a fucking sudden?
;;     (message "%s" (read-char))
;;     ;; TODO: do we actually need this?
;;     (discard-input))

;;   ;; NOTE: this is pretty much what we want. `read-key-sequence' still reads nonexistent keys
;;   (let ((overriding-terminal-local-map nil)
;;         (overriding-local-map evil-blink-popup-map)
;;         (old-global (current-global-map))
;;         keys)
;;     (use-global-map (make-sparse-keymap))
;;     (setq keys (read-key-sequence ""))
;;     (use-global-map old-global)
;;     keys)

;;   ;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;;   ;; TODO: we could
;;   ;; - `let' `overriding-local-map' be `evil-blink-popup-map'
;;   ;; - then use a similar technique to `evil-read-key' to read sequences
;;   ;; - maybe clear any keys lying around with `discard-input'... but
;;   ;;   will this conflict with `evil-execute-macro'?
;;   ;;       - NOTE: in case of typos like C-x f insteaad of C-x d
;;   ;;       - I think not, but we can still check to be sure
;;   ;; but we should figure out how to push to unread-command-events still
;;   ;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;;   (read-key-sequence "")
;;   ;; TODO: a problem is that if we don't disable other maps, we'll read extra keys
;;   ;;       for example, maybe the user wants to use the SPC register, but we read SPC ; , e
;;   )

;; (unless t
;;   (defcustom evil-blink-scroll-down-key (kbd "C-f")
;;     "The key to scroll the popup buffer down."
;;     :type 'key-sequence)

;;   (defcustom evil-blink-scroll-up-key (kbd "C-b")
;;     "The key to scroll the popup buffer up."
;;     :type 'key-sequence)

;;   (defun evil-blink--read-key (&optional prompt)
;;     (let ((evil-read-key-map (copy-keymap evil-read-key-map)))
;;       ;; `evil-read-key' expects `evil-read-key-map' to contain
;;       ;; commands that return key sequences, so we need to call
;;       ;; `evil-read-key' again inside the scroll commands.
;;       (define-key evil-read-key-map evil-blink-scroll-down-key
;;         (lambda ()
;;           (interactive)
;;           (condition-case nil
;;               (scroll-up)
;;             (beginning-of-buffer nil))
;;           (evil-read-key prompt)))
;;       (define-key evil-read-key-map evil-blink-scroll-up-key
;;         (lambda ()
;;           (interactive)
;;           (condition-case nil
;;               (scroll-down)
;;             (end-of-buffer nil))
;;           (evil-read-key prompt)))
;;       (evil-read-key)))
;;   ;; then we can just put a cl-letf in the generator macro
;;   (evil-blink--read-key)
;;   )

(progn ; test customizations
  (custom-set-faces
   '(evil-blink-group-name ((t (:inherit font-lock-function-name-face :weight bold :underline t))))
   '(evil-blink-entry-name ((t (:inherit font-lock-function-name-face)))))
  (set-face-background 'internal-border
                       (face-foreground 'font-lock-comment-face))
  (gsetq evil-blink-register-char-limit 100
         evil-blink-idle-delay 0.1)
  (gsetq evil-blink-extra-posframe-args
         `(
           :background-color ,(face-background 'mode-line)
           :right-fringe 8
           :width 50
           :height 20
           :poshandler posframe-poshandler-point-bottom-left-corner
           :internal-border-width 1
           )))

(unless t
  (progn
    (when (posframe-workable-p)
      (posframe-show evil-blink--buffer
                     :string (evil-blink--registers-string)
                     :position (point)
                     :poshandler #'posframe-poshandler-point-bottom-left-corner
                     :right-fringe 10
                     :width 50
                     :height 10
                     :internal-border-width 1
                     ;; NOTE: seems like the border doesn't update for
                     ;; me unless the background color is provided
                     :background-color (face-attribute 'mode-line :background nil t)
                     ))
    )
  )


;; NOTE: this could be the default format if format-spec allows it
;; if they want a header, they can just add it with the header string spec
;;         - so then, should we provide a different header format and
;;           separator for both registers and marks?
(format "%s%-8d%-8d%s" "Header:\nline    column  buffer\n" 1 1 (buffer-name))
(format "l: %-5d c: %-5d in %s" 1 1 (buffer-name))

(let* ((mark (char-to-string ?a))
       (line 10)
       (column 10)
       (buffer (current-buffer))
       (spec (format-spec-make ?m mark ?l line ?c column ?b buffer)))
  ;; (format-spec "%m: l=%-5l c=%-5c" spec)
  ;; (format-spec "%m: l=%-5l c=%-5c in %b" spec)
  (format-spec "%m: [l: %-5l, c: %-5c]" spec)
  (format-spec "%m: [l: %-5l, c: %-5c] %b" spec))
