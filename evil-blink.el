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
(defconst evil-blink--buffer " *evil-blink*"
  "The buffer name for the posframe.")

(defvar evil-blink--timer nil
  "The timer for the posframe.")

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

;; * Minor Mode
;; TODO: Maybe keyword args just to be more readable (:wrap :display)
(defmacro evil-blink--define-wrapper (name wrapped-fn string-fn)
  `(defun ,name ()
     (interactive)
     (setq this-command #',wrapped-fn)
     (unwind-protect
         (progn
           (evil-blink--idle-show (,string-fn))
           (call-interactively #',wrapped-fn))
       (evil-blink--hide))))

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
(progn ; test customizations
  (custom-set-faces
   '(evil-blink-group-name ((t (:inherit font-lock-keyword-face :weight bold))))
   '(evil-blink-entry-name ((t (:inherit font-lock-function-name-face :weight bold)))))
  (set-face-background 'internal-border
                       (face-foreground 'font-lock-comment-face))
  (gsetq evil-blink-register-char-limit 100)
  (gsetq evil-blink-extra-posframe-args
         `(
           ;; NOTE: looks like :background-color only enables the
           ;; internal border if the color is different from the usual
           ;; background.
           :background-color ,(face-background 'mode-line)
           :right-fringe 8
           ;; TODO: dynamic resizing?
           ;; or could we impose a general display limit?
           ;; - evil-blink-entry-description-limit
           :width 80
           :height 20
           :poshandler posframe-poshandler-point-bottom-left-corner
           ;; :internal-border-width 1
           )))

(general-def 'normal
  "q"  'evil-blink-record-macro
  "\"" 'evil-blink-use-register
  "@"  'evil-blink-execute-macro)

(general-def 'insert
  "C-r" 'evil-blink-paste-from-register)

;; (setq evil-blink-register-group-alist (assoc-delete-all "Named" evil-blink-register-group-alist))
;; NOTE: setting the register limit to 100 makes the execution almost instant
;; (gsetq evil-blink-register-char-limit 100)
;; (gsetq evil-blink-register-char-limit 50)
;; even with yanking yasnippet.el 9 times
;; NOTE: should put in readme that I recommend to set the limit to one
;; past the width of the :height posframe parameter, if supplied.
(general-def 'normal
  "SPC SPC" (lambda ()
              (interactive)
              (message "%S"
                       (evil-blink--registers-string))))

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

;; independent format string arg order
(progn
  ;; actually there's that one function
  "[^%]%c" ;; TODO: we should count the number of preceding %. Or replace the double %'s with a single.

  (replace-regexp-in-string "%%" "%" "%%%%%%%%%%%"))
