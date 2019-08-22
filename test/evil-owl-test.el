(require 'ert)
(require 'evil)
(require 'evil-owl)
(require 'evil-test-helpers)

;; NOTE: If the posframe is open and the mouse is over it, then
;; pressing anything that closes the posframe results in the message
;; "Showing all blocks... done" and nothing happens.  This is probably
;; why posframe banishes the mouse by default.

;; Some registers are a bit harder to reset or control for.
(defconst evil-owl--test-register-groups
  `(("Named"     . ,(cl-loop for c from ?a to ?z collect c))
    ("Numbered"  . ,(cl-loop for c from ?0 to ?9 collect c))
    ("Special"   . (?\" ?-))
    ("Read-only" . (?/ ?: ?.)))
  "Register groups used for testing.")

(defmacro evil-owl--with-test-env (&rest body)
  "Execute BODY with certain variables set to appropriate values for testing."
  (declare (indent 0))
  `(let ((evil-owl-register-groups evil-owl--test-register-groups)
         (kill-ring nil)
         (search-ring nil)
         (regexp-search-ring nil)
         (evil-ex-history nil)
         (evil-last-insertion nil)
         (evil-last-small-deletion nil)
         (register-alist nil))
     (cl-letf (((default-value 'evil-markers-alist) (default-value 'evil-markers-alist)))
       ,@body)))

(defun evil-owl--execute-and-check-quit (keys)
  "Execute KEYS and return whether a quit condition was signaled."
  (condition-case nil
      (progn
        (execute-kbd-macro keys)
        nil)
    (quit t)))

(evil-owl-mode)

(ert-deftest evil-owl-test-register-string ()
  "Check register display strings."
  (ert-info ("All registers empty")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[a]sdf\njkl;\nline\nowl\n"
        (should (equal (evil-owl--register-display-string)
                       "Named\n\nNumbered\n\nSpecial\n\nRead-only\n"))
        "[a]sdf\njkl;\nline\nowl\n")))
  (ert-info ("One group with empty registers")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[a]sdf\njkl;\nline\nowl\n"
        ("\"ayy" "j" "\"byy")
        (should (equal (evil-owl--register-display-string)
                       "Named
 a: asdf^J
 b: jkl;^J

Numbered
 1: jkl;^J
 2: asdf^J

Special
 \": jkl;^J

Read-only
"))
        "asdf\n[j]kl;\nline\nowl\n")))
  (ert-info ("At least one non-empty register per group")
    (evil-owl--with-test-env
      (evil-test-buffer
        "asdf\n[j]kl;\nline\nowl\n"
        ("\"ayy" "k" "diw" ":normal" [return] "/line" [return] "\"ryiw")
        (should (equal (evil-owl--register-display-string)
                       "Named
 a: jkl;^J
 r: line

Numbered
 1: line
 2: asdf
 3: jkl;^J

Special
 \": line
 -: asdf

Read-only
 /: line
 :: normal
"))
        "\njkl;\n[l]ine\nowl\n"))))

(ert-deftest evil-owl-test-mark-string ()
  "Check mark display strings."
  (ert-info ("No marks set")
    (evil-owl--with-test-env
      (evil-test-buffer
        ""
        (should (equal (evil-owl--mark-display-string)
                       "Named Local\n\nNamed Global\n\nNumbered\n\nSpecial\n"))
        "")))
  (ert-info ("One group with no marks set")
    (evil-owl--with-test-env
      (evil-test-buffer
        "asdf\nfds[a]\nelisp\n"
        (rename-buffer " *evil-owl-test*")
        ("mB")
        (evil-test-buffer
          "asdf\n[j]kl;\nline\nowl\n"
          ("ma" "j" "iemacs" [return] [return] [escape] "gg2l" "ml")
          (should (equal (evil-owl--mark-display-string)
                         "Named Local
 a: [l: 2    , c: 0    ]
 l: [l: 1    , c: 2    ]

Named Global
 B: [l: 2    , c: 3    ]  *evil-owl-test*

Numbered

Special
 ^: [l: 5    , c: 0    ]
 {: [l: 1    , c: 0    ]
 }: [l: 4    , c: 0    ]
"))
          "as[d]f\njkl;\nemacs\n\nline\nowl\n")
        "asdf\nfds[a]\nelisp\n")))
  (ert-info ("At least one mark set per group")
    (evil-owl--with-test-env
      (evil-test-buffer
        "asdf\nfds[a]\nelisp\n"
        (rename-buffer " *evil-owl-test*")
        ("mB" "G" "mP")
        (evil-test-buffer
          "asdf\n[j]kl;\nline\nowl\n"
          ("ma" "j" "iemacs" [return] [return] [escape] "gg2l" "m0")
          (should (equal (evil-owl--mark-display-string)
                         "Named Local
 a: [l: 2    , c: 0    ]

Named Global
 B: [l: 2    , c: 3    ]  *evil-owl-test*
 P: [l: 4    , c: 0    ]  *evil-owl-test*

Numbered
 0: [l: 1    , c: 2    ]

Special
 ^: [l: 5    , c: 0    ]
 {: [l: 1    , c: 0    ]
 }: [l: 4    , c: 0    ]
"))
          "as[d]f\njkl;\nemacs\n\nline\nowl\n")
        "asdf\nfdsa\nelisp\n[]")))
  (ert-info ("Display context lines")
    (evil-owl--with-test-env
     (let ((evil-owl-local-mark-format " %m: [l: %-5l, c: %-5c]\n    %s")
           (evil-owl-global-mark-format " %m: [l: %-5l, c: %-5c] %b\n    %s"))
       (evil-test-buffer
        "asdf\nfds[a]\nelisp\n"
        (rename-buffer " *evil-owl-test*")
        ("mB" "G" "mP")
        (evil-test-buffer
         "asdf\n[j]kl;\nline\nowl\n"
         ("ma" "j" "iemacs" [return] [return] [escape] "gg2l" "m0")
         (should (equal (evil-owl--mark-display-string)
                        "Named Local
 a: [l: 2    , c: 0    ]
    jkl;

Named Global
 B: [l: 2    , c: 3    ]  *evil-owl-test*
    fdsa
 P: [l: 4    , c: 0    ]  *evil-owl-test*
    

Numbered
 0: [l: 1    , c: 2    ]
    asdf

Special
 ^: [l: 5    , c: 0    ]
    line
 {: [l: 1    , c: 0    ]
    asdf
 }: [l: 4    , c: 0    ]
    
"))
         "as[d]f\njkl;\nemacs\n\nline\nowl\n")
        "asdf\nfdsa\nelisp\n[]")))))

(ert-deftest evil-owl-test-register-commands ()
  "Check that evil-owl's register commands function normally."
  (ert-info ("Keyboard quit")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        (should (evil-owl--execute-and-check-quit "\"\C-g"))
        (should (evil-owl--execute-and-check-quit "i\C-r\C-g"))
        "[t]est\nbuffer\ntext\n")))
  (ert-info ("Without scroll keys")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        ("\"ayy" "2j" "i\C-ra")
        (should (equal "test\n" (evil-get-register ?a)))
        "test\nbuffer\ntest\n[t]ext\n")))
  (ert-info ("With scroll keys")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        ("\"\C-f\C-f\C-fayy" "2j" "i\C-r\C-b\C-ba")
        (should (equal "test\n" (evil-get-register ?a)))
        "test\nbuffer\ntest\n[t]ext\n"))))

(ert-deftest evil-owl-test-macro-commands ()
  "Check that evil-owl's macro commands function normally."
  (ert-info ("Keyboard quit")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        (should (evil-owl--execute-and-check-quit "q\C-g"))
        (should (evil-owl--execute-and-check-quit "@\C-g"))
        "[t]est\nbuffer\ntext\n")))
  (ert-info ("Without scroll keys")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        ;; `execute-kbd-macro' conflicts with `evil-record-macro', so
        ;; we have to resort to `evil-set-register'.  This means we
        ;; can't test `evil-owl-record-macro' effectively.
        (evil-set-register ?q "yyp")
        ("j" "4@q")
        (should (equal (evil-get-register ?q) "yyp"))
        "test\nbuffer\nbuffer\nbuffer\nbuffer\n[b]uffer\ntext\n")))
  (ert-info ("With scroll keys")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        (evil-set-register ?q "yyp")
        ("j" "4@\C-f\C-f\C-b\C-fq")
        (should (equal (evil-get-register ?q) "yyp"))
        "test\nbuffer\nbuffer\nbuffer\nbuffer\n[b]uffer\ntext\n"))))

(ert-deftest evil-owl-test-mark-commands ()
  "Check that evil-owl's mark commands function normally."
  (ert-info ("Keyboard quit")
    (evil-owl--with-test-env
      (evil-test-buffer
        "[t]est\nbuffer\ntext\n"
        (should (evil-owl--execute-and-check-quit "m\C-g"))
        (should (evil-owl--execute-and-check-quit "'\C-g"))
        (should (evil-owl--execute-and-check-quit "`\C-g"))
        "[t]est\nbuffer\ntext\n")))
  (ert-info ("Without scroll keys")
    (evil-owl--with-test-env
      (evil-test-buffer
        "test\nbu[f]fer\ntext\n"
        ("ma" "3G2|" "`a")
        (should (= (evil-get-marker ?a) 8))
        (should (= (point) 8))
        ("G" "'a")
        (should (= (evil-get-marker ?a) 8))
        (should (= (point) 6))
        "test\n[b]uffer\ntext\n")))
  (ert-info ("With scroll keys")
    (evil-owl--with-test-env
      (evil-test-buffer
        "test\nbu[f]fer\ntext\n"
        ("m\C-f\C-fa" "3G2|" "`\C-ba")
        (should (= (evil-get-marker ?a) 8))
        (should (= (point) 8))
        ("G" "'\C-f\C-b\C-fa")
        (should (= (evil-get-marker ?a) 8))
        (should (= (point) 6))
        "test\n[b]uffer\ntext\n"))))
