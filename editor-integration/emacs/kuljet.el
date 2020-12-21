(require 'flycheck)

(defconst kuljet--keywords
  '("serve" "get" "post" "let" "in" "fun" "table" "insert" "delete"
    "then" "as" "limit" "order" "asc" "desc" "select" "where"
    "natJoin" "and" "or" "if" "else"))

(defconst kuljet--font-lock
  `((,(concat "\\_<" (regexp-opt kuljet--keywords) "\\_>") . font-lock-keyword-face)
    ("<[a-z0-9]+>" . font-lock-constant-face)
    (,(concat "\\(let\\|table\\|insert\\|delete\\|as\\)\\s-+\\(" lisp-mode-symbol-regexp "\\)") . ((2 font-lock-variable-name-face)))
    ))

;;;###autoload
(define-derived-mode kuljet-mode prog-mode "Kuljet"
  "Major mode for editing Kuljet files"
  (setq-local compile-command "kuljet check")
  (setq-local font-lock-defaults '(kuljet--font-lock))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("^---$" (0 "!")))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.kj\\'") 'kuljet-mode))

(define-key kuljet-mode-map (kbd "C-c C-c") #'compile)


;;; Checker

(flycheck-define-checker kuljet
  "A checker for Kuljet source files"
  :command ("kuljet" "check" source)
  :modes (kuljet-mode)
  :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message))
     (error line-start (file-name) ":" (message))))

(add-to-list 'flycheck-checkers 'kuljet)
(add-hook 'kuljet-mode-hook 'flycheck-mode)

(provide 'kuljet)
