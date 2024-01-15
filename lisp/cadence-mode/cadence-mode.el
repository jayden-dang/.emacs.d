;;; cadence-mode.el --- A major-mode for editing Cadence language -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Eamon Dang

;; Author: Eamon Dang
;; URL: https://github.com/eamondang/cadence-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; This package implements a major-mode for editing smart contracts on Flow Protocol
;; written in Cadence.

;;; Code:



(require 'compile)

;;; Constants for use with Customization =================================== ;;;

(defconst cadence-core-builtin-functions
  '("assert!" "borrow_global" "freeze" "cadence_to" "singer")
  "Built-in functions from Core Cadence.")

;;; Customization ========================================================== ;;;

(defgroup cadence-lang nil
  "Support for Cadence source code."
  :link '(url-link "https://github.com/onflow/cadence")
  :group 'languages)

(defcustom cadence-indent-offset 2
  "Number of spaces to indent cadence code by."
  :type  'integer
  :group 'rust-mode
  :safe #'integerp)

(defcustom cadence-builtins cadence-core-builtin-functions
  "Functions to highlight as builtins (mutations require restarting font-lock)."
  :type '(list string)
  :group 'cadence-lang)

(defcustom cadence-bin "cadence"
  "Name of or path to cadence CLI binary."
  :type 'string
  :group 'cadence-lang)

(defcustom cadence-default-arguments ""
  "Default arguments when running common cadence CLI commands."
  :type 'string
  :group 'cadence-lang)

;;; Faces ================================================================== ;;;

(defface cadence-compilation-message-face
  '((t :inherit default))
  "`cadence-compilation-mode'-specific override of `compilation-message-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'cadence-lang)

(defface cadence-compilation-error-face
  '((t :inherit default))
  "`cadence-compilation-mode'-specific override of `compilation-error-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'cadence-lang)

(defface cadence-compilation-warning-face
  '((t :inherit default))
  "`cadence-compilation-mode'-specific override of `compilation-warning-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'cadence-lang)

(defface cadence-compilation-line-face
  '((t :inherit default))
  "`cadence-compilation-mode'-specific override of `compilation-line-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'cadence-lang)

(defface cadence-compilation-column-face
  '((t :inherit default))
  "`cadence-compilation-mode'-specific override of `compilation-column-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'cadence-lang)

;;; Syntax ================================================================= ;;;

(defconst cadence-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (op '(?+ ?- ?* ?/ ?% ?& ?^ ?| ?< ?> ?! ?&))
      (modify-syntax-entry op "." table))

    ;; Parentheses
    (modify-syntax-entry ?\(  "()" table)
    (modify-syntax-entry ?\)  ")(" table)
    (modify-syntax-entry ?\{  "(}" table)
    (modify-syntax-entry ?\}  "){" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    ;; Comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23n"  table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)

    table))

(defconst cadence-mode-syntax-table+<>
  (let ((table (copy-syntax-table cadence-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    table)
  "Variant of syntax table recognising angle braces as bracketed.

For use in detecting generic paramters.")

;;; Keybindings ============================================================ ;;;

;; (defvar cadence-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-c C-b") #'cadence-build)
;;     (define-key map (kbd "C-c C-c C-d") #'cadence-disassemble)
;;     (define-key map (kbd "C-c C-c C-p") #'cadence-prover)
;;     (define-key map (kbd "C-c C-c C-t") #'cadence-test)
;;     map))

;;; Compilation ============================================================ ;;;

(defvar cadence-error-pattern
  (let* ((err  "error\\[E[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat err "\n" box file ":" line ":" col)))
    (list patt #'cadence--expand-compilation-source 2 3 0))
  "Link to sources for compilation errors.")

(defvar cadence-warning-pattern
  (let* ((warn "warning\\[W[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat warn "\n" box file ":" line ":" col)))
    (list patt #'cadence--expand-compilation-source 2 3 1))
  "Link to sources for compilation warnings.")

;;; Modes ================================================================== ;;;

;;;###autoload
(define-derived-mode cadence-mode prog-mode "Cadence"
  "Major mode for Cadence source code.

\\{cadence-mode-map}"
  :group 'cadence-lang
  :syntax-table cadence-mode-syntax-table

  (setq-local font-lock-defaults
              '(cadence-mode-font-lock-keywords
                nil ;; KEYWORDS-ONLY
                nil ;; CASE-FOLD
                nil ;; SYNTAX-ALIST
                ;;;;;; VARIABLES
                (font-lock-syntactic-face-function
                 . cadence-mode-distinguish-comments)))

  ;; ! is punctuation unless it's at the end of a word, in which case,
  ;; it should be treated like piece of the preceding word.
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("\\sw\\(!\\)" (1 "w"))))

  ;; This is a weirdly lisp-specific variable
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Indentation
  (setq-local indent-line-function #'cadence-mode-indent-line)
  ;; (setq-local electric-indent-chars
  ;;             (cons ?} (and (boundp 'electric-indent-chars)
  ;;                           electric-indent-chars)))

  ;; Comments
  (setq-local comment-end        "")
  (setq-local comment-line-break-function #'cadence-mode-comment-line-break)
  (setq-local comment-multi-line t)
  (setq-local comment-start      "// ")
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)\\s-*")

  ;; Set paragraph-start to stop multi-line comments creeping up onto
  ;; an empty initial line, or across lines that contain only a `*'
  ;; and are therefore effectively empty.
  (setq-local paragraph-start
              (concat "\\s-*\\(?:" comment-start-skip ;; Comment start
                      "\\|\\*/?[[:space:]]*"          ;; Empty line in a comment
                      "\\|\\)$"))                     ;; Just plain empty
  (setq-local paragraph-separate
              paragraph-start)

  ;; Fill Paragraph
  (setq-local fill-paragraph-function   #'cadence-mode-fill-paragraph)
  (setq-local normal-auto-fill-function #'cadence-mode-auto-fill)
  (setq-local adaptive-fill-function    #'cadence-mode-adaptive-fill)
  (setq-local adaptive-fill-first-line-regexp ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cdc\\'" . cadence-mode))

(define-compilation-mode cadence-compilation-mode "cadence-compilation"
  "Cadence compilation mode.

Defines regexps for matching file names in compiler output, replacing defaults."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'cadence-error cadence-error-pattern))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'cadence-warning cadence-warning-pattern))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'cadence-error)
  (add-to-list 'compilation-error-regexp-alist 'cadence-warning)

  (setq-local compilation-message-face 'cadence-compilation-message-face)
  (setq-local compilation-error-face   'cadence-compilation-error-face)
  (setq-local compilation-warning-face 'cadence-compilation-warning-face)
  (setq-local compilation-column-face  'cadence-compilation-column-face)
  (setq-local compilation-line-face    'cadence-compilation-line-face)

  (add-hook 'compilation-filter-hook
            #'cadence--ansi-color-compilation-filter nil t))

;;; Font Lock ============================================================== ;;;

(defconst cadence-integer-types
  '("UInt8" "UInt16" "UInt32" "UInt64" "UInt128" "UInt256"))

(defconst cadence-builtin-types
  (append cadence-integer-types '("address" "bool" "vector")))

(defconst cadence-abilities
  '("contracts"))

(defconst cadence-keywords
  '("pre" "post" "as" "break" "const" "continue" "copy" "else" "entry" "before" "while"
    "false" "friend" "fun" "has" "if" "log" "let" "var" "loop" "module" "cadence" "priv" "execute" "access"
    "mut" "create" "pub" "return" "transaction" "script" "spec" "struct" "init" "self"
    "true" "use" "destroy" "contract" "import" "enum" "&" "from" "prepare" "attachment"))

(defvar cadence-keyword-face
  '((t (:foreground "yellow")))
  "Face for highlighting Cadence keywords.")

(defvar cadence-font-lock-keywords
  `(
    ;; comments
    (";.*$" . font-lock-comment-face)
    ;; keywords
    ("\\<\\(" ,(regexp-opt cadence-keywords 'words) "\\)\\>" . font-lock-keyword-face)
    ;; types
    ("\\<\\(int\\|string\\)\\>" . font-lock-type-face)
    ;; abilities
    ("\\<\\(ability1\\)\\>" . (1 font-lock-function-name-face))
    ("\\<\\(ability2\\)\\)\\>" . (1 (:foreground "blue" :weight bold)))
    ;; contract keyword
    ("\\<\\(contract\\)\\>" . font-lock-keyword-face))
  "Default syntax highlighting for Cadence mode.")

(defconst cadence-integer-with-type-re
  (concat "\\_<"
          "\\(?:0x?\\|[1-9]\\)"
          "[[:digit:]a-fA-F]*"
          (regexp-opt cadence-integer-types t)
          "\\_>"))

(defconst cadence-ident-re
  "[a-zA-Z][a-zA-Z0-9_]*\\|_[a-zA-Z0-9_]+")

(defconst cadence-type-re
  "\\_<[A-Z][a-zA-Z0-9_]*\\_>")

(defconst cadence-limit-by-<>-form
  '(if (not (char-equal ?< (char-after))) (point)
       (with-syntax-table cadence-mode-syntax-table+<>
         (save-excursion (forward-sexp) (point))))
  "Returns position one after a matching closed angle bracket.

When the form is evaluaed with the point over an open angled bracket.")

(defconst cadence-generic-constraint-matcher
  `(,(regexp-opt cadence-abilities 'symbols)
    ,cadence-limit-by-<>-form nil
    (0 font-lock-type-face))
  "Font lock sub-matcher for type constraints on generic type parameters.

Generic type parameters are enclosed by type parameters.")

(defvar cadence-mode-font-lock-keywords
  `((,(regexp-opt cadence-keywords 'symbols)      . font-lock-keyword-face)
    (,(regexp-opt cadence-builtin-types 'symbols) . font-lock-type-face)
    ("\\(#\\[[^]]*\\]\\)"                      1 font-lock-preprocessor-face keep)
    (,cadence-integer-with-type-re                1 font-lock-type-face)

    ;; "Types" heuristic -- CapitalizedIdentifiers.
    (,cadence-type-re                             . font-lock-type-face)

    ;; Module components
    (,(concat "\\(" cadence-ident-re "\\)::")     1 font-lock-constant-face)

    ;; Fields, function params, local variables with explicit types
    (,(concat "\\(" cadence-ident-re "\\)\\s-*:[^:]")
     1 font-lock-variable-name-face)

    ;; Let bindings with inferred type
    (,(concat "\\_<let\\s-+\\(" cadence-ident-re "\\)\\_>")
     1 font-lock-variable-name-face)

    ;; var bindings with inferred type
    (,(concat "\\_<var\\s-+\\(" cadence-ident-re "\\)\\_>")
     1 font-lock-variable-name-face)

    (,(concat "\\_<@\\s-+\\(" cadence-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,cadence-generic-constraint-matcher)

    (,(concat "\\_<access\\s-+\\(" cadence-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,cadence-generic-constraint-matcher)

    ;; Function declarations
    (,(concat "\\_<fun\\s-+\\(" cadence-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,cadence-generic-constraint-matcher)

    (,(concat "\\_<.\\s-+\\(" cadence-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,cadence-generic-constraint-matcher)

    (,(concat "\\_<resource\\s-+\\(" cadence-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face))

    ;; Struct declarations
    (,(concat "\\_<struct\\s-+\\(" cadence-ident-re "\\)\\s-*")
     (1 font-lock-type-face)

     ("\\_<phantom\\_>"
      ,cadence-limit-by-<>-form
      (with-syntax-table cadence-mode-syntax-table+<>
        (up-list) (backward-list))
      (0 font-lock-keyword-face))

     ,cadence-generic-constraint-matcher)

    ("\\_<has\\_>"

     (,(regexp-opt cadence-abilities 'symbols)
      (save-excursion
        (re-search-forward "{" (point-at-eol) t +1)
        (point))

      nil

      (0 font-lock-type-face)))

    (eval cadence--register-builtins)))

;;; Interactive Functions ================================================== ;;;

(defun cadence-mode-indent-line ()
  "Set the indent of the current line.

The column is calculated by CADENCE--INDENT-COLUMN.  Jump to that column if the
point is currently before it, leave the point in place otherwise."
  (interactive)
  (let ((indent (cadence--indent-column)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion
          (indent-line-to indent))))))

;;; Comments and Fill ====================================================== ;;;

(defun cadence-mode-distinguish-comments (state)
  "Distinguish between doc comments and normal comments in syntax STATE."
  (save-excursion
    (goto-char (nth 8 state))
    (cond ((looking-at "//[/!][^/!]")
           'font-lock-doc-face)
          ((looking-at "/[*][*!][^*!]")
           'font-lock-doc-face)
          ('font-lock-comment-face))))

(defun cadence-mode-comment-line-break (&optional soft)
  "Create a new line continuing the comment at point.

SOFT is forwarded to `comment-indent-new-line'."
  (let ((fill-prefix (cadence-mode-adaptive-fill)))
    (comment-indent-new-line soft)))

(defun cadence-mode-fill-paragraph (&rest args)
  "Cadence comment-aware wrapper for `fill-paragraph'.

ARGS are forwarded to a call of `fill-paragraph', as-is."
  (let ((fill-prefix (cadence-mode-adaptive-fill))
        (fill-paragraph-handle-comment t)
        (fill-paragraph-function
         (unless (eq fill-paragraph-function #'cadence-mode-fill-paragraph)
           fill-paragraph-function)))
    (apply #'fill-paragraph args) t))

(defun cadence-mode-auto-fill (&rest args)
  "Cadence comment-aware wrapper for `do-auto-fill'.

ARGS are forwarded to a call of `do-auto-fill', as-is."
  (let ((fill-prefix (cadence-mode-adaptive-fill)))
    (apply #'do-auto-fill args) t))

(defun cadence-mode-adaptive-fill ()
  "Pick the `fill-prefix' based on context.

If the point is currently in a comment, return the fill prefix to us to continue
that comment, otherwise return the existing `fill-prefix'."
  (save-match-data
    (save-excursion
      (if (not (cadence--ppss-in-comment))
          fill-prefix
        (let* ((comment-start
                (cadence--ppss-comment-start))
               (comment-indent
                (progn (goto-char comment-start)
                       (beginning-of-line)
                       (buffer-substring-no-properties
                        (point) comment-start))))
          (goto-char comment-start)
          (cond
           ((looking-at "//[/!]*\\s-*")
            (concat comment-indent (match-string-no-properties 0)))
           ((looking-at "/\\*[*!]?\\s-*")
            (concat comment-indent " * "))
           (t fill-prefix)))))))

;;; Private Helper Functions =============================================== ;;;

(defun cadence--expand-compilation-source ()
  "Resolve compiler error/warning files relative to `compilation-directory'."
  (expand-file-name (match-string-no-properties 1) compilation-directory))

(defun cadence--compilation-start (sub-command &rest args)
  "Run a `cadence' sub-command from the Cadence project root.

Invokes `cadence-bin' with `cadence-default-arguments' SUB-COMMAND, and ARGS."
  (let* ((compilation-directory
          (locate-dominating-file default-directory "Cadence.toml")))
    (compilation-start
     (combine-and-quote-strings
      (append (list cadence-bin sub-command)
              (split-string-and-unquote cadence-default-arguments)
              args))
     'cadence-compilation-mode)))

(defun cadence--register-builtins ()
  "Generate a font-lock matcher form for built-in constructs.

The list of built-ins is specified via the `cadence-builtins' custom variable."
  `(,(regexp-opt cadence-builtins 'symbols) . font-lock-builtin-face))

(defun cadence--ppss-inner-paren ()
  "Character address of innermost containing list, or nil if none."
  (nth 1 (syntax-ppss)))

(defun cadence--ppss-in-comment ()
  "Whether or not the cursor is within a comment.

NIL if outside a comment, T if inside a non-nestable comment, or an integer --
the level of nesting -- if inside a nestable comment."
  (nth 4 (syntax-ppss)))

(defun cadence--ppss-comment-start ()
  "Character address for start of comment or string."
  (nth 8 (syntax-ppss)))

(defun cadence--prev-assignment (bound)
  "Find the previous assignment character after BOUND.

Search backwards from the current point until BOUND looking for an `='
character that isn't in a comment.  Returns T on success, with the point over
the character, and NIL otherwise with the point at an indeterminate position."
  (and (search-backward "=" bound t)
       (or (not (cadence--ppss-in-comment))
           (cadence--prev-assignment bound))))

(defun cadence--next-terminator (bound)
  "Find the next statement terminator before BOUND.

Search forwards from the current point until BOUND looking for a `;' character
that isn't in a comment.  Returns T on success, with the point over the
character, and NIL otherwise with the point at an indeterminate position."
  (and (search-forward ";" bound t)
       (or (not (cadence--ppss-in-comment))
           (cadence--next-terminator bound))))

(defun cadence--indent-column ()
  "Calculates the column to indent the current line to.

The default indent is `cadence-indent-offset' greater than the indent of the line
containing the innermost parenthesis at point, or 0 if there is no such
innermost paren.

This column is modified for closing parens, which are dedented by the offset,
continuation lines of `/*'-style comments, which are indented by 1 to line up
their `*', and assignment continuation lines, which are indented by a further
offset."
  (save-excursion
    (back-to-indentation)
    (let* ((current-posn   (point))
           (parent-paren   (cadence--ppss-inner-paren))
           (default-indent (if (not parent-paren) 0
                             (save-excursion
                               (goto-char parent-paren)
                               (back-to-indentation)
                               (+ (current-column) cadence-indent-offset)))))
      (cond
       ;; `/*'-style comment continuation lines
       ((and (cadence--ppss-in-comment)
             (looking-at "*"))
        (+ default-indent 1))

       ;; Top-level items will remain completely unindented.
       ((= default-indent 0) 0)

       ;; Closing parentheses
       ((looking-at "[]})]")
        (- default-indent cadence-indent-offset))

       ;; Assignment continuation lines
       ((save-excursion
          (and parent-paren
               (save-excursion (goto-char parent-paren)
                               (looking-at "{"))
               (cadence--prev-assignment parent-paren)
               (not (cadence--next-terminator current-posn))))
        (+ default-indent cadence-indent-offset))

       (t default-indent)))))

(defun cadence--ansi-color-compilation-filter ()
  "Backport ANSI color compilation filter to support earlier versions of Emacs."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(provide 'cadence-mode)

;;; cadence-mode.el ends here
