;;; tolk-mode.el --- A major-mode for editing Tolk language -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Jayden Dang <jayden.dangvu@gmail.com>

;; Author: Jayden
;; URL: https://github.com/jayden/tolk-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; This package implements a major-mode for editing smart contracts
;; written in Tolk.

;;; Code:


(require 'compile)
(require 'eldoc)

;;; Constants for use with Customization =================================== ;;;

(defconst tolk-core-builtin-functions
  '("assert!"
    "borrow_global"
    "freeze"
    "tolk_from"
    "tolk_to"
    "timestamp"
    "signer"
    "simple_map"
    "hash"
    "bcs"
    "borrow"
    "borrow_mut"
    "borrow_global"
    "borrow_global_mut"
    )
  "Built-in functions from Core Tolk.")

(defconst tolk-prover-keywords
  '("aborts_if"
    "aborts_with"
    "apply"
    "assume"
    "axiom"
    "choose"
    "decreases"
    "ensures"
    "emits"
    "except"
    "exists"
    "forall"
    "global"
    "include"
    "internal"
    "local"
    "min"
    "modifies"
    "old"
    "post"
    "pragma"
    "requires"
    "schema"
    "succeeds_if"
    "to"
    "update"
    "with"
    "where"
    )
  "Keywords that are only used by the tolk prover.

Can be added to TOLK-BUILTINS to enable highlighting, defaults to not.")

;;; Customization ========================================================== ;;;

(defgroup tolk-lang nil
  "Support for Tolk source code."
  :link '(url-link "https://github.com/tolk-language/tolk")
  :group 'languages)

(defcustom tolk-indent-offset 4
  "Number of spaces to indent tolk code by."
  :type  'integer
  :group 'rust-mode
  :safe #'integerp)

(defcustom tolk-builtins tolk-core-builtin-functions
  "Functions to highlight as builtins (mutations require restarting font-lock)."
  :type '(list string)
  :group 'tolk-lang)

(defcustom tolk-bin "tolk"
  "Name of or path to tolk CLI binary."
  :type 'string
  :group 'tolk-lang)

(defcustom tolk-default-arguments ""
  "Default arguments when running common tolk CLI commands."
  :type 'string
  :group 'tolk-lang)

;;; Faces ================================================================== ;;;

(defface tolk-compilation-message-face
  '((t :inherit default))
  "`tolk-compilation-mode'-specific override of `compilation-message-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'tolk-lang)

(defface tolk-compilation-error-face
  '((t :inherit default))
  "`tolk-compilation-mode'-specific override of `compilation-error-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'tolk-lang)

(defface tolk-compilation-warning-face
  '((t :inherit default))
  "`tolk-compilation-mode'-specific override of `compilation-warning-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'tolk-lang)

(defface tolk-compilation-line-face
  '((t :inherit default))
  "`tolk-compilation-mode'-specific override of `compilation-line-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'tolk-lang)

(defface tolk-compilation-column-face
  '((t :inherit default))
  "`tolk-compilation-mode'-specific override of `compilation-column-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'tolk-lang)

;;; Syntax ================================================================= ;;;

(defconst tolk-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (op '(?+ ?- ?* ?/ ?% ?& ?^ ?| ?< ?> ?! ?&))
      (modify-syntax-entry op "." table))

    ;; Parentheses
    (modify-syntax-entry ?\(  "\(\)" table)
    (modify-syntax-entry ?\)  "\)\(" table)
    (modify-syntax-entry ?\{  "\(\}" table)
    (modify-syntax-entry ?\}  "\)\{" table)
    (modify-syntax-entry ?\[  "\(\]" table)
    (modify-syntax-entry ?\]  "\)\[" table)

    ;; Comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23n"  table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)

    table))

(defconst tolk-mode-syntax-table+<>
  (let ((table (copy-syntax-table tolk-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    table)
  "Variant of syntax table recognising angle braces as bracketed.

For use in detecting generic paramters.")

;;; Keybindings ============================================================ ;;;

(defvar tolk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-b") #'tolk-build)
    (define-key map (kbd "C-c C-c C-d") #'tolk-disassemble)
    (define-key map (kbd "C-c C-c C-p") #'tolk-prover)
    (define-key map (kbd "C-c C-c C-t") #'tolk-test)
    map))

;;; Compilation ============================================================ ;;;

(defvar tolk-error-pattern
  (let* ((err  "error\\[E[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat err "\n" box file ":" line ":" col)))
    (list patt #'tolk--expand-compilation-source 2 3 0))
  "Link to sources for compilation errors.")

(defvar tolk-warning-pattern
  (let* ((warn "warning\\[W[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat warn "\n" box file ":" line ":" col)))
    (list patt #'tolk--expand-compilation-source 2 3 1))
  "Link to sources for compilation warnings.")

;;; Modes ================================================================== ;;;

;;;###autoload
(define-derived-mode tolk-mode prog-mode "Tolk"
  "Major mode for Tolk source code.

\\{tolk-mode-map}"
  :group 'tolk-lang
  :syntax-table tolk-mode-syntax-table

  ;; (setq-local font-lock-defaults
  ;;             '(tolk-mode-font-lock-keywords
  ;;               nil ;; KEYWORDS-ONLY
  ;;               nil ;; CASE-TOLK
  ;;               nil ;; SYNTAX-ALIST
  ;;               ;;;;;; VARIABLES
  ;;               (font-lock-syntactic-face-function
  ;;                . tolk-mode-distinguish-comments)))

  (setq-local font-lock-defaults
              '(tolk-mode-font-lock-keywords
                nil ;; KEYWORDS-ONLY
                nil ;; CASE-TOLK
                nil ;; SYNTAX-ALIST
                nil ;; SYNTAX-BEGIN
                (font-lock-syntactic-face-function
                 . tolk-mode-distinguish-comments)))

  ;; ! is punctuation unless it's at the end of a word, in which case,
  ;; it should be treated like piece of the preceding word.
  ;; (setq-local syntax-propertize-function
  ;;             (syntax-propertize-rules ("\\sw\\(!\\)" (1 "w"))))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ("\\(\\sw\\|\\s_\\)\\(!\\)"
                (2 "w"))))



  ;; This is a weirdly lisp-specific variable
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Indentation
  (setq-local indent-line-function #'tolk-mode-indent-line)
  ;; (setq-local electric-indent-chars
  ;;             (cons ?} (and (boundp 'electric-indent-chars)
  ;;                           electric-indent-chars)))

  ;; Comments
  (setq-local comment-end        "")
  (setq-local comment-line-break-function #'tolk-mode-comment-line-break)
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
  (setq-local fill-paragraph-function   #'tolk-mode-fill-paragraph)
  (setq-local normal-auto-fill-function #'tolk-mode-auto-fill)
  (setq-local adaptive-fill-function    #'tolk-mode-adaptive-fill)
  (setq-local adaptive-fill-first-line-regexp ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tolk\\'" . tolk-mode))

(define-compilation-mode tolk-compilation-mode "tolk-compilation"
  "Tolk compilation mode.

Defines regexps for matching file names in compiler output, replacing defaults."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'tolk-error tolk-error-pattern))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'tolk-warning tolk-warning-pattern))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'tolk-error)
  (add-to-list 'compilation-error-regexp-alist 'tolk-warning)

  (setq-local compilation-message-face 'tolk-compilation-message-face)
  (setq-local compilation-error-face   'tolk-compilation-error-face)
  (setq-local compilation-warning-face 'tolk-compilation-warning-face)
  (setq-local compilation-column-face  'tolk-compilation-column-face)
  (setq-local compilation-line-face    'tolk-compilation-line-face)

  (add-hook 'compilation-filter-hook
            #'tolk--ansi-color-compilation-filter nil t))

;;; Font Lock ============================================================== ;;;

(defconst tolk-keywords
  '(
    "else"
    "false"
    "fun"
    "if"
    "invariant"
    "let"
    "var"
    "get"
    "asm"
    "throw"
    "module"
    "tolk"
    "return"
    "true"
    ))

(defconst tolk-integer-types
  '("int"
    ))

(defconst tolk-builtin-types
  (append tolk-integer-types
          '(
            "cell"
            "slice"
            "builder"
            "void"
            )))

(defconst tolk-abilities
  '(
    "inline"
    "inline_ref"
    "pure"
    "method_id"
    ))

(defconst tolk-integer-with-type-re
  (concat "\\_<"
          "\\(?:0x?\\|[1-9]\\)"
          "[[:digit:]a-fA-F]*"
          (regexp-opt tolk-integer-types t)
          "\\_>"))


(defconst tolk-ident-re
  "[a-zA-Z][a-zA-Z0-9_]*\\|_[a-zA-Z0-9_]+")

(defconst tolk-type-re
  "\\_<[A-Z][a-zA-Z0-9_]*\\_>")

(defconst tolk-limit-by-<>-form
  '(if (not (char-equal ?< (char-after))) (point)
       (with-syntax-table tolk-mode-syntax-table+<>
         (save-excursion (forward-sexp) (point))))
  "Returns position one after a matching closed angle bracket.

When the form is evaluaed with the point over an open angled bracket.")

(defconst tolk-generic-constraint-matcher
  `(,(regexp-opt tolk-abilities 'symbols)
    ,tolk-limit-by-<>-form nil
    (0 font-lock-type-face))
  "Font lock sub-matcher for type constraints on generic type parameters.

Generic type parameters are enclosed by type parameters.")

(defvar tolk-mode-font-lock-keywords
  `((,(regexp-opt tolk-keywords 'symbols)      . font-lock-keyword-face)
    (,(regexp-opt tolk-builtin-types 'symbols) . font-lock-type-face) ;; Line 388
    ("\\(#\\[[^]]*\\]\\)"                      1 font-lock-preprocessor-face keep)
    (,tolk-integer-with-type-re                1 font-lock-type-face)
    (,tolk-type-re                             . font-lock-type-face)

    (,(concat "\\(" tolk-ident-re "\\)::")     1 font-lock-constant-face)
    (,(concat "\\(" tolk-ident-re "\\)\\s-*:[^:]")
     1 font-lock-variable-name-face)

    (,(concat "\\_<let\\s-+\\(" tolk-ident-re "\\)\\_>")
     1 font-lock-variable-name-face)

    (,(concat "\\_<fun\\s-+\\(" tolk-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,tolk-generic-constraint-matcher)

    (,(concat "\\_<struct\\s-+\\(" tolk-ident-re "\\)\\s-*")
     (1 font-lock-type-face)
     ("\\_<phantom\\_>"
      ,tolk-limit-by-<>-form
      (with-syntax-table tolk-mode-syntax-table+<>
        (up-list) (backward-list))
      (0 font-lock-keyword-face))
     ,tolk-generic-constraint-matcher)

    ("\\_<has\\_>"
     (,(regexp-opt tolk-abilities 'symbols)
      (save-excursion
        (re-search-forward "{" (point-at-eol) t +1)
        (point))
      nil
      (0 font-lock-type-face)))
    (eval tolk--register-builtins)))

;;; Interactive Functions ================================================== ;;;

(defun tolk-build ()
  "Run `tolk build', returning output in a compilation buffer.

`tolk' refers to the tolk binary, which is customizable at `tolk-bin'."
  (interactive)
  (tolk--compilation-start "build"))

(defun tolk-prover ()
  "Run `tolk prover', returning output in a compilation buffer.

`tolk' refers to the tolk binary, which is customizable at `tolk-bin'."
  (interactive)
  (tolk--compilation-start "prover"))

(defun tolk-test ()
  "Run `tolk test', returning output in a compilation buffer.

`tolk' refers to the tolk binary, which is customizable at `tolk-bin'."
  (interactive)
  (tolk--compilation-start "test"))

(defun tolk-disassemble (module-name)
  "Disassemble MODULE-NAME, returning the output in a compilation buffer.

Uses the `disassemble' subcommand, passing MODULE-NAME with its `--name'
argument.  `tolk' refers to the tolk binary, which is customizable at
`tolk-bin'."
  (interactive "sModule: ")
  (tolk--compilation-start "disassemble" "--name" module-name))

(defun tolk-mode-indent-line ()
  "Set the indent of the current line.

The column is calculated by TOLK--INDENT-COLUMN.  Jump to that column if the
point is currently before it, leave the point in place otherwise."
  (interactive)
  (let ((indent (tolk--indent-column)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion
          (indent-line-to indent))))))

;;; Comments and Fill ====================================================== ;;;

(defun tolk-mode-distinguish-comments (state)
  "Distinguish between doc comments and normal comments in syntax STATE."
  (save-excursion
    (goto-char (nth 8 state))
    (cond ((looking-at "//[/!][^/!]")
           'font-lock-doc-face)
          ((looking-at "/[*][*!][^*!]")
           'font-lock-doc-face)
          ('font-lock-comment-face))))

(defun tolk-mode-comment-line-break (&optional soft)
  "Create a new line continuing the comment at point.

SOFT is forwarded to `comment-indent-new-line'."
  (let ((fill-prefix (tolk-mode-adaptive-fill)))
    (comment-indent-new-line soft)))

(defun tolk-mode-fill-paragraph (&rest args)
  "Tolk comment-aware wrapper for `fill-paragraph'.

ARGS are forwarded to a call of `fill-paragraph', as-is."
  (let ((fill-prefix (tolk-mode-adaptive-fill))
        (fill-paragraph-handle-comment t)
        (fill-paragraph-function
         (unless (eq fill-paragraph-function #'tolk-mode-fill-paragraph)
           fill-paragraph-function)))
    (apply #'fill-paragraph args) t))

(defun tolk-mode-auto-fill (&rest args)
  "Tolk comment-aware wrapper for `do-auto-fill'.

ARGS are forwarded to a call of `do-auto-fill', as-is."
  (let ((fill-prefix (tolk-mode-adaptive-fill)))
    (apply #'do-auto-fill args) t))

(defun tolk-mode-adaptive-fill ()
  "Pick the `fill-prefix' based on context.

If the point is currently in a comment, return the fill prefix to us to continue
that comment, otherwise return the existing `fill-prefix'."
  (save-match-data
    (save-excursion
      (if (not (tolk--ppss-in-comment))
          fill-prefix
        (let* ((comment-start
                (tolk--ppss-comment-start))
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

(defun tolk--expand-compilation-source ()
  "Resolve compiler error/warning files relative to `compilation-directory'."
  (expand-file-name (match-string-no-properties 1) compilation-directory))

(defun tolk--compilation-start (sub-command &rest args)
  "Run a `tolk' sub-command from the Tolk project root.

Invokes `tolk-bin' with `tolk-default-arguments' SUB-COMMAND, and ARGS."
  (let* ((compilation-directory
          (locate-dominating-file default-directory "Tolk.toml")))
    (compilation-start
     (combine-and-quote-strings
      (append (list tolk-bin sub-command)
              (split-string-and-unquote tolk-default-arguments)
              args))
     'tolk-compilation-mode)))

(defun tolk--register-builtins ()
  "Generate a font-lock matcher form for built-in constructs.

The list of built-ins is specified via the `tolk-builtins' custom variable."
  `(,(regexp-opt tolk-builtins 'symbols) . font-lock-builtin-face))

(defun tolk--ppss-inner-paren ()
  "Character address of innermost containing list, or nil if none."
  (nth 1 (syntax-ppss)))

(defun tolk--ppss-in-comment ()
  "Whether or not the cursor is within a comment.

NIL if outside a comment, T if inside a non-nestable comment, or an integer --
the level of nesting -- if inside a nestable comment."
  (nth 4 (syntax-ppss)))

(defun tolk--ppss-comment-start ()
  "Character address for start of comment or string."
  (nth 8 (syntax-ppss)))

(defun tolk--prev-assignment (bound)
  "Find the previous assignment character after BOUND.

Search backwards from the current point until BOUND looking for an `='
character that isn't in a comment.  Returns T on success, with the point over
the character, and NIL otherwise with the point at an indeterminate position."
  (and (search-backward "=" bound t)
       (or (not (tolk--ppss-in-comment))
           (tolk--prev-assignment bound))))

(defun tolk--next-terminator (bound)
  "Find the next statement terminator before BOUND.

Search forwards from the current point until BOUND looking for a `;' character
that isn't in a comment.  Returns T on success, with the point over the
character, and NIL otherwise with the point at an indeterminate position."
  (and (search-forward ";" bound t)
       (or (not (tolk--ppss-in-comment))
           (tolk--next-terminator bound))))

(defun tolk--indent-column ()
  "calculates the column to indent the current line to.

the default indent is `tolk-indent-offset' greater than the indent of the line
containing the innermost parenthesis at point, or 0 if there is no such
innermost paren.

this column is modified for closing parens, which are dedented by the offset,
continuation lines of `/*'-style comments, which are indented by 1 to line up
their `*', and assignment continuation lines, which are indented by a further
offset.

Additionally, it now accounts for #[...] macros and blocks enclosed in curly braces to prevent unintended indentation."
  (save-excursion
    (back-to-indentation)
    (let* ((current-posn   (point))
           (parent-paren   (tolk--ppss-inner-paren))
           (default-indent (if (not parent-paren) 0
                             (save-excursion
                               (goto-char parent-paren)
                               (back-to-indentation)
                               (+ (current-column) tolk-indent-offset)))))
      (cond
       ;; `/*'-style comment continuation lines
       ((and (tolk--ppss-in-comment)
             (looking-at "*"))
        (+ default-indent 1))

       ;; top-level items will remain completely unindented.
       ((= default-indent 0) 0)

       ;; closing parentheses
       ((looking-at "[]})]")
        (- default-indent tolk-indent-offset))

       ;; assignment continuation lines (corrected handling for braces)
       ((save-excursion
          (and parent-paren
               (not (save-excursion (goto-char parent-paren)
                                   (looking-at "{")))
               (tolk--prev-assignment parent-paren)
               (not (tolk--next-terminator current-posn))))
        (+ default-indent tolk-indent-offset))

       ;; #[...] macro handling
       ((and (save-excursion (goto-char parent-paren)
                              (looking-at "#\\["))
               (save-excursion (goto-char parent-paren)
                              (looking-back "[^#]*#[^[]*"))
               (looking-at "{"))
        default-indent)

       ;; Brace handling
       ((looking-at "{")
        default-indent)

       (t default-indent)))))

(defun tolk--ansi-color-compilation-filter ()
  "Backport ANSI color compilation filter to support earlier versions of Emacs."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(provide 'tolk-mode)
;;; tolk-mode eldocs <<<

(defvar tolk-function-docs
  '(("aptos_framework" . "This is the reference documentation of the Aptos framework.")
    ("key" . "Value can be used as a key for global storage operations")
    ("copy" . "Value can be copied ( or cloned by value )")
    ))

(defun tolk-get-function-doc (symbol)
  "Lấy mô tả của hàm SYMBOL từ tolk-function-docs."
  (cdr (assoc symbol tolk-function-docs)))

(defun tolk-eldoc-function ()
  "Hàm để lấy thông tin eldoc cho `tolk-mode`."
  (let ((symbol (thing-at-point 'symbol)))
    (tolk-get-function-doc symbol)))

(add-hook 'tolk-mode-hook
          (lambda ()
            (setq-local eldoc-documentation-function #'tolk-eldoc-function)
            (eldoc-mode 1)))
;;; tolk-mode eldocs <<<
