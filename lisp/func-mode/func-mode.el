;;; func-mode.el --- A major-mode for editing Func language -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Jayden Dang <jayden.dangvu@gmail.com>

;; Author: Jayden
;; URL: https://github.com/jayden/func-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; This package implements a major-mode for editing smart contracts
;; written in Func.

;;; Code:


(require 'compile)
(require 'eldoc)

;;; Constants for use with Customization =================================== ;;;

(defconst func-core-builtin-functions
  '(
    "throw"
    )
  "Built-in functions from Core Func.")

(defconst func-prover-keywords
  '(
    "throw_if"
    "save_data"
    )
  "Keywords that are only used by the func prover.

Can be added to FUNC-BUILTINS to enable highlighting, defaults to not.")

;;; Customization ========================================================== ;;;

(defgroup func-lang nil
  "Support for Func source code."
  :link '(url-link "https://github.com/func-language/func")
  :group 'languages)

(defcustom func-indent-offset 4
  "Number of spaces to indent func code by."
  :type  'integer
  :group 'rust-mode
  :safe #'integerp)

(defcustom func-builtins func-core-builtin-functions
  "Functions to highlight as builtins (mutations require restarting font-lock)."
  :type '(list string)
  :group 'func-lang)

(defcustom func-bin "func"
  "Name of or path to func CLI binary."
  :type 'string
  :group 'func-lang)

(defcustom func-default-arguments ""
  "Default arguments when running common func CLI commands."
  :type 'string
  :group 'func-lang)

;;; Faces ================================================================== ;;;

(defface func-compilation-message-face
  '((t :inherit default))
  "`func-compilation-mode'-specific override of `compilation-message-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'func-lang)

(defface func-compilation-error-face
  '((t :inherit default))
  "`func-compilation-mode'-specific override of `compilation-error-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'func-lang)

(defface func-compilation-warning-face
  '((t :inherit default))
  "`func-compilation-mode'-specific override of `compilation-warning-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'func-lang)

(defface func-compilation-line-face
  '((t :inherit default))
  "`func-compilation-mode'-specific override of `compilation-line-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'func-lang)

(defface func-compilation-column-face
  '((t :inherit default))
  "`func-compilation-mode'-specific override of `compilation-column-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'func-lang)

;;; Syntax ================================================================= ;;;

(defconst func-mode-syntax-table
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

    ;; Comments - Modified for ;; style
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)

    table))

(defconst func-mode-syntax-table+<>
  (let ((table (copy-syntax-table func-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    table)
  "Variant of syntax table recognising angle braces as bracketed.

For use in detecting generic paramters.")

;;; Keybindings ============================================================ ;;;

(defvar func-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-b") #'func-build)
    (define-key map (kbd "C-c C-c C-d") #'func-disassemble)
    (define-key map (kbd "C-c C-c C-p") #'func-prover)
    (define-key map (kbd "C-c C-c C-t") #'func-test)
    map))

;;; Compilation ============================================================ ;;;

(defvar func-error-pattern
  (let* ((err  "error\\[E[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat err "\n" box file ":" line ":" col)))
    (list patt #'func--expand-compilation-source 2 3 0))
  "Link to sources for compilation errors.")

(defvar func-warning-pattern
  (let* ((warn "warning\\[W[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat warn "\n" box file ":" line ":" col)))
    (list patt #'func--expand-compilation-source 2 3 1))
  "Link to sources for compilation warnings.")

;;; Modes ================================================================== ;;;

;;;###autoload
(define-derived-mode func-mode prog-mode "Func"
  "Major mode for Func source code.

\\{func-mode-map}"
  :group 'func-lang
  :syntax-table func-mode-syntax-table


  ;; (setq-local font-lock-defaults
  ;;             '(func-mode-font-lock-keywords
  ;;               nil ;; KEYWORDS-ONLY
  ;;               nil ;; CASE-FOLD
  ;;               nil ;; SYNTAX-ALIST
  ;;               ;;;;;; VARIABLES
  ;;               (font-lock-syntactic-face-function
  ;;                . func-mode-distinguish-comments)))

  (setq-local font-lock-defaults
              '(func-mode-font-lock-keywords
                nil ;; KEYWORDS-ONLY
                nil ;; CASE-FOLD
                nil ;; SYNTAX-ALIST
                nil ;; SYNTAX-BEGIN
                (font-lock-syntactic-face-function
                 . func-mode-distinguish-comments)))

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
  (setq-local indent-line-function #'func-mode-indent-line)
  ;; (setq-local electric-indent-chars
  ;;             (cons ?} (and (boundp 'electric-indent-chars)
  ;;                           electric-indent-chars)))

  ;; Comments
  ;; Comment configuration
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function #'func-mode-comment-line-break)
  (setq-local comment-start      ";; ")
  (setq-local comment-end        "")
  (setq-local comment-start-skip "\\(;;[;!]*\\)\\s-*")
  (setq-local comment-start-skip "\\(?:;;[/!]*\\|/\\*[*!]?\\)\\s-*")
  (setq-local comment-multi-line nil)

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
  (setq-local fill-paragraph-function   #'func-mode-fill-paragraph)
  (setq-local normal-auto-fill-function #'func-mode-auto-fill)
  (setq-local adaptive-fill-function    #'func-mode-adaptive-fill)
  (setq-local adaptive-fill-first-line-regexp ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fc\\'" . func-mode))

(define-compilation-mode func-compilation-mode "func-compilation"
  "Func compilation mode.

Defines regexps for matching file names in compiler output, replacing defaults."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'func-error func-error-pattern))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'func-warning func-warning-pattern))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'func-error)
  (add-to-list 'compilation-error-regexp-alist 'func-warning)

  (setq-local compilation-message-face 'func-compilation-message-face)
  (setq-local compilation-error-face   'func-compilation-error-face)
  (setq-local compilation-warning-face 'func-compilation-warning-face)
  (setq-local compilation-column-face  'func-compilation-column-face)
  (setq-local compilation-line-face    'func-compilation-line-face)

  (add-hook 'compilation-filter-hook
            #'func--ansi-color-compilation-filter nil t))

;;; Font Lock ============================================================== ;;;

(defconst func-keywords
  '("abort"
    "null"
    "var"
    ;; boolean
    "()"
    "false"
    "global"
    "const"
    "true"
    "return"
    ))

(defconst func-integer-types
  '("a8"
    "u16"
    "u32"
    "u64"
    "u128"
    "u256"
    ))

(defconst func-builtin-types
  (append func-integer-types '("slice"
                               "int"
                               "cell"
                               "builder"
                               "tuple"
                               "cont"
                               )))

(defconst func-abilities
  '("impure"
    "inline"
    "inline_ref"
    "method_id"
    ))

(defconst func-integer-with-type-re
  (concat "\\_<"
          "\\(?:0x?\\|[1-9]\\)"
          "[[:digit:]a-fA-F]*"
          (regexp-opt func-integer-types t)
          "\\_>"))


(defconst func-ident-re
  "[a-zA-Z][a-zA-Z0-9_]*\\|_[a-zA-Z0-9_]+")

(defconst func-type-re
  "\\_<[A-Z][a-zA-Z0-9_]*\\_>")

(defconst func-limit-by-<>-form
  '(if (not (char-equal ?< (char-after))) (point)
       (with-syntax-table func-mode-syntax-table+<>
         (save-excursion (forward-sexp) (point))))
  "Returns position one after a matching closed angle bracket.

When the form is evaluaed with the point over an open angled bracket.")

(defconst func-generic-constraint-matcher
  `(,(regexp-opt func-abilities 'symbols)
    ,func-limit-by-<>-form nil
    (0 font-lock-type-face))
  "Font lock sub-matcher for type constraints on generic type parameters.

Generic type parameters are enclosed by type parameters.")

(defvar func-mode-font-lock-keywords
  `((,(regexp-opt func-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt func-builtin-types 'symbols) . font-lock-type-face)
    (,(regexp-opt func-abilities 'symbols) . font-lock-type-face)
    ("\\(#\\[[^]]*\\]\\)" 1 font-lock-preprocessor-face keep)
    ;; Include directives
    ("#include\\s-+\"\\([^\"]+\\)\"" 1 font-lock-string-face)
    ("#\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-preprocessor-face)

    (,func-integer-with-type-re 1 font-lock-type-face)
    (,func-type-re . font-lock-type-face)

    ;; Stdlib functions (after ~)
    ("~\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*("
     1 'font-lock-builtin-face)

    ;; Standard function calls inside braces
    ("{[^}]*\\(?:[.~]\\|^\\|[^a-zA-Z0-9_]\\)\\([a-zA-Z][a-zA-Z0-9_]*\\)("
     1 font-lock-builtin-face)

    ;; Method chaining - highlight function calls after dots
    ("\\.\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*("
     1 'font-lock-builtin-face)
     ;; 1 font-lock-function-name-face)

    ;; Function declarations
    ("(\\([^)]*\\))\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*(.*?)\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)*"
     (1 font-lock-type-face)
     (2 font-lock-function-name-face))

    ("()\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*("
     1 font-lock-function-name-face)

    (,(concat "\\(" func-ident-re "\\)::") 1 font-lock-constant-face)
    (,(concat "\\(" func-ident-re "\\)\\s-*:[^:]")
     1 font-lock-variable-name-face)

    (,(concat "\\_<let\\s-+\\(" func-ident-re "\\)\\_>")
     1 font-lock-variable-name-face)))

;;; Interactive Functions ================================================== ;;;

(defun func-build ()
  "Run `func build', returning output in a compilation buffer.

`func' refers to the func binary, which is customizable at `func-bin'."
  (interactive)
  (func--compilation-start "build"))

(defun func-prover ()
  "Run `func prover', returning output in a compilation buffer.

`func' refers to the func binary, which is customizable at `func-bin'."
  (interactive)
  (func--compilation-start "prover"))

(defun func-test ()
  "Run `func test', returning output in a compilation buffer.

`func' refers to the func binary, which is customizable at `func-bin'."
  (interactive)
  (func--compilation-start "test"))

(defun func-disassemble (module-name)
  "Disassemble MODULE-NAME, returning the output in a compilation buffer.

Uses the `disassemble' subcommand, passing MODULE-NAME with its `--name'
argument.  `func' refers to the func binary, which is customizable at
`func-bin'."
  (interactive "sModule: ")
  (func--compilation-start "disassemble" "--name" module-name))

(defun func-mode-indent-line ()
  "Set the indent of the current line.

The column is calculated by FUNC--INDENT-COLUMN.  Jump to that column if the
point is currently before it, leave the point in place otherwise."
  (interactive)
  (let ((indent (func--indent-column)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion
          (indent-line-to indent))))))

;;; Comments and Fill ====================================================== ;;;

(defun func-mode-distinguish-comments (state)
  "Distinguish between doc comments and normal comments in syntax STATE."
  (save-excursion
    (goto-char (nth 8 state))
    (cond ((looking-at ";;[;!][^;!]")
           'font-lock-doc-face)
          ('font-lock-comment-face))))

(defun func-mode-comment-line-break (&optional soft)
  "Create a new line continuing the comment at point.

SOFT is forwarded to `comment-indent-new-line'."
  (let ((fill-prefix (func-mode-adaptive-fill)))
    (comment-indent-new-line soft)))

(defun func-mode-fill-paragraph (&rest args)
  "Func comment-aware wrapper for `fill-paragraph'.

ARGS are forwarded to a call of `fill-paragraph', as-is."
  (let ((fill-prefix (func-mode-adaptive-fill))
        (fill-paragraph-handle-comment t)
        (fill-paragraph-function
         (unless (eq fill-paragraph-function #'func-mode-fill-paragraph)
           fill-paragraph-function)))
    (apply #'fill-paragraph args) t))

(defun func-mode-auto-fill (&rest args)
  "Func comment-aware wrapper for `do-auto-fill'.

ARGS are forwarded to a call of `do-auto-fill', as-is."
  (let ((fill-prefix (func-mode-adaptive-fill)))
    (apply #'do-auto-fill args) t))

(defun func-mode-adaptive-fill ()
  "Pick the `fill-prefix' based on context.

If the point is currently in a comment, return the fill prefix to us to continue
that comment, otherwise return the existing `fill-prefix'."
  (save-match-data
    (save-excursion
      (if (not (func--ppss-in-comment))
          fill-prefix
        (let* ((comment-start
                (func--ppss-comment-start))
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

(defun func--expand-compilation-source ()
  "Resolve compiler error/warning files relative to `compilation-directory'."
  (expand-file-name (match-string-no-properties 1) compilation-directory))

(defun func--compilation-start (sub-command &rest args)
  "Run a `func' sub-command from the Func project root.

Invokes `func-bin' with `func-default-arguments' SUB-COMMAND, and ARGS."
  (let* ((compilation-directory
          (locate-dominating-file default-directory "Func.toml")))
    (compilation-start
     (combine-and-quote-strings
      (append (list func-bin sub-command)
              (split-string-and-unquote func-default-arguments)
              args))
     'func-compilation-mode)))

(defun func--register-builtins ()
  "Generate a font-lock matcher form for built-in constructs.

The list of built-ins is specified via the `func-builtins' custom variable."
  `(,(regexp-opt func-builtins 'symbols) . font-lock-builtin-face))

(defun func--ppss-inner-paren ()
  "Character address of innermost containing list, or nil if none."
  (nth 1 (syntax-ppss)))

(defun func--ppss-in-comment ()
  "Whether or not the cursor is within a comment.

NIL if outside a comment, T if inside a non-nestable comment, or an integer --
the level of nesting -- if inside a nestable comment."
  (nth 4 (syntax-ppss)))

(defun func--ppss-comment-start ()
  "Character address for start of comment or string."
  (nth 8 (syntax-ppss)))

(defun func--prev-assignment (bound)
  "Find the previous assignment character after BOUND.

Search backwards from the current point until BOUND looking for an `='
character that isn't in a comment.  Returns T on success, with the point over
the character, and NIL otherwise with the point at an indeterminate position."
  (and (search-backward "=" bound t)
       (or (not (func--ppss-in-comment))
           (func--prev-assignment bound))))

(defun func--next-terminator (bound)
  "Find the next statement terminator before BOUND.

Search forwards from the current point until BOUND looking for a `;' character
that isn't in a comment.  Returns T on success, with the point over the
character, and NIL otherwise with the point at an indeterminate position."
  (and (search-forward ";" bound t)
       (or (not (func--ppss-in-comment))
           (func--next-terminator bound))))

(defun func--indent-column ()
  "calculates the column to indent the current line to.

the default indent is `func-indent-offset' greater than the indent of the line
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
           (parent-paren   (func--ppss-inner-paren))
           (default-indent (if (not parent-paren) 0
                             (save-excursion
                               (goto-char parent-paren)
                               (back-to-indentation)
                               (+ (current-column) func-indent-offset)))))
      (cond
       ;; `/*'-style comment continuation lines
       ((and (func--ppss-in-comment)
             (looking-at "*"))
        (+ default-indent 1))

       ;; top-level items will remain completely unindented.
       ((= default-indent 0) 0)

       ;; closing parentheses
       ((looking-at "[]})]")
        (- default-indent func-indent-offset))

       ;; assignment continuation lines (corrected handling for braces)
       ((save-excursion
          (and parent-paren
               (not (save-excursion (goto-char parent-paren)
                                   (looking-at "{")))
               (func--prev-assignment parent-paren)
               (not (func--next-terminator current-posn))))
        (+ default-indent func-indent-offset))

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

(defun func--ansi-color-compilation-filter ()
  "Backport ANSI color compilation filter to support earlier versions of Emacs."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(provide 'func-mode)
;;; func-mode eldocs <<<

(defvar func-function-docs
  '(("begin_parse" . "slice begin_parse(cell c) asm \"CTOS\"; \nConverts cell into slice. Notice that c must be either an ordinary cell or an exotic cell (see TVM.pdf, 3.1.2) which is automatically loaded to yield an ordinary cell c'converted into slice afterwards.")

    ("end_parse" . "() end_parse(slice s) impure asm \"ENDS\"; \nChecks if s is empty. If not, throws an exception.")

    ("load_ref" . "(slice, cell) load_ref(slice s) asm( -> 1 0) \"LDREF\"; \nLoads the first reference from a slice.")

    ("preload_ref" . "cell preload_ref(slice s) asm \"PLDREF\"; \nPreloads the first reference from a slice.")

    ("load_bits" . "(slice, slice) load_bits(slice s, int len) asm(s len -> 1 0) \"LDSLICEX\"; \nLoads the first 0 ≤ len ≤ 1023 bits from slice s into a separate slice s''.")

    ("preload_bits" . "slice preload_bits(slice s, int len) asm \"PLDSLICEX\"; \nPreloads the first 0 ≤ len ≤ 1023 bits from slice s into a separate slice s''.")

    ("load_uint" . "(slice, int) ~load_uint(slice s, int len) asm( -> 1 0) \"LDUX\"; \nLoads an unsigned len-bit integer from a slice.")

    ("preload_uint" . "int preload_uint(slice s, int len) asm \"PLDUX\"; \nPreloads an unsigned len-bit integer from a slice.")

    ("load_int" . "(slice, int) ~load_int(slice s, int len) asm(s len -> 1 0) \"LDIX\"; \nLoads a signed len-bit integer from a slice.")

    ("preload_int" . "int preload_int(slice s, int len) asm \"PLDIX\"; \nPreloads a signed len-bit integer from a slice.")

    ("load_coins" . "(slice, int) load_coins(slice s) asm( -> 1 0) \"LDGRAMS\"; \nLoads serialized amount of Toncoins (any unsigned integer up to 2^120 - 1).")

    ("skip_bits" . "slice skip_bits(slice s, int len) asm \"SDSKIPFIRST\"; \nReturns all but the first 0 ≤ len ≤ 1023 bits of s.")

    ("first_bits" . "slice first_bits(slice s, int len) asm \"SDCUTFIRST\"; \nReturns the first 0 ≤ len ≤ 1023 bits of s.")

    ("skip_last_bits" . "slice skip_last_bits(slice s, int len) asm \"SDSKIPLAST\"; \nReturns all but the last 0 ≤ len ≤ 1023 bits of s.")

    ("slice_last" . "slice slice_last(slice s, int len) asm \"SDCUTLAST\"; \nReturns the last 0 ≤ len ≤ 1023 bits of s.")

    ("load_dict" . "(slice, cell) load_dict(slice s) asm( -> 1 0) \"LDDICT\"; \nLoads a dictionary D from slice s. May be applied to dictionaries or to values of arbitrary Maybe ^Y types.")

    ("preload_dict" . "cell preload_dict(slice s) asm \"PLDDICT\"; \nPreloads a dictionary D from slice s.")

    ("skip_dict" . "slice skip_dict(slice s) asm \"SKIPDICT\"; \nLoads a dictionary as load_dict but returns only the remainder of the slice.")

    ("slice_refs" . "int slice_refs(slice s) asm \"SREFS\"; \nReturns the number of references in slice s.")

    ("slice_bits" . "int slice_bits(slice s) asm \"SBITS\"; \nReturns the number of data bits in slice s.")

    ("slice_bits_refs" . "(int, int) slice_bits_refs(slice s) asm \"SBITREFS\"; \nReturns both the number of data bits and the number of references in s.")

    ("slice_empty?" . "int slice_empty?(slice s) asm \"SEMPTY\"; \nChecks whether slice s is empty (i.e., contains no bits of data and no cell references).")

    ("slice_data_empty?" . "int slice_data_empty?(slice s) asm \"SDEMPTY\"; \nChecks whether slice s has no bits of data.")

    ("slice_refs_empty?" . "int slice_refs_empty?(slice s) asm \"SREMPTY\"; \nChecks whether slice s has no references.")

    ("slice_depth" . "int slice_depth(slice s) asm \"SDEPTH\"; \nReturns the depth of slice s. If s has no references, returns 0; otherwise returns one plus the maximum depths of cells referred to from s.")

    ("begin_cell" . "builder begin_cell() asm \"NEWC\"; \nCreates a new empty builder.")

    ("end_cell" . "cell end_cell(builder b) asm \"ENDC\"; \nConverts builder into an ordinary cell.")

    ("store_ref" . "builder store_ref(builder b, cell c) asm(c b) \"STREF\"; \nStores a reference to cell c into builder b.")

    ("store_uint" . "builder store_uint(builder b, int x, int len) asm(x b len) \"STUX\"; \nStores an unsigned len-bit integer x into b for 0 ≤ len ≤ 256.")

    ("store_int" . "builder store_int(builder b, int x, int len) asm(x b len) \"STIX\"; \nStores a signed len-bit integer x into b for 0 ≤ len ≤ 257.")

    ("store_slice" . "builder store_slice(builder b, slice s) asm \"STSLICER\"; \nStores slice s into builder b.")

    ("store_grams" . "builder store_grams(builder b, int x) asm \"STGRAMS\"; \nStores (serializes) an integer x in the range 0..2^120 − 1 into builder b.")

    ("store_dict" . "builder store_dict(builder b, cell c) asm(c b) \"STDICT\"; \nStores dictionary D represented by cell c or null into builder b.")

    ("store_maybe_ref" . "builder store_maybe_ref(builder b, cell c) asm(c b) \"STOPTREF\"; \nEquivalent to store_dict.")

    ("builder_refs" . "int builder_refs(builder b) asm \"BREFS\"; \nReturns the number of cell references already stored in builder b.")

    ("builder_bits" . "int builder_bits(builder b) asm \"BBITS\"; \nReturns the number of data bits already stored in builder b.")

    ("builder_depth" . "int builder_depth(builder b) asm \"BDEPTH\"; \nReturns the depth of builder b. If no cell references are stored in b, returns 0; otherwise returns one plus the maximum depths of cells referred to from b.")

    ("cell_depth" . "int cell_depth(cell c) asm \"CDEPTH\"; \nReturns the depth of cell c. If c has no references, returns 0; otherwise returns one plus the maximum depths of cells referred to from c. If c is null, returns zero.")

    ("cell_null?" . "int cell_null?(cell c) asm \"ISNULL\"; \nChecks whether c is a null. Usually a null-cell represents an empty dictionary.")

    ("slice_hash" . "int slice_hash(slice s) asm \"HASHSU\"; \nComputes the hash of slice s and returns it as a 256-bit unsigned integer x.")

    ("cell_hash" . "int cell_hash(cell c) asm \"HASHCU\"; \nComputes the representation hash of cell c and returns it as a 256-bit unsigned integer x.")

    ("string_hash" . "int string_hash(slice s) asm \"SHA256U\"; \nComputes sha256 of the data bits of slice s. Returns hash as 256-bit unsigned integer.")

    ("check_signature" . "int check_signature(int hash, slice signature, int public_key) asm \"CHKSIGNU\"; \nChecks the Ed25519 signature of hash using public_key. Returns -1 if valid, 0 if invalid.")

    ("check_data_signature" . "int check_data_signature(slice data, slice signature, int public_key) asm \"CHKSIGNS\"; \nChecks whether signature is a valid Ed25519 signature of the data portion using public_key.")

    ("now" . "int now() asm \"NOW\"; \nReturns the current Unix time as an Integer")

    ("my_address" . "slice my_address() asm \"MYADDR\"; \nReturns the internal address of the current smart contract as a Slice with MsgAddressInt.")

    ("get_balance" . "[int, cell] get_balance() asm \"BALANCE\"; \nReturns the remaining balance of the smart contract as tuple with int (balance in nanotoncoins) and cell (extra currencies dictionary).")

    ("min" . "int min(int x, int y) asm \"MIN\"; \nComputes the minimum of two integers x and y.")

    ("max" . "int max(int x, int y) asm \"MAX\"; \nComputes the maximum of two integers x and y.")

    ("minmax" . "(int, int) minmax(int x, int y) asm \"MINMAX\"; \nSorts two integers.")

    ("abs" . "int abs(int x) asm \"ABS\"; \nComputes the absolute value of the integer x.")

    ("null" . "forall X -> X null() asm \"PUSHNULL\"; \nReturns null value that can have any atomic type.")

    ("random" . "int random() impure asm \"RANDU256\"; \nGenerates a new pseudo-random unsigned 256-bit integer.")

    ("rand" . "int rand(int range) impure asm \"RAND\"; \nGenerates a new pseudo-random integer in range 0..range−1 (or range..−1 if range < 0).")
    ))

(defun func-get-function-doc (symbol)
  "Lấy mô tả của hàm SYMBOL từ func-function-docs."
  (cdr (assoc symbol func-function-docs)))

(defun func-eldoc-function ()
  "Hàm để lấy thông tin eldoc cho `func-mode`."
  (let ((symbol (thing-at-point 'symbol)))
    (func-get-function-doc symbol)))

(add-hook 'func-mode-hook
          (lambda ()
            (setq-local eldoc-documentation-function #'func-eldoc-function)
            (eldoc-mode 1)))
;;; func-mode eldocs <<<
