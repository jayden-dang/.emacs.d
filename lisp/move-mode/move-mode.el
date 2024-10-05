;;; move-mode.el --- A major-mode for editing Move language -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Jayden Dang <jayden.dangvu@gmail.com>

;; Author: Jayden
;; URL: https://github.com/jayden/move-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; This package implements a major-mode for editing smart contracts
;; written in Move.

;;; Code:


(require 'compile)
(require 'eldoc)

;;; Constants for use with Customization =================================== ;;;

(defconst move-core-builtin-functions
  '("assert!"
    "borrow_global"
    "freeze"
    "move_from"
    "move_to"
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
  "Built-in functions from Core Move.")

(defconst move-prover-keywords
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
  "Keywords that are only used by the move prover.

Can be added to MOVE-BUILTINS to enable highlighting, defaults to not.")

;;; Customization ========================================================== ;;;

(defgroup move-lang nil
  "Support for Move source code."
  :link '(url-link "https://github.com/move-language/move")
  :group 'languages)

(defcustom move-indent-offset 4
  "Number of spaces to indent move code by."
  :type  'integer
  :group 'rust-mode
  :safe #'integerp)

(defcustom move-builtins move-core-builtin-functions
  "Functions to highlight as builtins (mutations require restarting font-lock)."
  :type '(list string)
  :group 'move-lang)

(defcustom move-bin "move"
  "Name of or path to move CLI binary."
  :type 'string
  :group 'move-lang)

(defcustom move-default-arguments ""
  "Default arguments when running common move CLI commands."
  :type 'string
  :group 'move-lang)

;;; Faces ================================================================== ;;;

(defface move-compilation-message-face
  '((t :inherit default))
  "`move-compilation-mode'-specific override of `compilation-message-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'move-lang)

(defface move-compilation-error-face
  '((t :inherit default))
  "`move-compilation-mode'-specific override of `compilation-error-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'move-lang)

(defface move-compilation-warning-face
  '((t :inherit default))
  "`move-compilation-mode'-specific override of `compilation-warning-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'move-lang)

(defface move-compilation-line-face
  '((t :inherit default))
  "`move-compilation-mode'-specific override of `compilation-line-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'move-lang)

(defface move-compilation-column-face
  '((t :inherit default))
  "`move-compilation-mode'-specific override of `compilation-column-face'.

Inherits from `default' face to avoid interfering with the ANSI colour filter."
  :group 'move-lang)

;;; Syntax ================================================================= ;;;

(defconst move-mode-syntax-table
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

(defconst move-mode-syntax-table+<>
  (let ((table (copy-syntax-table move-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    table)
  "Variant of syntax table recognising angle braces as bracketed.

For use in detecting generic paramters.")

;;; Keybindings ============================================================ ;;;

(defvar move-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-b") #'move-build)
    (define-key map (kbd "C-c C-c C-d") #'move-disassemble)
    (define-key map (kbd "C-c C-c C-p") #'move-prover)
    (define-key map (kbd "C-c C-c C-t") #'move-test)
    map))

;;; Compilation ============================================================ ;;;

(defvar move-error-pattern
  (let* ((err  "error\\[E[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat err "\n" box file ":" line ":" col)))
    (list patt #'move--expand-compilation-source 2 3 0))
  "Link to sources for compilation errors.")

(defvar move-warning-pattern
  (let* ((warn "warning\\[W[0-9]+\\]:\s[^\n]+")
         (box  "\s*\\(?:\u2502\s+\\)*\u250c\u2500\s")
         (file "\\([^\n]+\\)")
         (line "\\([0-9]+\\)")
         (col  "\\([0-9]+\\)")
         (patt (concat warn "\n" box file ":" line ":" col)))
    (list patt #'move--expand-compilation-source 2 3 1))
  "Link to sources for compilation warnings.")

;;; Modes ================================================================== ;;;

;;;###autoload
(define-derived-mode move-mode prog-mode "Move"
  "Major mode for Move source code.

\\{move-mode-map}"
  :group 'move-lang
  :syntax-table move-mode-syntax-table

  ;; (setq-local font-lock-defaults
  ;;             '(move-mode-font-lock-keywords
  ;;               nil ;; KEYWORDS-ONLY
  ;;               nil ;; CASE-FOLD
  ;;               nil ;; SYNTAX-ALIST
  ;;               ;;;;;; VARIABLES
  ;;               (font-lock-syntactic-face-function
  ;;                . move-mode-distinguish-comments)))

  (setq-local font-lock-defaults
              '(move-mode-font-lock-keywords
                nil ;; KEYWORDS-ONLY
                nil ;; CASE-FOLD
                nil ;; SYNTAX-ALIST
                nil ;; SYNTAX-BEGIN
                (font-lock-syntactic-face-function
                 . move-mode-distinguish-comments)))

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
  (setq-local indent-line-function #'move-mode-indent-line)
  ;; (setq-local electric-indent-chars
  ;;             (cons ?} (and (boundp 'electric-indent-chars)
  ;;                           electric-indent-chars)))

  ;; Comments
  (setq-local comment-end        "")
  (setq-local comment-line-break-function #'move-mode-comment-line-break)
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
  (setq-local fill-paragraph-function   #'move-mode-fill-paragraph)
  (setq-local normal-auto-fill-function #'move-mode-auto-fill)
  (setq-local adaptive-fill-function    #'move-mode-adaptive-fill)
  (setq-local adaptive-fill-first-line-regexp ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))

(define-compilation-mode move-compilation-mode "move-compilation"
  "Move compilation mode.

Defines regexps for matching file names in compiler output, replacing defaults."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'move-error move-error-pattern))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'move-warning move-warning-pattern))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'move-error)
  (add-to-list 'compilation-error-regexp-alist 'move-warning)

  (setq-local compilation-message-face 'move-compilation-message-face)
  (setq-local compilation-error-face   'move-compilation-error-face)
  (setq-local compilation-warning-face 'move-compilation-warning-face)
  (setq-local compilation-column-face  'move-compilation-column-face)
  (setq-local compilation-line-face    'move-compilation-line-face)

  (add-hook 'compilation-filter-hook
            #'move--ansi-color-compilation-filter nil t))

;;; Font Lock ============================================================== ;;;

(defconst move-keywords
  '("abort"
    "acquires"
    "as"
    "break"
    "const"
    "continue"
    "else"
    "entry"
    "false"
    "friend"
    "fun"
    "has"
    "if"
    "invariant"
    "let"
    "loop"
    "module"
    "move"
    "mut"
    "native"
    "public"
    "macro"
    "return"
    "script"
    "spec"
    "struct"
    "true"
    "use"
    "while"
    "match"
    "sui"
    "aptos_framework"
    "aptos_token_objects"
    "aptos_std"
    ))

(defconst move-integer-types
  '("u8"
    "u16"
    "u32"
    "u64"
    "u128"
    "u256"
    ))

(defconst move-builtin-types
  (append move-integer-types '("address"
                               "bool"
                               "vector"
                               "outer")))

(defconst move-abilities
  '(
    "copy"
    "drop"
    "store"
    "key"
    ))

(defconst move-integer-with-type-re
  (concat "\\_<"
          "\\(?:0x?\\|[1-9]\\)"
          "[[:digit:]a-fA-F]*"
          (regexp-opt move-integer-types t)
          "\\_>"))


(defconst move-ident-re
  "[a-zA-Z][a-zA-Z0-9_]*\\|_[a-zA-Z0-9_]+")

(defconst move-type-re
  "\\_<[A-Z][a-zA-Z0-9_]*\\_>")

(defconst move-limit-by-<>-form
  '(if (not (char-equal ?< (char-after))) (point)
       (with-syntax-table move-mode-syntax-table+<>
         (save-excursion (forward-sexp) (point))))
  "Returns position one after a matching closed angle bracket.

When the form is evaluaed with the point over an open angled bracket.")

(defconst move-generic-constraint-matcher
  `(,(regexp-opt move-abilities 'symbols)
    ,move-limit-by-<>-form nil
    (0 font-lock-type-face))
  "Font lock sub-matcher for type constraints on generic type parameters.

Generic type parameters are enclosed by type parameters.")

(defvar move-mode-font-lock-keywords
  `((,(regexp-opt move-keywords 'symbols)      . font-lock-keyword-face)
    (,(regexp-opt move-builtin-types 'symbols) . font-lock-type-face) ;; Line 388
    ("\\(#\\[[^]]*\\]\\)"                      1 font-lock-preprocessor-face keep)
    (,move-integer-with-type-re                1 font-lock-type-face)
    (,move-type-re                             . font-lock-type-face)

    (,(concat "\\(" move-ident-re "\\)::")     1 font-lock-constant-face)
    (,(concat "\\(" move-ident-re "\\)\\s-*:[^:]")
     1 font-lock-variable-name-face)

    (,(concat "\\_<let\\s-+\\(" move-ident-re "\\)\\_>")
     1 font-lock-variable-name-face)

    (,(concat "\\_<fun\\s-+\\(" move-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,move-generic-constraint-matcher)

    (,(concat "\\_<struct\\s-+\\(" move-ident-re "\\)\\s-*")
     (1 font-lock-type-face)
     ("\\_<phantom\\_>"
      ,move-limit-by-<>-form
      (with-syntax-table move-mode-syntax-table+<>
        (up-list) (backward-list))
      (0 font-lock-keyword-face))
     ,move-generic-constraint-matcher)

    ("\\_<has\\_>"
     (,(regexp-opt move-abilities 'symbols)
      (save-excursion
        (re-search-forward "{" (point-at-eol) t +1)
        (point))
      nil
      (0 font-lock-type-face)))
    (eval move--register-builtins)))

;;; Interactive Functions ================================================== ;;;

(defun move-build ()
  "Run `move build', returning output in a compilation buffer.

`move' refers to the move binary, which is customizable at `move-bin'."
  (interactive)
  (move--compilation-start "build"))

(defun move-prover ()
  "Run `move prover', returning output in a compilation buffer.

`move' refers to the move binary, which is customizable at `move-bin'."
  (interactive)
  (move--compilation-start "prover"))

(defun move-test ()
  "Run `move test', returning output in a compilation buffer.

`move' refers to the move binary, which is customizable at `move-bin'."
  (interactive)
  (move--compilation-start "test"))

(defun move-disassemble (module-name)
  "Disassemble MODULE-NAME, returning the output in a compilation buffer.

Uses the `disassemble' subcommand, passing MODULE-NAME with its `--name'
argument.  `move' refers to the move binary, which is customizable at
`move-bin'."
  (interactive "sModule: ")
  (move--compilation-start "disassemble" "--name" module-name))

(defun move-mode-indent-line ()
  "Set the indent of the current line.

The column is calculated by MOVE--INDENT-COLUMN.  Jump to that column if the
point is currently before it, leave the point in place otherwise."
  (interactive)
  (let ((indent (move--indent-column)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion
          (indent-line-to indent))))))

;;; Comments and Fill ====================================================== ;;;

(defun move-mode-distinguish-comments (state)
  "Distinguish between doc comments and normal comments in syntax STATE."
  (save-excursion
    (goto-char (nth 8 state))
    (cond ((looking-at "//[/!][^/!]")
           'font-lock-doc-face)
          ((looking-at "/[*][*!][^*!]")
           'font-lock-doc-face)
          ('font-lock-comment-face))))

(defun move-mode-comment-line-break (&optional soft)
  "Create a new line continuing the comment at point.

SOFT is forwarded to `comment-indent-new-line'."
  (let ((fill-prefix (move-mode-adaptive-fill)))
    (comment-indent-new-line soft)))

(defun move-mode-fill-paragraph (&rest args)
  "Move comment-aware wrapper for `fill-paragraph'.

ARGS are forwarded to a call of `fill-paragraph', as-is."
  (let ((fill-prefix (move-mode-adaptive-fill))
        (fill-paragraph-handle-comment t)
        (fill-paragraph-function
         (unless (eq fill-paragraph-function #'move-mode-fill-paragraph)
           fill-paragraph-function)))
    (apply #'fill-paragraph args) t))

(defun move-mode-auto-fill (&rest args)
  "Move comment-aware wrapper for `do-auto-fill'.

ARGS are forwarded to a call of `do-auto-fill', as-is."
  (let ((fill-prefix (move-mode-adaptive-fill)))
    (apply #'do-auto-fill args) t))

(defun move-mode-adaptive-fill ()
  "Pick the `fill-prefix' based on context.

If the point is currently in a comment, return the fill prefix to us to continue
that comment, otherwise return the existing `fill-prefix'."
  (save-match-data
    (save-excursion
      (if (not (move--ppss-in-comment))
          fill-prefix
        (let* ((comment-start
                (move--ppss-comment-start))
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

(defun move--expand-compilation-source ()
  "Resolve compiler error/warning files relative to `compilation-directory'."
  (expand-file-name (match-string-no-properties 1) compilation-directory))

(defun move--compilation-start (sub-command &rest args)
  "Run a `move' sub-command from the Move project root.

Invokes `move-bin' with `move-default-arguments' SUB-COMMAND, and ARGS."
  (let* ((compilation-directory
          (locate-dominating-file default-directory "Move.toml")))
    (compilation-start
     (combine-and-quote-strings
      (append (list move-bin sub-command)
              (split-string-and-unquote move-default-arguments)
              args))
     'move-compilation-mode)))

(defun move--register-builtins ()
  "Generate a font-lock matcher form for built-in constructs.

The list of built-ins is specified via the `move-builtins' custom variable."
  `(,(regexp-opt move-builtins 'symbols) . font-lock-builtin-face))

(defun move--ppss-inner-paren ()
  "Character address of innermost containing list, or nil if none."
  (nth 1 (syntax-ppss)))

(defun move--ppss-in-comment ()
  "Whether or not the cursor is within a comment.

NIL if outside a comment, T if inside a non-nestable comment, or an integer --
the level of nesting -- if inside a nestable comment."
  (nth 4 (syntax-ppss)))

(defun move--ppss-comment-start ()
  "Character address for start of comment or string."
  (nth 8 (syntax-ppss)))

(defun move--prev-assignment (bound)
  "Find the previous assignment character after BOUND.

Search backwards from the current point until BOUND looking for an `='
character that isn't in a comment.  Returns T on success, with the point over
the character, and NIL otherwise with the point at an indeterminate position."
  (and (search-backward "=" bound t)
       (or (not (move--ppss-in-comment))
           (move--prev-assignment bound))))

(defun move--next-terminator (bound)
  "Find the next statement terminator before BOUND.

Search forwards from the current point until BOUND looking for a `;' character
that isn't in a comment.  Returns T on success, with the point over the
character, and NIL otherwise with the point at an indeterminate position."
  (and (search-forward ";" bound t)
       (or (not (move--ppss-in-comment))
           (move--next-terminator bound))))

(defun move--indent-column ()
  "calculates the column to indent the current line to.

the default indent is `move-indent-offset' greater than the indent of the line
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
           (parent-paren   (move--ppss-inner-paren))
           (default-indent (if (not parent-paren) 0
                             (save-excursion
                               (goto-char parent-paren)
                               (back-to-indentation)
                               (+ (current-column) move-indent-offset)))))
      (cond
       ;; `/*'-style comment continuation lines
       ((and (move--ppss-in-comment)
             (looking-at "*"))
        (+ default-indent 1))

       ;; top-level items will remain completely unindented.
       ((= default-indent 0) 0)

       ;; closing parentheses
       ((looking-at "[]})]")
        (- default-indent move-indent-offset))

       ;; assignment continuation lines (corrected handling for braces)
       ((save-excursion
          (and parent-paren
               (not (save-excursion (goto-char parent-paren)
                                   (looking-at "{")))
               (move--prev-assignment parent-paren)
               (not (move--next-terminator current-posn))))
        (+ default-indent move-indent-offset))

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

(defun move--ansi-color-compilation-filter ()
  "Backport ANSI color compilation filter to support earlier versions of Emacs."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(provide 'move-mode)
;;; move-mode eldocs <<<

(defvar move-function-docs
  '(("aptos_framework" . "This is the reference documentation of the Aptos framework.")
    ("key" . "Value can be used as a key for global storage operations")
    ("copy" . "Value can be copied ( or cloned by value )")
    ("drop" . "Value can be dropped by the end of scope")
    ("store" . "value can be stored inside global storage")
    ;; Object
    ("object" . "This defines the Move object model with the following properties:\n
+ Simplified storage interface that supports a heterogeneous collection of resources to be stored together. This enables data types to share a common core data layer (e.g., tokens), while having richer extensions (e.g., concert ticket, sword).
+ Globally accessible data and ownership model that enables creators and developers to dictate the application and lifetime of data.
+ Extensible programming model that supports individualization of user applications that leverage the core framework including tokens.
+ Support emitting events directly, thus improving discoverability of events associated with objects.
+ Considerate of the underlying system by leveraging resource groups for gas efficiency, avoiding costly deserialization and serialization costs, and supporting deletability.")
    ("Object" . "A pointer to an object -- these can only provide guarantees based upon the underlying data type, that is the validity of T existing at an address is something that cannot be verified by any other module than the module that defined T. Similarly, the module that defines T can remove it from storage at any point in time")
    ("ConstructorRef" . "This is a one time ability given to the creator to configure the object as necessary")
    ("create_named_object" . "Create a new named object and return the ConstructorRef. Named objects can be queried globally by knowing the user generated seed used to create them. Named objects cannot be deleted.\n
public fun create_named_object(creator: &signer, seed: vector<u8>): object::ConstructorRef")
    ("create_user_derived_object" . "Create a new object whose address is derived based on the creator account address and another object. Derivde objects, similar to named objects, cannot be deleted.\n
public(friend) fun create_user_derived_object(creator_address: address, derive_ref: &object::DeriveRef): object::ConstructorRef")
    ("create_object" . "Create a new object by generating a random unique address based on transaction hash. The unique address is computed sha3_256([transaction hash | auid counter | 0xFB]). The created object is deletable as we can guarantee the same unique address can never be regenerated with future txs.\n
public fun create_object(owner_address: address): object::ConstructorRef")
    ("DeleteRef" . "Used to remove an object from storage")
    ("ExtendRef" . "Used to create events or move additional resources into object storage")
    ("TransferRef" . "Used to create LinearTransferRef, hence ownership transfer.")
    ("LinearTransferRef" . "Used to perform transfers. This locks transferring ability to a single time use bound to the current owner")
    ("DeriveRef" . "Used to create derived objects from a given objects.")
    ("TransferEvent" . "Emitted whenever the object's owner field is changed.")
    ("Transfer" . "Emitted whenever the object's owner field is changed.")
    ("object_from_constructor_ref" . "Returns an Object from within a ConstructorRef\n
public fun object_from_constructor_ref<T: key>(ref: &object::ConstructorRef): object::Object<T>")
    ("address_from_constructor_ref" . "Returns the address associated with the constructor\n
public fun address_from_constructor_ref(ref: &object::ConstructorRef): address")

    ;; Primary Fungible Store
    ("primary_fungible_store" . "This module provides a way for creators of fungible assets to enable support for creating primary (deterministic) stores for their users. This is useful for assets that are meant to be used as a currency, as it allows users to easily create a store for their account and deposit/withdraw/transfer fungible assets to/from it.\n
The transfer flow works as below:\n
    1. The sender calls transfer on the fungible asset metadata object to transfer amount of fungible asset to recipient.
    2. The fungible asset metadata object calls ensure_primary_store_exists to ensure that both the sender's and the recipient's primary stores exist. If either doesn't, it will be created.
    3. The fungible asset metadata object calls withdraw on the sender's primary store to withdraw amount of fungible asset from it. This emits a withdraw event.
    4. The fungible asset metadata object calls deposit on the recipient's primary store to deposit amount of fungible asset to it. This emits an deposit event.")

    ("create_primary_store_enabled_fungible_asset" . "Create a fungible asset with primary store support. When users transfer fungible assets to each other, their primary stores will be created automatically if they don't exist. Primary stores have deterministic addresses so that users can easily deposit/withdraw/transfer fungible assets.\n
public fun create_primary_store_enabled_fungible_asset(
   constructor_ref: &object::ConstructorRef,
   maximum_supply: option::Option<u128>,
   name: string::String,
   symbol: string::String,
   decimals: u8,
   icon_uri: string::String,
   project_uri: string::String
)\n
public fun create_primary_store_enabled_fungible_asset(
    constructor_ref: &ConstructorRef,
    maximum_supply: Option<u128>,
    name: String,
    symbol: String,
    decimals: u8,
    icon_uri: String,
    project_uri: String,
) {
    fungible_asset::add_fungibility(
        constructor_ref,
        maximum_supply,
        name,
        symbol,
        decimals,
        icon_uri,
        project_uri,
    );
    let metadata_obj = &object::generate_signer(constructor_ref);
    move_to(metadata_obj, DeriveRefPod {
        metadata_derive_ref: object::generate_derive_ref(constructor_ref),
    });
}")
    ))

(defun move-get-function-doc (symbol)
  "Lấy mô tả của hàm SYMBOL từ move-function-docs."
  (cdr (assoc symbol move-function-docs)))

(defun move-eldoc-function ()
  "Hàm để lấy thông tin eldoc cho `move-mode`."
  (let ((symbol (thing-at-point 'symbol)))
    (move-get-function-doc symbol)))

(add-hook 'move-mode-hook
          (lambda ()
            (setq-local eldoc-documentation-function #'move-eldoc-function)
            (eldoc-mode 1)))
;;; move-mode eldocs <<<
