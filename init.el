;; -*- lexical-binding: t; -*-
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defvar native-comp-deferred-compilation-deny-list ())
(defvar native-comp-jit-compilation-deny-list ())
(defvar comp-deferred-compilation-deny-list ()) ; workaround, otherwise straight shits itself
(customize-set-variable 'native-comp-speed 3)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq straight-host-usernames
      '((github . "jayden-dang")
        (gitlab . "jayden-dang")))

(setq straight-vc-git-default-remote-name "straight")

(straight-use-package '(use-package :build t))
(setq use-package-always-ensure t)

(auth-source-pass-enable)
(setq epg-gpg-program "gpg2")

(customize-set-variable 'epg-pinentry-mode 'loopback)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

  ;; Add my library path to load-path
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (add-to-list 'load-path "~/.emacs.d/lisp/screenshot.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/oauth2.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/cadence-mode")
  (add-to-list 'load-path "~/.emacs.d/lisp/cadence-mode/cadence-mode.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/solidity/solidity-mode.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/move-mode/move-mode.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/move-mode")
  (add-to-list 'load-path "~/.emacs.d/lisp/func-mode")
  (add-to-list 'load-path "~/.emacs.d/lisp/func-mode/func-mode.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/tolk-mode")
  (add-to-list 'load-path "~/.emacs.d/lisp/tolk-mode/tolk-mode.el")
  (add-to-list 'load-path "~/.emacs.d/lisp/maple-iedit")
  (add-to-list 'load-path "~/.emacs.d/lisp/protobuf-mode/")

  (require 'cadence-mode)
  (require 'solidity-mode)
  (require 'oauth2)
  (require 'screenshot)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)

  ;; Tốc độ cuộn (1.0 là mặc định)
  (setq pixel-scroll-precision-interpolation-factor 1.0)

  ;; Độ trễ (đơn vị giây, mặc định là 0.01)
  (setq pixel-scroll-precision-interpolation-between-scroll 0.01)

  ;; Làm mượt cuộn chuột
  (setq pixel-scroll-precision-use-momentum t)

  ;; Tốc độ giảm dần sau khi cuộn
  (setq pixel-scroll-precision-momentum-min-velocity 5.0)

  ;; Các thiết lập khác tùy theo sở thích
  (setq pixel-scroll-precision-momentum-seconds 0.9)
  (setq pixel-scroll-precision-momentum-tick 0.01)
)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.emacs.d/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(add-hook 'before-save-hook #'whitespace-cleanup)
(server-start)

(setq-default sentence-end-double-space nil)

(setq-default initial-major-mode 'emacs-lisp-mode)

(setq switch-to-buffer-obey-display-actions t)

(dolist (mode '(prog-mode-hook latex-mode-hook))
  (add-hook mode #'display-line-numbers-mode))

(dolist (mode '(prog-mode-hook latex-mode-hook))
  (add-hook mode #'hs-minor-mode))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
                                                         user-emacs-directory))))

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode 1)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; (defun dqv/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 150
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :straight (:build t)
;;   :hook (org-mode . dqv/org-mode-visual-fill))

(setq user-full-name       "Dang Quang Vu"
      user-real-login-name "Dang Quang Vu"
      user-login-name      "jaydendang"
      user-mail-address    "jayden.dangvu@gmail.com")

(setq visible-bell t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq x-stretch-cursor t)

(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "…"))

;; (require 'time)
;; (setq display-time-format "%Y-%m-%d %H:%M")
;; (display-time-mode 1) ; display time in modeline

(let ((battery-str (battery)))
  (unless (or (equal "Battery status not available" battery-str)
              (string-match-p (regexp-quote "N/A") battery-str))
    (display-battery-mode 1)))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun modeline-contitional-buffer-encoding ()
  "Hide \"LF UTF-8\" in modeline.

It is expected of files to be encoded with LF UTF-8, so only show
the encoding in the modeline if the encoding is worth notifying
the user."
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'modeline-contitional-buffer-encoding)

    (set-face-attribute 'default nil
                        ;; :font "JetBrains Mono"
                        :font "Iosevka"
                        :weight 'Light
                        :height 150)

    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil
                        ;; :font "JetBrains Mono"
                        :font "Iosevka"
                        :weight 'Light
                        :height 150)


    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil
                        ;; :font "Cantarell"
                        :font "Iosevka Aile"
                        :weight 'Light
                        :height 150)

;;(set-fontset-font t 'symbol "Noto Color Emoji")
;;(set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package emojify
  :straight (:build t)
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat user-emacs-directory "emojify/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t)
  :config
  (global-emojify-mode 1))

;; Use moody for the mode bar
(use-package moody
  :straight (:build t)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :straight (:build t)
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(setq evil-insert-state-cursor '((bar . 2) "orange")
      evil-normal-state-cursor '(box "orange"))

(defmacro csetq (&rest forms)
  "Bind each custom variable FORM to the value of its VAL.

FORMS is a list of pairs of values [FORM VAL].
`customize-set-variable' is called sequentially on each pair
contained in FORMS. This means `csetq' has a similar behavior as
`setq': each VAL expression is evaluated sequentially, i.e., the
first VAL is evaluated before the second, and so on. This means
the value of the first FORM can be used to set the second FORM.

The return value of `csetq' is the value of the last VAL.

\(fn [FORM VAL]...)"
  (declare (debug (&rest sexp form))
           (indent 1))
  ;; Check if we have an even number of arguments
  (when (= (mod (length forms) 2) 1)
    (signal 'wrong-number-of-arguments (list 'csetq (1+ (length forms)))))
  ;; Transform FORMS into a list of pairs (FORM . VALUE)
  (let (sexps)
    (while forms
      (let ((form  (pop forms))
            (value (pop forms)))
        (push `(customize-set-variable ',form ,value)
              sexps)))
    `(progn ,@(nreverse sexps))))

(use-package git-gutter-fringe
  :straight (:build t)
  :hook ((prog-mode     . git-gutter-mode)
         (org-mode      . git-gutter-mode)
         (markdown-mode . git-gutter-mode)
         (latex-mode    . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package all-the-icons
  :defer t
  :straight t)

(defun prog-mode-set-symbols-alist ()
  (setq prettify-symbols-alist '(("lambda"  . ?λ)
                                 ("null"    . ?∅)
                                 ("NULL"    . ?∅)))
  (prettify-symbols-mode 1))

(add-hook 'prog-mode-hook #'prog-mode-set-symbols-alist)

(setq-default lisp-prettify-symbols-alist '(("lambda"    . ?λ)
                                            ("defun"     . ?𝑓)
                                            ("defvar"    . ?𝑣)
                                            ("defcustom" . ?𝑐)
                                            ("defconst"  . ?𝐶)))

(defun lisp-mode-prettify ()
  (setq prettify-symbols-alist lisp-prettify-symbols-alist)
  (prettify-symbols-mode -1)
  (prettify-symbols-mode 1))

(dolist (lang '(emacs-lisp lisp common-lisp scheme))
  (add-hook (intern (format "%S-mode-hook" lang))
            #'lisp-mode-prettify))

(setq prettify-symbols-unprettify-at-point t)

(use-package ligature
  :straight (ligature :type git
                      :host github
                      :repo "mickeynp/ligature.el"
                      :build t)
  :config
  (ligature-set-ligatures 't
                          '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures '(eww-mode org-mode elfeed-show-mode)
                          '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  (global-ligature-mode t))

(use-package valign
  :defer t
  :straight (:build t)
  :after (org markdown-mode)
  :hook ((org-mode markdown-mode) . valign-mode)
  :custom ((valign-fancy-bar t)))

(use-package solaire-mode
  :defer t
  :straight (:build t)
  :init (solaire-global-mode +1))

(use-package modus-themes
  ;; :straight (modus-themes :type built-in)
  :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-custom-auto-reload t
        modus-themes-disable-other-themes t
        modus-themes-common-palette-overrides '((bg-mode-line-active bg-yellow-subtle)
                                                (bg-main "#120B11")
                                                (border-mode-line-active yellow)
                                                (comment yellow-faint)
                                                (constant magenta-cooler)
                                                (docmarkup magenta-faint)
                                                (docstring green-faint)
                                                (fg-mode-line-active fg-main)
                                                (fnname magenta-warmer)
                                                (fnname green-cooler)
                                                (keyword cyan)
                                                (keyword yellow)
                                                (preprocessor cyan-cooler)
                                                (rx-backslash blue-cooler)
                                                (rx-construct magenta-warmer)
                                                (string green-cooler)
                                                (type magenta-cooler)
                                                (variable blue-warmer)
                                                (builtin magenta))
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))
        modus-themes-headings
        '((1 . (variable-pitch light 1.6))
          (2 . (overline semibold 1.5))
          (3 . (monochrome overline 1.4 background))
          (4 . (overline 1.3))
          (5 . (rainbow 1.2))
          (6 . (rainbow 1.15))
          (t . (rainbow 1.1)))))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

(use-package doom-themes
  :straight (:build t)
  :ensure t
  :config
  ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-frappe t)
  ;; (load-theme 'catppuccin-macchiato t)
  (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'rose-pine t)
  ;; (load-theme 'amarena t)
  ;; (load-theme 'oxocarbon t)
  ;; (load-theme 'kman t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'doom-tokyo-night t)
  ;; (load-theme 'modus-vivendi t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  :config
  ;; FIX Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
          remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
                  repl lsp minor-modes input-method indent-info buffer-encoding major-mode
                  process vcs checker time "   ")))
(setq evil-normal-state-tag   (propertize "[NORMAL]" 'face '((:background "lightgreen" :foreground "black")))
      evil-emacs-state-tag    (propertize "[EMACS]" 'face '((:background "yellow" :foreground "black")))
      evil-insert-state-tag   (propertize "[INSERT]" 'face '((:background "red") :foreground "white"))
      evil-motion-state-tag   (propertize "[MOTION]" 'face '((:background "blue") :foreground "white"))
      evil-visual-state-tag   (propertize "[VISUAL]" 'face '((:background "orange" :foreground "black")))
      evil-operator-state-tag (propertize "[OPERATOR]" 'face '((:background "purple"))))

(use-package rainbow-delimiters
  :straight (:build t)
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package info-colors
  :straight (:build t)
  :commands info-colors-fnontify-node
  :hook (Info-selection . info-colors-fontify-node)
  :hook (Info-mode      . mixed-pitch-mode))

(defun dqv/open-marked-files (&optional files)
  "Open all marked FILES in Dired buffer as new Emacs buffers."
  (interactive)
  (let* ((file-list (if files
                        (list files)
                      (if (equal major-mode "dired-mode")
                          (dired-get-marked-files)
                        (list (buffer-file-name))))))
    (mapc (lambda (file-path)
            (find-file file-path))
          (file-list))))

(defun add-all-to-list (list-var elements &optional append compare-fn)
  "Add ELEMENTS to the value of LIST-VAR if it isn’t there yet.

ELEMENTS is a list of values. For documentation on the variables
APPEND and COMPARE-FN, see `add-to-list'."
  (let (return)
    (dolist (elt elements return)
      (setq return (add-to-list list-var elt append compare-fn)))))

(defun scroll-half-page-up ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-down ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun dqv/switch-to-previous-buffer ()
  "Switch to previously open buffer.
        Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

        Move point to the first non-whitespace character on this line.
        If point is already there, move to the beginning of the line.
        Effectively toggle between the first non-whitespace character and
        the beginning of the line.

        If ARG is not nil or 1, move forward ARG - 1 lines first.  If
        point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'

(defun dqv/goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % ."
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))
(global-set-key (kbd "s-;") #'dqv/goto-match-paren)

(defun dqv/delete-this-file (&optional trash)
  "Delete this file.

When called interactively, TRASH is t if no prefix argument is given.
With a prefix argument, TRASH is nil."
  (interactive)
  (when (and (called-interactively-p 'interactive)
             (not current-prefix-arg))
    (setq trash t))
  (if-let ((file (buffer-file-name)))
      (when (y-or-n-p "Delete this file? ")
        (delete-file file trash)
        (kill-buffer (current-buffer)))
    (user-error "Current buffer is not visiting a file")))

(defun dqv/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package which-key
  :straight (:build t)
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package which-key-posframe
  :config
  (which-key-posframe-mode))

(use-package general
  :straight (:build t)
  :init
  (general-auto-unbind-keys)
  :config
  (general-create-definer dqv/underfine
    :keymaps 'override
    :states '(normal emacs))
  (general-create-definer dqv/evil
    :states '(normal))
  (general-create-definer dqv/leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer dqv/major-leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-m"))

(use-package evil
  :straight (:build t)
  :after (general)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  (require 'evil-vars)
  (evil-set-undo-system 'undo-tree)
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "w" 'evil-avy-goto-word-1)
  (global-set-key (kbd "s-'") #'evil-window-next)
  
  (general-define-key
   :keymaps 'evil-motion-state-map
   "SPC" nil
   ","   nil)
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-t" nil)
  (general-define-key
   :keymaps 'evil-insert-state-map
   "U"   nil
   "C-a" nil
   "C-y" nil
   "C-e" nil)
  ;; (dolist (key '("c" "C" "t" "T" "s" "S" "r" "R" "h" "H" "j" "J" "k" "K" "l" "L"))
  ;;   (general-define-key :states 'normal key nil))
  
  ;; (general-define-key
  ;;  :states 'motion
  ;;  "h" 'evil-replace
  ;;  "H" 'evil-replace-state
  ;;  "j" 'evil-find-char-to
  ;;  "J" 'evil-find-char-to-backward
  ;;  "k" 'evil-substitute
  ;;  "K" 'evil-smart-doc-lookup
  ;;  "l" 'evil-change
  ;;  "L" 'evil-change-line
  
  ;;  "c" 'evil-backward-char
  ;;  "C" 'evil-window-top
  ;;  "t" 'evil-next-visual-line
  ;;  "T" 'evil-join
  ;;  "s" 'evil-previous-visual-line
  ;;  "S" 'evil-lookup
  ;;  "r" 'evil-forward-char
  ;;  "R" 'evil-window-bottom)
  (evil-mode 1)
  (setq evil-want-fine-undo t) ; more granular undo with evil
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :straight (:build t)
  :config
  ;; jayden conversion
  ;; (defun my/jd-rotate-evil-collection (_mode mode-keymaps &rest _rest)
  ;;   (evil-collection-translate-key 'normal mode-keymaps
  ;;     ;; jd ctsr is qwerty hjkl
  ;;     "c" "h"
  ;;     "t" "j"
  ;;     "s" "k"
  ;;     "r" "l"
  ;;     ;; add back ctsr
  ;;     "h" "c"
  ;;     "j" "t"
  ;;     "k" "s"
  ;;     "l" "r"))
  ;; (add-hook 'evil-collection-setup-hook #'my/jd-rotate-evil-collection)
  (evil-collection-init))

(use-package undo-tree
  :defer t
  :straight (:build t)
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name (file-name-as-directory "undo-tree-hist")
                               user-emacs-directory))))
  :init
  (global-undo-tree-mode)
  :config
  
  ;; (when (executable-find "zstd")
  ;;   (defun my/undo-tree-append-zst-to-filename (filename)
  ;;     "Append .zst to the FILENAME in order to compress it."
  ;;     (concat filename ".zst"))
  ;;   (advice-add 'undo-tree-make-history-save-file-name
  ;;               :filter-return
  ;;               #'my/undo-tree-append-zst-to-filename))
  (setq undo-tree-visualizer-diff       t
        undo-tree-auto-save-history     t
        undo-tree-enable-undo-in-region t
        undo-limit        (* 800 1024)
        undo-strong-limit (* 12 1024 1024)
        undo-outer-limit  (* 128 1024 1024)))

(use-package hydra
  :straight (:build t)
  :defer t)

(defhydra windows-adjust-size ()
  "
^Zoom^                                ^Other
^^^^^^^-----------------------------------------
[_j_/_k_] shrink/enlarge vertically   [_q_] quit
[_l_/_h_] shrink/enlarge horizontally
"
  ("q" nil :exit t)
  ("l" shrink-window-horizontally)
  ("j" enlarge-window)
  ("k" shrink-window)
  ("h" enlarge-window-horizontally))

(use-package citeproc
  :after (org)
  :defer t
  :straight (:build t))

  (use-package org
      :straight t
      :defer t
      :commands (orgtbl-mode)
      :hook (org-mode . visual-line-mode)
      ;; (org-mode . org-num-mode))
      :custom-face
      (org-macro ((t (:foreground "#b48ead"))))
      :init
      (auto-fill-mode -1)
      :config
      (defhydra org-babel-transient ()
        "
      ^Navigate^                    ^Interact
      ^^^^^^^^^^^------------------------------------------
      [_j_/_k_] navigate src blocs  [_x_] execute src block
      [_g_]^^   goto named block    [_'_] edit src block
      [_z_]^^   recenter screen     [_q_] quit
      "
        ("q" nil :exit t)
        ("j" org-babel-next-src-block)
        ("k" org-babel-previous-src-block)
        ("g" org-babel-goto-named-src-block)
        ("z" recenter-top-bottom)
        ("x" org-babel-execute-maybe)
        ("'" org-edit-special :exit t))
      (require 'ox-beamer)
      (require 'org-protocol)
      (setq org-hide-leading-stars             nil
            org-hide-macro-markers             t
            org-ellipsis                       " ⤵"
            org-image-actual-width             1200
            org-image-actual-height            1000
            org-redisplay-inline-images        t
            org-display-inline-images          t
            org-startup-with-inline-images     "inlineimages"
            org-pretty-entities                t
            org-fontify-whole-heading-line     t
            org-fontify-done-headline          t
            org-fontify-quote-and-verse-blocks t
            org-startup-indented               t
            org-startup-align-all-tables       t
            org-use-property-inheritance       t
            org-list-allow-alphabetical        t
            org-M-RET-may-split-line           nil
            org-src-window-setup               'split-window-right
            org-src-fontify-natively           t
            org-src-tab-acts-natively          t
            org-src-preserve-indentation       t
            org-log-done                       'time
            org-directory                      "~/Dropbox/Org"
            org-default-notes-file             (expand-file-name "notes.org" org-directory))
      (with-eval-after-load 'oc
        (setq org-cite-global-bibliography '("~/Dropbox/Org/bibliography/references.bib")))
      (setq org-agenda-files (list "~/Dropbox/Org/" "~/Dropbox/Roam/" "~/Dropbox/Roam/blockchain/" "~/Dropbox/Roam/daily"))
      (add-hook 'org-mode-hook (lambda ()
                                 (interactive)
                                 (electric-indent-local-mode -1)))
      (defvar org-personal-file "~/Dropbox/Org/Personal.org")
      (defvar org-vocabulary-file "~/Dropbox/Org/Vocabulary.org")
      (setq org-capture-templates
            '(
              ("p" "Personal" entry
                (file+headline org-personal-file "Personal")
                (file "~/.emacs.d/capture/schedule.orgcaptmpl"))
              ("v" "Vocabulary")))
      (defun org-emphasize-bold ()
        "Emphasize as bold the current region."
        (interactive)
        (org-emphasize 42))
      (defun org-emphasize-italic ()
        "Emphasize as italic the current region."
        (interactive)
        (org-emphasize 47))
      (defun org-emphasize-underline ()
        "Emphasize as underline the current region."
        (interactive)
        (org-emphasize 95))
      (defun org-emphasize-verbatim ()
        "Emphasize as verbatim the current region."
        (interactive)
        (org-emphasize 61))
      (defun org-emphasize-code ()
        "Emphasize as code the current region."
        (interactive)
        (org-emphasize 126))
      (defun org-emphasize-strike-through ()
        "Emphasize as strike-through the current region."
        (interactive)
        (org-emphasize 43))
      
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((C . t)
         (emacs-lisp . t)
         (gnuplot . t)
         (latex . t)
         (makefile . t)
         (restclient . t)
         (js . t)
         (plantuml . t)
         (python . t)
         (sass . t)
         (shell . t)
         (sql . t)))
      (setq org-use-sub-superscripts (quote {}))
      (setq org-latex-compiler "xelatex")
      (require 'engrave-faces)
      (csetq org-latex-src-block-backend 'engraved)
      (dolist (package '(("AUTO" "inputenc" t ("pdflatex"))
                         ("T1"   "fontenc"  t ("pdflatex"))
                         (""     "grffile"  t)))
        (delete package org-latex-default-packages-alist))
      
      (dolist (package '(("capitalize" "cleveref")
                         (""           "booktabs")
                         (""           "tabularx")))
        (add-to-list 'org-latex-default-packages-alist package t))
      
      (setq org-latex-reference-command "\\cref{%s}")
      (setq org-export-latex-hyperref-format "\\ref{%s}")
      ;; (setq org-latex-pdf-process
      ;;       '("tectonic -Z shell-escape --synctex --outdir=%o %f"))
      (setq org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                    "%bibtex -output-directory %o %f"
                                    "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                    "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")
            org-latex-remove-logfiles t
            org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls"
                                            "figlist" "idx" "log" "nav" "out" "ptc"
                                            "run.xml" "snm" "toc" "vrb" "xdv"))
      (dolist (ext '("bbl" "lot"))
        (add-to-list 'org-latex-logfiles-extensions ext t))
      (use-package org-re-reveal
        :defer t
        :after org
        :straight (:build t)
        :init
        (add-hook 'org-mode-hook (lambda () (require 'org-re-reveal)))
        :config
        (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
              org-re-reveal-revealjs-version "4"))
      (setq org-html-validation-link nil)
      (eval-after-load "ox-latex"
        '(progn
           (add-to-list 'org-latex-classes
                        '("conlang"
                          "\\documentclass{book}"
                          ("\\chapter{%s}" . "\\chapter*{%s}")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
           (add-to-list 'org-latex-classes
                        `("beamer"
                          ,(concat "\\documentclass[presentation]{beamer}\n"
                                   "[DEFAULT-PACKAGES]"
                                   "[PACKAGES]"
                                   "[EXTRA]\n")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
      
      
      (setq org-publish-project-alist
            `(
              
              
              
              
              
              
              ))
      :general
      (dqv/evil
        :keymaps 'org-mode-map
        :packages 'org
        "RET" 'org-open-at-point)
      (dqv/major-leader-key
        :keymaps 'org-mode-map
        :packages 'org
        ;; Various
        "RET" #'org-ctrl-c-ret
        "*" #'org-ctrl-c-star
        "'" #'org-edit-special
        "-" #'org-ctrl-c-minus
        "a" #'org-agenda
        "c" #'org-capture
        "C" #'org-columns
        "e" #'org-export-dispatch
        "l" #'org-store-link
        "p" #'org-priority
        "r" #'org-reload
        ;; Babels
        "b" '(:ignore t :which-key "babel")
        "b." #'org-babel-transient/body
        "bb" #'org-babel-execute-buffer
        "bc" #'org-babel-check-src-block
        "bC" #'org-babel-tangle-clean
        "be" #'org-babel-execute-maybe
        "bf" #'org-babel-tangle-file
        "bn" #'org-babel-next-src-block
        "bo" #'org-babel-open-src-block-result
        "bp" #'org-babel-previous-src-block
        "br" #'org-babel-remove-result-one-or-many
        "bR" #'org-babel-goto-named-result
        "bt" #'org-babel-tangle
        "bi" #'org-babel-view-src-block-info
        ;; Dates
        "d" '(:ignore t :which-key "Dates")
        "dd" #'org-deadline
        "ds" #'org-schedule
        "dt" #'org-time-stamp
        "dT" #'org-time-stramp-inactive
        ;; Insert
        "i" '(:ignore t :which-key "Insert")
        "ib" #'org-insert-structure-template
        "id" #'org-insert-drawer
        "ie" '(:ignore t :which-key "Emphasis")
        "ieb" #'org-emphasize-bold
        "iec" #'org-emphasize-code
        "iei" #'org-emphasize-italic
        "ies" #'org-emphasize-strike-through
        "ieu" #'org-emphasize-underline
        "iev" #'org-emphasize-verbatim
        "iE" #'org-set-effort
        "if" #'org-footnote-new
        "ih" #'org-insert-heading
        "iH" #'counsel-org-link
        "ii" #'org-insert-item
        "il" #'org-insert-link
        "in" #'org-add-note
        "ip" #'org-set-property
        "is" #'org-insert-subheading
        "it" #'org-set-tags-command
        ;; Tables
        "t" '(:ignore t :which-key "Table")
        "th" #'org-table-move-column-left
        "tj" #'org-table-move-row-down
        "tk" #'org-table-move-row-up
        "tl" #'org-table-move-column-right
        "ta" #'org-table-align
        "te" #'org-table-eval-formula
        "tf" #'org-table-field-info
        "tF" #'org-table-edit-formulas
        "th" #'org-table-convert
        "tl" #'org-table-recalculate
        "tp" #'org-plot/gnuplot
        "tS" #'org-table-sort-lines
        "tw" #'org-table-wrap-region
        "tx" #'org-table-shrink
        "tN" #'org-table-create-with-table.el
        "td" '(:ignore t :which-key "Delete")
        "tdc" #'org-table-delete-column
        "tdr" #'org-table-kill-row
        "ti" '(:ignore t :which-key "Insert")
        "tic" #'org-table-insert-column
        "tih" #'org-table-insert-hline
        "tir" #'org-table-insert-row
        "tiH" #'org-table-hline-and-move
        "tt" '(:ignore t :which-key "Toggle")
        "ttf" #'org-table-toggle-formula-debugger
        "tto" #'org-table-toggle-coordinate-overlays
        ;; Toggle
        "T" '(:ignore t :which-key "Toggle")
        "Tc" #'org-toggle-checkbox
        "Ti" #'org-toggle-inline-images
        "Tl" #'org-latex-preview
        "Tn" #'org-num-mode
        "Ts" #'dqv/toggle-org-src-window-split
        "Tt" #'org-show-todo-tree
        "<SPC>" #'org-todo
        )


      (dqv/leader-key
        :packages 'org
        :infix "o"
        ""  '(:ignore t :which-key "org")
        "c" #'org-capture)
      (dqv/major-leader-key
        :packages 'org
        :keymaps 'org-src-mode-map
        "'" #'org-edit-src-exit
        "k" #'org-edit-src-abort))

(defun dqv/my-open-urls-in-region (beg end)
  "Open URLs between BEG and END.
        TODO: Get better at detecting and opening all URLs"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-any-link-re nil t)
        (save-excursion
          (backward-char)
          (org-open-at-point))))))

(use-package evil-org
  :straight (:build t)
  :after (org)
  :hook (org-mode . evil-org-mode)
  :config
  (setq-default evil-org-movement-bindings
                '((up    . "k")
                  (down  . "j")
                  (left  . "h")
                  (right . "l")))
  (evil-org-set-key-theme '(textobjects navigation calendar additional shift operators))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-contrib
  :after (org)
  :defer t
  :straight (:build t)
  :init
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(setq ob-mermaid-cli-path "/Users/dangeamon/.nvm/versions/node/v22.5.1/bin/mmdc")
(use-package ob-async
  :straight (:build t)
  :defer t
  :after (org ob))

(use-package ob-latex-as-png
  :after org
  :straight (:build t))

(use-package restclient
  :straight (:build t)
  :defer t)
(setq url-debug t)

(use-package ob-restclient
  :straight (:build t)
  :defer t
  :after (org ob)
  :init
  (add-to-list 'org-babel-load-languages '(restclient . t)))

(use-package toc-org
  :straight (:build t)
  :after (org)
  :init
  (add-to-list 'org-tag-alist '("TOC" . ?T))
  :hook (org-mode . toc-org-mode)
  :hook (org-mode . toc-org-enable)
  :hook (markdown-mode . toc-org-enable))

(defun dqv/toggle-org-src-window-split ()
  "This function allows the user to toggle the behavior of
`org-edit-src-code'. If the variable `org-src-window-setup' has
the value `split-window-right', then it will be changed to
`split-window-below'. Otherwise, it will be set back to
`split-window-right'"
  (interactive)
  (if (equal org-src-window-setup 'split-window-right)
      (setq org-src-window-setup 'split-window-below)
    (setq org-src-window-setup 'split-window-right))
  (message "Org-src buffers will now split %s"
           (if (equal org-src-window-setup 'split-window-right)
               "vertically"
             "horizontally")))

(use-package ox-epub
  :after (org ox)
  :straight (:build t))

(use-package ox-gemini
  :defer t
  :straight (:build t)
  :after (ox org))

;; (use-package htmlize
;;   :defer t
;;   :straight (:build t))

(use-package preview-org-html-mode
  :defer t
  :after (org)
  :straight (preview-org-html-mode :build t
                                   :type git
                                   :host github
                                   :repo "jakebox/preview-org-html-mode")
  :general
  (dqv/major-leader-key
    :keymaps 'org-mode-map
    :packages 'preview-org-html-mode
    :infix "P"
    ""  '(:ignore t :which-key "preview")
    "h" #'preview-org-html-mode
    "r" #'preview-org-html-refresh
    "p" #'preview-org-html-pop-window-to-frame)
  :config
  (setq preview-org-html-refresh-configuration 'save))

(use-package engrave-faces
  :defer t
  :straight (:build t)
  :after org)

(use-package org-re-reveal
  :defer t
  :after org
  :straight (:build t)
  :init
  (add-hook 'org-mode-hook (lambda () (require 'org-re-reveal)))
  :config
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
        org-re-reveal-revealjs-version "4"))

(use-package ox-ssh
  :after (ox org)
  :straight (:build t))

'("conlang"
  "\\documentclass{book}"
  ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

`("beamer"
  ,(concat "\\documentclass[presentation]{beamer}\n"
           "[DEFAULT-PACKAGES]"
           "[PACKAGES]"
           "[EXTRA]\n")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

(eval-after-load "ox-latex"
  '(progn
     (add-to-list 'org-latex-classes
                  '("conlang"
                    "\\documentclass{book}"
                    ("\\chapter{%s}" . "\\chapter*{%s}")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
     (add-to-list 'org-latex-classes
                  `("beamer"
                    ,(concat "\\documentclass[presentation]{beamer}\n"
                             "[DEFAULT-PACKAGES]"
                             "[PACKAGES]"
                             "[EXTRA]\n")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(setq org-publish-project-alist
      `(
        
        
        
        
        
        
        ))

(use-package reftex
  :commands turn-on-reftex
  :config (setq reftex-default-bibliography "~/Dropbox/Org/bibliography/references.bib"
                reftex-plug-into-AUCTeX     t))

(use-package org-ref
  ;; :after (org ox-bibtex pdf-tools)
  :after org
  :defer t
  :straight (:build t)
  :custom-face
  (org-ref-cite-face ((t (:weight bold))))
  :init
  (setq org-ref-completion-library    'org-ref-ivy-cite
        org-latex-logfiles-extensions '("lof" "lot" "aux" "idx" "out" "log" "fbd_latexmk"
                                        "toc" "nav" "snm" "vrb" "dvi" "blg" "brf" "bflsb"
                                        "entoc" "ps" "spl" "bbl" "pygtex" "pygstyle"))
  (add-hook 'org-mode-hook (lambda () (require 'org-ref)))
  :config
  (setq bibtex-completion-pdf-field    "file"
        bibtex-completion-notes-path   "~/Dropbox/Org/bibliography/notes/"
        bibtex-completion-bibliography "~/Dropbox/Org/bibliography/references.bib"
        bibtex-completion-library-path "~/Dropbox/Org/bibliography/bibtex-pdfs/"
        bibtex-completion-pdf-symbol   "⌘"
        bibtex-completion-notes-symbol "✎")
  :general
  (dqv/evil
    :keymaps 'bibtex-mode-map
    :packages 'org-ref
    "C-j" #'org-ref-bibtex-next-entry
    "C-k" #'org-ref-bibtex-previous-entry
    "gj"  #'org-ref-bibtex-next-entry
    "gk"  #'org-ref-bibtex-previous-entry)
  (dqv/major-leader-key
    :keymaps '(bibtex-mode-map)
    :packages 'org-ref
    ;; Navigation
    "j" #'org-ref-bibtex-next-entry
    "k" #'org-ref-bibtex-previous-entry

    ;; Open
    "b" #'org-ref-open-in-browser
    "n" #'org-ref-open-bibtex-notes
    "p" #'org-ref-open-bibtex-pdf

    ;; Misc
    "h" #'org-ref-bibtex-hydra/body
    "i" #'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
    "s" #'org-ref-sort-bibtex-entry

    "l" '(:ignore t :which-key "lookup")
    "la" #'arxiv-add-bibtex-entry
    "lA" #'arxiv-get-pdf-add-bibtex-entry
    "ld" #'doi-utils-add-bibtex-entry-from-doi
    "li" #'isbn-to-bibtex
    "lp" #'pubmed-insert-bibtex-from-pmid)
  (dqv/major-leader-key
    :keymaps 'org-mode-map
    :pakages 'org-ref
    "ic" #'org-ref-insert-link))

(use-package ivy-bibtex
  :defer t
  :straight (:build t)
  :config
  (setq bibtex-completion-pdf-open-function #'find-file)
  :general
  (dqv/leader-key
    :keymaps '(bibtex-mode-map)
    :packages 'ivy-bibtex
    "m" #'ivy-bibtex))

(setq org-return-follows-link t
      org-use-speed-commands t
      org-deadline-warning-days 14
      org-agenda-span 7
      org-agenda-start-on-weekday nil
      org-agenda-tags-column 74)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IDEA(i)" "NEXT(n)" "MUST(m)" "SHOULD(s)" "CLASS(c)" "INPROGRESS(p)" "REVIEW(r)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(defun +log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'+log-todo-next-creation-date)

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("home"      . ?h)
        ("research"  . ?r)
        ("work"      . ?w)
        (:endgroup   . nil)
        (:startgroup . nil)
        ("tool"      . ?o)
        ("dev"       . ?d)
        ("report"    . ?p)
        (:endgroup   . nil)
        (:startgroup . nil)
        ("easy"      . ?e)
        ("medium"    . ?m)
        ("hard"      . ?a)
        (:endgroup   . nil)
        ("urgent"    . ?u)
        ("key"       . ?k)
        ("bonus"     . ?b)
        ("ignore"    . ?i)
        ("noexport"  . ?x)))

(setq org-tag-faces
      '(("home"     . (:foreground "goldenrod"  :weight bold))
        ("research" . (:foreground "goldenrod"  :weight bold))
        ("work"     . (:foreground "goldenrod"  :weight bold))
        ("tool"     . (:foreground "IndianRed1" :weight bold))
        ("dev"      . (:foreground "IndianRed1" :weight bold))
        ("report"   . (:foreground "IndianRed1" :weight bold))
        ("urgent"   . (:foreground "red"        :weight bold))
        ("key"      . (:foreground "red"        :weight bold))
        ("easy"     . (:foreground "green4"     :weight bold))
        ("medium"   . (:foreground "orange"     :weight bold))
        ("hard"     . (:foreground "red"        :weight bold))
        ("bonus"    . (:foreground "goldenrod"  :weight bold))
        ("ignore"   . (:foreground "Gray"       :weight bold))
        ("noexport" . (:foreground "LimeGreen"  :weight bold))))

(use-package mixed-pitch
  :after org
  :straight (:build t)
  :hook
  (org-mode           . mixed-pitch-mode)
  (emms-browser-mode  . mixed-pitch-mode)
  (emms-playlist-mode . mixed-pitch-mode)
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (mixed-pitch-mode -1))))

(use-package org-appear
  :after org
  :straight (:build t)
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis   t
        org-hide-emphasis-markers t
        org-appear-autolinks      t
        org-appear-autoentities   t
        org-appear-autosubmarkers t)
  (run-at-time nil nil #'org-appear--set-elements))

(use-package org-fragtog
  :defer t
  :after org
  :straight (:build t)
  :hook (org-mode . org-fragtog-mode))

(use-package org-modern
  :straight (:build t)
  :after org
  :defer t
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda))

(setq org-modern-todo-faces
      '(("TODO" :background "OrangeRed" :foreground "yellow" :weight bold)
        ("MUST" :background "red" :foreground "yellow" :weight bold)
        ("SECTION" :background "#363636" :foreground "white" :weight bold)
        ("DOCUMENT" :background "Yellow" :foreground "white" :weight bold)
        ("SHOULD" :background "Yellow" :foreground "black")
        ("IDEA" :background "Orange" :foreground "black")
        ("INPROGRESS" :background "#1aa7ec" :foreground "black")
        ("REVIEW" :background "#363636" :foreground "yellow" :weight bold)
        ("CLASS" :background "DarkGreen" :foreground "white")
        ("NEXT" :background "#6272a4" :foreground "white")))

(setq org-modern-priority
      (quote ((65 . "❗")
              (66 . "🔥")
              (67 . "⬆")
              (68 . "⬇"))))

(setq org-modern-priority-faces
      (quote '((65 :background "red" :foreground "yellow")
               (66 :background "orange" :foreground "white")
               (67 :background "orange" :foreground "white")
               (68 :background "DarkGreen" :foreground "black"))))

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq org-modern-label-border 1)

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string

 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

(use-package org-tree-slide
  :defer t
  :after org
  :straight (:build t)
  :config
  (setq org-tree-slide-skip-done nil)
  :general
  (dqv/major-leader-key
    :keymaps 'org-mode-map
    :packages 'org-mode
    "P" #'org-tree-slide-mode
    "wj" #'org-tree-slide-move-next-tree
    "wk" #'org-tree-slide-move-previous-tree
    "wu" #'org-tree-slide-content))

(use-package org-roll
  :defer t
  :after org
  :straight (:build t :type git :host github :repo "zaeph/org-roll"))

(setq plstore-cache-passphrase-for-symmetric-encryption t)

(setq org-gcal-client-id "173861024396-9pjbm2u9afoof7f3126rvj66lcin3p5v.apps.googleusercontent.com"
      org-gcal-client-secret "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"
      org-gcal-fetch-file-alist '(("jayden.dangvu@gmail.com" .  "~/Dropbox/Org/Personal.org")
                                  ("afcb1caf732361737371b195bc1215ef240e1d905d269bcd08deb2c9a75a091d@group.calendar.google.com" .  "~/Dropbox/Org/Near.org")
                                  ("87dfe7295cad2f0a87b54892de422e657fec4ec38cc8f0c36ea9796525930cb5@group.calendar.google.com" .  "~/Dropbox/Org/Rust.org")
                                  ("693a349513817913e9e6576b6b9dae59668214e00d08f1318c05ece5cdf6d867@group.calendar.google.com" .  "~/Dropbox/Org/Move.org")
                                  ("6daac2cc37c1b859926fd72ccef0595968465a0cce36fa820a2b84ed4428b59d@group.calendar.google.com" .  "~/Dropbox/Org/Work.org")))

(use-package org-gcal
  :straight t
  :config
  (org-gcal-reload-client-id-secret)
  (setq org-gcal-remove-api-cancelled-events t))

(defun my-org-gcal-set-effort (_calendar-id event _update-mode)
  "Set Effort property based on EVENT if not already set."
  (when-let* ((stime (plist-get (plist-get event :start)
                                :dateTime))
              (etime (plist-get (plist-get event :end)
                                :dateTime))
              (diff (float-time
                     (time-subtract (org-gcal--parse-calendar-time-string etime)
                                    (org-gcal--parse-calendar-time-string stime))))
              (minutes (floor (/ diff 60))))
    (let ((effort (org-entry-get (point) org-effort-property)))
      (unless effort
        (message "need to set effort - minutes %S" minutes)
        (org-entry-put (point)
                       org-effort-property
                       (apply #'format "%d:%02d" (cl-floor minutes 60)))))))
(add-hook 'org-gcal-after-update-entry-functions #'my-org-gcal-set-effort)

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+AUTHOR: Dang Quang Vu\n#+EMAIL: jayden.dangvu@gmail.com\n#+SETUPFILE: ~/theme-readtheorg.setup\n#+HTML_HEAD: <style>pre.src{background:#343131;color:white;} </style>")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+AUTHOR: Dang Quang Vu\n#+EMAIL: jayden.dangvu@gmail.com")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+filetags: Project\n#+AUTHOR: Dang Quang Vu\n#+EMAIL: jayden.dangvu@gmail.com")
      :unnarrowed t)))
  :config
  (org-roam-setup))

(require 'appt)

;; Agenda-to-appointent hooks
(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
(appt-activate 1)                ;; activate appointment notification
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '60)  ;; warn every 10 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '720
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function


(defun dqv/send-notification (title msg)
  (let ((notifier-path (executable-find "terminal-notifier")))
    (start-process
     "Appointment Alert"
     "*Appointment Alert*" ; use `nil` to not capture output; this captures output in background
     notifier-path
     "-message" msg
     "-title" title
     "-sender" "org.gnu.Emacs"
     "-activate" "org.gnu.Emacs")))

(defun dqv/appt-display-native (min-to-app new-time msg)
  (dqv/send-notification
   (format "Appointment in %s minutes" min-to-app) ; Title
   (format "%s" msg)))                             ; Message/detail text

(setq appt-disp-window-function (function dqv/appt-display-native))

(use-package company
  :straight (:build t)
  :defer t
  :hook (company-mode . evil-normalize-keymaps)
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length     2
        company-toolsip-limit             5
        company-idle-delay                0.2
        company-tooltip-align-annotations t
        company-require-match             'never
        company-global-modes              '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-preview-frontend
          company-echo-metadata-frontend)
        company-backends '(company-capf)
        company-auto-commit         nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase    nil))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package company-dict
  :after company
  :straight (:build t)
  :config
  (setq company-dict-dir (expand-file-name "dicts" user-emacs-directory)))

(use-package company-box
  :straight (:build t)
  :after (company all-the-icons)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors       nil
        company-box-max-candidates        50
        company-box-icons-alist           'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `(
            (Unknown . ,(all-the-icons-material "find_in_page" :face 'all-the-icons-purple))
            (Text . ,(all-the-icons-material "text_fields" :face 'all-the-icons-green))
            (Method . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Function . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Constructor . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Field . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Variable . ,(all-the-icons-material "adjust" :face 'all-the-icons-blue))
            (Class . ,(all-the-icons-material "class" :face 'all-the-icons-red))
            (Interface . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module . ,(all-the-icons-material "view_module" :face 'all-the-icons-red))
            (Property . ,(all-the-icons-material "settings" :face 'all-the-icons-red))
            (Unit . ,(all-the-icons-material "straighten" :face 'all-the-icons-red))
            (Value . ,(all-the-icons-material "filter_1" :face 'all-the-icons-red))
            (Enum . ,(all-the-icons-material "plus_one" :face 'all-the-icons-red))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :face 'all-the-icons-red))
            (Snippet . ,(all-the-icons-material "short_text" :face 'all-the-icons-red))
            (Color . ,(all-the-icons-material "color_lens" :face 'all-the-icons-red))
            (File . ,(all-the-icons-material "insert_drive_file" :face 'all-the-icons-red))
            (Reference . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
            (Folder . ,(all-the-icons-material "folder" :face 'all-the-icons-red))
            (EnumMember . ,(all-the-icons-material "people" :face 'all-the-icons-red))
            (Constant . ,(all-the-icons-material "pause_circle_filled" :face 'all-the-icons-red))
            (Struct . ,(all-the-icons-material "streetview" :face 'all-the-icons-red))
            (Event . ,(all-the-icons-material "event" :face 'all-the-icons-red))
            (Operator . ,(all-the-icons-material "control_point" :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class" :face 'all-the-icons-red))
            (Template . ,(all-the-icons-material "short_text" :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle" :face 'all-the-icons-blue))
            (ElispFeature . ,(all-the-icons-material "stars" :face 'all-the-icons-orange))
            (ElispFace . ,(all-the-icons-material "format_paint" :face 'all-the-icons-pink))
            ))))

(use-package ivy
  :straight t
  :defer t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-u" . ivy-scroll-up-command)
         ("C-d" . ivy-scroll-down-command)
         :map ivy-switch-buffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-wrap                        t
        ivy-height                      17
        ivy-sort-max-size               50000
        ivy-fixed-height-minibuffer     t
        ivy-read-action-functions       #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system    'ivy
        ivy-on-del-error-function       #'ignore
        ivy-use-selectable-prompt       t))

(use-package ivy-prescient
  :after ivy
  :straight (:build t))

(use-package all-the-icons-ivy
  :straight (:build t)
  :after (ivy all-the-icons)
  :hook (after-init . all-the-icons-ivy-setup))

(use-package ivy-posframe
  :defer t
  :after (:any ivy helpful)
  :hook (ivy-mode . ivy-posframe-mode)
  :straight (:build t)
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width   10
        ivy-posframe-parameters
        `((min-width  . 90)
          (min-height . ,ivy-height))))

(use-package ivy-rich
  :straight (:build t)
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :straight (:build t)
  :after recentf
  :after ivy
  :bind (("M-x"     . counsel-M-x)
         ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package yasnippet
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :straight (:build t)
  :config
  (yas-global-mode)
  :custom
  (yas-prompt-functions '(yas-completing-prompt))
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet
  :straight (:build t))

(use-package yatemplate
  :defer t
  :after yasnippet
  :straight (:build t))

(use-package ivy-yasnippet
  :defer t
  :after (ivy yasnippet)
  :straight (:build t)
  :general
  (dqv/leader-key
    :infix "i"
    :packages 'ivy-yasnippet
    "y" #'ivy-yasnippet))

(use-package dockerfile-mode
  :defer t
  :straight (:build t)
  :hook (dockerfile-mode . lsp-deferred)
  :init
  (put 'docker-image-name 'safe-local-variable #'stringp)
  :mode "Dockerfile\\'")

(use-package docker
  :defer t
  :straight (:build t))

(use-package pdf-tools
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :straight (:build t)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :general
  (dqv/evil
    :keymaps 'pdf-view-mode-map
    :packages 'pdf-tools
    "y"   #'pdf-view-kill-ring-save
    "j"   #'evil-collection-pdf-view-next-line-or-next-page
    "k"   #'evil-collection-pdf-view-previous-line-or-previous-page)
  (dqv/major-leader-key
    :keymaps 'pdf-view-mode-map
    :packages 'pdf-tools
    "a"  '(:ignore t :which-key "annotations")
    "aD" #'pdf-annot-delete
    "at" #'pdf-annot-attachment-dired
    "ah" #'pdf-annot-add-highlight-markup-annotation
    "al" #'pdf-annot-list-annotations
    "am" #'pdf-annot-markup-annotation
    "ao" #'pdf-annot-add-strikeout-markup-annotation
    "as" #'pdf-annot-add-squiggly-markup-annotation
    "at" #'pdf-annot-add-text-annotation
    "au" #'pdf-annot-add-underline-markup-annotation

    "f"  '(:ignore t :which-key "fit")
    "fw" #'pdf-view-fit-width-to-window
    "fh" #'pdf-view-fit-height-to-window
    "fp" #'pdf-view-fit-page-to-window

    "s"  '(:ignore t :which-key "slice/search")
    "sb" #'pdf-view-set-slice-from-bounding-box
    "sm" #'pdf-view-set-slice-using-mouse
    "sr" #'pdf-view-reset-slice
    "ss" #'pdf-occur

    "o"  'pdf-outline
    "m"  'pdf-view-midnight-minor-mode)
  :config
  (with-eval-after-load 'pdf-view
    (csetq pdf-view-midnight-colors '("#d8dee9" . "#2e3440"))))

(use-package pdf-view-restore
  :after pdf-tools
  :defer t
  :straight (:build t)
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename (expand-file-name ".tmp/pdf-view-restore"
                                                    user-emacs-directory)))

(use-package magit
  ;; :diminish magit-auto-revert-mode
  ;; :diminish auto-revert-mode
  :straight (:build t)
  :defer t
  :init
  (setq forge-add-default-bindings nil)
  (setq magit-merge-preview-mode t)
  (setq magit-auto-revert-mode nil)
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (defadvice magit-insert-unstaged-changes (around sacha activate)
    (if my-magit-limit-to-directory
        (let ((magit-current-diff-range (cons 'index 'working))
              (magit-diff-options (copy-sequence magit-diff-options)))
          (magit-git-insert-section (unstaged "Unstaged changes:")
                                    #'magit-wash-raw-diffs
                                    "diff-files"
                                    "--" my-magit-limit-to-directory))
      ad-do-it))

  (csetq magit-clone-default-directory "~/fromGIT/"
         magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (with-eval-after-load 'evil-collection
    (dqv/evil
      :packages '(evil-collection magit)
      :keymaps '(magit-mode-map magit-log-mode-map magit-status-mode-map)
      :states 'normal
      "t" #'magit-tag
      "s" #'magit-stage))
  :general
  (:keymaps '(git-rebase-mode-map)
            :packages 'magit
            "C-j" #'evil-next-line
            "C-k" #'evil-previous-line)
  ;; (dqv/major-leader-key
  ;;   :keymaps 'git-rebase-mode-map
  ;;   :packages 'magit
  ;;   "," #'with-editor-finish
  ;;   "k" #'with-editor-cancel
  ;;   "a" #'with-editor-cancel)
  ;; (dqv/major-leader-key
  ;;   :packages 'magit
  ;;   "," #'with-editor-finish
  ;;   "k" #'with-editor-cancel
  ;;   "a" #'with-editor-cancel)
  (dqv/leader-key
    :infix   "g"
    :packages 'magit
    ""   '(:ignore t :which-key "git")
    "b"  #'magit-blame
    "c"  #'magit-clone
    "d"  #'magit-dispatch
    "i"  #'magit-init
    "s"  #'magit-status
    "y"  #'my/yadm
    "S"  #'magit-stage-file
    "U"  #'magit-unstage-file
    "f"  '(:ignore t :which-key "file")
    "fd" #'magit-diff
    "fc" #'magit-file-checkout
    "fl" #'magit-file-dispatch
    "fF" #'magit-find-file))

(use-package hl-todo
  :defer t
  :straight (:build t)
  :init (global-hl-todo-mode 1)
  :general
  (dqv/leader-key
    :packages '(hl-todo)
    :infix "c"
    ""  '(:ignore t :which-key "todos")
    "n" #'hl-todo-next
    "p" #'hl-todo-previous))

(use-package magit-todos
  :defer t
  :straight (:build t)
  :after (magit hl-todo)
  :init
  (with-eval-after-load 'magit
    (defun my/magit-todos-if-not-yadm ()
      "Deactivate magit-todos if in yadm Tramp connection.
If `magit--default-directory' points to a yadm Tramp directory,
deactivate `magit-todos-mode', otherwise enable it."
      (if (string-prefix-p "/yadm:" magit--default-directory)
          (magit-todos-mode -1)
        (magit-todos-mode +1)))
    (add-hook 'magit-mode-hook #'my/magit-todos-if-not-yadm))
  :config
  (csetq magit-todos-ignore-case t)
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:"))

(use-package magit-gitflow
  :defer t
  :after magit
  :straight (magit-gitflow :build t
                           :type git
                           :host github
                           :repo "jtatarik/magit-gitflow")
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package forge
  :ensure t
  :after magit
  :straight (:build t)
  :config
  :general
  (dqv/major-leader-key
    :keymaps 'forge-topic-mode-map
    "c"  #'forge-create-post
    "e"  '(:ignore t :which-key "edit")
    "ea" #'forge-edit-topic-assignees
    "ed" #'forge-edit-topic-draft
    "ek" #'forge-delete-comment
    "el" #'forge-edit-topic-labels
    "em" #'forge-edit-topic-marks
    "eM" #'forge-merge
    "en" #'forge-edit-topic-note
    "ep" #'forge-edit-post
    "er" #'forge-edit-topic-review-requests
    "es" #'forge-edit-topic-state
    "et" #'forge-edit-topic-title))

(use-package code-review
  :after magit
  :bind (:map forge-topic-mode-map ("C-c r" . #'code-review-forge-pr-at-point))
  :bind (:map code-review-mode-map (("C-c n" . #'code-review-comment-jump-next)
                                    ("C-c p" . #'code-review-comment-jump-previous))))

(use-package git-messenger
  :straight (:build t)
  :bind (("C-x v m" . git-messenger:popup-message)))

(defun buffer-insert-at-end (string)
  "Insert STRING at the maximal point in a buffer."
  (save-excursion
    (goto-char (point-max))
    (end-of-line)
    (insert ?\n string)
    (unless (string-suffix-p "\n" string)
      (insert ?\n))))

(defun get-knuth-number-from-string (string)
  "Return KNUTH issue number from STRING.
  Return nil if STRING does not contain a KNUTH issue.
  STRING may be nil."
  (if (and string (string-match "\\(KNUTH-[[:digit:]]\+\\)" string))
      (match-string 1 string)
    nil))

(defun insert-knuth-ticket-number-from-branch ()
  "If we're on a KNUTH feature branch, insert the ticket number."
  (interactive)
  (let ((knuth (get-knuth-number-from-string (magit-get-current-branch))))
    (if (and knuth (not (buffer-line-matches-p (concat "^" knuth)))) (buffer-insert-at-end knuth))))

(defun buffer-line-matches-p (needle)
  "Return t if the last line matches NEEDLE.
  Ignores comments"
  (save-excursion
    (goto-char 0)
    (search-forward-regexp needle nil 'noerror)))

(add-hook 'git-commit-setup-hook 'insert-knuth-ticket-number-from-branch)

(defun my-magit-stage-all-and-commit (message)
  (interactive (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
  (magit-stage-modified)
  (magit-commit-create (list "-m" message))
  (call-interactively #'magit-push-current-to-pushremote))
(defvar my-magit-limit-to-directory nil "Limit magit status to a specific directory.")
(defun my-magit-status-in-directory (directory)
  "Displays magit status limited to DIRECTORY.
 Uses the current `default-directory', or prompts for a directory
 if called with a prefix argument. Sets `my-magit-limit-to-directory'
 so that it's still active even after you stage a change. Very experimental."
  (interactive (list (expand-file-name
                      (if current-prefix-arg
                          (read-directory-name "Directory: ")
                        default-directory))))
  (setq my-magit-limit-to-directory directory)
  (magit-status directory))

(use-package ripgrep
  :if (executable-find "rg")
  :straight (:build t)
  :defer t)

(use-package projectile
  :straight (:build t)
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  (add-to-list 'projectile-ignored-projects "~/")
  (add-to-list 'projectile-ignored-projects "^/.algokit")
  (add-to-list 'projectile-ignored-projects "~/.emacs.d")
  (add-to-list 'projectile-ignored-projects "~/Dropbox")
  (add-to-list 'projectile-globally-ignored-directories "^node_modules$")
  (add-to-list 'projectile-globally-ignored-directories "^/.algokit")
  (add-to-list 'projectile-globally-ignored-directories "~/.rustup")
  (add-to-list 'projectile-globally-ignored-directories "~/.cargo")
  (add-to-list 'projectile-globally-ignored-directories "~/.cache")
  (add-to-list 'projectile-globally-ignored-directories "~/.emacs.d")
  :general
  (dqv/leader-key
    "p" '(:keymap projectile-command-map :which-key "projectile")))

(use-package counsel-projectile
  :straight (:build t)
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package recentf
  :straight (:build t :type built-in)
  :custom ((recentf-max-saved-items 2000))
  :config
  (setq recentf-exclude
        '(
          ;; Các pattern như bạn đã cấu hình
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
          "^/tmp/"
          "^/ssh:"
          "^/sudo:"
          "~/Dropbox/Roam"  ; Thêm pattern cụ thể này
          "~/Dropbox/Org"  ; Thêm pattern cụ thể này
          "~/.emacs.d/snippets/"
          "~/.emacs.d/bookmarks"
          "~/.config/"
          "\\(^\\|/\\)\\.cache/"
          "\\(^\\|/\\)\\.git/"
          "\\(^\\|/\\)node_modules/"
          "\\(^\\|/\\)logs/"))


  ;; Thêm hook để xóa các mục khỏi recentf-list
  (defun remove-unwanted-recentf-entries ()
    "Remove entries from recentf-list based on custom patterns."
    (setq recentf-list
          (cl-remove-if
           (lambda (file)
             (or (string-match-p "~/Dropbox/" file)
                 (string-match-p "~/Areas/Obsidian/" file)))
           recentf-list)))

  ;; Chạy mỗi khi recentf-list được cập nhật
  (add-hook 'recentf-dialog-mode-hook 'remove-unwanted-recentf-entries)
  (advice-add 'recentf-save-list :before #'remove-unwanted-recentf-entries)

  ;; Xóa danh sách hiện tại
  (setq recentf-list nil)
 )

(use-package shell-pop
  :defer t
  :straight (:build t)
  :custom
  (shell-pop-default-directory "/Users/dangeamon")
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda () (eshell shell-pop-term-shell)))))
  (shell-pop-window-size 30)
  (shell-pop-full-span nil)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))

(use-package popwin
  :straight t)

(with-eval-after-load 'popwin
  (dqv/leader-key
    "oe" '(+popwin:eshell :which-key "Eshell popup")
    "oE" '(eshell :which-key "Eshell"))
  (defun +popwin:eshell ()
    (interactive)
    (popwin:display-buffer-1
     (or (get-buffer "*eshell*")
         (save-window-excursion
           (call-interactively 'eshell)))
     :default-config-keywords '(:position :bottom :height 14))))

(use-package vterm
  :defer t
  :straight t
  :general
  (dqv/leader-key
    "ot" '(+popwin:vterm :which-key "vTerm popup")
    "oT" '(vterm :which-key "vTerm"))
  :preface
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :custom
  (vterm-max-scrollback 5000)
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000)
  (with-eval-after-load 'popwin
    (defun +popwin:vterm ()
      (interactive)
      (popwin:display-buffer-1
       (or (get-buffer "*vterm*")
           (save-window-excursion
             (call-interactively 'vterm)))
       :default-config-keywords '(:position :bottom :height 15)))))

(use-package multi-vterm
  :after vterm
  :defer t
  :straight (:build t)
  :general
  (dqv/major-leader-key
    :packages '(vterm multi-vterm)
    :keymap 'vterm-mode-map
    "c" #'multi-vterm
    "j" #'multi-vterm-next
    "k" #'multi-vterm-prev))

(use-package leetcode
  :ensure t
  :straight (:build t))
(setq leetcode-prefer-language "rust"
      leetcode-prefer-sql "mysql"
      leetcode-save-solutions t
      leetcode-directory "~/Development/leetcode-solution")

(use-package editorconfig
  :defer t
  :straight (:build t)
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t))

(use-package evil-nerd-commenter
  :after evil
  :straight (:build t))
(global-set-key (kbd "s-/") #'evilnc-comment-or-uncomment-lines)

(use-package evil-iedit-state
  :defer t
  :straight (:build t)
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-curent-symbol-default     t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default        nil)
  :general
  (dqv/leader-key
    :infix "r"
    :packages '(iedit evil-iedit-state)
    "" '(:ignore t :which-key "refactor")
    "i" #'evil-iedit-state/iedit-mode)
  (general-define-key
   :keymaps 'evil-iedit-state-map
   "c" nil
   "s" nil
   "J" nil
   "S" #'iedit-expand-down-a-line
   "T" #'iedit-expand-up-a-line
   "h" #'evil-iedit-state/evil-change
   "k" #'evil-iedit-state/evil-substitute
   "K" #'evil-iedit-state/substitute
   "q" #'evil-iedit-state/quit-iedit-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/smartparens")
(use-package smartparens
  :defer t
  ;; :straight (:build t)
  :hook (prog-mode . smartparens-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/wat-mode/")
(require 'wat-mode)

(use-package parinfer-rust-mode
  :defer t
  :straight (:build t)
  :diminish parinfer-rust-mode
  :hook emacs-lisp-mode common-lisp-mode scheme-mode
  :init
  (setq parinfer-rust-auto-download     t
        parinfer-rust-library-directory (concat user-emacs-directory
                                                "parinfer-rust/"))
  (add-hook 'parinfer-rust-mode-hook
            (lambda () (smartparens-mode -1)))
  :general
  (dqv/major-leader-key
    :keymaps 'parinfer-rust-mode-map
    "m" #'parinfer-rust-switch-mode
    "M" #'parinfer-rust-toggle-disable))

(use-package maple-iedit
  :ensure nil
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (setq maple-iedit-ignore-case t)

  (defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind (:map evil-visual-state-map
              ("n" . maple/iedit/body)
              ;; ("C-n" . maple-iedit-match-next)
              ;; ("C-p" . maple-iedit-match-previous)
              ("C-t" . maple-iedit-skip-and-match-next)))

(use-package engine-mode
  :config
  (engine/set-keymap-prefix (kbd "C-c s"))

  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        engine/browser-function 'browse-url-default-macosx-browser)

  (defengine solana-localnet
    "https://explorer.solana.com/tx/%s?cluster=custom&customUrl=http://localhost:8899"
    :keybinding "s")

  (defengine near-testnet
    "https://testnet.nearblocks.io/txns/%s"
    :keybinding "n")

  (defengine sui-localnet
    "https://suiexplorer.com/txblock/%s?network=local"
    :keybinding "S")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "1")

  (defengine crates
    "https://crates.io/search?q=%s"
    :keybinding "c")

  (defengine localhost
    "http://localhost:%s"
    :keybinding "l")

  (defengine vocabulary
    "https://www.vocabulary.com/dictionary/%s"
    :keybinding "t")

  (defengine translate
    "https://translate.google.com/?hl=vi&sl=en&tl=vi&text=%s&op=translate"
    :keybinding "T")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (engine-mode 1))

(use-package bm
  :demand t
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t

        ;; where to store persistant files
        bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  ;; keys binding
  :bind (("C-M-s-x" . bm-toggle)
         ("C-M-s-w" . bm-lifo-next)
         ("C-M-s-q" . bm-lifo-previous)
         ("C-M-s-z" . bm-show-all))
  )

(use-package move-text
  :straight (:build t))

(global-set-key (kbd "s-j") #'move-text-down)
(global-set-key (kbd "s-k") #'move-text-up)

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-<tab>" . hs-cycle)
  ("C-<iso-lefttab>" . hs-global-cycle)
  ("C-S-<tab>" . hs-global-cycle))
(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (hs-hide-block)
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children)))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))

(use-package eyebrowse
  :straight (:build t)
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(dqv/leader-key
  "TAB"  '(:ignore t :which-key "Window Management")
  "TAB 0" '(eyebrowse-switch-to-window-config-0 :which-key "Select Windown 0")
  "TAB 1" '(eyebrowse-switch-to-window-config-1 :which-key "Select Window 1")
  "TAB 2" '(eyebrowse-switch-to-window-config-2 :which-key "Select Window 2")
  "TAB 3" '(eyebrowse-switch-to-window-config-3 :which-key "Select Window 3")
  "TAB 4" '(eyebrowse-switch-to-window-config-4 :which-key "Select Window 4")
  "TAB 5" '(eyebrowse-switch-to-window-config-5 :which-key "Select Window 5")
  "TAB 6" '(eyebrowse-switch-to-window-config-6 :which-key "Select Window 6")
  "TAB 7" '(eyebrowse-switch-to-window-config-7 :which-key "Select Window 7")
  "TAB 8" '(eyebrowse-switch-to-window-config-8 :which-key "Select Window 8")
  "TAB 9" '(eyebrowse-switch-to-window-config-9 :which-key "Select Window 9")
  "TAB r" '(eyebrowse-rename-window-config :which-key "Rename Window")
  "TAB n" '(eyebrowse-create-named-window-config :which-key "Create New Window")
  "TAB TAB" '(eyebrowse-switch-to-window-config :which-key "Switch Window")
  "TAB d" '(eyebrowse-close-window-config :which-key "Delete Window")
  "TAB k" '(eyebrowse-next-window-config :which-key "Next Window")
  "TAB j" '(eyebrowse-prev-window-config :which-key "Previous Window"))

(use-package dirvish
  :straight (:build t)
  :defer t
  :init (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("b" "~/Bootcamp/" "Bootcamp")
     ("r" "~/Dropbox/Roam/" "Roam")
     ("D" "~/Develop/" "Develop")
     ("C" "~/Documents/conlanging/content" "Conlanging")))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(all-the-icons file-size  subtree-state vc-state  collapse )) ;; git-msg file-time
  (setq delete-by-moving-to-trash t)
  :config
  (dirvish-peek-mode)
  (csetq dired-mouse-drag-files                   t
         mouse-drag-and-drop-region-cross-program t)
  
  (setq-default truncate-lines t)
  (csetq dired-listing-switches (string-join '("--all"
                                               "--human-readable"
                                               "--time-style=long-iso"
                                               "--group-directories-first"
                                               "-lv1")
                                             " "))
  (let ((my/file (lambda (path &optional dir)
                   (expand-file-name path (or dir user-emacs-directory))))
        (my/dir (lambda (path &optional dir)
                  (expand-file-name (file-name-as-directory path)
                                    (or dir user-emacs-directory)))))
    (csetq image-dired-thumb-size             150
           image-dired-dir                    (funcall my/dir "dired-img")
           image-dired-db-file                (funcall my/file "dired-db.el")
           image-dired-gallery-dir            (funcall my/dir "gallery")
           image-dired-temp-image-file        (funcall my/file "temp-image" image-dired-dir)
           image-dired-temp-rotate-image-file (funcall my/file "temp-rotate-image" image-dired-dir)))
  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa")
    (when (file-directory-p file)
      `(shell . ("exa"  "-al" "--color=always" "--icons" "--group-directories-first" ,file))))
  
  (set-face-attribute 'ansi-color-blue nil :foreground "#FFFFFF")
  (add-to-list 'dirvish-preview-dispatchers 'exa)
  (csetq dired-dwim-target         t
         dired-recursive-copies    'always
         dired-recursive-deletes   'top
         delete-by-moving-to-trash t)
  :general
  (dqv/evil
    :keymaps 'dirvish-mode-map
    :packages '(dired dirvish)
    "q" #'dirvish-quit
    "TAB" #'dirvish-subtree-toggle)
  (dqv/major-leader-key
    :keymaps 'dirvish-mode-map
    :packages '(dired dirvish)
    "A"   #'gnus-dired-attach
    "a"   #'dirvish-quick-access
    "d"   #'dirvish-dispatch
    "e"   #'dirvish-emerge-menu
    "f"   #'dirvish-fd-jump
    "F"   #'dirvish-file-info-menu
    "h"   '(:ignore t :which-key "history")
    "hp"  #'dirvish-history-go-backward
    "hn"  #'dirvish-history-go-forward
    "hj"  #'dirvish-history-jump
    "hl"  #'dirvish-history-last
    "l"   '(:ignore t :which-key "layout")
    "ls"  #'dirvish-layout-switch
    "lt"  #'dirvish-layout-toggle
    "m"   #'dirvish-mark-menu
    "s"   #'dirvish-quicksort
    "S"   #'dirvish-setup-menu
    "y"   #'dirvish-yank-menu
    "n"   #'dirvish-narrow))

(use-package dired-rsync
  :if (executable-find "rsync")
  :defer t
  :straight (:build t)
  :general
  (dqv/evil
    :keymaps 'dired-mode-map
    :packages 'dired-rsync
    "C-r" #'dired-rsync))

(setq image-use-external-converter t)

(use-package bufler
  :straight (:build t)
  :bind (("C-M-j" . bufler-switch-buffer)
         ("C-M-k" . bufler-workspace-frame-set))
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "D"           'bufler-list-buffer-kill)

  (setf bufler-groups
        (bufler-defgroups
          ;; Subgroup collecting all named workspaces.
          (group (auto-workspace))
          ;; Subgroup collecting buffers in a projectile project.
          (group (auto-projectile))
          ;; Grouping browser windows
          (group
           (group-or "Browsers"
                     (name-match "Vimb" (rx bos "vimb"))
                     (name-match "Qutebrowser" (rx bos "Qutebrowser"))
                     (name-match "Chromium" (rx bos "Chromium"))))
          (group
           (group-or "Chat"
                     (mode-match "Telega" (rx bos "telega-"))))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "Help/Info"
                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                     ;; (mode-match "*Helpful*" (rx bos "helpful-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (name-match "**Special**"
                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*"))))
          ;; Group remaining buffers by major mode.
          (auto-mode))))

(use-package helpful
  :straight (:build t)
  :after (counsel ivy)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package auctex
  :defer t
  :straight (:build t)
  :hook (tex-mode . lsp-deferred)
  :hook (latex-mode . lsp-deferred)
  :init
  (setq TeX-command-default   (if (executable-find "latexmk") "LatexMk" "LaTeX")
        TeX-engine            (if (executable-find "xetex")   'xetex    'default)
        TeX-auto-save                     t
        TeX-parse-self                    t
        TeX-syntactic-comment             t
        TeX-auto-local                    ".auctex-auto"
        TeX-style-local                   ".auctex-style"
        TeX-source-correlate-mode         t
        TeX-source-correlate-method       'synctex
        TeX-source-correlate-start-server nil
        TeX-electric-sub-and-superscript  t
        TeX-fill-break-at-separators      nil
        TeX-save-query                    t)
  :config
  (setq font-latex-match-reference-keywords
        '(;; BibLaTeX.
          ("printbibliography" "[{") ("addbibresource" "[{")
          ;; Standard commands.
          ("cite" "[{")       ("citep" "[{")
          ("citet" "[{")      ("Cite" "[{")
          ("parencite" "[{")  ("Parencite" "[{")
          ("footcite" "[{")   ("footcitetext" "[{")
          ;; Style-specific commands.
          ("textcite" "[{")   ("Textcite" "[{")
          ("smartcite" "[{")  ("Smartcite" "[{")
          ("cite*" "[{")      ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists.
          ("cites" "[{")      ("Cites" "[{")
          ("parencites" "[{") ("Parencites" "[{")
          ("footcites" "[{")  ("footcitetexts" "[{")
          ("smartcites" "[{") ("Smartcites" "[{")
          ("textcites" "[{")  ("Textcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands.
          ("autocite" "[{")   ("Autocite" "[{")
          ("autocite*" "[{")  ("Autocite*" "[{")
          ("autocites" "[{")  ("Autocites" "[{")
          ;; Text commands.
          ("citeauthor" "[{") ("Citeauthor" "[{")
          ("citetitle" "[{")  ("citetitle*" "[{")
          ("citeyear" "[{")   ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands.
          ("fullcite" "[{")
          ;; Cleveref.
          ("cref" "{")          ("Cref" "{")
          ("cpageref" "{")      ("Cpageref" "{")
          ("cpagerefrange" "{") ("Cpagerefrange" "{")
          ("crefrange" "{")     ("Crefrange" "{")
          ("labelcref" "{")))
  
  (setq font-latex-match-textual-keywords
        '(;; BibLaTeX brackets.
          ("parentext" "{") ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; Auxiliary commands.
          ("textelp" "{")   ("textelp*" "{")
          ("textins" "{")   ("textins*" "{")
          ;; Subcaption.
          ("subcaption" "[{")))
  
  (setq font-latex-match-variable-keywords
        '(;; Amsmath.
          ("numberwithin" "{")
          ;; Enumitem.
          ("setlist" "[{")     ("setlist*" "[{")
          ("newlist" "{")      ("renewlist" "{")
          ("setlistdepth" "{") ("restartlist" "{")
          ("crefname" "{")))
  (setq TeX-master t)
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda ()
                             (setq ispell-parser          'tex
                                   fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))))
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  :general
  (dqv/major-leader-key
    :packages 'auctex
    :keymaps  '(latex-mode-map LaTeX-mode-map)
    "v" '(TeX-view            :which-key "View")
    "c" '(TeX-command-run-all :which-key "Compile")
    "m" '(TeX-command-master  :which-key "Run a command")))

(use-package tex-mode
  :defer t
  :straight (:type built-in)
  :config
  (setq LaTeX-section-hook '(LaTeX-section-heading
                             LaTeX-section-title
                             LaTeX-section-toc
                             LaTeX-section-section
                             LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent              0))

(use-package preview
  :defer t
  :straight (:type built-in)
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  (setq preview-auto-cache-preamble nil)
  (dqv/major-leader-key
    :packages 'auctex
    :keymaps '(latex-mode-map LaTeX-mode-map)
    "p" #'preview-at-point
    "P" #'preview-clearout-at-point))

(use-package cdlatex
  :defer t
  :after auctex
  :straight (:build t)
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode   . org-cdlatex-mode)
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil)
  :general
  (dqv/major-leader-key
    :packages 'cdlatex
    :keymaps 'cdlatex-mode-map
    "$" nil
    "(" nil
    "{" nil
    "[" nil
    "|" nil
    "<" nil
    "^" nil
    "_" nil
    [(control return)] nil))

(use-package adaptive-wrap
  :defer t
  :after auctex
  :straight (:build t)
  :hook (LaTeX-mode . adaptative-wrap-prefix-mode)
  :init (setq-default adaptative-wrap-extra-indent 0))

(use-package auctex-latexmk
  :after auctex
  :defer t
  :straight (:build t)
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-hook 'LaTeX-mode (lambda () (setq TeX-command-default "LatexMk")))
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :defer t
  :after (company auctex)
  :straight (:build t)
  :config
  (company-auctex-init))

(use-package company-math
  :defer t
  :straight (:build t)
  :after (company auctex)
  :config
  (defun my-latex-mode-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends)))
  (add-hook 'TeX-mode-hook #'my-latex-mode-setup))

(use-package avy
  :defer t
  :straight t
  :config
  (csetq avy-keys           '(?a ?u ?i ?e ?c ?t ?s ?r ?n)
         avy-dispatch-alist '((?x . avy-action-kill-move)
                              (?X . avy-action-kill-stay)
                              (?T . avy-action-teleport)
                              (?m . avy-action-mark)
                              (?C . avy-action-copy)
                              (?y . avy-action-yank)
                              (?Y . avy-action-yank-line)
                              (?I . avy-action-ispell)
                              (?z . avy-action-zap-to-char)))
  (defun my/avy-goto-url ()
    "Jump to url with avy."
    (interactive)
    (avy-jump "https?://"))
  (defun my/avy-open-url ()
    "Open url selected with avy."
    (interactive)
    (my/avy-goto-url)
    (browse-url-at-point))
  :general
  (dqv/evil
    :pakages 'avy
    "gc" #'evil-avy-goto-char-timer
    "gl" #'evil-avy-goto-line)
  (dqv/leader-key
    :packages 'avy
    :infix "A"
    "c"  '(:ignore t :which-key "copy")
    "cl" #'avy-copy-line
    "cr" #'avy-copy-region
    "k"  '(:ignore t :which-key "kill")
    "kl" #'avy-kill-whole-line
    "kL" #'avy-kill-ring-save-whole-line
    "kr" #'avy-kill-region
    "kR" #'avy-kill-ring-save-region
    "m"  '(:ignore t :which-key "move")
    "ml" #'avy-move-line
    "mr" #'avy-move-region
    "mt" #'avy-transpose-lines-in-region
    "n"  #'avy-next
    "p"  #'avy-prev)
  (dqv/major-leader-key
    :packages '(avy org)
    :keymaps 'org-mode-map
    "A" '(:ignore t :which-key "avy")
    "Ar" #'avy-org-refile-as-child
    "Ah" #'avy-org-goto-heading-timer))

(use-package keycast
  :defer t
  :straight (:build t)
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast " ")))

(use-package winum
  :straight (:build t)
  :init (winum-mode))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(use-package protobuf-mode
  :mode "\\.proto3")

(use-package tsc
  :straight (:build t))
(use-package tree-sitter
  :defer t
  :straight (:build t)
  :init (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :defer t
  :after tree-sitter
  :straight (:build t))

(use-package flycheck
  :straight (:build t)
  :preface
  :defer t
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don’t recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to on briefly. This allows “refreshing” the syntax check
  ;; state for several buffers quickly after e.g. changing a config
  ;; file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.2))

(use-package ispell
  :if (executable-find "aspell")
  :defer t
  :straight (:type built-in)
  :config
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (setq ispell-program-name "aspell"
        ispell-extra-args   '("--sug-mode=ultra" "--run-together")
        ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")
        ispell-personal-dictionary (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                                                     user-emacs-directory)))

(use-package flyspell
  :defer t
  :straight (:type built-in)
  :ghook 'org-mode 'markdown-mode 'TeX-mode
  :init
  (defhydra flyspell-hydra ()
    "
Spell Commands^^           Add To Dictionary^^              Other
--------------^^---------- -----------------^^------------- -----^^---------------------------
[_b_] check whole buffer   [_B_] add word to dict (buffer)  [_t_] toggle spell check
[_r_] check region         [_G_] add word to dict (global)  [_q_] exit
[_d_] change dictionary    [_S_] add word to dict (session) [_Q_] exit and disable spell check
[_n_] next error
[_c_] correct before point
[_s_] correct at point
"
    ("B" nil)
    ("b" flyspell-buffer)
    ("r" flyspell-region)
    ("d" ispell-change-dictionary)
    ("G" nil)
    ("n" flyspell-goto-next-error)
    ("c" flyspell-correct-wrapper)
    ("Q" flyspell-mode :exit t)
    ("q" nil :exit t)
    ("S" nil)
    ("s" flyspell-correct-at-point)
    ("t" nil))
  :config
  (provide 'ispell) ;; force loading ispell
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :defer t
  :straight (:build t)
  :general ([remap ispell-word] #'flyspell-correct-at-point)
  :config
  (require 'flyspell-correct-ivy nil t))

(use-package flyspell-correct-ivy
  :defer t
  :straight (:build t)
  :after flyspell-correct)

(use-package flyspell-lazy
  :defer t
  :straight (:build t)
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package lsp-mode
  :defer t
  :straight (:build t)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)))
  :hook ((c-mode              . lsp-deferred)
         (c++-mode            . lsp-deferred)
         (html-mode           . lsp-deferred)
         (sh-mode             . lsp-deferred)
         (rustic-mode         . lsp-deferred)
         (go-mode             . lsp-deferred)
         (go-mod-ts-mode      . lsp-deferred)
         (move-mode           . lsp-deferred)
         (toml-mode           . lsp-deferred)
         (toml-ts-mode        . lsp-deferred)
         (python-ts-mode      . lsp-deferred)
         (sql-mode            . lsp-deferred)
         (java-mode           . lsp-deferred)
         (java-ts-mode           . lsp-deferred)
         (json-mode           . lsp-deferred)
         (solidity-mode       . lsp-deferred)
         (json-ts-mode        . lsp-deferred)
         (zig-mode            . lsp-deferred)
         (typescript-mode     . lsp-deferred)
         (typescript-tsx-mode . lsp-deferred)
         (lsp-mode            . lsp-enable-which-key-integration)
         (lsp-mode            . lsp-ui-mode))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-use-plist t)
  (lsp-inlay-hint-enable nil)
  (lsp-inlay-hints-mode nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t) ;; Hints
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;; (lsp-rust-analyzer-store-path "~/.rustup/toolchains/stable-aarch64-apple-darwin/bin/rust-analyzer")
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "shellcheck")
                    :major-modes '(sh-mode)
                    :remote? t
                    :server-id 'shellcheck-remote)))

(setq lsp-sqls-workspace-config-path nil)
(setq lsp-enable-indentation nil)

;; (use-package semgrep
;;   :ensure t
;;   :config
;;   (setq semgrep-ls-binary "/Library/Frameworks/Python.framework/Versions/3.12/bin/semgrep")
;; )

(defun toggle-lsp-inlay-hints ()
  "Toggle LSP inlay hints."
  (interactive)
  (setq lsp-inlay-hint-enable (not lsp-inlay-hint-enable))
  (lsp-inlay-hints-mode (if lsp-inlay-hint-enable 1 -1))
  (message "LSP inlay hints %s" (if lsp-inlay-hint-enable "enabled" "disabled"))
  (when lsp-inlay-hint-enable
    (lsp-restart-workspace)))

(use-package lsp-ui
  :after lsp
  :defer t
  :straight (:build t)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  :general
  (dqv/major-leader-key
    :keymaps 'lsp-ui-peek-mode-map
    :packages 'lsp-ui
    "h" #'lsp-ui-pook--select-prev-file
    "j" #'lsp-ui-pook--select-next
    "k" #'lsp-ui-pook--select-prev
    "l" #'lsp-ui-pook--select-next-file))

(defun dqv/lsp-workspace-remove-missing-projects ()
  (interactive)
  (dolist (dead-project (seq-filter (lambda (x) (not (file-directory-p x))) (lsp-session-folders (lsp-session))))
    (lsp-workspace-folders-remove dead-project)))

(use-package lsp-ivy
  :straight (:build t)
  :defer t
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package treesit
  :custom (treesit-font-lock-level 4)
  :straight (:type  built-in))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :config (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package lsp-treemacs
  :defer t
  :straight (:build t)
  :requires treemacs
  :config
  (treemacs-resize-icons 15))

(use-package exec-path-from-shell
  :defer t
  :straight (:build t)
  :init (exec-path-from-shell-initialize))

(use-package consult-lsp
  :defer t
  :after lsp
  :straight (:build t)
  :general
  (dqv/evil
    :keymaps 'lsp-mode-map
    [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package dap-mode
  :straight (:build t)
  :ensure
  :config
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb-mi"
                                     :request "launch"
                                     :name "LLDB::Run"
                                     :gdbpath "rust-lldb"
                                     :target nil
                                     :cwd nil)))
(setq dap-auto-configure-features '(sessions locals controls breakpoints expressions repl tooltip))

(use-package cc-mode
  :straight (:type built-in)
  :defer t
  :init
  (put 'c-c++-backend 'safe-local-variable 'symbolp)
  (add-hook 'c-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'c++-mode-hook #'tree-sitter-hl-mode)
  :config
  (require 'compile)
  :general
  (dqv/underfine
    :keymaps '(c-mode-map c++-mode-map)
    ";" nil)
  (dqv/major-leader-key
    :keymaps '(c-mode-map c++-mode-map)
    "l"  '(:keymap lsp-command-map :which-key "lsp" :package lsp-mode))
  (dqv/evil
    :keymaps '(c-mode-map c++-mode-map)
    "ga" #'projectile-find-other-file
    "gA" #'projectile-find-other-file-other-window))

(use-package clang-format+
  :straight (:build t)
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package modern-cpp-font-lock
  :straight (:build t)
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package lisp-mode
  :straight (:type built-in)
  :defer t
  :after parinfer-rust-mode
  :hook (lisp-mode . parinfer-rust-mode)
  :config
  (put 'defcommand 'lisp-indent-function 'defun)
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform"))

(use-package eldoc
  :diminish eldoc-mode
  ;; :commands turn-on-eldoc-mode
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; :init
  ;; (progn
  ;;   (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  ;;   (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  ;;   (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))
  :config
  (eldoc-add-command-completions "combobulate-")
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "lispy-"))

;; (use-package eldoc-box
;;   :straight (:build t)
;;   :defer t)

(add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-mode -1)))

(use-package elisp-demos
  :defer t
  :straight (:build t)
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package epdh
  :straight (epdh :type git
                  :host github
                  :repo "alphapapa/emacs-package-dev-handbook"
                  :build t)
  :defer t)

(dqv/major-leader-key
  :keymaps 'emacs-lisp-mode-map
  "'"   #'ielm
  "c"   '(emacs-lisp-byte-compile :which-key "Byte compile")
  "C"   '(:ignore t :which-key "checkdoc")
  "Cc"  #'checkdoc
  "Cs"  #'checkdoc-start
  "v"   '(:ignore t :which-key "eval")
  "eb"  #'eval-buffer
  "ed"  #'eval-defun
  "ee"  #'eval-last-sexp
  "er"  #'eval-region

  "h"   '(:ignore t :which-key "help")
  "hh"  #'helpful-at-point

  "t"   '(:ignore t :which-key "toggle")
  "tP"  '(:ignore t :which-key "parinfer")
  "tPs" #'parinfer-rust-switch-mode
  "tPd" #'parinfer-rust-mode-disable
  "tPp" #'parinfer-rust-toggle-paren-mode)

(use-package package-lint
  :defer t
  :straight (:build t)
  :general
  (dqv/major-leader-key
    :keymaps 'emacs-lisp-mode-map
    :packages 'package-lint
    "l" #'package-lint-current-buffer))

(use-package cask-mode
  :defer t
  :straight (:build t))

(use-package python
  :defer t
  :straight (:build t)
  :after ob
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("[./]flake8\\'" . conf-mode)
         ("/Pipfile\\'"   . conf-mode))
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-local-vars-hook #'lsp)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "/usr/local/bin/python3")
             (string= python-shell-interpreter "/usr/local/bin/python3"))
    (setq python-shell-interpreter "/usr/local/bin/python3"))
  (setq python-interpreter "/usr/local/bin/python3"))

(use-package pytest
  :defer t
  :straight (:build t)
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-last-failed
             pytest-pdb-last-failed
             pytest-module
             pytest-pdb-module)
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg")
  :general
  (dqv/major-leader-key
    :keymaps 'python-mode-map
    :infix "t"
    :packages 'pytest
    ""  '(:ignore t :which-key "test")
    "a" #'python-pytest
    "f" #'python-pytest-file-dwim
    "F" #'python-pytest-file
    "t" #'python-pytest-function-dwim
    "T" #'python-pytest-function
    "r" #'python-pytest-repeat
    "p" #'python-pytest-dispatch))

(use-package poetry
  :defer t
  :straight (:build t)
  :commands (poetry-venv-toggle
             poetry-tracking-mode)
  :config
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package pip-requirements
  :defer t
  :straight (:build t))

(use-package pippel
  :defer t
  :straight (:build t)
  :general
  (dqv/major-leader-key
    :keymaps 'python-mode-map
    :packages 'pippel
    "P" #'pippel-list-packages))

(use-package pipenv
  :defer t
  :straight (:build t)
  :commands (pipenv-activate
             pipenv-deactivate
             pipenv-shell
             pipenv-open
             pipenv-install
             pipenv-uninstall)
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :general
  (dqv/major-leader-key
    :keymaps 'python-mode-map
    :packages 'pipenv
    :infix "e"
    ""  '(:ignore t :which-key "pipenv")
    "a" #'pipenv-activate
    "d" #'pipenv-deactivate
    "i" #'pipenv-install
    "l" #'pipenv-lock
    "o" #'pipenv-open
    "r" #'pipenv-run
    "s" #'pipenv-shell
    "u" #'pipenv-uninstall))

(use-package pyenv
  :defer t
  :straight (:build t)
  :config
  (add-hook 'python-mode-hook #'pyenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyenv-virtual-env-name (" venv:" pyenv-virtual-env-name " "))
               'append))

(use-package pyenv-mode
  :defer t
  :after python
  :straight (:build t)
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :general
  (dqv/major-leader-key
    :packages 'pyenv-mode
    :keymaps 'python-mode-map
    :infix "v"
    "u" #'pyenv-mode-unset
    "s" #'pyenv-mode-set))

(use-package pyimport
  :defer t
  :straight (:build t)
  :general
  (dqv/major-leader-key
    :packages 'pyimport
    :keymaps 'python-mode-map
    :infix "i"
    ""  '(:ignore t :which-key "imports")
    "i" #'pyimport-insert-missing
    "r" #'pyimport-remove-unused))

(use-package py-isort
  :defer t
  :straight (:build t)
  :general
  (dqv/major-leader-key
    :keymaps 'python-mode-map
    :packages 'py-isort
    :infix "i"
    ""  '(:ignore t :which-key "imports")
    "s" #'py-isort-buffer
    "R" #'py-isort-region))

(use-package counsel-pydoc
  :defer t
  :straight (:build t))

(use-package sphinx-doc
  :defer t
  :straight (:build t)
  :init
  (add-hook 'python-mode-hook #'sphinx-doc-mode)
  :general
  (dqv/major-leader-key
    :keymaps 'python-mode-map
    :packages 'sphinx-doc
    :infix "S"
    ""  '(:ignore t :which-key "sphinx-doc")
    "e" #'sphinx-doc-mode
    "d" #'sphinx-doc))

(use-package cython-mode
  :defer t
  :straight (:build t)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
  :general
  (dqv/major-leader-key
    :keymaps 'cython-mode-map
    :packages 'cython-mode
    :infix "c"
    ""  '(:ignore t :which-key "cython")
    "c" #'cython-compile))

(use-package flycheck-cython
  :defer t
  :straight (:build t)
  :after cython-mode)

(use-package blacken
  :defer t
  :straight (:build t)
  :init
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package lsp-pyright
  :after lsp-mode
  :defer t
  :straight (:buidl t))

(use-package rustic
  :defer t
  :straight (:build t)
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode-local-vars . rustic-setup-lsp)
  :hook (rustic-mode . lsp-deferred)
  ;; :hook (rustic-mode . eglot-ensure)
  :init
  (with-eval-after-load 'org
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  ;; (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 2)
              (setq rust-indent-offset 2)))
  :general
  (general-define-key
   :keymaps 'rustic-mode-map
   :packages 'lsp
   "M-t" #'lsp-ui-imenu
   "M-?" #'lsp-find-references)
  (dqv/major-leader-key
    :keymaps 'rustic-mode-map
    :packages 'rustic
    "b"  '(:ignore t :which-key "Build")
    "bB" #'rustic-cargo-build
    "bB" #'rustic-cargo-bench
    "bc" #'rustic-cargo-check
    "bC" #'rustic-cargo-clippy
    "bn" #'rustic-cargo-new
    "bo" #'rustic-cargo-outdated
    "d" '(:ignore t :which-key "Debugging")
    "dr" #'dap-debug
    "dh" #'dap-hydra
    "dl" #'dap-debug-last
    "dR" #'dap-debug-restart
    "dq" #'dap-disconnect
    "da" #'dap-breakpoint-add
    "dt" #'dap-breakpoint-toggle
    "dd" #'dap-breakpoint-delete
    "dD" #'dap-breakpoint-delete-all
    "D" #'rustic-cargo-doc
    "f" '(:ignore t :which-key "LSP Find")
    "fr" #'lsp-find-references
    "fd" #'lsp-find-definition
    "ff" #'lsp-find-declaration
    "a" #'rustic-cargo-add
    "r" #'rustic-cargo-run
    "s" #'toggle-lsp-inlay-hints
    "c" '(:ignore t :which-key "clippy")
    "cf" #'rustic-cargo-clippy-fix
    "cr" #'rustic-cargo-clippy-run
    "cc" #'rustic-cargo-clippy
    "i" #'lsp-execute-code-action ;; auto import
    "l"  '(:ignore t :which-key "Lsp")
    "lr" #'lsp-rename
    "lq" #'lsp-workspace-restart
    "lQ" #'lsp-workspace-shutdown
    "ls" #'lsp-rust-analyzer-status
    "o" '(:ignore t :which-key "Lsp Open")
    "od" #'lsp-rust-analyzer-open-external-docs
    "ot" #'lsp-rust-analyzer-open-cargo-toml
    "t" '(:ignore t :which-key "Testing")
    "tc" #'rustic-cargo-current-test
    "tt" #'rustic-cargo-run-nextest
    "tr" #'lsp-rust-analyzer-related-tests
    "e" #'lsp-rust-analyzer-expand-macro
    "E" #'rustic-cargo-expand-rerun)
  :config
  (setq rustic-indent-method-chain    t
        rustic-babel-format-src-block nil
        rustic-format-trigger         nil)
  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flymake-mode-off)
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp))
;; Use Rustfmt for formatting Rust code in Rustic mode
(setq rustic-format-on-save t)
(setq rustic-format-display-method 'echo)
(setq rustic-format-trigger 'on-save)
(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-lsp-format t)
(setq rustic-lsp-client nil)
(setq rustic-rustfmt-bin (executable-find "rustfmt"))
(setq rustic-rustfmt-config "~/.rustfmt.toml")

(defun jd/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(defun my-rust-mode-auto-switch ()
  (when (and buffer-file-name
             (string-match-p "\\.rs\\'" buffer-file-name))
    (rustic-mode)))

(add-hook 'find-file-hook #'my-rust-mode-auto-switch)

(use-package emmet-mode
  :straight (:build t)
  :defer t
  :hook ((css-mode  . emmet-mode)
         (html-mode . emmet-mode)
         (web-mode  . emmet-mode)
         (sass-mode . emmet-mode)
         (scss-mode . emmet-mode)
         (web-mode  . emmet-mode))
  :config
  (general-define-key
   :keymaps 'emmet-mode-keymap
   "M-RET" #'emmet-expand-yas)
  (dqv/major-leader-key
    :keymaps 'web-mode-map
    :packages '(web-mode emmet-mode)
    "e" '(:ignore t :which-key "emmet")
    "ee" #'emmet-expand-line
    "ep" #'emmet-preview
    "eP" #'emmet-preview-mode
    "ew" #'emmet-wrap-with-markup))

(use-package impatient-mode
  :straight (:build t)
  :defer t)

(use-package web-mode
  :defer t
  :straight (:build t)
  :hook html-mode
  :hook (web-mode . prettier-js-mode)
  :hook (web-mode . lsp-deferred)
  :mode (("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.twig\\'"       . web-mode)
         ("\\.xml\\'"        . web-mode)
         ("\\.html\\'"       . web-mode)
         ("\\.htm\\'"        . web-mode)
         ("\\.[gj]sp\\'"     . web-mode)
         ("\\.as[cp]x?\\'"   . web-mode)
         ("\\.eex\\'"        . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'"        . web-mode)
         ("\\.eco\\'"        . web-mode)
         ("\\.ejs\\'"        . web-mode)
         ("\\.svelte\\'"     . web-mode)
         ("\\.ctp\\'"        . web-mode)
         ("\\.djhtml\\'"     . web-mode)
         ("\\.vue\\'"        . web-mode))
  :config
  (csetq web-mode-markup-indent-offset 2
         web-mode-code-indent-offset   2
         web-mode-css-indent-offset    2
         web-mode-style-padding        0
         web-mode-script-padding       0)
  :general
  (dqv/major-leader-key
    :keymaps 'web-mode-map
    :packages 'web-mode
    "="  '(:ignore t :which-key "format")
    "E"  '(:ignore t :which-key "errors")
    "El" #'web-mode-dom-errors-show
    "g"  '(:ignore t :which-key "goto")
    "gb" #'web-mode-element-beginning
    "ge" #'web-mode-element-end
    "gc" #'web-mode-element-child
    "gp" #'web-mode-element-parent
    "gs" #'web-mode-element-sibling-next
    "h"  '(:ignore t :which-key "dom")
    "hp" #'web-mode-dom-xpath
    "r"  '(:ignore t :which-key "refactor")
    "j"  '(web-mode-tag-match :which-key "Jump Match")
    "rc" #'web-mode-element-clone
    "rd" #'web-mode-element-vanish
    "rk" #'web-mode-element-kill
    "rr" #'web-mode-element-rename
    "rw" #'web-mode-element-wrap
    "z"  #'web-mode-fold-or-unfold)
  (dqv/major-leader-key
    :keymaps 'web-mode-map
    :packages '(lsp-mode web-mode)
    "l" '(:keymap lsp-command-map :which-key "lsp")))

(use-package company-web
  :defer t
  :straight (:build t)
  :after (emmet-mode web-mode))

(use-package css-mode
  :defer t
  :straight (:type built-in)
  :hook (css-mode . smartparens-mode)
  :hook (css-mode . lsp-deferred)
  :hook (scss-mode . prettier-js-mode)
  :init
  (put 'css-indent-offset 'safe-local-variable #'integerp)
  :general
  (dqv/major-leader-key
    :keymaps 'css-mode-map
    :packages 'css-mode
    "=" '(:ignore :which-key "format")
    "g" '(:ignore :which-key "goto")))

(use-package scss-mode
  :straight (:build t)
  :hook (scss-mode . smartparens-mode)
  :hook (scss-mode . lsp-deferred)
  :hook (scss-mode . prettier-js-mode)
  :defer t
  :mode "\\.scss\\'")

(use-package counsel-css
  :straight (:build t)
  :defer t
  :init
  (cl-loop for (mode-map . mode-hook) in '((css-mode-map  . css-mode-hook)
                                           (scss-mode-map . scss-mode-hook))
           do (add-hook mode-hook #'counsel-css-imenu-setup)
           (dqv/major-leader-key
             :keymaps mode-map
             "gh" #'counsel-css)))

(use-package less-css-mode
  :straight  (:type built-in)
  :defer t
  :mode "\\.less\\'"
  :hook (less-css-mode . smartparens-mode)
  :hook (less-css-mode . lsp-deferred)
  :hook (less-css-mode . prettier-js-mode))

(use-package rjsx-mode
  :defer t
  :straight (:build t)
  :after compile
  :mode "\\.[mc]?jsx?\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :hook (rjsx-mode . rainbow-delimiters-mode)
  :hook (rjsx-mode . lsp-deferred)
  :hook (rjsx-mode . prettier-js-mode)
  :init
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                      2 3 4))
  :general
  (dqv/major-leader-key
    :keymaps 'rjsx-mode-map
    "rr" #'rjsx-rename-tag-at-point
    "rj" #'rjsx-jump-tag)
  (dqv/evil
    :keymaps 'rjsx-mode-map
    "s-;" #'rjsx-jump-tag
    "s-r" #'rjsx-rename-tag-at-point)
  :config
  (setq js-chain-indent                  t
        js2-basic-offset                 2
        ;; ignore shebangs
        js2-skip-preprocessor-directives t
        ;; Flycheck handles this already
        js2-mode-show-parse-errors       nil
        js2-mode-show-strict-warnings    nil
        ;; conflicting with eslint, Flycheck already handles this
        js2-strict-missing-semi-warning  nil
        js2-highlight-level              3
        js2-idle-timer-delay             0.15))

(use-package js2-refactor
  :defer t
  :straight (:build t)
  :after (js2-mode rjsx-mode)
  :hook (js2-mode . js2-refactor-mode)
  :hook (rjsx-mode . js2-refactor-mode))

(use-package prettier-js
  :defer t
  :straight (:build t)
  :after (rjsx-mode web-mode typescript-mode)
  :hook (rjsx-mode . prettier-js-mode)
  :hook (js-mode . prettier-js-mode)
  :hook (typescript-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '("--trailing-comma" "all" "--bracket-spacing" "true")))

(use-package typescript-mode
  :defer t
  :straight (:build t)
  :hook (typescript-mode     . rainbow-delimiters-mode)
  :hook (typescript-mode     . lsp-deferred)
  :hook (typescript-mode     . prettier-js-mode)
  :hook (typescript-tsx-mode . lsp-deferred)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . prettier-js-mode)
  ;; :hook (typescript-tsx-mode . eglot-ensure)
  :commands typescript-tsx-mode
  :after flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  :general
  (dqv/major-leader-key
    :packages 'lsp
    :keymaps '(typescript-mode-map typescript-tsx-mode-map)
    :infix "a"
    ""  '(:keymap lsp-command-map :which-key "lsp")
    "=" '(:ignore t :which-key "format")
    "a" '(:ignore t :which-key "actions"))
  (dqv/major-leader-key
    :packages 'typescript-mode
    :keymaps '(typescript-mode-map typescript-tsx-mode-map)
    "n" '(:keymap npm-mode-command-keymap :which-key "pnpm"))
  :config
  (setq typescript-indent-level 2)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
    (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
  (when (fboundp 'web-mode)
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX"))
  (autoload 'js2-line-break "js2-mode" nil t))

(defun my-tsx-mode-auto-switch ()
  "Auto convert tsx-mode"
  (when (and buffer-file-name
             (string-match-p "\\.tsx\\'" buffer-file-name))
    (typescript-tsx-mode)))

(defun my-ts-mode-auto-switch ()
  "Auto convert typescript-mode"
  (when (and buffer-file-name
             (string-match-p "\\.ts\\'" buffer-file-name))
    (typescript-mode)))

(add-hook 'find-file-hook #'my-ts-mode-auto-switch)
(add-hook 'find-file-hook #'my-tsx-mode-auto-switch)

(use-package tide
  :defer t
  :straight (:build t)
  :hook (tide-mode . tide-hl-identifier-mode)
  :config
  (setq tide-completion-detailed              t
        tide-always-show-documentation        t
        tide-server-may-response-length       524288
        tide-completion-setup-company-backend nil)

  (advice-add #'tide-setup :after #'eldoc-mode)

  :general
  (dqv/major-leader-key
    :keymaps 'tide-mode-map
    "R"   #'tide-restart-server
    "f"   #'tide-format
    "rrs" #'tide-rename-symbol
    "roi" #'tide-organize-imports))

(use-package zig-mode
  :defer t
  :straight (:build t)
  :after flycheck
  :config
  ;; This is from DoomEmacs
  (flycheck-define-checker zig
    "A zig syntax checker using the zig-fmt interpreter."
    :command ("zig" "fmt" (eval (buffer-file-name)))
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
    :modes zig-mode)
  (add-to-list 'flycheck-checkers 'zig)
  :general
  (dqv/major-leader-key
    :packages 'zig-mode
    :keymaps 'zig-mode-map
    "c" #'zig-compile
    "f" #'zig-format-buffer
    "r" #'zig-run
    "t" #'zig-test-buffer))

(setq lsp-zig-zls-executable "~/opt/homebrew/bin/zls")

(use-package go-mode
  :straight (:build t)
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (tree-sitter-hl-mode)
              (lsp-go-install-save-hooks)))
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'before-save-hook #'gofmt-before-save))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-to-list 'auto-mode-alist '("\\.mod\\'" . go-mod-ts-mode))

(use-package go-snippets
  :defer t)

(defun my-go-mode-auto-switch ()
  (when (and buffer-file-name
             (string-match-p "\\.go\\'" buffer-file-name))
    (go-mode)))


(add-hook 'find-file-hook #'my-go-mode-auto-switch)

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(defun my/local-tab-indent ()
  (setq-local indent-tabs-mode 1))

(add-hook 'makefile-mode-hook #'my/local-tab-indent)

(use-package dotenv-mode
  :defer t
  :straight (:build t))
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode


;; (defun my-env-mode-auto-switch ()
;;   (when (and buffer-file-name
;;              (string-match-p "\\.env\\..*\\'" buffer-file-name))
;;     (dotenv-mode)))

;; (add-hook 'find-file-hook #'my-env-mode-auto-switch)

(add-hook 'solidity-mode-hook
  (lambda ()
  (set (make-local-variable 'company-backends)
    (append '((company-solidity company-capf company-dabbrev-code))
      company-backends))))

;; (require 'solidity-mode
;;   :defer t
;;   :straight (:build t)
;;   :config
;;   (csetq solidity-comment-style 'star))

;; (setq solidity-solc-path "/opt/homebrew/bin/solc")
(setq solidity-solium-path "/home/lefteris/.nvm/versions/node/v22.3.0/bin/solium")

(use-package solidity-flycheck
  :defer t
  :straight (:build t)
  :ensure t
  :config
  ;; (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-solium-checker-active t))

(use-package company-solidity
  :straight (:build t)
  :ensure t
  :defer t
)

(add-hook 'solidity-mode-hook
            (lambda ()
              (require 'lsp-mode)
              (add-to-list 'lsp-language-id-configuration '(solidity-mode . "soldity"))
              (add-to-list 'lsp-language-id-configuration '("\\.sol$" . "solidity"))
              (lsp-register-client
               (make-lsp-client :new-connection (lsp-stdio-connection '("solidity-ls" "--stdio"))
                                :activation-fn (lsp-activate-on "solidity")
                                :notification-handlers
                                (ht ("textDocument/didOpen" #'ignore))
                                :server-id 'solidity))
              (lsp)))

(use-package json-mode
  :straight (:build t)
  :mode "\\.json$"
  :config
  (add-to-list 'flycheck-disabled-checkers 'json-python-json)
  :general
  (dqv/major-leader-key
    :packages 'json-mode
    :keymaps 'json-mode-map
    "f" #'json-pretty-print-buffer))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))

(defun my-json-ts-mode-auto-switch ()
  (when (and buffer-file-name
             (string-match-p "\\.json\\'" buffer-file-name))
    (json-ts-mode)))

;; (add-hook 'find-file-hook #'my-json-ts-mode-auto-switch)

(use-package toml-mode
  :straight (:build t)
  :defer t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(use-package yaml-mode
  :defer t
  :straight (:build t)
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;; (use-package move-mode
;;   :straight t)

(require 'move-mode)

(defun jd/move-lsp-project-root (dir)
  (and-let* (((boundp 'eglot-lsp-context))
             (eglot-lsp-context)
             (override (locate-dominating-file dir "Move.toml")))
    (cons 'Move.toml override)))

(cl-defmethod project-root ((project (head Move.toml)))
  (cdr project))
(add-hook 'project-find-functions #'jd/move-lsp-project-root)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(move-mode . "move"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "move-analyzer")
    :activation-fn (lsp-activate-on "move")
    :priority -1
    :server-id 'move-analyzer)))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(move-mode "aptos-move-analyzer"))
  (add-to-list 'eglot-server-programs '(move-mode "sui-move-analyzer"))
  (add-to-list 'eglot-server-programs '(move-mode "move-analyzer")))

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;                          :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                          :build (:not compile))
;;   :init
;;   (setq lsp-bridge-default-mode-hooks
;;         (seq-remove (lambda (hook)
;;                       (eq hook 'move-mode))
;;                     lsp-bridge-default-mode-hooks))
;;   (global-lsp-bridge-mode))

(require 'func-mode)
(require 'tolk-mode)

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(use-package lsp-java
  :ensure t
  :after (lsp-mode)
  :straight (:build t)
  :config
  (add-hook 'java-mode-hook #'lsp-java-lens-mode))

;; (setq lsp-java-server-install-dir "/opt/homebrew/bin/jdtls")

(dqv/evil
  ;;:packages '(counsel)
  "s" '(window-configuration-to-register :wk "Register Window")
  "f" '(jump-to-register :wk "Jump Register")
  "K"   #'eldoc-print-current-symbol-info
  "+"   #'evil-goto-last-change
  "_"   #'evil-search-unbounded-word-forward
  "-"   #'evil-search-unbounded-word-backward
  "U"   #'evil-redo
  "C-a" #'my-smarter-move-beginning-of-line
  "C-e" #'end-of-line
  "C-y" #'yank
  "C-M-s-p"    #'scroll-half-page-up
  "C-M-s-n"    #'scroll-half-page-down
  "M-y" #'counsel-yank-pop)

(dqv/leader-key
  "SPC" '(counsel-M-x :which-key "M-x")
  "."  '(dirvish-dwim :which-key "Dirvish")
  ;; "."  '(dired-jump :which-key "Dirvish")
  "'"   '(shell-pop :which-key "shell-pop")
  ","  '(magit-status :which-key "Magit Status")
  "j" '(bufler-switch-buffer :which-key "Switch Buffer")
  "k" '(dqv/switch-to-previous-buffer :which-key "Switch to previous buffer")
  "oa" '(org-agenda :color blue :which-key "Agenda")
  "ol" '(my/avy-open-url :color blue :which-key "Open Url")
  "of" '(browse-file-directory :which-key "Open File in Directory")
  "h" '(:ignore t :which-key "Hash function")
  "hk" '(hash-keccak256 :which-key "Hash Region Keccak256")
  "hf" '(hash-keccak256-function-id :which-key "Hash Region Keccak256 FnId")
  "1" '(treemacs :which-key "Open Treemacs")

  "a" '(:ignore t :which-key "Application")
  "ac" '(calendar :which-key "Calendar")

  ;; "s" '(:ignore t :which-key "Set Timer")
  ;; "st" '(org-timer :which-key "Timer")
  ;; "si" '(org-timer-item :which-key "Timer")
  ;; "ss" '(org-timer-set-timer :which-key "Set Timer")
  ;; "sp" '(org-timer-pause-or-continue :which-key "Pause / Continue")
  ;; "s1" '(org-timer-start :which-key "Start")
  ;; "s2" '(org-timer-stop :which-key "Stop")
  "wr" '(writeroom-mode :which-key "Write Room")

  "d" '(:ignore t :which-key "Dirvish")
  "os" '(dirvish-side :which-key "Side")
  "il" '(org-insert-last-stored-link :which-key "Insert Stored Link")
  "is" '(org-store-link :which-key "Store Link"))

(dqv/leader-key
  :packages '(bufler)
  "b" '(:ignore t :which-key "Buffers")
  "J" '(bufler :which-key "Buflers")
  "bb" '(bufler-switch-buffer :which-key "Switch Buffer")
  "bB" '(bury-buffer :which-key "Bury Buffer")
  "bc" '(clone-indirect-buffer :which-key "Clone Indirect")
  "bC" '(clone-indirect-buffer-other-window :which-key "Clone Indirect Other Window")
  "bl" '(bufler :which-key "Bufler")
  "br" '(rename-buffer :which-key "Rename buffer")
  "bk" '(kill-this-buffer :which-key "Kill This Buffer")
  "bD" '(kill-buffer :which-key "Kill Buffer")
  "bh" '(dashboard-refresh-buffer :which-key "Dashboard Refresh Buffer")
  "bm" '(switch-to-message-buffer :which-key "Switch to message buffer")
  "bn" '(next-buffer :which-key "Next Buffer")
  "bp" '(previous-buffer :which-key "Next Buffer")
  "bs" '(switch-to-scratch-buffer :which-key "Scratch Buffer"))

(dqv/leader-key
  :packages '(treemacs)
  "t" '(:ignore t :wk "Treemacs")
  "tc" '(:ignore t :wk "Create")
  "tcd" '(treemacs-create-dir :which-key "Create Dir")
  "tcf" '(treemacs-create-file :which-key "Create File")
  "tci" '(treemacs-create-icon :which-key "Create Icon")
  "tct" '(treemacs-create-theme :which-key "Create Theme")
  "td" '(treemacs-delete-file :which-key "delete")
  "tw" '(:ignore t :wk "Workspace")
  "tws" '(treemacs-switch-workspace :which-key "Switch Workspace")
  "twc" '(treemacs-create-workspace :which-key "Create Workspace")
  "twr" '(treemacs-remove-workspace :which-key "Remove Workspace")
  "tf" '(:ignore t :wk "Files")
  "tff" '(treemacs-find-file :which-key "Find File")
  "tft" '(treemacs-find-tag :which-key "Find Tag")
  "tl" '(:ignore t :wk "LSP")
  "tls" '(treemacs-expand-lsp-symbol :which-key "Lsp Symbol")
  "tt" '(typescript-tsx-mode :which-key "TSX Mode")

  "tld" '(treemacs-expand-lsp-treemacs-deps :which-key "Lsp treemacs deps")
  "tlD" '(treemacs-collapse-lsp-treemacs-deps :which-key "Collapse lsp Deps")
  "tlS" '(treemacs-collapse-lsp-symbol :which-key "Collapse Lsp Symbol")
  "tp" '(:ignore t :wk "Projcets")
  "tpa" '(treemacs-add-project :which-key "Add project")
  "tpf" '(treemacs-project-follow-mode :which-key "Follow mode")
  "tpn" '(treemacs-project-of-node :which-key "Of Node")
  "tpp" '(treemacs-project-at-point :which-key "At Point")
  "tpr" '(treemacs-remove-project-from-workspace :which-key "Remove project")
  "tpt" '(treemacs-move-project-down :which-key "Project Down")
  "tps" '(treemacs-move-project-up :which-key "Project Up")
  "tr" '(:ignore t :wk "Rename")
  "trf" '(treemacs-rename-file :which-key "Rename File")
  "trp" '(treemacs-rename-project :which-key "Rename project")
  "trr" '(treemacs-rename :which-key "Rename")
  "trw" '(treemacs-rename-workspace :which-key "Rename Workspace")
  "tT" '(:ignore t :wk "Toggle")
  "tTd" '(treemacs-toggle-show-dotfiles :which-key "Toggle show Dotfiles")
  "tTn" '(treemacs-toggle-node :which-key "Toggle Node")
  "tv" '(:ignore t :wk "Visit Node")
  "tva" '(treemacs-visit-node-ace :which-key "Visit Ace")
  "tvc" '(treemacs-visit-node-close-treemacs :which-key "Visit Node Close")
  "tvn" '(treemacs-visit-node-default :which-key "Visit Node")
  "ty" '(:ignore t :wk "Yank")
  "tya" '(treemacs-copy-absolute-path-at-point :which-key "Absolute")
  "typ" '(treemacs-copy-project-path-at-point :which-key "Project")
  "tyr" '(treemacs-copy-relative-path-at-point :which-key "Relative")
  "tyr" '(treemacs-copy-file :which-key "file"))

(dqv/leader-key
  "c"   '(:ignore t :wk "code")
  "cl"  #'evilnc-comment-or-uncomment-lines

  "e"  '(:ignore t :which-key "errors")
  "e." '(hydra-flycheck/body :wk "hydra")
  "el" '(counsel-flycheck :wk "Flycheck")
  "eF" #'flyspell-hydra/body

  "f"   '(:ignore t :wk "Files")
  "ff" '(counsel-find-file :wk "Find Files")
  "fD" '(dqv/delete-this-file :wk "Delete Files")
  "fr" '(counsel-recentf :wk "Recentf Files"))

(dqv/leader-key
  ;; "h"   '(:ignore t :wk "Help")
  ;; "hi" '(info :wk "Info")
  ;; "hI" '(info-display-manual :wk "Info Display")
  ;; "hd"   '(:ignore t :wk "Describe")
  ;; "hdk" '(helpful-key :wk "Key")
  ;; "hdm" '(helpful-macro :wk "Macro")
  ;; "hds" '(helpful-symbol :wk "Symbol")
  ;; "hdv" '(helpful-variable :wk "Variable")

  "i"   '(:ignore t :wk "insert")
  "iu"  #'counsel-unicode-char

  "t"   '(:ignore t :wk "Insert")
  "tT"  #'counsel-load-theme
  "tml"  #'modus-themes-load-operandi
  "tmd"  #'modus-themes-load-vivendi
  "td"  '(:ignore t :wk "Debug")
  "tde"  #'toggle-debug-on-error
  "tdq"  #'toggle-debug-on-quit
  "ti"   '(:ignore t :wk "Input")
  "tit"  #'toggle-input-method
  "tis"  #'set-input-method

  "T"   '(:ignore t :wk "Input")
  "Te"  #'string-edit-at-point
  "Tu"  #'downcase-region
  "TU"  #'upcase-region
  "Tz"  #'hydra-zoom/body

  "w"   '(:ignore t :wk "Windows")
  "wh" '(evil-window-left :wk "Left")
  "wj" '(evil-window-down :wk "Down")
  "wk" '(evil-window-up :wk "Up")
  "wl" '(evil-window-right :wk "Right")
  "ws" '(split-window-below-and-focus :wk "Split")
  "wv" '(split-window-right-and-focus :wk "Verticle Split")
  "wi" '(windows-adjust-size/body :wk "Window Size")
  "w1" #'winum-select-window-1
  "w2" #'winum-select-window-2
  "w3" #'winum-select-window-3
  "w4" #'winum-select-window-4
  "wc" '(kill-buffer-and-delete-window :wk "Kill & Delete")
  "wO" '(dqv/kill-other-buffers :wk "Kill other window")
  "wd" '(delete-window :wk "Delete window")
  "wo" '(delete-other-windows :wk "delete others window")

  "ag"   '(:ignore t :wk "Gcal")
  "agp"  #'org-gcal-post-at-point
  "agR"  #'org-gcal-reload-client-id-secret
  "ags"  #'org-gcal-sync
  "agS"  #'org-gcal-sync-buffer
  "agf"  #'org-gcal-fetch
  "agF"  #'org-gcal-fetch-buffer
  "agd"  #'org-gcal-delete-at-point
  "agr"  #'org-gcal-request-token
  "agt"  #'org-gcal-toggle-debug

  "n"   '(:ignore t :wk "Gcal")
  "nn"  #'org-roam-node-find
  "naa"  #'org-roam-alias-add
  "nar"  #'org-roam-alias-remove
  "ni"  #'org-roam-node-insert
  "nl"  #'org-roam-buffer-toggle
  "nct"  #'org-roam-dailies-capture-today
  "ncT"  #'org-roam-dailies-capture-tomorrow
  "nfd"  #'org-roam-dailies-find-date
  "nft"  #'org-roam-dailies-find-today
  "nfy"  #'org-roam-dailies-find-yesterday
  "nfT"  #'org-roam-dailies-find-tomorrow
  "ng"  #'org-roam-graph
  "nbs"  #'bookmark-set
  "nbj"  #'bookmark-jump
  "nbi"  #'bookmark-insert
  "nbl"  #'bookmark-bmenu-list

  "l"   '(:ignore t :wk "Lsp")
  "ll"  #'lsp
  "lm"  #'lsp-ui-imenu
  "lwr"  #'lsp-workspace-restart
  "ls"  #'lsp-treemacs-symbols
  "le"  #'lsp-treemacs-errors-list
  "lE"  #'lsp-ui-flycheck-list
  "ld"  #'lsp-find-definition
  "lr"  #'lsp-find-references
  "lh"  #'lsp-inlay-hints-mode
  "lD"  #'xref-find-definitions
  "lr"  #'xref-find-definitions
  "lR"  #'lsp-rename

  "all" #'leetcode
  "ald" #'leetcode-daily
  "alo" #'leetcode-show-problem-in-browser
  "alO" #'leetcode-show-problem-by-slub
  "alS" #'leetcode-submit
  "als" #'leetcode-show-problem

  "p" '(:ignore t :wk "Projectile")
  "p!" #'projectile-run-shell-command-in-root
  "p&" #'projectile-run-async-shell-command-in-root
  "pb" #'counsel-projectile-switch-to-buffer
  "pc" #'counsel-projectile
  "pr" #'projectile-remove-known-project
  "pd" #'counsel-projectile-find-dir
  "pe" #'projectile-edit-dir-locals
  "pf" #'counsel-projectile-find-file
  "pF" #'counsel-projectile-switch-to-buffer
  "pg" #'projectile-find-tag
  "pk" #'project-kill-buffers
  "pp" #'counsel-projectile-switch-project
  "pt" #'ivy-magit-todos
  "pv" #'projectile-vc

  "u"   #'universal-argument
  "U"   #'undo-tree-visualize

  "fd"  '((lambda ()
            (interactive)
            (find-file "~/Dropbox/Roam/"))
          :which-key "Roam")

  "fo"  '((lambda ()
            (interactive)
            (find-file "~/Dropbox/Roam/README.org"))
          :which-key "init.el")


  "fc"  '((lambda ()
            (interactive)
            (find-file "~/.emacs.d/jayden.org"))
          :which-key "jayden.org")

  "fi"  '((lambda ()
            (interactive)
            (find-file (concat user-emacs-directory "init.el")))
          :which-key "init.el")

  "fR"  '((lambda ()
            (interactive)
            (counsel-find-file ""
                               (concat user-emacs-directory
                                       (file-name-as-directory "straight")
                                       (file-name-as-directory "repos"))))
          :which-key "straight package")

  "owgp"  '((lambda ()
              (interactive)
              (browse-url "https://github.com/jayden_dangvu"))
            :wk "My Github")

  "owgw"  '((lambda ()
              (interactive)
              (browse-url "https://github.com/orgs/TOCE-Team/repositories"))
            :wk "Work Github")

  "owe"  '((lambda ()
             (interactive)
             (browse-url "https://remix.ethereum.org/"))
           :wk "Remix IDE")

  "owr"  '((lambda ()
             (interactive)
             (browse-url "https://reddit.com/"))
           :wk "Reddit")

  "owc"  '((lambda ()
             (interactive)
             (browse-url "https://calendar.google.com/calendar/u/0/r?pli=1"))
           :wk "My Calender")

  "owwc"  '((lambda ()
              (interactive)
              (browse-url "https://chat.openai.com"))
            :wk "Chat GPT"))

(dqv/leader-key
  "dd" '(docker :which-key "Docker")
  "dI" '(docker-images :which-key "Docker Images")
  "dV" '(docker-volumes :which-key "Docker Volumes")
  "dC" '(docker-containers :which-key "Docker Containers")
  "dN" '(docker-networks :which-key "Docker Networks")
  )

(use-package lsp-grammarly
  :straight (:build t)
  :ensure nil
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp-deferred))))  ; or lsp-deferred

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(use-package markdown-mode
  :straight t)

(defun split-window-right-and-focus ()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun jd/eldoc-and-focus ()
  (interactive)
  (eldoc-print-current-symbol-info)
  (evil-window-next))

(defun split-window-below-and-focus ()
  "Spawn a new window below the current one and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-open-point ()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (org-open-at-point))


(defun kill-buffer-and-delete-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))

(defun display-buffer-same-window (buffer alist)
  (unless (or (cdr (assq 'inhibit-same-window alist))
              (window-minibuffer-p)
              (window-dedicated-p))
    (window--display-buffer buffer (selected-window) 'reuse alist)))

(defun hash-keccak256 (start end)
  "Prompt for input text, show the Keccak-256 hash in a Popwin popup (if enabled), and copy it to the clipboard."
  (interactive "r")
  (let* ((selected-text (buffer-substring-no-properties start end))
         (input (read-string "Input text: " selected-text))
         (temp-file (make-temp-file "keccak256")))
    (with-temp-file temp-file
      (insert input))
    (let ((hash (shell-command-to-string (format "keccak-256sum %s | awk '{print $1}'" temp-file))))
      (delete-file temp-file)
      (kill-new hash)
      ;; Uncomment the following block to use popwin
      ;; (let ((popwin:special-display-config '(("*Keccak-256 Hash*"
      ;;                                        :position bottom
      ;;                                        :height 15))))
      ;;   (popwin:display-buffer (get-buffer-create "*Keccak-256 Hash*"))
      ;;   (with-current-buffer "*Keccak-256 Hash*"
      ;;     (erase-buffer)
      ;;     (insert hash)
      ;;     (goto-char (point-min))))
      (message "Original text: %s\nKeccak-256 hash: %s" input hash))))

(defun hash-keccak256-function-id (start end)
  "Prompt for input function signature, hash the cleaned function signature using Keccak-256, and copy the hash to the clipboard."
  (interactive "r")
  (let* ((selected-text (buffer-substring-no-properties start end))
         (input (read-string "Input function signature: " selected-text))
         ;; Remove spaces around commas for uniformity
         (clean-input (replace-regexp-in-string "\\s-*\\(,\\)\\s-*" "," input))
         ;; Remove function parameters names and leave only types
         (cleaned-signature (replace-regexp-in-string "\\(\\w+\\)\\s-+\\(\\w+\\)" "\\1" clean-input))
         (temp-file (make-temp-file "keccak256")))
    (with-temp-file temp-file
      (insert cleaned-signature))
    (let ((hash (shell-command-to-string (format "keccak-256sum %s | awk '{print $1}'" temp-file))))
      (delete-file temp-file)
      (kill-new hash)
      (message "Cleaned function signature: %s\nKeccak-256 hash of function ID: %s" cleaned-signature hash))))

(use-package gptel
  :straight (:build t))

(setq
 gptel-model 'gemini-pro
 gptel-backend (gptel-make-gemini "Gemini"
                 :key "AIzaSyC1p8zt6ZFYK8DD7FPzKaKRLLfM_ls7Ie0"
                 :stream t))
