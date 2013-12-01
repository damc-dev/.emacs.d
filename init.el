;;; Many ideas from:
;; What the .emacs.d?! (whattheemacsd.com)
;; Emacks Rocks (emacsrocks.com)

;;; Initialization Config
(let ((rmg:el-get-packages '(el-get markdown-mode google-c-style)))

;;; State file directory
  (setq rmg:state-directory (concat user-emacs-directory "state/"))
  (make-directory rmg:state-directory t)

;;; User info
  (setq user-full-name "Robert Grimm"
        user-mail-address (rot13 "tevzz.ebo@tznvy.pbz"))

;;; Package Installation Setup
  ;; Store cookies in state
  (setq url-cookie-file (concat rmg:state-directory "url-cookies"))

  ;; ELPA setup
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages")
               t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               t)
  (package-initialize t)

  ;; el-get directory
  (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

  ;; load
  (unless (require 'el-get nil t)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (let (el-get-master-branch)
        (eval-print-last-sexp))))

  ;; el-get package directory
  (setq-default el-get-user-package-directory (concat user-emacs-directory
                                                      "init-files/"))

  ;; custom recipes
  (setq el-get-sources
        '((:name auto-complete
                 :after (progn
                          ;; Load auto-complete
                          (require 'auto-complete-config)

                          ;; Autocomplete dictionary directory
                          (make-directory (concat user-emacs-directory
                                                  "ac-dicts/")
                                          t)
                          (add-to-list 'ac-dictionary-directories
                                       (concat user-emacs-directory
                                               "ac-dicts/"))

                          ;; Autocomplete completion history file
                          (setq ac-comphist-file (concat rmg:state-directory
                                                         "ac-comphist.dat"))

                          ;; Include dictionaries as autocomplete sources
                          (add-to-list 'ac-sources
                                       'ac-source-dictionary)

                          ;; ac-sources becomes buffer local when changed
                          ;; so set the new value as default too
                          (setq-default ac-sources ac-sources)

                          ;; Autocomplete in every buffer
                          (global-auto-complete-mode 1)

                          ;; Other settings
                          (setq ac-auto-show-menu 4    ; show menu after 4 sec
                                ac-auto-start 1        ; start after 1 char
                                ac-ignore-case nil     ; case-specific
                                ac-use-menu-map t)))   ; hotkeys in ac options
          (:name auto-complete-emacs-lisp
                 :after (progn
                          ;; Include emacs-lisp-features in autocomplete options
                          (add-to-list 'ac-sources
                                       'ac-source-emacs-lisp-features)

                          ;; Set default (see auto-complete)
                          (setq-default ac-sources ac-sources)))
          (:name auto-complete-yasnippet
                 :after (progn
                          ;; Include yasnippet into autocomplete options
                          (add-to-list 'ac-sources
                                       'ac-source-yasnippet)

                          ;; Set default (see auto-complete)
                          (setq-default ac-sources ac-sources)))
          (:name color-theme-twilight
                 :after (progn
                          (when (display-color-p)
                            (color-theme-initialize)
                            (if (display-graphic-p)
                                (progn
                                  (add-hook 'window-setup-hook
                                            #'color-theme-twilight
                                            t)
                                  (color-theme-twilight))
                              (color-theme-euphoria)))))
          (:name diminish
                 :after (progn
                          (defmacro rmg-rename-modeline (package-name
                                                         mode
                                                         &optional new-name)
                            `(eval-after-load ,package-name
                               '(defadvice ,mode (after
                                                  rename-modeline
                                                  activate)
                                  (setq mode-name ,new-name))))))
          (:name find-file-in-project
                 :after (progn
                          ;; From http://whattheemacsd.com/key-bindings.el-04.html
                          (defun ffip-create-pattern-file-finder (&rest
                                                                  patterns)
                            (lexical-let ((patterns patterns))
                              (lambda ()
                                (interactive)
                                (let ((ffip-patterns patterns))
                                  (find-file-in-project)))))))
          (:name guide-key
                 :website "https://github.com/kbkbkbkb1/guide-key#readme"
                 :type github
                 :pkgname "kbkbkbkb1/guide-key"
                 :depends (popwin)
                 :after (progn
                          ;; Load guide-key immediately
                          (require 'guide-key)

                          ;; Show guides for certain sets of key bind prefixes
                          (setq guide-key/guide-key-sequence '("C-x r"
                                                               "C-x v"
                                                               "C-x 4"
                                                               "C-x 5"
                                                               "C-x 6"
                                                               "C-x 8"
                                                               "C-c"))

                          ;; Show recursively
                          (setq guide-key/recursive-key-sequence-flag t)

                          ;; Show guide at the bottom
                          (setq guide-key/popup-window-position 'bottom)

                          ;; Don't show guide-key immediately
                          (setq guide-key/polling-time 1)

                          ;; Enable guide-key mode
                          (guide-key-mode 1)))
          (:name ido-ubiquitous
                 :after (progn
                          ;; Auto-start ido-ubiquitous, which is only active
                          ;; when ido-mode is also active
                          (require 'ido-ubiquitous)

                          ;; Use ido-mode to find files and buffers
                          (setq ido-save-directory-list-file
                                (concat rmg:state-directory "ido.last"))
                          (ido-mode 1)
                          (setq ido-auto-merge-work-directories-length -1
                                ;; There's a bug with ido and magit for
                                ;; .emacs.d, so dot-prefix has to be nil for
                                ;; now
                                ido-enable-dot-prefix nil
                                ido-enable-flex-matching t
                                ido-enable-regexp t
                                ido-ignore-extensions t)
                          (add-to-list 'ido-ignore-directories
                                       "^\\.$"
                                       "^node_modules$")

                          ;; Start ido-ubiquitous
                          (ido-ubiquitous-mode 1)))
          (:name jade-mode
                 :website "https://github.com/brianc/jade-mode#readme"
                 :type github
                 :pkgname "brianc/jade-mode"
                 :prepare (progn
                            (autoload 'sws-mode "sws-mode.el"
                              "Significant WhiteSpace Mode; see sws-mode.el." t)
                            (autoload 'jade-mode "jade-mode.el"
                              "JADE mode; see jade-mode.el" t))
                 :after (progn
                          (add-to-list 'auto-mode-alist
                                       '("\\.styl$" . sws-mode))
                          (add-to-list 'auto-mode-alist
                                       '("\\.jade$" . jade-mode))))
          (:name js2-mode
                 :after (progn
                          ;; Set all declaractions to pretty print
                          (setq-default js2-pretty-multiline-declarations 'all)
                          (setq js2-pretty-multiline-declarations 'all)

                          ;; Allow code folding on javascript
                          (add-hook 'js2-mode-hook
                                    (lambda ()
                                      ;; Scan the file for nested code blocks
                                      (imenu-add-menubar-index)
                                      ;; Activate the folding mode
                                      (hs-minor-mode 1)))))
          ;;(:name js-comint
          ;;       :after (progn
          ;;                ;; Prevent node from using readline
          ;;                (setenv "NODE_NO_READLINE" "1")))
          (:name smex
                 :after (progn
                          ;; Save file
                          (setq smex-save-file (concat rmg:state-directory
                                                       "smex-items"))))
          (:name undo-tree
                 :after (progn
                          (global-undo-tree-mode 1)))
          (:name yasnippet
                 :after (progn
                          ;; Shut up. Seriously. Wayyyyy too spammy
                          (setq yas-verbosity 0)

                          ;; Yas should work everywhere
                          (yas-global-mode 1)

                          ;; Save new snippets into .emacs.d/snippets
                          (setq yas/root-directory
                                (make-list 1 (concat user-emacs-directory
                                                     "snippets/")))

                          ;; Also load from the default
                          ;; (Disabling these, because they're annoying)
                          ;; (add-to-list 'yas/root-directory
                          ;;              (concat default-directory "snippets/")
                          ;;              t)

                          ;; Load snippets automatically
                          (mapc 'yas/load-directory yas/root-directory)

                          ;; Set the display style to match auto-complete
                          (setq yas-prompt-functions '(yas-dropdown-prompt))))
          ))

  ;; Don't load magit in w32
  (unless (equal (window-system) 'w32)
    (add-to-list 'el-get-sources
                 '(:name magit
                         :after (progn
                                  ;; Use ido for magit
                                  (setq magit-completing-read-function
                                        'magit-ido-completing-read)

                                  ;; Magit Projects
                                  (setq magit-repo-dirs `("~/Projects"
                                                          ,user-emacs-directory)
                                        )))))


  ;; Include local sources into rmg:el-get-packages
  (setq rmg:el-get-packages
        (append rmg:el-get-packages
                (mapcar 'el-get-source-name el-get-sources)))

  ;; Initialize el-get synchronously
  (el-get 'sync rmg:el-get-packages)

;;; Customizations and defuns
  ;; Custom panel setup and maximize
  (add-to-list 'load-path (concat user-emacs-directory "custom"))
  (require 'rmg-panels-v2)
  (require 'rmg-maximize)
  (require 'rmg-host-specific)

  ;; Function to move reverse through windows
  (defun rmg-prev-window ()
    "Perform the opposite operation of (other-window)"
    (interactive)
    (other-window -1))

  ;; Function to kill region or kill backward a word
  (defun rmg-kill-region-or-backward-word ()
    "If a region is active, kill it. Otherwise, kill the previous word."
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))

  ;; Function to temporarily show line numbers
  (defun rmg-goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))

  ;; Function to delete a character, or kill a process, or kill the buffer
  (defun rmg-comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

;;; General
  ;; Backup and saves
  (make-directory (concat user-emacs-directory "backup/") t)
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backup/")))
        backup-by-copying t   ; Don't delink hardlinks
        version-control t     ; Use version numbers on backups
        delete-old-versions t ; Automatically delete excess backups
        kept-new-versions 20  ; how many of the newest versions to keep
        kept-old-versions 5   ; and how many of the old
        vc-make-backup-files t; Make backups even when in git/svn/etc
        )

  (setq tramp-backup-directory-alist backup-directory-alist)

  ;; Auto-save
  (make-directory (concat user-emacs-directory "auto-save/") t)
  (make-directory (concat user-emacs-directory "auto-save-list/") t)
  (setq auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "auto-save/") t)))
  (setq tramp-auto-save-directory (concat user-emacs-directory "auto-save/"))

  ;; Save place
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (concat rmg:state-directory "places"))

  ;; Tetris scores
  (make-directory (concat rmg:state-directory "games/") t)
  (setq tetris-score-file (concat rmg:state-directory "games/tetris-scores"))

  ;; Load eshell
  (require 'eshell)
  (setq eshell-directory-name (concat user-emacs-directory "eshell/"))
  (setq eshell-login-script (concat eshell-directory-name "login"))
  (setq eshell-rc-script (concat eshell-directory-name "profile"))

;;; Display
  ;; No splash screen
  (setq inhibit-splash-screen t)

  ;; Line and Column number mode
  (line-number-mode 1)
  (column-number-mode 1)

  ;; Setup graphical window
  (add-hook 'window-setup-hook
            (lambda ()
              (when (display-graphic-p)
                ;; Title format
                (setq frame-title-format (concat "%b - "
                                                 (downcase user-login-name)
                                                 "@"
                                                 (downcase system-name)))

                ;; No scrollbars
                (when (fboundp 'scroll-bar-mode)
                  (scroll-bar-mode -1))
                ;; No toolbar
                (when (fboundp 'tool-bar-mode)
                  (tool-bar-mode -1))
                ;; Fringe only on right
                (set-fringe-mode '(0 . 8)))))

  ;; Show bad whitespace
  (global-whitespace-mode 1)
  (setq-default whitespace-style '(face
                                   trailing
                                   tabs
                                   lines-tail
                                   empty
                                   indentation
                                   space-after-tab
                                   space-before-tab))

  ;; Always font lock
  (global-font-lock-mode 1)

  ;; Don't blink the cursor
  (blink-cursor-mode -1)

  ;; Highlight matching parenthesis
  (show-paren-mode 1)
  (setq show-paren-style 'mixed)

  ;; Highlight the current line when GUI
  (when (display-graphic-p)
    (global-hl-line-mode 1))

  ;; Show active region
  (transient-mark-mode 1)
  (make-variable-buffer-local 'transient-mark-mode)
  (put 'transient-mark-mode 'permanent-local t)
  (setq-default transient-mark-mode t)

;;; Behavior
  ;; Set unique file names to filename.txt<distinguishing/path/to>
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

  ;; Pretty-print eval
  (global-set-key [remap eval-expression] 'pp-eval-expression)
  (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

  ;; Apropos should cover everything
  (setq apropos-do-all t)

  ;; Allow recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  ;; UTF-8
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  ;; Default to Chinese pinyin (C-\ is default to toggle IME)
  (setq default-input-method 'chinese-py)

  ;; Sentences end with a dot, not with two spaces
  (setq sentence-end-double-space nil)

  ;; Automatically revert buffers
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Don't use shift to mark things
  (setq shift-select-mode nil)

  ;; Transparently open compressed files
  (auto-compression-mode t)

  ;; Fill at 79, not 70
  (setq default-fill-column 79)

  ;; Middle click should paste at cursor position, not mouse position
  (when (display-mouse-p)
    (setq mouse-yank-at-point t))

  ;; Set code style
  (require 'rmg-code-style)

  ;; Load keybindings
  (require 'rmg-keybindings)

  ;; Use y-or-n-p instead of yes-or-no-p
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Don't use this init.el for customizations
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror)

  ;; Start the emacs server for emacsclient
  (server-start)

  ;; Diminish global minor modes
  (when (functionp 'diminish)
    (defmacro rmg-diminish (mode &optional new-name)
      `(when (assq ,mode minor-mode-alist)
         (diminish ,mode ,new-name)))
    (rmg-diminish 'auto-complete-mode)
    (rmg-diminish 'global-whitespace-mode)
    (rmg-diminish 'guide-key-mode)
    (rmg-diminish 'undo-tree-mode)
    (rmg-diminish 'yas-minor-mode))

;;; End
)
