;;; Initialization Config
(let ((rmg:indent-spaces 2)
      (rmg:el-get-packages '(el-get)))

;;; User info
  (setq user-full-name "Robert Grimm"
        user-mail-address (concat "grimm" ".rob@" "gmail" ".com"))

;;; Package Installation Setup
  ;; el-get directory
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

  ;; load
  (unless (require 'el-get nil t)
    (with-current-buffer
    (url-retrieve-synchronously
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
       (goto-char (point-max))
       (eval-print-last-sexp)))

  ;; el-get package directory
  (setq-default el-get-user-package-directory "~/.emacs.d/init-files/")

  ;; custom recipes
  (setq el-get-sources
        '((:name auto-complete
                 :after (progn
                          ;; Load auto-complete
                          (require 'auto-complete-config)

                          ;; Autocomplete dictionary directory
                          (make-directory "~/.emacs.d/ac-dicts" t)
                          (add-to-list 'ac-dictionary-directories
                                       "~/.emacs.d/ac-dicts")

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
                          (color-theme-initialize)
                          (if window-system
                              (progn
                                (add-hook 'window-setup-hook
                                          #'color-theme-twilight
                                          t)
                                (color-theme-twilight))
                            (color-theme-euphoria))))
          (:name google-c-style
                 :after (progn
                          ;; Auto-start google C style
                          (add-hook 'c-mode-common-hook 'google-set-c-style)))
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

                          ;; Default to 2 spaces
                          (setq-default js2-basic-offset rmg:indent-spaces)
                          (setq js2-basic-offset rmg:indent-spaces)

                          ;; Allow code folding on javascript
                          (add-hook 'js2-mode-hook
                                    (lambda ()
                                      ;; Scan the file for nested code blocks
                                      (imenu-add-menubar-index)
                                      ;; Activate the folding mode
                                      (hs-minor-mode 1)))))
          (:name js-comint
                 :after (progn
                          ;; Prevent node from using readline
                          (setenv "NODE_NO_READLINE" "1")))
          (:name magit
                 :after (progn
                          ;; Keybinding for magit-status
                          (global-set-key (kbd "C-x C-z") 'magit-status)

                          ;; Magit Projects
                          (setq magit-repo-dirs '("~/Projects"
                                                  "~/.emacs.d"))))
          (:name smex
                 :after (progn
                          ;; Save file
                          (setq smex-save-file "~/.emacs.d/smex-items")

                          ;; Keybinding (replace default M-x)
                          (global-set-key (kbd "M-x") 'smex)

                          ;; (global-set-key (kbd "M-X")
                          ;;                'smex-major-mode-commands)
                          ))
          (:name yasnippet
                 :after (progn
                          ;; Shut up. Seriously. Wayyyyy too spammy
                          (setq yas-verbosity 0)

                          ;; Yas should work everywhere
                          (yas-global-mode 1)

                          ;; Save new snippets into .emacs.d/snippets
                          (setq yas/root-directory '("~/.emacs.d/snippets"))

                          ;; Also load from the default
                          (add-to-list 'yas/root-directory
                                       (concat default-directory "snippets")
                                       t)

                          ;; Load snippets automatically
                          (mapc 'yas/load-directory yas/root-directory)

                          ;; Set the display style to match auto-complete
                          (setq yas-prompt-functions '(yas-dropdown-prompt))))
          ))

  ;; Include local sources into rmg:el-get-packages
  (setq rmg:el-get-packages
        (append rmg:el-get-packages
                (mapcar 'el-get-source-name el-get-sources)))

  ;; Initialize el-get synchronously
  (el-get 'sync rmg:el-get-packages)

;;; Customizations
  ;; Custom panel setup and maximize
  (add-to-list 'load-path "~/.emacs.d/custom")
  (require 'rmg-panels-v2)
  (require 'rmg-maximize)
  (require 'rmg-host-specific)

  ;; Hotkeys for standard window setups
  (global-set-key (kbd "C-x C-1") 'rmg-setup-windows)
  (global-set-key (kbd "C-x C-!") 'rmg-setup-windows)
  (global-set-key (kbd "C-x C-2") 'rmg-setup-windows-2)
  (global-set-key (kbd "C-x C-@") 'rmg-setup-windows-2)

  ;; Load eshell
  (require 'eshell)
  (setq eshell-directory-name "~/.emacs.d/eshell/")
  (setq eshell-login-script "~/.emacs.d/eshell/login")
  (setq eshell-rc-script "~/.emacs.d/eshell/profile")

  ;; Hotkeys to move back and forth between frames
  (global-set-key (kbd "C-o") 'other-window)
  (defun rmg-prev-window ()
    "Perform the opposite operation of (other-window)"
    (interactive)
    (other-window -1))
  (global-set-key (kbd "M-o") 'rmg-prev-window)

;;; General
  ;; Backup and saves
  (make-directory "~/.emacs.d/backup" t)
  (setq backup-directory-alist `(("." . "~/.emacs.d/backup"))
        backup-by-copying t   ; Don't delink hardlinks
        version-control t     ; Use version numbers on backups
        delete-old-versions t ; Automatically delete excess backups
        kept-new-versions 20  ; how many of the newest versions to keep
        kept-old-versions 5   ; and how many of the old
        )

  ;; Auto-save
  (make-directory "~/.emacs.d/auto-save" t)
  (make-directory "~/.emacs.d/auto-save-list" t)
  (setq auto-save-file-name-transforms
        `((".*" ,"~/.emacs.d/auto-save/" t)))

  ;; Tetris scores
  (make-directory "~/.emacs.d/games" t)
  (setq tetris-score-file "~/.emacs.d/games/tetris-scores")

;;; Display
  ;; No splash screen
  (setq inhibit-splash-screen t)

  ;; Line and Column number mode
  (line-number-mode 1)
  (column-number-mode 1)

  ;; Setup graphical window
  (add-hook 'window-setup-hook
            (lambda ()
              (when window-system
                ;; Title format
                (setq frame-title-format (concat "%b - "
                                                 (downcase user-login-name)
                                                 "@"
                                                 (downcase system-name)))

                ;; No scrollbars
                (scroll-bar-mode -1)
                ;; No toolbar
                (tool-bar-mode -1)
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

  ;; Don't blink the cursor
  (blink-cursor-mode -1)

  ;; Always highlight the current line
  (global-hl-line-mode 1)

;;; Behavior
  ;; UTF-8
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  ;; Default to Chinese pinyin
  (setq default-input-method 'chinese-py)

  ;; Sentences end with a dot, not with two spaces
  (setq sentence-end-double-space nil)

  ;; Never insert tabs automatically
  (setq-default indent-tabs-mode nil)

  ;; Default space width
  (setq default-tab-width rmg:indent-spaces)
  (setq-default tab-width rmg:indent-spaces)

  (setq-default c-basic-offset rmg:indent-spaces)
  (setq c-basic-offset rmg:indent-spaces)

  (setq css-indent-offset rmg:indent-spaces)

  (setq-default sh-basic-offset rmg:indent-spaces)
  (setq-default sh-indentation rmg:indent-spaces)

  (setq-default perl-indent-level rmg:indent-spaces)

  (setq-default js-indent-level rmg:indent-spaces)

  ;; Fill at 79, not 70
  (setq default-fill-column 79)

  ;; Middle click should paste at cursor position, not mouse position
  (setq mouse-yank-at-point t)

  ;; Additional hideshow hotkeys
  (add-hook 'hs-minor-mode-hook (lambda ()
                                  (when hs-minor-mode
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h H") 'hs-hide-all)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h S") 'hs-show-all)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h h") 'hs-hide-block)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h s") 'hs-show-block)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h t") 'hs-toggle-hiding))))

  ;; Don't use this init.el for customizations
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file 'noerror)

  ;; Start the emacs server for emacsclient
  ;;(server-start)

;;; End
)
