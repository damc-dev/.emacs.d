;;; Initialization Config
(let ((rmg:indent-spaces 2)
      (rmg:el-get-packages '(auto-complete-yasnippet
                             color-theme-twilight
                             el-get
                             google-c-style
                             js-comint
                             js2-mode
                             magit
                             smex)))

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
        '((:name color-theme-twilight
                 :after (progn
                          (if window-system
                              (progn
                                (add-hook 'window-setup-hook
                                          (lambda ()
                                            (color-theme-twilight))
                                          t)
                                (color-theme-twilight))
                            (color-theme-euphoria))))
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
          (:name magit
                 :after (progn
                          (global-set-key (kbd "C-x C-z") 'magit-status)))
          (:name smex
                 :after (progn
                          (setq smex-save-file "~/.emacs.d/smex-items")
                          (global-set-key (kbd "M-x") 'smex)
                          ;; (global-set-key (kbd "M-X")
                          ;;                'smex-major-mode-commands)
                          ))
          ))

  ;; Initialize el-get synchronously
  (el-get 'sync rmg:el-get-packages)

;;; Customizations
  ;; Custom panel setup and maximize
  (add-to-list 'load-path "~/.emacs.d/custom")
  (require 'rmg-panels-v2)
  (require 'rmg-maximize)
  (global-set-key (kbd "C-x C-1") 'rmg-setup-windows)
  (global-set-key (kbd "C-x C-!") 'rmg-setup-windows)
  (global-set-key (kbd "C-x C-2") 'rmg-setup-windows-2)
  (global-set-key (kbd "C-x C-@") 'rmg-setup-windows-2)

  ;; Load eshell
  (require 'eshell)
  (setq eshell-directory-name "~/.emacs.d/eshell/")
  (setq eshell-login-script "~/.emacs.d/eshell/login")
  (setq eshell-rc-script "~/.emacs.d/eshell/profile")

  ;; Magit Projects
  (setq magit-repo-dirs '("~/Projects"
                          "~/.emacs.d"))

  ;; Hotkeys to move back and forth between frames
  (global-set-key (kbd "C-o") 'other-window)
  (defun rmg-prev-window ()
    "Perform the opposite operation of (other-window)"
    (interactive)
    (other-window -1))
  (global-set-key (kbd "M-o") 'rmg-prev-window)

;;; General
  ;; Backup
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

;;; Display
  ;; No splash screen
  (setq inhibit-splash-screen t)

  ;; Line and Column number mode
  (line-number-mode 1)
  (column-number-mode 1)

  ;; Setup window
  (add-hook 'window-setup-hook
            (lambda ()
              (when window-system
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

;;; Base Behavior
  ;; Never insert tabs automatically
  (setq-default indent-tabs-mode nil)

  ;; Default spaces for C-style
  (setq-default c-basic-offset rmg:indent-spaces)
  (setq c-basic-offset rmg:indent-spaces)

;;; End
)
