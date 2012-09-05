;;; Package Setup
;; el-get directory
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; load
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

;; init directory
(setq-default el-get-user-package-directory "~/.emacs.d/init-files/")

;; Sync
(el-get 'sync)

;;; Customizations
;; Custom panel setup and maximize
(add-to-list 'load-path "~/.emacs.d/custom")
(require 'rmg-panels)
(require 'rmg-maximize)
(global-set-key (kbd "C-x C-1") 'rmg-setup-windows)

;; Load eshell
(require 'eshell)
(setq eshell-directory-name "~/.emacs.d/eshell/")
(setq eshell-login-script "~/.emacs.d/eshell/login")
(setq eshell-rc-script "~/.emacs.d/eshell/profile")

;; Magit Projects
(setq magit-repo-dirs '("~/Projects"))

;; Hotkeys to move back and forth between frames
(global-set-key (kbd "C-o") 'other-window)
(defun rmg-prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-o") 'rmg-prev-window)

;;; General
;; Backup
(setq backup-directory-alist `(("." . "~/.emacs.d/backup"))
  backup-by-copying t   ; Don't delink hardlinks
  version-control t     ; Use version numbers on backups
  delete-old-version t  ; Automatically delete excess backups
  kept-new-versions 20  ; how many of the newest versions to keep
  kept-old-versions 5   ; and how many of the old
  )

;; Auto-save
(setq auto-save-file-name-transforms
  `((".*" ,"~/.emacs.d/auto-save/" t)))

;;; Display
;; Set theme
(color-theme-twilight)

;; Column number mode
(setq-default column-number-mode t)
(setq column-number-mode t)

;; No scrollbars
(scroll-bar-mode -1)

;; No toolbar
(add-hook 'window-setup-hook
          (lambda ()
            (tool-bar-mode -1)))

;; Fringe only on right
(set-fringe-mode '(0 nil))

;; Show bad whitespace
(global-whitespace-mode 1)
(setq-default whitespace-style '(face
                                 tabs
                                 trailing
                                 lines-tail
                                 empty
                                 indentation
                                 space-after-tab
                                 space-before-tab))


;;; Base Behavior
;; Never insert tabs automatically
(setq-default indent-tabs-mode 'nil)

;; Default to 2 spaces
(setq-default c-basic-offset 2)
(setq c-basic-offset 2)

;;; JS behavior
;; Set all
(setq-default js2-pretty-multiline-declarations 'all)
(setq js2-pretty-multiline-declarations 'all)

;; Default to 2 spaces
(setq-default js2-basic-offset 2)
(setq js2-basic-offset 2)

;; Allow code folding on javascript
(add-hook 'js2-mode-hook
          (lambda ()
            ; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ; Activate the folding mode
            (hs-minor-mode t)))
