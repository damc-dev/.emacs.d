;; Load yasnippet
(require 'yasnippet)

;; Shut up. Seriously. Wayyyy to spammy
(setq yas-verbosity 0)

;; Yas should work everywhere
(yas-global-mode 1)

;; Save new snippets into the top directory
(setq yas/root-directory '("~/.emacs.d/snippets"
                           ; And load snippets from each of the following
                           "~/.emacs.d/el-get/yasnippet/snippets"))

;; Load snippets from each directory in yas/root-directory
(mapc 'yas/load-directory yas/root-directory)

;; Set the display style to match auto-complete
(setq yas-prompt-functions '(yas-dropdown-prompt))

(provide 'init-yasnippet)
