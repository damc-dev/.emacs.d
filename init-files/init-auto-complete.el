; Require
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dicts")

(add-to-list 'ac-sources 'ac-source-dictionary)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-yasnippet))

(global-auto-complete-mode t)      ; enable global-mode
(setq ac-auto-start 1)             ; automatically start after 1 character
(setq ac-auto-show-menu 4)         ; automatically show menu after 4 seconds
(setq ac-ignore-case nil)          ; case-specific

(provide 'init-auto-complete)
