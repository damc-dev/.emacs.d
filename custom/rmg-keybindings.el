;;; Global keybindings
;; Preferred window layout
(global-set-key (kbd "C-x C-1") 'rmg-setup-windows-1)
(global-set-key (kbd "C-x C-!") 'rmg-setup-windows-1)
(global-set-key (kbd "C-x C-2") 'rmg-setup-windows-2)
(global-set-key (kbd "C-x C-@") 'rmg-setup-windows-2)

;; Move between windows
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-o") 'rmg-prev-window)

;; Swap isearch and isearch-regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Goto line (remap)
(global-set-key [remap goto-line] 'rmg-goto-line-with-feedback)

;; Editing
(global-set-key (kbd "C-w") 'rmg-kill-region-or-backward-word)

;; Join lines
(global-set-key (kbd "M-j")
                (lambda()
                  (interactive)
                  (join-line -1)))

;; Set input method (clear C-x C-m for Smex)
(global-set-key (kbd "C-M-\\") 'set-input-method)

;; Webjump
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Make ido-mode lines a little more sane
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-o") nil)
            (define-key ido-completion-map (kbd "M-o") nil)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;;; Conditional Keybindings
;; Magit
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x C-z") 'magit-status))

;; Smart M-x
(when (fboundp 'smex)
  ;; Use Steve Yegge's advice to use C-x C-m
  (global-set-key (kbd "C-x C-m") 'smex)
  ;; C-m gets interpreted as RET
  (global-set-key (kbd "C-x RET") 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; Use <select> as a keybinding for end-of-line (end key over SSH)
(unless (display-graphic-p)
  (global-set-key (kbd "<select>") 'move-end-of-line))

;;; Mode-specific keybindings
;; Additional hideshow hotkeys
(when (fboundp 'hs-minor-mode)
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
                                      (kbd "C-c h t") 'hs-toggle-hiding)))))


(provide 'rmg-keybindings)
