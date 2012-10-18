;;; Inspired by http://fullofsta.rs/2012/01/an-improved-emacs-window-setup/

;;; Definition
;; Setup custom framework
(defun rmg-setup-windows ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (other-window 2)
  (split-window)
  (other-window -2)
  (enlarge-window-horizontally (- 80 (window-width)))
  (other-window 1)
  (enlarge-window-horizontally (- 80 (window-width)))
  (other-window -1)
  (setq rmg-temporary-window-top (nth 2 (window-list)))
  (setq rmg-temporary-window-bot (nth 3 (window-list))))

(defun rmg-setup-windows-2 ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1)
  (split-window)
  (other-window -1)
  (enlarge-window-horizontally (- 80 (window-width)))
  (setq rmg-temporary-window-top (nth 1 (window-list)))
  (setq rmg-temporary-window-bot (nth 2 (window-list))))

(setq rmg-temporary-window-top 'nil)
(setq rmg-temporary-window-bot 'nil)

;; List of special buffers to place in temporary windows
(setq special-display-regexps
      '("^\\*magit"
        "^\\*eshell"
        "^\\*Completions\\*$latex "
        "^\\*Ido Completions\\*$"
        "^\\*ack\\*$latex "
        "^\\*Help\\*$"
        "^\\*grep\\*$latex "
        "^\\*Apropos\\*$"
        "^\\*elisp macroexpansion\\*$latex "
        "^\\*local variables\\*$"
        "^\\*Compile-Log\\*$latex "
        "^\\*Quail Completions\\*$"
        "^\\*Occur\\*$latex "
        "^\\*frequencies\\*$"
        "^\\*compilation\\*$latex "
        "^\\*Locate\\*$"
        "^\\*Colors\\*$latex "
        "^\\*tumme-display-image\\*$"
        "^\\*SLIME Description\\*$latex "
        "^\\*.* output\\*$"                  ; tex compilation buffer
        "^\\*TeX Help\\*$latex "
        "^\\*Shell Command Output\\*$"
        "^\\*Async Shell Command\\*$latex "
        "^\\*Backtrace\\*$"))

;; Try placing in a specific window; otherwise, use current
(defun jpt-try-window (window)
  (if (window-live-p window)
      window
    (first (window-list))))

;; Select the window for a special buffer (either top or bottom)
(defun jpt-window-for-buffer (buffer)
  (if (string-match "^\\*magit" (buffer-name buffer))
      (jpt-try-window rmg-temporary-window-bot)
    (jpt-try-window rmg-temporary-window-top)))

;; Switch to the buffer in a special window
(defun grb-special-display (buffer &optional data)
  (let ((window (jpt-window-for-buffer buffer)))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))

(setq special-display-function #'grb-special-display)

(provide 'rmg-panels)
