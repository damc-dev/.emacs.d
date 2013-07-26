;; Default font size in GUIs is 9pt
(lexical-let ((rmg:font-height 90)
      (rmg:maximize-host-regexps '("reaper2.*"
                                  "niub30")))

  ;; Font size on reaper2 should be 11pt (higher density screen)
  (when (string-match "reaper2.*" (system-name))
    (setq rmg:font-height 110))

  ;; Add window setup hook for face size
  (add-hook 'window-setup-hook
            (lambda ()
              (when (display-graphic-p)
                (set-face-attribute 'default nil :height rmg:font-height)))
            t)

  ;; Maximize automatically for specific hosts
  (dolist (name-regex rmg:maximize-host-regexps)
    (when (string-match name-regex (system-name))
      (add-hook 'window-setup-hook
                #'rmg-maximize-frame
                t)))
  )

(provide 'rmg-host-specific)
