(lexical-let ((rmg:indent-spaces 2))

  ;; Never insert tabs automatically
  (setq-default indent-tabs-mode nil)

  ;; Set up basic offset for JavaScript when js2-basic-offset variable is bound
  (when (boundp 'js2-basic-offset)
    ;; Set basic offset
    (setq-default js2-basic-offset rmg:indent-spaces)
    (setq js2-basic-offset rmg:indent-spaces))

  ;; Default indent width
  (setq default-tab-width rmg:indent-spaces)
  (setq-default tab-width rmg:indent-spaces)

  (setq css-indent-offset rmg:indent-spaces)

  (setq-default sh-basic-offset rmg:indent-spaces)
  (setq-default sh-indentation rmg:indent-spaces)

  (setq-default perl-indent-level rmg:indent-spaces)
  (setq-default python-indent-offset rmg:indent-spaces)

  (setq-default js-indent-level rmg:indent-spaces)

  ;; Set up google style hooks when google-set-c-style function is bound
  (when (fboundp 'google-set-c-style)
    ;; Auto-start google C style
    (add-hook 'c-mode-hook 'google-set-c-style)
    (add-hook 'c++-mode-hook 'google-set-c-style))

  ;; Set up java style
  (defconst rmg-java-style
    `("java"
      (c-offsets-alist . ((arglist-cont-nonempty . ++)))))
  (add-hook 'java-mode-hook
            (lambda ()
              ;; Add custom style
              (c-add-style "rmg-java" rmg-java-style t)

              ;; Treat Java 1.5 @-style annotations as comments
              (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
              (modify-syntax-entry ?@ "< b" java-mode-syntax-table))))

(provide 'rmg-code-style)
