(require 'js-comint)

; Use node as our repl
(setq-default inferior-js-program-command (format "~/.local/node/%s/bin/node"
                                          (substring
                                           (with-temp-buffer
                                             (insert-file-contents
                                              "~/.local/node/alias/default")
                                             (buffer-string))
                                           0 -1)))

(setq-default inferior-js-mode-hook
      (lambda ()
        ; We like nice colors
        (ansi-color-for-comint-mode-on)

        ; Fix the output
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string
                        ".*1G\.\.\..*5G" "..."
                        (replace-regexp-in-string ".*1G.*3G" ">" output))))))
