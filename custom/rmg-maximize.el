(defun rmg-x11-maximize-frame ()
  "Maximize the current window in X11"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun rmg-w32-maximize-frame ()
  "Maximize the current window in Windows"
  (w32-send-sys-command 61488))

(defun rmg-maximize-frame ()
  "Maximize the current GUI window"
  (case window-system
    (x (rmg-x11-maximize-frame))
    (w32 (rmg-w32-maximize-frame))))

(provide 'rmg-maximize)
