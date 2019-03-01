(defun get-first-line (&optional buffer)
  "get the first line of the current buffer or the specified buffer"
  (if (not buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (buffer-substring (point-min) (point-at-eol))))


(defun get-command-output (command)
  "Returns the output of the command given as parameter."
  (with-temp-buffer
  (call-process-shell-command command nil (current-buffer))
  (get-first-line (current-buffer))))

(defun gk/is-installed-p (prog)
  "Returns non nil if the command which prog returns a path.
It is usefull to know wether a program is installed or not for
configuration."
  (not (string= "" (get-command-output (concat "which " prog)))))

(provide 'util)
