;;; Package --- summary

;;; Commentary:
;;; Code:

(defun gk/get-first-line (&optional buffer)
  "Get the first line of the current buffer or the specified BUFFER."
  (if (not buffer) (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (buffer-substring (point-min) (point-at-eol))))

(defun gk/get-command-output (command)
  "Return the output of the COMMAND given as parameter."
  (with-temp-buffer
  (call-process-shell-command command nil (current-buffer))
  (gk/get-first-line (current-buffer))))

(defun gk/is-installed-p (prog)
  "Return non nil if the command which PROG return a path.
It is usefull to know wether a program is installed or not for
configuration."
  (not (string= "" (gk/get-command-output (concat "which " prog)))))

(provide 'util)
;;; util.el ends here
