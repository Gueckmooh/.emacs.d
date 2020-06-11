(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir (format "%s/%s" (getenv "HOME") "Mail"))

(setq mu4e-sent-folder   "/sent")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-trash-folder  "/trash")

(provide 'setup-mu4e)
