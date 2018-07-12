(use-package emms
:ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-vlc)
    (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
    (setq emms-seek-seconds 5)
    (setq emms-player-list '(emms-player-vlc))
  :bind
    ("C-c m m" . emms)
    ("C-c m b" . emms-smart-browse)
    ("C-c m r" . emms-player-mpd-update-all-reset-cache)
    ("<XF86AudioPrev>" . emms-previous)
    ("<XF86AudioNext>" . emms-next)
    ("<XF86AudioPlay>" . emms-pause)
    ("<XF86AudioStop>" . emms-stop)
    ("C-c m <left>" . emms-previous)
    ("C-c m <rigtt>" . emms-next)
    ("C-c m <down>" . emms-pause)
    ("C-c m <up>" . emms-stop))

(use-package helm-emms
  :init
  (setq emms-source-file-default-directory '"~/Musique/")

  (global-set-key (kbd "C-c h C-e f") 'emms-add-file))

(provide 'setup-emms)
