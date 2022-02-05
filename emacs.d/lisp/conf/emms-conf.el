;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS

;http://www.gnu.org/software/emms/configs/lb-emms.el

(require 'emms-setup)
(emms-devel)

(require 'emms-streams)
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")

(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(emms-cache-disable)

;; Show the current track each time EMMS starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")

;; When asked for emms-play-directory, always start from this one
(setq emms-source-file-default-directory "/mnt/music_ro/")

(require 'emms-player-simple)
(define-emms-simple-player mikmod '(file)
  (regexp-opt '(".669" ".AMF" ".DSM" ".FAR" ".GDM" ".IT" ".IMF"
                ".MED" ".MTM" ".OKT" ".S3M" ".STM" ".STX" ".ULT"
                ".APUN" ".XM" ".MOD" ".amf" ".dsm" ".far" ".gdm"
                ".it" ".imf" ".mod" ".med" ".mtm" ".okt" ".s3m"
                ".stm" ".stx" ".ult" ".apun" ".xm" ".mod" ".MOD")
              ) "mikmod" "-q" "-p" "1" "-X")

(require 'emms-player-mpv)

(setq emms-player-list '(emms-player-mpv emms-player-vlc))
(add-to-list 'emms-player-mpv-parameters "--no-video")
(add-to-list 'emms-player-list 'emms-player-mikmod)

;; Libtag support
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

;; Stolen and adapted from TWB
(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (if (and (emms-track-get track 'info-artist)
           (emms-track-get track 'info-title))
      (let ((pmin (emms-track-get track 'info-playing-time-min))
            (psec (emms-track-get track 'info-playing-time-sec))
            (ptot (emms-track-get track 'info-playing-time))
            (art  (emms-track-get track 'info-artist))
            (tit  (emms-track-get track 'info-title)))
        (cond ((and pmin psec) (format "%s - %s [%02d:%02d]" art tit pmin psec))
              (ptot (format  "%s - %s [%02d:%02d]" art tit (/ ptot 60) (% ptot 60)))
              (t (emms-track-simple-description track))))))

(setq emms-track-description-function 'my-emms-info-track-description)

(emms-mode-line 0)
(emms-playing-time 1)


(global-set-key (kbd "<f1>")    'emms-add-directory-tree)
(global-set-key (kbd "C-<f1>")    'emms-play-directory-tree)
(global-set-key (kbd "<f2>")    'emms-smart-browse)
(global-set-key (kbd "C-<f3>")    'emms-streams)
(global-set-key (kbd "<f3>")    'emms-playlist-mode-go)
(global-set-key (kbd "<S-f3>")  'emms-stream-popup)
(global-set-key (kbd "C-c <up>") 'emms-start)
(global-set-key (kbd "C-c <down>") 'emms-stop)
(global-set-key (kbd "C-c SPC") 'emms-pause)

(define-key dired-mode-map (kbd "M-<f1>") 'emms-add-dired)
