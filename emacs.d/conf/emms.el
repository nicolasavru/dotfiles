;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS

;http://www.gnu.org/software/emms/configs/lb-emms.el


(require 'emms-setup)
(emms-devel)
;(emms-standard)
;(require 'emms-player-mpd)
;(setq emms-player-mpd-server-name "localhost")
;(setq emms-player-mpd-server-port "6600")
;(emms-player-mpd-connect)
(require 'emms-streams)
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")

(setq emms-lastfm-client-session-key-file "~/.emacs.d/emms/emms-lastfm-client-sessionkey")
(require 'emms-lastfm-client)

;(emms-lastfm-client)
(emms-lastfm-scrobbler-enable)
(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(emms-cache-disable)

;; Show the current track each time EMMS starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")

;; When asked for emms-play-directory, always start from this one
(setq emms-source-file-default-directory "~/music_ro/")

(require 'emms-player-simple)
(define-emms-simple-player mikmod '(file)
  (regexp-opt '(".669" ".AMF" ".DSM" ".FAR" ".GDM" ".IT" ".IMF"
                ".MED" ".MTM" ".OKT" ".S3M" ".STM" ".STX" ".ULT"
                ".APUN" ".XM" ".MOD" ".amf" ".dsm" ".far" ".gdm"
                ".it" ".imf" ".mod" ".med" ".mtm" ".okt" ".s3m"
                ".stm" ".stx" ".ult" ".apun" ".xm" ".mod" ".MOD")
              ) "mikmod" "-q" "-p" "1" "-X")
;(add-to-list 'emms-player-list 'emms-player-mpd)
(require 'emms-player-mplayer)
;(add-to-list 'emms-player-list 'emms-player-mplayer)
;(emms-default-players)
;(setq emms-player-list '(emms-player-mplayer-playlist emms-player-mplayer emms-player-vlc))
(setq emms-player-list '(emms-player-mplayer emms-player-vlc))
;(setq emms-player-list '(emms-player-mpg321 emms-player-ogg123 emms-player-mplayer))
(add-to-list 'emms-player-list 'emms-player-mikmod)

;(setq emms-player-list '(emms-player-vlc))

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

;;; Add music file to playlist on '!', --lgfang
;; (setq dired-guess-shell-alist-user
;;       (list
;;        (list "\\.\\(flac\\|mp3\\|ogg\\|wav\\)\\'"
;;              '(if (y-or-n-p "Add to emms playlist?")
;;                   (progn (emms-add-file (dired-get-filename))
;;                          (keyboard-quit))
;;                 "mplayer"))))

;; (setq dired-guess-shell-alist-user
;;       (list
;;        (list "\.(flac|mp3|ogg|wav)$"
;;              '(if (y-or-n-p "Add to emms playlist?")
;;                   (progn (emms-add-file (dired-get-filename))
;;                          (keyboard-quit))
;;                 "mplayer"))))

(global-set-key (kbd "<f1>")    'emms-add-directory-tree)
(global-set-key (kbd "C-<f1>")    'emms-play-directory-tree)
(global-set-key (kbd "<f2>")    'emms-smart-browse)
(global-set-key (kbd "C-<f3>")    'emms-streams)
(global-set-key (kbd "<f3>")    'emms-playlist-mode-go)
(global-set-key (kbd "<S-f3>")  'emms-stream-popup)
(global-set-key (kbd "C-c <up>") 'emms-start)
(global-set-key (kbd "C-c <down>") 'emms-stop)
(global-set-key (kbd "C-c SPC") 'emms-pause)
(global-set-key (kbd "<f14>") 'emms-next)
(global-set-key (kbd "<f13>") 'emms-previous)
(global-set-key (kbd "<f15>") 'emms-get-lyrics-current-song)
(global-set-key (kbd "C-<f15>") 'emms-get-lyrics-custom)
(global-set-key (kbd "C-<XF86AudioRaiseVolume>") 'emms-volume-raise)
(global-set-key (kbd "C-<XF86AudioLowerVolume>") 'emms-volume-lower)

(define-key dired-mode-map (kbd "M-<f1>") 'emms-add-dired)

(require 'emms-get-lyrics)
(setq emms-get-lyrics-use-files nil)
