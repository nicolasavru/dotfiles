;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC

; http://nflath.com/2009/10/bitlbee-and-emacs/
; http://www.emacswiki.org/emacs/ERC
; http://hg.quodlibetor.com/hg/hgwebdir.cgi/emacs.d/file/tip/customize/chat.el
; http://www.emacswiki.org/emacs-en/AlexSchroederErcConfig

(require 'erc)

(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))
          (setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

(setq erc-modules '(completion scrolltobottom services track highlight-nicknames spelling netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list log))

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

(setq erc-log-channels-directory "~/.logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-hide-list '("MODE"))
(defun erc-ignore-unimportant (msg)
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
          (string-match "You\'re already logged in" msg)
          (string-match "Unknown error while loading configuration" msg))
      (setq erc-insert-this nil)))
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant)


(setq erc-keywords '((".*Online.*" (:foreground "green"))
                     (".*Busy" (:foreground "red"))
                     (".*Away" (:foreground "red"))
                     (".*Idle" (:foreground "orange"))
                     ("gtalk" (:foreground "blue"))
                     ("aim" (:foreground "yellow"))
                     ("fb" (:foreground "lightblue"))
                     ))

(erc-spelling-mode 1)

;; (defun erc-notify-on-msg (msg)
;;   "Send a message via notify-send if a message specifically to me"
;;    (if (or (string-match "nicolasavru:" msg)
;;            (and (string= "localhost" erc-session-server)
;;                 (not (string-match "\\*\\*\\*" msg))
;;                 (not (string-match "\\" msg))))
;;        (let ((nameless-msg (replace-regexp-in-string "^ " "" msg)))
;;          (shell-command (concat "notify-send \"" (buffer-name) "\" \"" nameless-msg "\"")))))
;; (add-hook 'erc-insert-pre-hook 'erc-notify-on-msg)


;;; Notify me when a keyword is matched (someone wants to reach me)
(require 'notifications)

(defvar my-erc-page-message "%s is calling your name."
  "Format of message to display in dialog box")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar my-erc-page-timeout 5
  "Number of seconds that must elapse between notifications from
the same person.")

(defun my-erc-page-popup-notification (nick)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory "~/"))
      ;; 8640000 milliseconds = 1 day
      (start-process "page-me" nil "notify-send"
                     "-u" "normal" "-t" "8640000" "ERC"
                     (format my-erc-page-message nick)))))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (null (string-match "\\`\\([sS]erver\\|localhost\\)" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick))
    (my-erc-page-popup-notification nick)))
;(add-hook 'erc-text-matched-hook 'my-erc-page-me)

;; (defun my-erc-page-me-PRIVMSG (proc parsed)
;;   (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
;;         (target (car (erc-response.command-args parsed)))
;;         (msg (erc-response.contents parsed)))
;;     (when (and (erc-current-nick-p target)
;;                (not (erc-is-message-ctcp-and-not-action-p msg))
;;                (my-erc-page-allowed nick))
;;       (notifications-notify
;;        :title nick
;;        :body (format my-erc-page-message nick)
;;        :urgency 'low)
;;      ;(my-erc-page-popup-notification nick)
;;     nil)))

; http://www.enigmacurry.com/2008/08/07/emacs-irc-erc-with-noticeable-notifications/

(defun erc-notify-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    ;;Handle true private/direct messages (non channel)
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (erc-current-nick-p target)
               (my-erc-page-allowed nick)
               )
      ;Do actual notification
      (play-sound-file "/usr/share/sounds/purple/receive.wav")
      (notifications-notify
       :title (format "%s - %s" nick
                      (format-time-string "%b %d %I:%M %p"))
       :body msg
       :urgency 'low)

      ;; (ding)
      ;; (notify-desktop (format "%s - %s" nick
      ;;                         (format-time-string "%b %d %I:%M %p"))
      ;;                 msg 0 "gnome-emacs")
      ;; )
    ;;Handle channel messages when my nick is mentioned
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (string-match (erc-current-nick) msg)
               (my-erc-page-allowed nick)
               )
      ;Do actual notification
      (play-sound-file "/usr/share/sounds/purple/receive.wav")
      (notifications-notify
       :title (format "%s - %s" nick
                      (format-time-string "%b %d %I:%M %p"))
       :body msg
       :urgency 'low)))))

      ;; (ding)
      ;; (notify-desktop (format "%s - %s" target
      ;;                         (format-time-string "%b %d %I:%M %p"))
      ;;                 (format "%s: %s" nick msg) 0 "gnome-emacs"))))

;(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)
(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-PRIVMSG)

;; (require 'notify)
;; (defun my-notify-erc (match-type nickuserhost message)
;;   "Notify when a message is received."
;;   (notify (format "%s in %s"
;;                   ;; Username of sender
;;                   (car (split-string nickuserhost "[\<\>]"))
;;                   ;; Channel
;;                   (or (erc-default-target) "#unknown"))
;;           ;; Remove duplicate spaces
;;           (replace-regexp-in-string " +" " " message)
;;           :icon "emacs-snapshot"
;;           :timeout -1))

;; (add-hook 'erc-insert-post-hook 'my-notify-erc)

;(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
;(setq erc-beep-match-types '("nicolasavru" "test"))


;; (defun erc-global-notify (match-type nick message)
;;   "Notify when a message is recieved."
;;   (notifications-notify
;;    :title nick
;;    :body message
;; ;   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
;;    :urgency 'low))

;; (add-hook 'erc-text-matched-hook 'erc-global-notify)

(setq erc-timestamp-only-if-changed-flag nil
          erc-timestamp-format "%H:%M:%S "
          erc-fill-prefix "      "
          erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-hide-timestamps nil)

(and
     (require 'erc-highlight-nicknames)
     (add-to-list 'erc-modules 'highlight-nicknames)
     (erc-update-modules))

(erc-track-mode 1)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
(setq erc-track-exclude-server-buffer t)

(require 'erc-nick-notify)


(global-set-key (kbd "C-c e") 'switch-to-irc)

(defun switch-to-irc ()
  "Switch to an IRC buffer, or run `erc-select'.
When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (and (fboundp 'erc-buffer-list)
                      (erc-buffer-list))))
    (when (eq (current-buffer) (car buffers))
      (bury-buffer)
      (setq buffers (cdr buffers)))
    (if buffers
        (switch-to-buffer (car buffers))
      ;(erc "localhost" 6667 erc-nick erc-user-full-name t)
      ;; (erc "irc.phasenet.co.uk" erc-port erc-nick erc-user-full-name t)
      ;; (erc "irc.slashnet.org" erc-port erc-nick erc-user-full-name t)
      (erc "irc.freenode.net" erc-port erc-nick erc-user-full-name t))))

 ;; fancy prompt with channel name, or ERC if nil
 ;; http://www.emacswiki.org/emacs/ErcConfiguration#toc5
(setq erc-prompt (lambda ()
                   (if (and (boundp 'erc-default-recipients)
                            (erc-default-target))
                       (erc-propertize (concat (erc-default-target) ">")
                                       'read-only t
                                       'rear-nonsticky t
                                       'front-nonsticky t)
                     (erc-propertize (concat "ERC>")
                                     'read-only t
                                     'rear-nonsticky t
                                     'front-nonsticky t))))


(setq erc-auto-discard-away nil)

(defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  "Insert last recipient after prompt."
  (let ((previous
         (save-excursion
           (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                    (search-forward-regexp (concat "^[^<]*<" erc-nick ">"
                                                   " *\\([^:]*: ?\\)") nil t))
               (match-string 1)))))
    ;; when we got something, and it was in the last 3 mins, put it in
    (when (and
           previous
           (> 180 (time-to-seconds
                   (time-since (get-text-property 0 'timestamp previous)))))
      (set-text-properties 0 (length previous) nil previous)
      (insert previous))))

;Add this to your .emacs to see the number of opped/voiced/normal members of the current channel in the modeline:

(define-minor-mode ncm-mode "" nil
  (:eval
   (let ((ops 0)
         (voices 0)
         (members 0))
     (maphash (lambda (key value)
                (when (erc-channel-user-op-p key)
                  (setq ops (1+ ops)))
                (when (erc-channel-user-voice-p key)
                  (setq voices (1+ voices)))
                (setq members (1+ members)))
              erc-channel-users)
     (format " %S/%S/%S" ops voices members))))

(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq erc-max-buffer-size 30000)
(setq erc-truncate-buffer-on-save t)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer t)
(setq erc-truncate-buffer-on-save t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitlbee

(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   bitlbee-password))))
(add-hook 'erc-join-hook 'bitlbee-identify)

(defun bitlbee-connect ()
  (interactive)
  (save-window-excursion
    (when (get-buffer "&bitlbee")
      (switch-to-buffer "&bitlbee")
      (erc-message "PRIVMSG" (concat (erc-default-target) " identify " bitlbee-password))
      (erc-message "PRIVMSG" (concat (erc-default-target) " account 0 on"))
      (erc-message "PRIVMSG" (concat (erc-default-target) " account 1 on"))
      (erc-message "PRIVMSG" (concat (erc-default-target) " account 2 on")))))
;(setq bitlbee-reconnect-timer (run-with-timer 0 60 'bitlbee-connect))

(defvar am-here-p t)
(setq timeout 300)
(setq erc-away-status "I am away.")

(defun custom-auto-set-away ()
  "Set away message to erc-away-status and clear it if already away."
  (interactive)
  ;(message (number-to-string (/ (string-to-number (shell-command-to-string "/usr/bin/xprintidle")) 1000.0)))
  (if (> (/ (string-to-number (shell-command-to-string "/usr/bin/xprintidle")) 1000.0) timeout)
      (when am-here-p
        (message "Going Away")
        (set-buffer "&bitlbee")
        (setq am-here-p nil)
        (erc-cmd-AWAY erc-away-status))
    (unless am-here-p
      (set-buffer "&bitlbee")
      (setq am-here-p t)
      (erc-cmd-AWAY ""))))

;; not using bitlbee as of now
;(run-at-time t 60 'custom-auto-set-away)
