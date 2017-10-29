(require 'mu4e)

;; default
;; (setq mu4e-maildir ("~/Maildir")

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/INBOX"    . ?i)
        ("/Sent"     . ?s)
        ("/Trash"    . ?t)
        ("/All Mail" . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
  user-mail-address "nicolasavru@gmail.com"
  user-full-name  "Nicolas Avrutin"
  mu4e-compose-signature "Nicolas Avrutin\n"
)

(setq mu4e-user-mail-address-list '("nicolasavru@gmail.com"
                                    "nicolas@avrutin.net"
                                    "avruti@cooper.edu"
                                    "rasputin@cooper.edu"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;  starttls-use-gnutls t
;;  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;  smtpmail-auth-credentials
;;  '(("smtp.gmail.com" 587 "nicolasavru@gmail.com" nil))
;;  smtpmail-default-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:

;; (require 'smtpmail-async)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


(setq mu4e-refile-folder
      (lambda (msg)
        (mu4e-ask-maildir "Refile to maildir: ")))

(setq
  mu4e-get-mail-command "offlineimap"
  mu4e-update-interval 300)

(setq mu4e-view-show-addresses t)
(setq mu4e-compose-dont-reply-to-self t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(imagemagick-register-types)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-view-prefer-html nil)
;; (setq mu4e-html2text-command "html2text -utf8 -nobs -width 72")
(setq mu4e-html2text-command "w3m -T text/html")

(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(add-to-list 'mu4e-view-actions
  '("xViewXWidget" . mu4e-action-view-with-xwidget) t)


(setq cucc-label-search (concat "maildir:/Cooper/CUCC/ateam@cooper.edu"
                                " OR "
                                "maildir:/Cooper/CUCC/guru@cooper.edu"
                                " OR "
                                "maildir:/Cooper/CUCC/sstaff@cooper.edu"
                                " OR "
                                "maildir:/Cooper/CUCC/staff@cooper.edu"
                                " OR "
                                "maildir:/Cooper/CUCC/ticket-user@cooper.edu")
      ulab-label-search "maildir:/Cooper/uLab OR maildir:/Cooper/uLab/archivist1 OR maildir:/Cooper/uLab/Trac OR maildir:/Cooper/uLab/ulab.cu@gmail.com OR maildir:/Cooper/uLab/zenoss@ee.cooper.edu"
      classes-label-search "maildir:/Cooper/Classes/ECE101 OR maildir:/Cooper/Classes/ECE111-ECE110 OR maildir:/Cooper/Classes/ECE114 OR maildir:/Cooper/Classes/ECE302 OR maildir:/Cooper/Classes/ECE416 OR maildir:/Cooper/Classes/ECE418 OR maildir:/Cooper/Classes/ECE460"
      cooper-misc-label-search "maildir:/Cooper/cunerds OR maildir:\"/Cooper/Alumni_NewsLetter\" OR maildir:/Cooper/bulkmail OR maildir:/Cooper/campus-notice OR maildir:/Cooper/Newsletter OR maildir:/Cooper/moodle")


(add-to-list 'mu4e-bookmarks
             (list cucc-label-search "CUCC" ?C))
(add-to-list 'mu4e-bookmarks
             (list ulab-label-search "uLab" ?U))
(add-to-list 'mu4e-bookmarks
             (list (concat "flag:unread AND NOT flag:trashed AND (maildir:/INBOX OR maildir:/Unigroup OR " ulab-label-search " OR " classes-label-search " OR "cooper-misc-label-search ")") "Unread - Important" ?I))


(global-set-key (kbd "C-c m") 'mu4e)

(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (setq user-mail-address
                    (if msg
                      (cond
                        ((mu4e-message-contact-field-matches msg :to "nicolasavru@gmail.com")
                         "nicolasavru@gmail.com")
                        ((mu4e-message-contact-field-matches msg :to "nicolas@avrutin.net")
                         "nicolas@avrutin.net")
                        ((mu4e-message-contact-field-matches msg :to "avruti@cooper.edu")
                         "avruti@cooper.edu")
                        ((mu4e-message-contact-field-matches msg :to "rasputin@cooper.edu")
                         "rasputin@cooper.edu")
                        (t "nicolasavru@gmail.com"))
                      "nicolasavru@gmail.com")))))

(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)
            (footnote-mode)))

;; (defun mu4e-checkmail-timer-function () (shell-command "~/.emacs.d/scripts/sauron.sh"))
;; (run-at-time "10 sec" 300 'mu4e-checkmail-timer-function)

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq mu4e-attachment-dir "/tmp")

(setq mu4e-headers-fields '((:human-date . 10)
                            (:flags . 6)
                            (:mailing-list . 10)
                            (:from-or-to . 22)
                            (:subject)))

(setq mu4e-headers-date-format "%Y-%m-%d")
(setq mu4e-headers-time-format "%T")


(defun mu4e-view-snarf-pgp-key (&optional msg)
  "Snarf the pgp key for the specified message."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
          (path (mu4e-message-field msg :path))
          (cmd (format "%s verify --verbose %s"
                 mu4e-mu-binary
                 (shell-quote-argument path)))
          (output (shell-command-to-string cmd)))
    (let ((case-fold-search nil))
      (when (string-match "key:\\([A-F0-9]+\\)" output)
        (let* ((cmd (format "%s --recv %s"
                            epg-gpg-program (match-string 1 output)))
               (output (shell-command-to-string cmd)))
          (message output))))))

(add-to-list 'mu4e-view-actions
             '("Snarf PGP keys" . mu4e-view-snarf-pgp-key) t)

(defun mu4e-headers-search-in-new-frame (&optional expr prompt edit ignore-history)
  "Execute `mu4e-headers-search' in a new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e-headers-search expr prompt edit ignore-history))
(global-set-key (kbd "C-c M") 'mu4e-headers-search-in-new-frame)

(mu4e-maildirs-extension)
;; Don't insert a newline before top-level maildirs.
(setq mu4e-maildirs-extension-before-insert-maildir-hook nil)

(defun mu4e-confirm-no-subject-send ()
  (unless (or (message-field-value "subject")
              (yes-or-no-p "Subject is nil. Are you sure you want to send this?"))
    (signal 'quit nil)))
(add-hook 'message-send-hook 'mu4e-confirm-no-subject-send)

;; (defun mu4e~draft-cite-original (msg)
;;   "Return a cited version of the original message MSG as a plist.
;; This function uses gnus' `mu4e-compose-cite-function', and as such
;; all its settings apply."
;;   (with-temp-buffer
;;     (when (fboundp 'mu4e-view-message-text) ;; keep bytecompiler happy
;;       (let ((mu4e-view-date-format "%Y-%m-%dT%T%z"))
;;         (insert (mu4e-view-message-text msg)))
;;       (message-yank-original)
;;       (goto-char (point-min))
;;       (push-mark (point-max))
;;       ;; set the the signature separator to 'loose', since in the real world,
;;       ;; many message don't follow the standard...
;;       (let ((message-signature-separator "^-- *$")
;;             (message-signature-insert-empty-line t))
;;         (eval
;;          `(let ,(if (symbolp message-cite-style)
;;                     (symbol-value message-cite-style)
;;                   message-cite-style)
;;             (funcall mu4e-compose-cite-function))))
;;          ;; (funcall mu4e-compose-cite-function))
;;       (pop-mark)
;;       (goto-char (point-min))
;;       (mu4e~fontify-cited)
;;       (buffer-string))))


(setq mu4e-compose-cite-function 'message-cite-original
      message-citation-line-format "On %e %B %Y %R, %f wrote:\n")
