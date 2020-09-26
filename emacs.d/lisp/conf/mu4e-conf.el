(require 'mu4e)

;; default
;; (setq mu4e-maildir ("~/Maildir")

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq message-sendmail-envelope-from 'header)
(setq mu4e-sent-messages-behavior
      (lambda ()
        (if (string= (message-sendmail-envelope-from) "nicolasavru@gmail.com")
            'delete
          'sent)))


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
(defvar my-mu4e-account-alist
  '(("nicolasavru@gmail.com"
     (user-mail-address "nicolasavru@gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     ;; (smtpmail-local-domain "account1.example.com")
     ;; (smtpmail-smtp-user "username1")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))
    ("nicolas@avrutin.net"
     (user-mail-address "nicolas@avrutin.net")
    ;(smtpmail-default-smtp-server "smtp.sendgrid.net")
     (smtpmail-default-smtp-server "mail.avrutin.net")
     ;; (smtpmail-local-domain "account2.example.com")
     ;; (smtpmail-smtp-user "username2")
     ;(smtpmail-smtp-server "smtp.sendgrid.net")
     (smtpmail-smtp-server "mail.avrutin.net")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.sendgrid.net"
;;       smtpmail-smtp-server "smtp.sendgrid.net"
;;       smtpmail-smtp-service 587)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (seq-find (lambda (x) x)
                        (mapcar
                         (lambda (my-address)
                           (message my-address)
                           (if (mu4e-message-contact-field-matches
                                mu4e-compose-parent-message
                                '(:to :cc :bcc :from) my-address)
                               my-address
                             nil))
                         mu4e-user-mail-address-list))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


(setq mu4e-refile-folder
      (lambda (msg)
        (mu4e-ask-maildir "Refile to maildir: ")))

(setq
 ;; mu4e-get-mail-command "offlineimap"
 mu4e-get-mail-command "mbsync gmail"
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

(remove-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function)
(define-key xwidget-webkit-mode-map (kbd "q")
  (lambda () (interactive) (quit-window t)))

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
               (when (equal major-mode 'xwidget-webkit-mode)
                 (xwidget-webkit-adjust-size-dispatch))))



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

(setq mu4e-use-fancy-chars nil)

(defun mu4e~write-body-to-html (msg)
  "Write the body (either html or text) to a temporary file;
return the filename."
  (let* ((html (mu4e-message-field msg :body-html))
         (txt (mu4e-message-field msg :body-txt))
         (tmpfile (mu4e-make-temp-file "html"))
         (attachments (remove-if (lambda (part)
                                   (or (null (plist-get part :attachment))
                                       (null (plist-get part :cid))))
                                 (mu4e-message-field msg :parts))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      (insert "<head><meta charset=\"UTF-8\"><style>body {display: none;}</style></head>\n")
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      ;; rewrite attachment urls
      (mapc (lambda (attachment)
              (goto-char (point-min))
              (while (re-search-forward (format "src=\"cid:%s\""
                                                (plist-get attachment :cid)) nil t)
                (if (plist-get attachment :temp)
                    (replace-match (format "src=\"%s\""
                                           (plist-get attachment :temp)))
                  (replace-match (format "src=\"%s%s\"" temporary-file-directory
                                         (plist-get attachment :name)))
                  (let ((tmp-attachment-name
                         (format "%s%s" temporary-file-directory
                                 (plist-get attachment :name))))
                    (mu4e~proc-extract 'save (mu4e-message-field msg :docid)
                                       (plist-get attachment :index)
                                       mu4e-decryption-policy tmp-attachment-name)
                    (mu4e-remove-file-later tmp-attachment-name)))))
            attachments)
      ;; Replce xhtml doctype with html; xhtml rendering seems to be broken in
      ;; webkit2gtk.
      (goto-char (point-min))
      (while (re-search-forward "<!DOCTYPE html.*\\(XHTML\\|xhtml\\).*>" nil t)
        (replace-match "<!DOCTYPE html>"))
      (goto-char (point-min))
      (while (re-search-forward "<html xmlns=\"http://www\\.w3\\.org/1999/xhtml\".*>" nil t)
        (replace-match "<html>"))
      (save-buffer)
      tmpfile)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun mu4e-xwidget-dark-mode ()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (get-string-from-file "~/.emacs.d/javascript/darkmode.js")))

(defun mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit. This is
only available in emacs 25+; also see the discussion of privacy
aspects in `(mu4e) Displaying rich-text messages'."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (xwidget-webkit-browse-url
   (concat "file://" (mu4e~write-body-to-html msg)) t)
  (mu4e-xwidget-dark-mode))

(define-key xwidget-webkit-mode-map (kbd "d") 'mu4e-xwidget-dark-mode)

(defvar xwidget-webkit-finished-loading-hook nil)

(add-hook 'xwidget-webkit-finished-loading-hook 'mu4e-xwidget-dark-mode)

(defun xwidget-webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (if (not (buffer-live-p (xwidget-buffer xwidget)))
      (xwidget-log
       "error: callback called for xwidget with dead buffer")
    (with-current-buffer (xwidget-buffer xwidget)
      (cond ((eq xwidget-event-type 'load-changed)
             (xwidget-webkit-execute-script
              xwidget "document.title"
              (lambda (title)
                (xwidget-log "webkit finished loading: '%s'" title)
                ;;TODO - check the native/internal scroll
                ;;(xwidget-adjust-size-to-content xwidget)
                (xwidget-webkit-adjust-size-to-window xwidget)
                (rename-buffer (format "*xwidget webkit: %s *" title))
                (run-hooks 'xwidget-webkit-finished-loading-hook)))
             (pop-to-buffer (current-buffer)))
            ((eq xwidget-event-type 'decide-policy)
             (let ((strarg  (nth 3 last-input-event)))
               (if (string-match ".*#\\(.*\\)" strarg)
                   (xwidget-webkit-show-id-or-named-element
                    xwidget
                    (match-string 1 strarg)))))
            ((eq xwidget-event-type 'javascript-callback)
             (let ((proc (nth 3 last-input-event))
                   (arg  (nth 4 last-input-event)))
               (funcall proc arg)))
            (t (xwidget-log "unhandled event:%s" xwidget-event-type))))))

(defun xwidget-webkit-new-session (url)
  "Create a new webkit session buffer with URL."
  (let*
      ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
       xw)
    (setq xwidget-webkit-last-session-buffer (switch-to-buffer
                                              (get-buffer-create bufname)))
    ;; The xwidget id is stored in a text property, so we need to have
    ;; at least character in this buffer.
    (insert " ")
    ;; Set the initial size to 0 to avoid the white canvas. It will be correctly resized later by the xwidget-webkit-adjust-size-to-window in xwidget-webkit-callback.
    (setq xw (xwidget-insert 1 'webkit bufname 0 0))
    (xwidget-put xw 'callback 'xwidget-webkit-callback)
    (xwidget-webkit-mode)
    (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url)))

(defun xwidget-webkit-forward ()
  "Go forward in history."
  (interactive)
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 "history.go(1);"))

(defun xwidget-webkit-get-current-url (proc)
  "Get the current webkit url and pass it to PROC."
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "document.URL;"
   proc))


(defun xwidget-webkit-open-current-url-in-default-browser ()
  "Open the webkit url in the default browser."
  (interactive)
  (xwidget-webkit-get-current-url (lambda (url) (browse-url-default-browser url))))


(define-key xwidget-webkit-mode-map "u" 'xwidget-webkit-current-url)
(define-key xwidget-webkit-mode-map "U"
  'xwidget-webkit-open-current-url-in-default-browser)
(define-key xwidget-webkit-mode-map [mouse-8] 'xwidget-webkit-back)
(define-key xwidget-webkit-mode-map [mouse-9] 'xwidget-webkit-forward)

(defun mu4e-headers-search-exclude-sms
    (&optional expr prompt edit ignore-history msgid show)
  "Wrapper around mu4e-headers-search to prepend 'NOT maildir:/SMS' to EXPR."
  (interactive)
  (mu4e-headers-search (concat (or expr "") "NOT maildir:/SMS ")
  prompt 't ignore-history msgid show))
(define-key mu4e-headers-mode-map "s" 'mu4e-headers-search-exclude-sms)
(define-key mu4e-main-mode-map "s" 'mu4e-headers-search-exclude-sms)
(define-key mu4e-view-mode-map "s" 'mu4e-headers-search-exclude-sms)
