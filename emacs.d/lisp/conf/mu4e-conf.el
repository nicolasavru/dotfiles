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
                            (:maildir . 10)
                            (:mailing-list . 10)
                            (:from-or-to . 22)
                            (:size . 6)
                            (:subject)))
(setq mu4e-headers-visible-columns 150)

(setq mu4e-view-fields '(:from
                         :to
                         :cc
                         :subject
                         :flags
                         :date
                         :maildir
                         :size
                         :mailing-list
                         :tags
                         :attachments
                         :signature
                         :decryption))

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

(defun mu4e-headers-search-exclude-sms
    (&optional expr prompt edit ignore-history msgid show)
  "Wrapper around mu4e-headers-search to prepend 'NOT maildir:/SMS' to EXPR."
  (interactive)
  (mu4e-headers-search (concat (or expr "") "NOT maildir:/SMS ")
  prompt 't ignore-history msgid show))
(define-key mu4e-headers-mode-map "s" 'mu4e-headers-search-exclude-sms)
(define-key mu4e-main-mode-map "s" 'mu4e-headers-search-exclude-sms)
(define-key mu4e-view-mode-map "s" 'mu4e-headers-search-exclude-sms)


(setq mu4e-change-filenames-when-moving t)

; https://github.com/djcb/mu/issues/1373
(defun my/mu4e-check-draft-rename nil
  "Check if the current file does not exist and if so, checks for another file
that has the same filename up to a ':' or ','. If a file is found, changes the buffer
filename to that file instead"
  (let ((buf-path (buffer-file-name)))
    (unless (file-exists-p buf-path)
      (let ((buf-file (file-name-nondirectory buf-path))
            (buf-dir (file-name-directory buf-path)))
        (when-let ((_ (string-match "^[^\\,\\:]*" buf-file))
                   (new-buf-name
                    (car (file-expand-wildcards
                          (concat buf-dir (match-string 0 buf-file) "*")))))
          (unless (string= new-buf-name buf-path)
            (let ((change-major-mode-with-file-name nil))
            (set-visited-file-name new-buf-name))))))))

(add-hook 'mu4e-compose-mode-hook
    (lambda nil (add-hook 'before-save-hook #'my/mu4e-check-draft-rename  nil t)))

;; https://emacs.stackexchange.com/questions/25012/how-can-i-copy-an-email-message-in-mu4e
;; Function to interactively prompt for a destination (minimally changed from mu4e~mark-get-move-target() )
(defun my~mark-get-copy-target ()
  "Ask for a copy target, and propose to create it if it does not exist."
   (interactive)
   ;;  (mu4e-message-at-point) ;; raises error if there is none
   (let* ((target (mu4e-ask-maildir "Copy message to: "))
          (target (if (string= (substring target 0 1) "/")
                      target
                    (concat "/" target)))
          (fulltarget (concat (mu4e-root-maildir) target)))
     (when (or (file-directory-p fulltarget)
               (and (yes-or-no-p
                     (format "%s does not exist.  Create now?" fulltarget))
                    (mu4e~proc-mkdir fulltarget)))
       target)))

;; Function to duplicate a message given by its docid, msg, and that will be copied to target when the mark is executed.
(defun copy-message-to-target (docid msg target)
  (let ((new_msg_path nil) ;; local variable
        (msg_flags (mu4e-message-field msg :flags)))
    ;; 1. target is already determined interactively when executing the mark
    ;; (:ask-target)

    ;; 2. Determine the path for the new file: we use
    ;; mu4e~draft-message-filename-construct from mu4e-draft.el to create a new
    ;; random filename, and append the original's msg_flags
    (setq new_msg_path (format "%s%s/cur/%s"  ;; "%s/%s/cur/%s"
                               (mu4e-root-maildir)
                               target
                               (mu4e~draft-message-filename-construct
    (mu4e-flags-to-string msg_flags))))

    ;; 3. Copy the message using file system call (copy-file) to new_msg_path:
    ;; (See e.g. mu4e-draft.el > mu4e-draft-open > resend)
    (copy-file (mu4e-message-field msg :path) new_msg_path)

    ;; 4. Add the information to the database (may need to update current search
    ;; query with 'g' if duplicating to current box. Try also 'V' to toggle the
    ;; display of duplicates)
    ;; (mu4e~proc-add new_msg_path (mu4e~mark-check-target target))
    (mu4e~proc-add new_msg_path)
    ))

;; Set this up for marking: see
;; https://www.djcbsoftware.nl/code/mu/mu4e/Adding-a-new-kind-of-mark.html
(add-to-list 'mu4e-marks
             '(copy
               :char ("c" . "c")
               :prompt "copy"
               :ask-target  my~mark-get-copy-target
               :action copy-message-to-target))

(mu4e~headers-defun-mark-for copy)
(define-key mu4e-headers-mode-map (kbd "c") 'mu4e-headers-mark-for-copy)

(setq my-mu4e-query-lock (make-mutex "my-mu4e-query-lock"))
(defvar *my-mu4e-query-cond-var* nil)
(defvar *my-mu4e-query-in-progress* nil)
(defvar *my-mu4e-query-result* nil)
(setq mu4e-proc-filter-lock (make-mutex "mu4e-proc-filter-lock"))

(defun with-mu4e-proc-filter-lock (orig-fun &rest args)
  (with-mutex mu4e-proc-filter-lock
    (apply orig-fun args)))

(advice-add 'mu4e~proc-filter :around #'with-mu4e-proc-filter-lock)

(defun my-mu4e-query (query)
  (with-mutex mu4e-proc-filter-lock
    (let ((*my-mu4e-query-cond-var*
           (make-condition-variable my-mu4e-query-lock "my-mu4e-query-cond-var"))
          (*my-mu4e-query-in-progress* t)
          (*my-mu4e-query-result* nil)
          (mu4e-header-func (lambda (msg) (push msg *my-mu4e-query-result*)))
          (mu4e-found-func (lambda (count)
                             (with-mutex my-mu4e-query-lock
                               (setq *my-mu4e-query-in-progress* nil)
                               ;; (condition-notify *my-mu4e-query-cond-var* t)
                               )))
          (mu4e-erase-func (lambda ())))
      (mu4e~proc-find query nil :date 'descending nil nil nil)
      (with-mutex my-mu4e-query-lock
        (while *my-mu4e-query-in-progress*
          ;; (condition-wait *my-mu4e-query-cond-var*)
          (sleep-for 0.10)
          ))
      *my-mu4e-query-result*)))

(defun mu4e-delete-message-from-all-maildirs (docid msg target)
  (let* ((msgid (plist-get msg :message-id))
         (msgs (my-mu4e-query (format "msgid:%s" msgid))))
    (mu4e-message "Deleting %s distinct messages with msgid %s."
                  (length msgs) msgid)
    (dolist (found_msg msgs)
      (mu4e~proc-move (plist-get found_msg :docid) (mu4e~mark-check-target target) "+T-N"))))


(mu4e~headers-defun-mark-for very-trash)
(add-to-list 'mu4e-marks
             '(very-trash
               :char ("v" . "v")
               :prompt "vtrash"
               :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
               :action mu4e-delete-message-from-all-maildirs))
(define-key mu4e-headers-mode-map (kbd "v") 'mu4e-headers-mark-for-very-trash)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; webkit
(require 'webkit)
(global-set-key (kbd "C-x w") 'webkit)  ;; Bind to whatever global key binding you want if you want
(require 'webkit-ace)  ;; If you want link hinting
(require 'webkit-dark)  ;; If you want to use the simple dark mode

(define-key webkit-mode-map (kbd "<next>") 'webkit-scroll-up)
(define-key webkit-mode-map (kbd "<prior>") 'webkit-scroll-down)


(defun mu4e-action-view-with-webkit (msg)
  (webkit (concat "file://" (mu4e~write-body-to-html msg)) t))

(add-to-list 'mu4e-view-actions
             '("wViewWebkit" . mu4e-action-view-with-webkit) t)


;; If you want history saved in a different place or
;; Set to `nil' to if you don't want history saved to file (will stay in memory)
(setq webkit-history-file nil)

;; If you want cookies saved in a different place or
;; Set to `nil' to if you don't want cookies saved
(setq webkit-cookie-file nil)

;; Set webkit as the default browse-url browser
(setq browse-url-browser-function 'webkit-browse-url)

;; Force webkit to always open a new session instead of reusing a current one
(setq webkit-browse-url-force-new nil)

;; ;; Globally disable javascript
;; (add-hook 'webkit-new-hook #'webkit-enable-javascript)

;; Globally use the simple dark mode
(setq webkit-dark-mode t)

;; ;; Fix blinkin by disabling emacs double buffering in themu4e frameg: https://github.com/akirakyle/emacs-webkit/issues/18
;; (advice-add 'mu4e :before
;;             (lambda (r)
;;               (modify-frame-parameters nil '((inhibit-double-buffering . t)))))



(load-file "~/.emacs.d/lisp/mu4e-views.el")
(use-package mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
              ("w" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
              ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
              ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
              ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
              )
  :config
  (setq mu4e-views-default-view-method "html")
  (mu4e-views-mu4e-use-view-msg-method "html")
  (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-view) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view


(setq mu4e-split-view 'vertical)
