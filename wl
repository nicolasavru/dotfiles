(setq wl-summary-toggle-mime "mime")
(require 'mime-w3m)
(setq mime-edit-split-message nil)
(setq wl-draft-reply-buffer-style 'full)
(setq wl-stay-folder-window t)

(setq wl-icon-directory "/usr/share/emacs/24.1/etc/wl/icons")

(autoload 'wl-user-agent-compose "wl-draft" nil t) 
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

(setq elmo-imap4-default-authenticate-type 'clear)

(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-message-id-domain "smtp.gmail.com")
(setq wl-from "Nicolas Avrutin <nicolasavru@mail.com>")
(setq wl-local-domain "smtp.gmail.com")

;; (setq wl-draft-always-delete-myself t)

;;; bbdb
(setq bbdb-use-alternate-names t)
(setq wl-temporary-file-directory "/tmp")
(setq bbdb-file "~/.bbdb")
;; (setq bbdb/mail-auto-create-p   'bbdb-ignore-some-messages-hook)

;; add record to .bbdb manually
;; (setq bbdb-new-nets-always-primary t)
;; (setq bbdb/mail-auto-create-p nil) 

;; height of BBDB's window
(setq bbdb-pop-up-window-size 10)

;; do not ask whether to add sender's address to bbdb
(setq wl-message-redisplay-hook nil)

;; ;;; popup display of record in the .bbdb
;; (setq bbdb-use-pop-up t)
;; (setq signature-use-bbdb t)
;; (setq bbdb-north-american-phone-numbers-p nil)

;;; hide bbdb field while wl-folder-suspend
;; (defadvice wl-folder-suspend (after wl-bbdb-suspend activate compile)
;;   (interactive)
;; (bbdb-wl-exit-2))
;; (defadvice wl-exit (after wl-bbdb-suspend activate compile)
;;   (interactive)
;; (bbdb-wl-exit-2))

(put 'ML 'field-separator "\n")
(put 'User-Agent 'field-separator "\n")

;; (setq bbdb-auto-notes-alist
;;        '(
;;        ("X-ML-Name" (".*$" ML 0))
;;        ("X-Mailinglist" (".*$" ML 0))
;;        ("X-Ml-Name" (".*$" ML 0))
;;        ("X-Mailer" (".*$" User-Agent 0))
;;        ("X-Newsreader" (".*$" User-Agent 0))
;;        ("User-Agent" (".*$" User-Agent 0))
;;        ("X-Face" ("[ \t\n]*\\([^ \t\n]*\\)\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
;;                                  face "\\1\\3\\5\\7"))
;;        ))

;; ;; expiry
;; (setq wl-expire-alist                                                       
;;         '(("^\\+trash$"   (date 14) remove)                                   
;;                                     ;; delete                                 
;;           ("^\\+tmp$"     (date 7) trash)                                     
;;                                     ;; re-file to wl-trash-folder             
;;           ("^\\%inbox"    (date 30) wl-expire-archive-date)                   
;;                              ;; archive by year and month (numbers discarded)
;;                  ))

;; (add-hook 'wl-summary-prepared-pre-hook 'wl-summary-expire)

;; (setq wl-expire-alist
;;         '(("Shopping Newsletters"     (date 7) trash)
;;           ))


;;auto-fill 
(setq mime-edit-mode-hook
      '(lambda ()
           (auto-fill-mode 1)))

(setq wl-message-visible-field-list '("^To" "^Subject" "^From" "^Date" "^Cc"))
(setq wl-message-ignored-field-list '("^"))

;;look in zip files as if they were folders
(setq elmo-archive-treat-file t)

;;show sent mail by who it was to
(setq wl-summary-showto-folder-regexp "(.*)Sent(.*)")
(setq wl-summary-from-function 'wl-summary-default-from)

;; refiling

;; (setq wl-refile-rule-alist
;;       '((("To" "Cc")
;;          ("^wl-en@lists.airs.net" . "+mlists"))))


;; IMAP 
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "nicolasavru@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "nicolasavru")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t) 

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(setq wl-message-buffer-prefetch-folder-list ".*")
(setq wl-message-buffer-prefetch-depth 9)
(setq elmo-message-fetch-threshold 20000000)
(setq wl-prefetch-confirm nil)

(setq wl-biff-check-folder-list '("%INBOX"))

(setq wl-summary-line-format "%n%T%P%Y-%M-%D(%W)-%h:%m %5S %t%[%28(%c %f%) %] %s")
(setq wl-summary-width 150)

(setq wl-use-scoring nil) 


(setq wl-forward-subject-prefix "Fwd: " )    ;; use "Fwd: " not "Forward: "

;; Invert behaviour of with and without argument replies.
;; just the author
(setq wl-draft-reply-without-argument-list
  '(("Reply-To" ("Reply-To") nil nil)
     ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
     ("From" ("From") nil nil)))


;; bombard the world
(setq wl-draft-reply-with-argument-list
  '(("Followup-To" nil nil ("Followup-To"))
     ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
     ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
     ("From" ("From") ("To" "Cc") ("Newsgroups"))))


(define-key wl-draft-mode-map (kbd "<C-tab>") 'bbdb-complete-mail)
(setq bbdb-complete-mail-allow-cycling t)

;; multiple accounts
;; You should set this variable if you use multiple e-mail addresses.
(setq wl-user-mail-address-list
      '("nicolasavru@gmail.com", "avruti@cooper.edu", "rasputin@cooper.edu"))

;; How messages with disposal mark ("d") are to be handled.
;; remove = instant removal (same as "D"), thrash = move to wl-trash-folder
;; string = move to string.
(setq wl-dispose-folder-alist
      '(("^%.*cooper\\.edu" . "%INBOX.Trash:avruti/clear@farley2.cooper.edu!")
        ("^%.*gmail\\.com" . "%[Gmail]/Trash")))

;;select correct email address when we _start_ writing a draft.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

;;is run when wl-draft-send-and-exit or wl-draft-send is invoked:
;;(NOTE: "M-: wl-draft-parent-folder" => %INBOX:myname/clear@imap.gmail.com:993)
(setq wl-draft-config-alist
      '(((string-match "cooper.edu" wl-draft-parent-folder)
         (template . "cooper-rasputin")
         (wl-smtp-posting-user . "avruti")
         (wl-smtp-posting-server . "farley2.cooper.edu")
         (wl-smtp-authenticate-type ."plain")
         (wl-smtp-connection-type . 'starttls)
         (wl-local-domain . "local.navru.net")
         (wl-draft-folder . "%INBOX.Drafts:avruti/clear@farley2.cooper.edu!"))
        ((string-match "gmail.com" wl-draft-parent-folder)
         (template . "gmail")
         (wl-smtp-posting-user . "nicolasavru")
         (wl-smtp-posting-server . "smtp.gmail.com")
         (wl-smtp-authenticate-type ."plain")
         (wl-smtp-connection-type . 'starttls)
         (wl-smtp-posting-port . 587)
         (wl-local-domain . "local.navru.net")
         (wl-message-id-domain . "smtp.gmail.com")
         (wl-draft-folder . "%[Gmail]/Drafts:nicolasavru/clear@imap.gmail.com:993!"))))


;;choose template with C-c C-j
(setq wl-template-alist
      '(("gmail"
         (wl-from . "Nicolas Avrutin <nicolasavru@gmail.com>")
         ("From" . wl-from))
        ("cooper-avruti"
         (wl-from . "Nicolas Avrutin <avruti@cooper.edu>")
         ("From" . wl-from)
         ("Fcc" . "%INBOX.Sent:avruti/clear@farley2.cooper.edu!"))
        ("cooper-rasputin"
         (wl-from . "Nicolas Avrutin <rasputin@cooper.edu>")
         ("From" . wl-from)
         ("Fcc" . "%INBOX.Sent:avruti/clear@farley2.cooper.edu!"))))


;; ;; Use different signature files based on From: address
;; (setq signature-file-alist
;;       `((("From" . "avruti@cooper.edu") . ,(expand-file-name "~/.emacs.d/signature.d/avruti@cooper.edu"))
;;         (("From" . "rasputin@cooper.edu") . ,(expand-file-name "~/.emacs.d/signature.d/rasputin@cooper.edu"))
;;         (("From" . "nicolasavru@gmail.com") . ,(expand-file-name "~/.emacs.d/signature.d/nicolasavru@gmail.com"))))

(setq signature-file-name "~/.signature")
(setq signature-insert-at-eof t)
(setq signature-delete-blank-lines-at-eof t)

;;Cycle through templates with arrow keys
(define-key wl-template-mode-map (kbd "<right>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<left>") 'wl-template-prev)

;;Only save draft when I tell it to! (C-x C-s or C-c C-s):
;;(arg: seconds of idle time untill auto-save).
(setq wl-auto-save-drafts-interval nil)


;; mark sent messages (folder carbon copy) as read.
(setq wl-fcc-force-as-read t)

(add-hook 'wl-draft-mode-hook 'flyspell-mode)

(setq org-footnote-auto-label 'plain)
(define-key wl-draft-mode-map (kbd "C-c w f") 'org-footnote-action)

(require 'boxquote)
(define-key wl-draft-mode-map (kbd "C-c w q") 'boxquote-region)

;; http://emacs-fu.blogspot.com/2008/12/some-simple-tricks-boxquote-footnote.html
(defun djcb-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]" 
              (if (= 0 (length summ)) "" (concat ": " summ))
              n 
              (if (= 1 n) "" "s")))))

(define-key wl-draft-mode-map (kbd "C-c w s") 'djcb-snip)
