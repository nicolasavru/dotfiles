;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((C . t)
;;    (python . t)
;;    (lisp . t)
;;    (latex . t)
;;    (sh . t)
;;    (lisp . t)
;;    ))

(setq org-log-done 'note)
(setq org-log-done 'time)
(setq org-todo-keywords
'((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(@!)" "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)")))

(eval-after-load "org-agenda"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert (quote t))
 '(auto-insert-alist
   (quote
    ((("\\.\\(h\\|hpp\\)\\'" . "C / C++ header")
      .
      ["skeleton.h" c++-mode my/autoinsert-yas-expand])
     (("\\.c\\'" . "C source")
      .
      ["skeleton.c" my/autoinsert-yas-expand])
     (("\\.cpp\\'" . "C++ source")
      .
      ["skeleton.cpp" my/autoinsert-yas-expand])
     (("\\.sh\\'" . "Shell script")
      .
      ["skeleton.sh" my/autoinsert-yas-expand])
     (("\\.el\\'" . "Emacs Lisp")
      .
      ["skeleton.el" my/autoinsert-yas-expand])
     (("\\.py\\'" . "Python script")
      .
      ["skeleton.py" my/autoinsert-yas-expand])
     (("[mM]akefile\\'" . "Makefile")
      .
      ["Makefile" my/autoinsert-yas-expand])
     (("\\.tex\\'" . "TeX/LaTeX")
      .
      ["skeleton.tex" my/autoinsert-yas-expand]))))
 '(org-agenda-custom-commands
   (quote
    (("d" todo "DELEGATED" nil)
     ("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda ""
      ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)
           (quote regexp)
           "
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/gdrive/My Drive/org/todo.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/gdrive/My Drive/org/notes.org")
 '(org-directory "~/gdrive/My Drive/org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-mobile-inbox-for-pull "~/gdrive/My Drive/org/todo.org")
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    ((116 "* TODO %?
  %u" "~/gdrive/My Drive/org/todo.org" "Tasks")
     (110 "* %u %?" "~/gdrive/My Drive/org/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (org htmlize yasnippet-snippets helm emms-player-mpv offlineimap rust-mode rustfmt jabber smart-tab scala-mode2 google-contacts doc-mode color-theme clojure-mode auto-complete-chunk auto-complete-c-headers auto-complete-auctex auctex apache-mode)))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))

(require 'org-wiki)
(setq org-wiki-location "~/gdrive/My Drive/org/wiki")
(setq org-wiki-user-init-file "~/dotfiles/emacs.org-wiki-export")

;; Call to set grep-find-template, which is unset by default. See /usr/share/emacs/25.3/lisp/progmodes/grep.el.gz:174
(grep-compute-defaults)

(global-set-key (kbd "C-c o w i") 'org-wiki-index)
(global-set-key (kbd "C-c o w h") 'org-wiki-helm)
(global-set-key (kbd "C-c o w c") 'org-wiki-create)
(global-set-key (kbd "C-c o w x") 'org-wiki-export-html)
(global-set-key (kbd "C-c o w s") 'org-wiki-search)


;; ;; TODO: find better way of doing this without relying on pup.
;; (defun get-url-title (url)
;;   (replace-regexp-in-string
;;    "[ \t\n]$" ""
;;    (shell-command-to-string
;;     (format "wget -q  '%s' -O - | pup --plain 'meta[property=og:title] attr{content}'"
;;             url))))

;; https://unix.stackexchange.com/a/103352
;; (defun get-url-title (url)
;;   (replace-regexp-in-string
;;    "[ \t\n]$" ""
;;    (shell-command-to-string
;;     (concat
;;      "lynx -cfg=<(printf '%s\n' 'PRINTER:P:printf \"%0s\\n\" \"$LYNX_PRINT_TITLE\">&3:TRUE') 3>&1 > /dev/null -nopause -noprint -accept_all_cookies -cmd_script <(printf '%s\n' \"key p\" \"key Select key\" \"key ^J\" exit) "
;;      url))))



(setq org-wiki-template
      (concat "#+INCLUDE: style.org\n"
              "#+SETUPFILE: ~/.emacs.d/org-html-themes/setup/theme-readtheorg-local.setup\n\n"
              "#+TITLE: %n\n"
              "#+DESCRIPTION:\n"
              "#+KEYWORDS:\n"
              "\n\n"
              "- [[wiki:index][Index]]\n\n"
              "- Related: \n\n"
              "* %n\n"))

; https://writequit.org/articles/emacs-org-mode-generate-ids.html
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    ;; TODO: consider overwriting CUSTOM_ID if the entry is renamed.
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        ;; (setq id (org-id-new (concat prefix "h")))
        (setq id
              (replace-regexp-in-string
               "[^[:alnum:]]" "-" (downcase (org-entry-get nil "ITEM"))))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(setq global-org-auto-id t)
(defun org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (or global-org-auto-id
              (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t))
      (org-map-entries
       (lambda () (org-custom-id-get (point) 'create))))))

;; automatically add ids to saved org-mode headlines
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (org-add-ids-to-headlines-in-file))))))


;; automatically add ids to captured headlines
(add-hook 'org-capture-prepare-finalize-hook
          (lambda () (org-custom-id-get (point) 'create)))


(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)
; Unbind org-meta* to windmove can keep these bindings.
(add-hook 'org-mode-hook
  (lambda()
    (local-unset-key (kbd "M-<up>"))
    (local-unset-key (kbd "M-<down>"))
    (local-unset-key (kbd "M-<left>"))
    (local-unset-key (kbd "M-<right>"))))

(setq org-startup-folded nil)

;; ;; TODO keyword faces
;; (setq org-todo-keyword-faces
;;       '(("PBUG" . (:background "gold" :foreground "indianred3" :weight bold))
;;     ("CBUG" . (:background "gold" :foreground "indianred3" :weight bold))
;;     ("SEGF" . (:background "gold" :foreground "indianred3" :weight bold))
;;     ("CNCL" . (:background "snow3" :foreground "black" :weight bold))
;;     ))

;; ;; TAG faces
;; (setq org-tag-faces
;;       '(("PROJ" :background "indianred3" :foreground "cornsilk2" :weight bold)
;;     ))

(setq org-mobile-directory "~/gdrive/My Drive/org/mobileorg")



;; https://unix.stackexchange.com/a/103331
(defun get-url-title (url)
  (replace-regexp-in-string
   "[ \t\n]$" ""
   (shell-command-to-string
    (format "python -c \"import bs4, urllib.request; print(bs4.BeautifulSoup(urllib.request.urlopen('%s'), 'lxml').title.text)\""
            url))))

(defun org-board-add (url)
  (interactive (list (completing-read "URL: " nil)))
  (org-insert-heading)
  (insert (format "[[%s][%s]]" url (get-url-title url)))
  (org-board-new url))


(global-set-key (kbd "C-c o b") org-board-keymap)
(global-set-key (kbd "C-c o b a") 'org-board-add)

(require 'org-board)
(add-to-list 'org-board-wget-switches "--span-hosts" t)
