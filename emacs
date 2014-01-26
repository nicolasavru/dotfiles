;(setq load-path (cons "~/.emacs.d" load-path))
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;·keysyms

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOC" [(meta right)])
(define-key input-decode-map "\e\eOD" [(meta left)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages

(setq package-user-dir "~/.emacs.d/lisp/elpa")
(load-file "~/.emacs.d/lisp/conf/packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom lisp
;; emacs or package lisp files which I have modified
;; (possibly awaiting patch acceptance)

(load-file "~/.emacs.d/lisp/custom-lisp/emms-lastfm-scrobbler.el")
(load-file "~/.emacs.d/lisp/custom-lisp/emms-get-lyrics.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private
;; passwords and the such

(load-file "~/.emacs.d/lisp/conf/private.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc functions

(load-file "~/.emacs.d/lisp/conf/misc_funcs.el")

;http://www.djcbsoftware.nl/dot-emacs.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings

(setq message-log-max 1000)              ;; set Message buffer length to 1000 lines

(setq x-select-enable-clipboard t        ;; copy-paste should work ...
  interprogram-paste-function            ;; ...with...
  'x-cut-buffer-or-selection-value)      ;; ...other X clients

(setq search-highlight t                 ;; highlight when searching...
  query-replace-highlight t)             ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

(setq completion-ignore-case t           ;; ignore case when completing...
  read-file-name-completion-ignore-case t) ;; ...filenames too

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))
(put 'narrow-to-region 'disabled nil)    ;; enable...
(put 'erase-buffer 'disabled nil)        ;; ... useful things
(file-name-shadow-mode t)                ;; be smart about filenames in mbuf

(setq require-final-newline t)           ;; end files with a newline

;; slick-copy: make copy-past a bit more intelligent
;; from: http://www.emacswiki.org/emacs/SlickCopy
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (message "Copied line")
      (list (line-beginning-position)
               (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; key board / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal

;start maximized
;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;; )
;;(run-with-idle-timer 0.1 nil 'toggle-fullscreen)
;(toggle-fullscreen)

;(defun toggle-fullscreen (&optional f)
;      (interactive)
;      (let ((current-value (frame-parameter nil 'fullscreen)))
;           (set-frame-parameter nil 'fullscreen
;                                (if (equal 'fullboth current-value)
;                                    (if (boundp 'old-fullscreen) old-fullscreen nil)
;                                    (progn (setq old-fullscreen current-value)
;                                           'fullboth)))))
;    (global-set-key [f11] 'toggle-fullscreen)
    ; Make new frames fullscreen by default. Note: this hook doesn't do
    ; anything to the initial frame if it's in your .emacs, since that file is
    ; read _after_ the initial frame is created.
;    (add-hook 'after-make-frame-functions 'toggle-fullscreen)

(windmove-default-keybindings 'meta)

; for console:
;; (global-set-key [(alt left)]  'windmove-left)
;; (global-set-key [(alt up)]    'windmove-up)
;; (global-set-key [(alt right)] 'windmove-right)
;; (global-set-key [(alt down)]  'windmove-down)

(tool-bar-mode -1)

(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 5
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)

; http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "C-c C-<left>") 'winner-undo)
(global-set-key (kbd "C-c C-<right>") 'winner-redo)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq Man-notify-method 'pushy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
;;
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the minibuffer
;;
(setq
  enable-recursive-minibuffers t         ;;  allow mb cmds in the mb
  max-mini-window-height 5
  minibuffer-scroll-window nil
  resize-mini-windows t)
(minibuffer-depth-indicate-mode t)

(icomplete-mode t)                       ;; completion in minibuffer
(setq
  icomplete-prospects-height 1           ;; don't spam my minibuffer
  icomplete-compute-delay 0)             ;; don't wait
(require 'icomplete+ nil 'noerror)       ;; drew adams' extras

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some handy packages

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default



;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; saveplace: save location in file when saving files
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package

(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

(require 'undo-tree)
(global-undo-tree-mode)

(require 'hippie-exp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some misc other packages

;; multi-term
(when (require 'multi-term nil 'noerror)
 (setq multi-term-program "/bin/zsh"))

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/lisp/auto-install")
;;(auto-install-update-emacswiki-package-name t)

;; fullscreen
(require 'fullscreen)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'unbound)

(require 'misc)
(global-set-key "\M-z" 'zap-up-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line
(global-set-key (kbd "M-G")         'goto-char)    ; M-g  'goto-char

(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))   ;; ibuffer

(global-set-key (kbd "H-<kp-4>") "a")
(global-set-key (kbd "H-S-<kp-left>") "A")
(global-set-key (kbd "H-<kp-5>") "b")
(global-set-key (kbd "H-S-<kp-begin>") "B")
(global-set-key (kbd "H-<kp-6>") "c")
(global-set-key (kbd "H-S-<kp-right>") "C")
(global-set-key (kbd "H-<kp-7>") "d")
(global-set-key (kbd "H-S-<kp-home>") "D")
(global-set-key (kbd "H-<kp-8>") "e")
(global-set-key (kbd "H-S-<kp-up>") "E")
(global-set-key (kbd "H-<kp-9>") "f")
(global-set-key (kbd "H-S-<kp-prior>") "F")
(global-set-key (kbd "H-<kp-add>") ":")
(global-set-key (kbd "H-<kp-subtract>") 'backward-delete-char-untabify)

(global-set-key (kbd "H-j") 'next-line)
(global-set-key (kbd "H-k") 'previous-line)
(global-set-key (kbd "H-h") 'backward-char)
(global-set-key (kbd "H-l") 'forward-char)

(global-set-key (kbd "H-C-h") 'previous-buffer)
(global-set-key (kbd "H-C-l") 'next-buffer)

(global-set-key (kbd "C-H-S-s-<f12>") 'butterfly)

(global-set-key (kbd "C-<f5>") 'linum-mode)                 ;; line numbers
(global-set-key (kbd "C-<f6>") 'magit-status)               ;; ...git mode
(global-set-key (kbd "C-<f7>") 'compile)                     ;; compile
(global-set-key (kbd "C-<f8>") 'comment-or-uncomment-region) ;; (un)comment

(global-set-key (kbd "C-c p") 'insert-debug-clause) ;; insert flagged printf
(global-set-key (kbd "C-c r") 'delete-printfs)  ;; remove flagged printfs

(global-set-key (kbd "C-c d") 'djcb-dup)

(when (fboundp 'fullscreen-toggle)
  (global-set-key (kbd "C-<f11>") 'fullscreen-toggle))

(when (fboundp 'multi-term-dedicated-toggle)
  (global-set-key (kbd "C-<f12>") 'multi-term-dedicated-toggle)) ; multi-term

(global-set-key (kbd "<f25>") 'browse-url-firefox)

(global-set-key (kbd "C-x C-S-e") 'eval-and-replace)

;; program shortcuts
(global-set-key (kbd "C-c E") ;; .emacs
  (lambda()(interactive)(find-file "~/.emacs")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global editor settings

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(show-paren-mode 1)

(autoload 'linum-mode "linum" "mode for line numbers" t)

(require 'whitespace)
(setq whitespace-style
        '(face tabs spaces newline space-mark tab-mark newline-mark indentation space-after-tab space-before-tab))
(global-whitespace-mode)
(global-linum-mode) ;)

(add-hook 'linum-before-numbering-hook
  (lambda ()
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (setq linum-format
            `(lambda (line)
               (propertize (concat
                            (truncate-string-to-width
                             "" (- ,w (length (number-to-string line)))
                             nil ?\x2007)
                            (number-to-string line))
                           'face 'linum))))))


;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;;; bind RET to py-newline-and-indent
;; (add-hook 'python-mode-hook '(lambda ()
;;     (define-key python-mode-map "C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(require 'cc-mode)
(setq c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUDA

(setq auto-mode-alist (append '(("/*.\.cu$" . c++-mode)) auto-mode-alist))
(setq cscope-indexer-suffixes (cons "*.cu" cscope-indexer-suffixes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(require 'haskell-mode)
;; (load "~/.emacs.d/haskellmode-emacs/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq auto-mode-alist (append '(("/*.\.hs$" . haskell-mode)) auto-mode-alist))
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master t)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;(add-hook 'LaTeX-mode-hook '(setq whitespace-line nil))
(setq reftex-plug-into-AUCTeX t)

; http://old.nabble.com/shell-escape-td1076639.html
(defun TeX-toggle-escape nil (interactive)
  (setq LaTeX-command
        (if (string= LaTeX-command "latex") "latex -shell-escape" "latex")))

(add-hook 'LaTeX-mode-hook
          (lambda nil
            (define-key LaTeX-mode-map "\C-c\C-t\C-x" 'TeX-toggle-escape)))


;; (add-hook 'TeX-PDF-mode-hook
;; (lambda () (setq TeX-source-correlate-method-active
;;         (if TeX-PDF-mode 'synctex 'source-specials))))


(defun my-TeX-command-master ()
    (interactive)
    (setq TeX-source-correlate-method-active
          (if TeX-PDF-mode 'synctex 'source-specials))
    (TeX-command-master))

(defun my-latex-hook ()
(define-key LaTeX-mode-map (kbd "C-c C-c") 'my-TeX-command-master))

(add-hook 'LaTeX-mode-hook 'my-latex-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode

;(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)
   (lisp . t)
   (latex . t)
   (sh . t)
   (lisp . t)
   ))

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
 '(org-directory "~/Dropbox/org")
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org")))
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-mobile-inbox-for-pull "~/Dropbox/org/todo.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
       ("c" todo "DONE|DEFERRED|CANCELLED" nil)
       ("w" todo "WAITING" nil)
       ("W" agenda "" ((org-agenda-ndays 21)))
       ("A" agenda ""
        ((org-agenda-skip-function
          (lambda nil
        (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
         (org-agenda-ndays 1)
         (org-agenda-overriding-header "Today's Priority #A tasks: ")))
       ("u" alltodo ""
        ((org-agenda-skip-function
          (lambda nil
        (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                      (quote regexp) "\n]+>")))
         (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/Dropbox/org/todo.org" "Tasks")
       (110 "* %u %?" "~/Dropbox/org/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))




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

(setq org-mobile-directory "~/Dropbox/org/mobileorg")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no-word

(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET

;; ;; Load CEDET.
;; ;; See cedet/common/cedet.info for configuration details.
;; (load-file "~/.emacs.d/cedet-1.0pre7/common/cedet.el")
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/cedet")

;; ;; Enable EDE (Project Management) features
;;(global-ede-mode 1)

;; ;; Enable EDE for a pre-existing C++ project
;; ;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; ;; Enabling Semantic (code-parsing, smart completion) features
;; ;; Select one of the following:

;; ;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)

;; ;; * This enables some tools useful for coding, such as summary mode
;; ;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; ;; * This enables even more coding tools such as intellisense mode
;; ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;  (semantic-load-enable-gaudy-code-helpers)

;; ;; * This enables the use of Exuberent ctags if you have it installed.
;; ;;   If you use C++ templates or boost, you should NOT enable it.
;; ;; (semantic-load-enable-all-exuberent-ctags-support)
;; ;;   Or, use one of these two types of support.
;; ;;   Add support for new languges only via ctags.
;; ;; (semantic-load-enable-primary-exuberent-ctags-support)
;; ;;   Add support for using ctags as a backup parser.
;; ;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; ;; Enable SRecode (Template management) minor-mode.
;; ;; (global-srecode-minor-mode 1)
;;(require 'semantic/sb)
;;(require 'semantic-gcc)
;;(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB

;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")

;;2. To load ecb at startup:
;;(require 'ecb)
;;- or -
;;To load ecb first after starting it by ecb-activate:
;;(require 'ecb-autoloads)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS

(load-file "~/.emacs.d/lisp/conf/emms-conf.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnuplot

;; these lines enable the use of gnuplot mode
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-8 key so that it opens a buffer into
;; gnuplot mode
  (global-set-key [(f8)] 'gnuplot-make-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc funcitons

;; (defadvice show-paren-function
;;   (after show-matching-paren-offscreen activate)
;;   "If the matching paren is offscreen, show the matching line in the
;;     echo area. Has no effect if the character before point is not of
;;     the syntax class ')'."
;;   (interactive)
;;   (let ((matching-text nil))
;;     ;; Only call `blink-matching-open' if the character before point
;;     ;; is a close parentheses type character. Otherwise, there's not
;;     ;; really any point, and `blink-matching-open' would just echo
;;     ;; "Mismatched parentheses", which gets really annoying.
;;     (if (char-equal (char-syntax (char-before (point))) ?\))
;;         (setq matching-text (blink-matching-open)))
;;     (if (not (null matching-text))
;;         (message matching-text))))

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))


(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(global-set-key (kbd "<3270_PA1>") 'goto-match-paren)
(global-set-key (kbd "C-<menu>") 'goto-match-paren)

(defun joc-enlarge-by-ten()
  "enlarges a window 10 lines"
  (interactive)
  (enlarge-window 10))

(defun joc-shrink-by-five()
  "shrinks a window 5 lines"
  (interactive)
  (shrink-window 5))

;http://www.dotemacs.de/dotfiles/KilianAFoth.emacs.html
;; insert flagged debug statement
(defun insert-debug-clause () (interactive)
  "Insert a fresh debugging printf()-statement and position point in it"
  (c-indent-command)
  (insert-string "/* DEBUG */\n")
  (c-indent-command)
  (insert-string "fprintf(stderr, \"\\n\");\n")
  (c-indent-command)
  (insert-string "/* DEBUG */")
  (previous-line 1)
  (forward-char 6)
  )

;; remove flagged debug statements
(defun delete-printfs () (interactive)
  "eliminate sections bounded by /* DEBUG */ <stuff> /* DEBUG */"
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp
     "[\t\n ]*/\\* DEBUG \\*/[^/]+/\\* DEBUG \\*/\n"
     "\n")))
;; This is a mostly straightforward regexp replacement. Note that we
;; also try to erase the whitespace with which the entire construction
;; is likely to be surrounded. But the final spaces following the
;; marker are most likely the indentation of the following statement,
;; so we make sure that the replaced text ends in a \n, leaving those
;; spaces alone. The entire operation fails if the bounded section
;; contains a /.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME

(require 'slime)

(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations
           '((sbcl ("/usr/bin/sbcl"))
             (ecl ("/usr/bin/ecl"))
             (clisp ("/usr/bin/clisp"))))
     (slime-setup '(
                    slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-indentation
                    slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser
                    ))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     ))

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq common-lisp-hyperspec-root "/usr/share/doc/HyperSpec/")

(global-set-key (kbd "<f5>") 'hyperspec-lookup)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC

(load-file "~/.emacs.d/lisp/conf/erc_conf.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes

;; (require 'color-theme)
;; (setq custom-theme-directory "~/.emacs.d/themes")
;; ;; (setq color-theme-is-global t)

;; (require 'color-theme)
;; (color-theme-initialize)

(setq custom-theme-directory "~/.emacs.d/lisp/themes")


;http://emacs-fu.blogspot.com/2009/03/color-theming.html
;; hook: test win sys and rerun color-theme
(defun test-win-sys(frame)
  (let ((color-theme-is-global nil))
    (select-frame frame)
    (if (window-system frame)
        (load-file "~/.emacs.d/lisp/themes/nicolasavru-dark-theme.el")
      (custom-set-faces '(whitespace-space ((((class color) (background light)) (:foreground "aquamarine3"))))))))

;; hook on after-make-frame-functions
(add-hook 'after-make-frame-functions 'test-win-sys)

;; default start
(let ((color-theme-is-global nil))
  (if (window-system)
      (load-file "~/.emacs.d/lisp/themes/nicolasavru-dark-theme.el")
    (custom-set-faces '(whitespace-space ((((class color) (background light)) (:foreground "aquamarine3")))))))


(set-face-attribute 'default nil :height 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial frames

(make-frame-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window/Frame Manager

(load-file "~/.emacs.d/lisp/conf/three-windows.el")
(require 'transpose-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git

(require 'magit)
(autoload 'magit-status "magit" nil t)
(global-set-key "\C-xg" 'magit-status)

(require 'git-gutter-fringe)
(global-git-gutter-mode 1)
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(setq git-gutter:verbosity 2) ; don't be that chatty

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)
;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term

(setq term-default-bg-color nil)
(setq term-default-fg-color "green")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic-modes

(require 'generic-x)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP

(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))


;; (defun smart-tab ()
;;   "If mark is active, indents region. Else if point is at the end of a symbol,
;; expands it. Else indents the current line. Acts as normal in minibuffer."
;;   (interactive)
;;   (cond (mark-active (indent-region (region-beginning) (region-end)))
;;         ((and (looking-at "\\_>") (not (looking-at "end")))
;;          (hippie-expand nil))
;;         (t (indent-for-tab-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart tab

; http://joost.zeekat.nl/2010/06/03/slime-hints-3-interactive-completions-and-smart-tabs/
(require 'smart-tab) ;; make sure smart-tab.el is reachable in your load-path first
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode 1) ;; switch on smart-tab everywhere

(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion) ;; this is the "default" emacs expansion function
;;        (mu4e-compose-mode . google-contacts-message-complete-function)
        (mu4e-compose-mode . completion-at-point)
        (clojure-mode . slime-complete-symbol)
        (lisp-mode . slime-complete-symbol)
        (matlab-shell-mode . matlab-shell-tab))) ;; see update below

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Line

;; http://emacs-fu.blogspot.com/2010/05/cleaning-up-mode-line.html

(require 'diminish)
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))
(eval-after-load "whitespace"
  '(diminish 'global-whitespace-mode))

(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (setq mode-name "el"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp

(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern
      (concat (if (featurep 'xemacs) "" "\\(?:^\\|\r\\)")
              "[^#$%>\n]*#?[#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sudo editing

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
         (space (+ 6 (- (window-width) (length warning))))
         (bracket (make-string (/ space 2) ?-))
         (warning (concat bracket warning bracket)))
    (setq header-line-format
          (propertize  warning 'face 'find-file-root-header-face))))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixed point completion

(load "~/.emacs.d/lisp/fixed-point-completion/fixed-point-completion.el")
(enable-fixed-point-completions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e-sink

(load "~/.emacs.d/lisp/e-sink/e-sink.el")
(require 'e-sink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matlab

;; (add-to-list 'load-path "~/.emacs.d/matlab-emacs")
(load-library "matlab-load")
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet

(require 'yasnippet)
;; (setq yas/root-directory '("~/.emacs.d/yasnippet/snippets" "~/.emacs.d/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
(yas/global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoinsert

(require 'autoinsert)
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/snippets/skeletons") ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(custom-set-variables
 '(auto-insert 't)
 '(auto-insert-alist '((("\\.\\(h\\|hpp\\)\\'" . "C / C++ header") . ["skeleton.h" c++-mode my/autoinsert-yas-expand])
                       (("\\.c\\'" . "C source") . ["skeleton.c" my/autoinsert-yas-expand])
                       (("\\.cpp\\'" . "C++ source") . ["skeleton.cpp" my/autoinsert-yas-expand])
                       (("\\.sh\\'" . "Shell script") . ["skeleton.sh" my/autoinsert-yas-expand])
                       (("\\.el\\'" . "Emacs Lisp") . ["skeleton.el" my/autoinsert-yas-expand])
                       (("\\.py\\'" . "Python script") . ["skeleton.py" my/autoinsert-yas-expand])
                       (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
                       (("\\.tex\\'" . "TeX/LaTeX") . ["skeleton.tex" my/autoinsert-yas-expand]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m

(require 'w3m-load)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point)
(defun w3m-mode-options ()
  (local-set-key (kbd "<left>") 'backward-char)
  (local-set-key (kbd "<right>") 'forward-char)
  (local-set-key (kbd "<up>") 'previous-line)
  (local-set-key (kbd "<down>") 'next-line)
  (local-set-key (kbd "M-<left>") 'w3m-view-previous-page)
  (local-set-key (kbd "M-<right>") 'w3m-view-next-page)
  (local-set-key (kbd "C-<tab>") 'w3m-next-buffer)
  (local-set-key (kbd "q") 'bury-buffer))
(add-hook 'w3m-mode-hook 'w3m-mode-options)
(setq w3m-use-cookies t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jabber

(require 'jabber-autoloads)

(setq jabber-show-resources 'always)
(setq jabber-autoaway-method 'jabber-xprintidle-get-idle-time)
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)
(setq jabber-chat-time-format "%H:%M:%S")
(setq jabber-chat-delayed-time-format "%Y-%m-%d %H:%M:%S")
(setq jabber-default-status "There is beauty in complexity, but elegance in simplicity.")
(setq jabber-auto-reconnect t)
;; ;(setq jabber-alert-presence-hooks nil)
;; (add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)

(jabber-connect-all)
;; (jabber-mode-line-mode)

;; don't clobber my minibuffer!
(define-jabber-alert echo "Show a message in the echo area"
  (lambda (msg)
    (unless (minibuffer-prompt)
      (message "%s" msg))))

(global-set-key (kbd "C-c j") (lambda ()
                               (interactive)
                               (switch-to-buffer "*-jabber-roster-*")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sr-speedbar

(require 'sr-speedbar)

;; (when (require 'sr-speedbar nil 'noerror)
;; ;;   (setq speedbar-supported-extension-expressions
;; ;;     '(".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?"
;; ;;        ".tex\\(i\\(nfo\\)?\\)?" ".el"
;; ;;        ".java" ".p[lm]" ".pm" ".py"  ".s?html"  "Make
;; ;;file.am" "configure.ac"))
;;   (setq
;;     sr-speedbar-width-x 20
;;     sr-speedbar-right-side t))

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-update-flag t
      sr-speedbar-width 30
      sr-speedbar-width-x 30
      sr-speedbar-auto-refresh t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil
      speedbar-directory-unshown-regexp "^$")

(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

(sr-speedbar-open)

(global-set-key (kbd "C-<f9>") 'sr-speedbar-toggle)
(global-set-key (kbd "C-<f10>") 'sr-speedbar-select-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ocaml

(setq auto-mode-alist (cons '("\\.ml[iylp]?\\'" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure

(require 'clojure-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; voice
(require 'festival)
(festival-start)
(require 'festival-extension)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sauron

(require 'sauron)
(global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)

(setq sauron-dbus-cookie t)

(setq
 sauron-max-line-length 120
 sauron-watch-patterns '("volhv")
 sauron-watch-nicks '("volhv"))

(add-hook 'sauron-event-added-functions
          (lambda (origin prio msg &optional props)
            (if (string-match "ping" msg)
                (sauron-fx-sox "/usr/share/sounds/trillian/generic-event.wav"))
            (cond
;             ((= prio 3) (sauron-fx-sox "/usr/share/sounds/trillian/message-inbound.wav"))
             ((= prio 3) (sauron-fx-festival msg))
             ((= prio 4) (sauron-fx-festival msg))
;             ((= prio 4) (sauron-fx-sox "/usr/share/sounds/trillian/message-inbound.wav"))
             ((= prio 5)
              (sauron-fx-sox "/usr/share/sounds/trillian/generic-event.wav")
              (sauron-fx-gnome-osd(format "%S: %s" origin msg) 5)))))

(defun sauron-fx-festival (msg)
  "Say MSG with festival."
  (festival-say msg))

(sauron-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google-contacts

;; (require 'google-contacts)
;; (require 'google-contacts-gnus)
;; (require 'google-contacts-message)
;; (add-hook 'completion-at-point-functions 'google-contacts-message-complete-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e

(load-file "~/.emacs.d/lisp/conf/mu4e-conf.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cscope

(require 'xcscope)


(festival-say "emacs initialized")
