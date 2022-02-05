;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GC

;; https://github.com/hlissner/doom-emacs/issues/310
(setq gc-cons-threshold (* 1024 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages

(load-file "~/.emacs.d/lisp/conf/packages-conf.el")

(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; https://github.com/joaotavora/yasnippet/issues/521
(setq inhibit-default-init t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;·keysyms

;; http://stackoverflow.com/questions/10867199/emacs-in-terminal-meta-arrow-keybindings
(add-hook 'tty-setup-hook
          '(lambda ()
             (define-key input-decode-map "\e\e[A" [(meta up)])
             (define-key input-decode-map "\e\e[B" [(meta down)])
             (define-key input-decode-map "\e\e[C" [(meta right)])
             (define-key input-decode-map "\e\e[D" [(meta left)])
             (define-key input-decode-map "\e[1;5A" [(control up)])
             (define-key input-decode-map "\e[1;5B" [(control down)])
             (define-key input-decode-map "\e[1;5C" [(control right)])
             (define-key input-decode-map "\e[1;5D" [(control left)])))

;; ;; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (expand-file-name "~/.emacs.d/lisp/elpa/auto-complete-20140322.32/dict"))
(setq ac-comphist-file (expand-file-name
                        "~/.emacs.d/ac-comphist.dat"))

;; (global-auto-complete-mode t)

;; (ac-config-default)
;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              ;; ac-omni-completion-sources is made buffer local so
;;              ;; you need to add it to a mode hook to activate on
;;              ;; whatever buffer you want to use it with.  This
;;              ;; example uses C mode (as you probably surmised).

;;              ;; auto-complete.el expects ac-omni-completion-sources to be
;;              ;; a list of cons cells where each cell's car is a regex
;;              ;; that describes the syntactical bits you want AutoComplete
;;              ;; to be aware of. The cdr of each cell is the source that will
;;              ;; supply the completion data.  The following tells autocomplete
;;              ;; to begin completion when you type in a . or a ->

;;              (add-to-list 'ac-omni-completion-sources
;;                           (cons "\\." '(ac-source-semantic)))
;;              (add-to-list 'ac-omni-completion-sources
;;                           (cons "->" '(ac-source-semantic)))

;;              ;; ac-sources was also made buffer local in new versions of
;;              ;; autocomplete.  In my case, I want AutoComplete to use
;;              ;; semantic and yasnippet (order matters, if reversed snippets
;;              ;; will appear before semantic tag completions).

;;              (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
;;              ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function-args
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom lisp
;; emacs or package lisp files which I have modified
;; (possibly awaiting patch acceptance)

;(load-file "~/.emacs.d/lisp/custom-lisp/emms-lastfm-scrobbler.el")
;(load-file "~/.emacs.d/lisp/custom-lisp/emms-get-lyrics.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private
;; passwords and the such

(load-file "~/.emacs.d/lisp/conf/private.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc functions

(load-file "~/.emacs.d/lisp/conf/misc_funcs.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings

(add-to-list 'term-file-aliases '("foot" . "xterm"))

(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

(setq completion-ignore-case t                 ;; ignore case when completing...
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

(setq-default fill-column 80)

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

(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

(setq sentence-end-double-space nil)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq search-upper-case t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
;;
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the minibuffer
;;
(setq
  enable-recursive-minibuffers t
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

(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

(require 'undo-tree)
(global-undo-tree-mode)

(require 'hippie-exp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some misc other packages

;; multi-term
;; (when (require 'multi-term nil 'noerror)
;;  (setq multi-term-program "/bin/zsh"))

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/lisp/auto-install")
;;(auto-install-update-emacswiki-package-name t)

;; fullscreen
(require 'fullscreen)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'unbound)

(require 'misc)
(global-set-key "\M-z" 'zap-up-to-char)


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

(global-set-key (kbd "<f25>") 'browse-url-firefox)

(global-set-key (kbd "C-x C-S-e") 'eval-and-replace)

;; program shortcuts
(global-set-key (kbd "C-c E") ;; .emacs
                (lambda()
                  (interactive)
                  (find-file "~/.emacs")
                  (emacs-lisp-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global editor settings

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(autoload 'linum-mode "linum" "mode for line numbers" t)

(require 'whitespace)
(setq whitespace-style
        '(face tabs spaces newline space-mark tab-mark newline-mark indentation space-after-tab space-before-tab trailing))
(global-whitespace-mode)

(global-display-line-numbers-mode)

(setq backup-inhibited t)
(setq auto-save-default nil)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(setq-default python-indent-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cscope

(require 'xcscope)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(require 'cc-mode)

(setq c-basic-offset 4
      comment-start "//"
      comment-end   ""
      fill-column 80)
(defun my-c-mode-hook ()
  (c-set-offset 'statement-block-intro '+)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'substatement-label '0)
  (c-set-offset 'label '0)
  (c-set-offset 'statement-cont '+)
  (c-set-offset 'access-label '/)
  (c-set-offset 'arglist-intro '+))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUDA

(setq auto-mode-alist (append '(("/*.\.cu$" . c++-mode)) auto-mode-alist))
; (setq cscope-indexer-suffixes (cons "*.cu" cscope-indexer-suffixes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(require 'haskell-mode)
(require 'inf-haskell)
;; (load "~/.emacs.d/haskellmode-emacs/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
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

(setq-default TeX-master "main")
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))


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
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "\\(" "\\)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode

(load-file "~/.emacs.d/lisp/conf/org-conf.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no-word

(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))


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
;; smartparens

(require 'smartparens-config)
(show-smartparens-global-mode)
(setq sp-show-pair-delay 1)

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

;; (defadvice show-paren-function
;;   (after show-matching-paren-offscreen activate)
;;   "If the matching paren is offscreen, show the matching line in the
;;     echo area. Has no effect if the character before point is not of
;;     the syntax class ')'."
;;   (interactive)
;;   (let* ((cb (char-before (point)))
;;          (matching-text (and cb
;;                              (char-equal (char-syntax cb) ?\) )
;;                              (blink-matching-open))))
;;     (when matching-text (message matching-text))))


(require 's)
;; (defadvice sp-show--pair-function
;;   (after sp-show--pair-function-offscreen activate)
;;   "If the matching paren is offscreen, show the matching line in the
;;           echo area."
;;   (interactive)
;;   (let ((vis-buf (save-excursion
;;            (cons (progn (move-to-window-line 0) (point))
;;              (progn (move-to-window-line -1) (line-end-position)))))
;;     (matching-sexp (if (and (sp-get (sp-get-sexp nil) :beg)
;;                 (= (point) (sp-get (sp-get-sexp nil) :beg)))
;;                (cons (sp-get (sp-get-sexp nil) :beg)
;;                  (sp-get (sp-get-sexp nil) :end))
;;              (if (and (sp-get (sp-get-sexp t) :end)
;;                   (= (point) (sp-get (sp-get-sexp t) :end)))
;;                  (cons (sp-get (sp-get-sexp t) :beg)
;;                    (sp-get (sp-get-sexp t) :end))
;;                nil))))
;;     (when matching-sexp
;;       (if (> (car vis-buf)
;;          (car matching-sexp))
;;       ;; opening delim is offscreen
;;       (message "Matches %s"
;;            (s-trim
;;             (save-excursion (goto-char (car matching-sexp))
;;                     (thing-at-point 'line))))
;;     (if (< (cdr vis-buf)
;;            (cdr matching-sexp))
;;         ;; closing delim is offscreen
;;         (message "Matches %s"
;;              (s-trim
;;               (save-excursion (goto-char (cdr matching-sexp))
;;                       (thing-at-point 'line)))))))))

;; (defun goto-match-paren (arg)
;;   "Go to the matching  if on (){}[], similar to vi style of % "
;;   (interactive "p")
;;   ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
;;   (cond ((looking-at (sp--get-opening-regexp)) (sp-forward-sexp))
;;         ((looking-back (sp--get-closing-regexp) 1) (sp-backward-sexp))
;;         ;; now, try to succeed from inside of a bracket
;;         ((looking-at (sp--get-closing-regexp)) (forward-char) (sp-backward-sexp))
;;         ((looking-back (sp--get-opening-regexp) 1) (backward-char) (sp-forward-sexp))
;;         ;; if we're inside a pair, go to the beginning of the pair
;;         ((sp-get-enclosing-sexp)
;;          (progn
;;              (push-mark)
;;              (while (not (looking-at (sp--get-opening-regexp)))
;;                (backward-char 1)
;;                (cond ((looking-at (sp--get-closing-regexp))
;;                       (message "->> )")
;;                       (forward-char 1)
;;                       (sp-backward-sexp)
;;                       (backward-char 1))))))
;;         (t nil)))

;; (global-set-key (kbd "<3270_PA1>") 'goto-match-paren)
;; (global-set-key (kbd "C-<menu>") 'goto-match-paren)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc funcitons

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

;; TODO: broken with nativecomp
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(require 'slime)

(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations
           '((sbcl ("/usr/bin/sbcl"))
             (ecl ("/usr/bin/ecl"))
             (clisp ("/usr/bin/clisp"))))
     (slime-setup '(slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-indentation
                    ;; slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     ))

(setq common-lisp-hyperspec-root "/usr/share/doc/HyperSpec/")
(global-set-key (kbd "<f5>") 'hyperspec-lookup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC

(load-file "~/.emacs.d/lisp/conf/erc_conf.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes

(setq custom-theme-directory "~/.emacs.d/lisp/themes")
(load-theme 'nicolasavru-dark t)

(set-face-font 'default "DejaVu Sans Mono:size=16:antialias=true:hinting=false:hintstyle=none:autohint=false:rgba=none")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial frames

(make-frame-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window/Frame Manager

(load-file "~/.emacs.d/lisp/conf/three-windows.el")
; https://www.emacswiki.org/emacs/TransposeFrame
(require 'transpose-frame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git

(require 'magit)
(autoload 'magit-status "magit" nil t)
(global-set-key "\C-xg" 'magit-status)

;; (require 'git-gutter-fringe)
;; (global-git-gutter-mode 1)
;; (add-hook 'ruby-mode-hook 'git-gutter-mode)
;; (setq git-gutter:verbosity 2) ; don't be that chatty

;; (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)
;; ;; Jump to next/previous hunk
;; (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; ;; Stage current hunk
;; (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; ;; Revert current hunk
;; (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)


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
        (matlab-shell-mode . matlab-shell-tab) ;; see update below
        (rust-mode . completion-at-point)
        ))


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
;; currently broken in minibuffer:
;; Making completion list...
;; let*: Symbol’s value as variable is void: frame

;; (load "~/.emacs.d/lisp/fixed-point-completion/fixed-point-completion.el")
;; (enable-fixed-point-completions)


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
;; (mapc 'yas/load-directory yas/root-directory)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m

(require 'w3m)

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
;; ocaml

(setq auto-mode-alist (cons '("\\.ml[iylp]?\\'" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure

(require 'clojure-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript

(setq js-indent-level 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google-contacts

;; (require 'google-contacts)
;; (require 'google-contacts-gnus)
;; (require 'google-contacts-message)
;; (add-hook 'completion-at-point-functions 'google-contacts-message-complete-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e

(load-file "~/.emacs.d/lisp/conf/mu4e-conf.el")


;; (festival-say "emacs initialized")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-function

(which-function-mode 0)

(setq mode-line-misc-info (delete (assoc 'which-function-mode
                                      mode-line-misc-info) mode-line-misc-info)
      which-func-header-line-format '(which-function-mode ("" which-func-format)))

(defadvice which-func-ff-hook (after header-line activate)
  (when which-function-mode
    (setq mode-line-misc-info (delete (assoc 'which-function-mode
                                          mode-line-misc-info) mode-line-misc-info)
          header-line-format which-func-header-line-format)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PKGBUILD

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP
; (setq tramp-use-ssh-controlmaster-options nil)
; (setq server-use-tcp t
;       server-port    9999)
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
 '(help-at-pt-display-when-idle (quote (flymake-diagnostic)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
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
 '(org-agenda-files nil)
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-directory "~/Dropbox/org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-mobile-inbox-for-pull "~/Dropbox/org/todo.org")
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    ((116 "* TODO %?
  %u" "~/Dropbox/org/todo.org" "Tasks")
     (110 "* %u %?" "~/Dropbox/org/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired

(load-file "~/.emacs.d/lisp/conf/dired-conf.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loccur

(require 'loccur)
;; ;; defines shortcut for loccur of the current word
;; (define-key global-map [(control o)] 'loccur-current)
;; ;; defines shortcut for the interactive loccur command
;; (define-key global-map [(control meta o)] 'loccur)
;; ;; defines shortcut for the loccur of the previously found word
;; (define-key global-map [(control shift o)] 'loccur-previous-match)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mentor

(require 'mentor)

(defconst mentor-rpc-volatile-d-methods
  '("d.local_id" ;; must not be removed
    "d.base_path"        "d.bytes_done"
    "d.directory"        "d.down.rate"
    "d.hashing"          "d.hashing_failed"
    "d.message"
    "d.priority"         "d.chunk_size"
    "d.up.rate"          "d.up.total"
    "d.state"            "d.views"
    "d.is_active"        "d.is_hash_checked"
    "d.is_hash_checking" "d.is_open"
    "d.is_pex_active"
    ;; "cat=\"$t.multicall=d.hash=,t.url=,cat=#,t.type=,cat=#,t.is_enabled=,cat=#,t.group=,cat=#,t.scrape_complete=,cat=#,t.scrape_incomplete=,cat=#,t.scrape_downloaded=,cat=#\""
    ))

(defconst mentor-rpc-t-methods
  '("t.url"
    "t.type" "t.is_enabled"
    "t.group" "t.scrape_complete"
    "t.scrape_incomplete" "t.scrape_downloaded"))



(defun mentor-download-tracker-name-column (&optional download)
  (let* ((t_urls (mentor-item-property 't_url download))
         (t_is_enableds (mentor-item-property 't_is_enabled download))
         (active-trackers
          (cl-mapcar (lambda (url is_enabled) (when is_enabled url))
                     t_urls t_is_enableds))
         (main-tracker (if active-trackers (car active-trackers) ""))
         (shortened (mentor-remove-subdomains
                     (mentor-keep-domain-name main-tracker))))
    (if (>= (length shortened) 20)
        (seq-subseq shortened -20)
      (format "%20s" shortened))))

(defun mentor-rpc-methods-to-properties (methods)
  (mapcar
   (lambda (method)
     (intern
      (replace-regexp-in-string
       "^\\([tp]\\)\\." "\\1_"
       (replace-regexp-in-string "^[df]\\.\\(get_\\)?\\|=$" "" method))))
   methods))

(defun mentor-join-t-methods (methods)
  (when methods
    (concat "cat=\"$t.multicall=d.hash=,"
            (mapconcat (lambda (m) (concat m "=,cat=#")) methods ",")
            "\"")))

(defun mentor-download-data-update-all ()
  (message "Updating torrent data...")
  (condition-case _err
      (progn
        (mentor-rpc-d.multicall mentor-rpc-volatile-d-methods mentor-rpc-t-methods)
        (message "Updating torrent data...DONE"))
    (mentor-need-init
     (mentor-download-data-init))))

(defun mentor-download-data-init ()
  "Initialize torrent data from rtorrent.

All torrent information will be re-fetched, making this an
expensive operation."
  (message "Initializing torrent data...")
  (mentor-rpc-d.multicall mentor-rpc-d-methods mentor-rpc-t-methods t)
  (mentor-views-update-views)
  (message "Initializing torrent data... DONE"))

(defun mentor-rpc-d.multicall (d-methods t-methods &optional is-init)
  "Call `d.multicall2' with METHODS.

  Optional argument IS-INIT if this is initializing."
  (let* ((d-methods=
          (mapcar (lambda (m) (concat m "=")) d-methods))
         (t-methods= (list (mentor-join-t-methods t-methods)))
         (all-methods (append d-methods= t-methods=))
         (list-of-values (apply 'mentor-rpc-command "d.multicall2"
                                "" mentor-current-view all-methods)))
    ;; (print list-of-values (create-file-buffer "mtmp2"))
    (mentor-view-torrent-list-clear)
    (let ((d-properties (mentor-rpc-methods-to-properties d-methods))
          (t-properties (mentor-rpc-methods-to-properties t-methods)))
      (dolist (values list-of-values)
        (mentor-download-update-from d-properties t-properties values is-init)))))

(defun mentor-download-update-from (d-methods t-methods values &optional is-init)
  (let ((result ())
        (d-values (butlast values))
        (t-values (butlast (split-string (car (last values)) "#")))
        (t-methods-len (length t-methods))
        (t-accum ()))
    (cl-mapc (lambda (m v) (push (cons m v) result)) d-methods d-values)
    ;; (print t-accum (create-file-buffer "mtmp5"))
    ;; (print t-values (create-file-buffer "mtmp7"))
    (while t-values
      (push (cl-subseq t-values 0 t-methods-len) t-accum)
      (setq t-values (nthcdr t-methods-len t-values))
      ;; (message "il: %s; %s" t-accum t-values)
      )
    ;; (print t-accum (create-file-buffer "mtmp6"))
    (setq t-accum (nreverse t-accum))
    ;; (message "tac: %s" t-accum)
    (setq t-accum (apply #'cl-mapcar #'list t-accum))
    ;; (print t-accum (create-file-buffer "mtmp8"))
    (cl-mapc (lambda (m v) (push (cons m v) result)) t-methods t-accum)
    (mentor-download-update
     (mentor-download-create result)
      is-init)))

(setq mentor-view-columns
      '(((mentor-download-state-column) -2 "State" mentor-download-state)
        ((mentor-download-speed-up-column) -5 "Up" mentor-download-speed-up)
        ((mentor-download-speed-down-column) -5 "Down" mentor-download-speed-down)
        ((mentor-download-progress-column) -3 "Cmp" mentor-download-progress)
        ((mentor-download-size-column) -4 "Size" mentor-download-size)
        (name -100 "Name" mentor-download-name)
        ((mentor-download-tracker-name-column) -20 "Tracker" mentor-tracker-name)
        (message -40 "Message" mentor-download-message)
        (directory -255 "Directory")))

;; ssh -X wuzzy@wuzzy -L 5001:/var/rtorrent/.scgi_loca
(setq mentor-rtorrent-external-rpc "scgi://127.0.0.1:5000")

(defun mentor ()
  "Start mentor or switch to mentor buffer."
  (interactive)
  (if (get-buffer "*mentor*")
      ;; Assume that it's set up correctly if it exists
      (switch-to-buffer (get-buffer-create "*mentor*"))
    ;; Otherwise create and set it up
    (switch-to-buffer (get-buffer-create "*mentor*"))
    (mentor-mode)
    (mentor-setup-rtorrent)
    ;; (setq mentor-item-update-this-fun 'mentor-download-update-this)
    (setq mentor-item-update-this-fun 'mentor-download-update-and-reinsert-at-point)
    (setq mentor-set-priority-fun 'mentor-download-set-priority-fun)
    (setq mentor--columns-var  'mentor-view-columns)
    (setq mentor-sort-list '((up.rate . t) name))
    (setq mentor-rtorrent-client-version (mentor-rpc-command "system.client_version")
          mentor-rtorrent-library-version (mentor-rpc-command "system.library_version")
          mentor-rtorrent-name (mentor-rpc-command "session.name"))
    ;; (let* ((pwd-with-trailing-newline (mentor-rpc-command "execute.capture" "" "pwd"))
    ;;        (pwd (substring pwd-with-trailing-newline 0 -1)))
    ;;   (cd (file-name-as-directory pwd)))
    (mentor-set-view mentor-default-view)
    (when (equal mentor-current-view mentor-last-used-view)
      (setq mentor-last-used-view (mentor-get-custom-view-name 2)))
    (mentor-download-data-init)
    (mentor-views-init)
    (mentor-redisplay)
    (mentor-init-header-line)
    (goto-char (point-min))))

;; (defun url-scgi (url callback cbargs)
;;   "Handle SCGI URLs from internal Emacs functions."
;;   (cl-check-type url url "Need a pre-parsed URL.")
;;   (declare (special url-scgi-connection-opened
;;                     url-callback-function
;;                     url-callback-arguments
;;                     url-current-object))

;;   (let* ((host (url-host url))
;;          (port (url-port url))
;;          (filename (url-filename url))
;;          (is-local-socket (string-match "^/." filename))
;;          (bufname (format " *scgi %s*" (if is-local-socket
;;                                            filename
;;                                          (format "%s:%d" host port))))
;;          (buffer (generate-new-buffer bufname))
;;          (connection (cond
;;                       (is-local-socket
;;                        (let ((filename (url-scgi-handle-home-dir filename)))
;;                         (make-network-process :name "scgi"
;;                                               :buffer buffer
;;                                               :remote filename)))
;;                       (t ; scgi over tcp
;;                        (url-open-stream host buffer host port)))))
;;     (if (not connection)
;;         ;; Failed to open the connection for some reason
;;         (progn
;;           (kill-buffer buffer)
;;           (setq buffer nil)
;;           (error "Could not create connection to %s:%d" host port))
;;       (with-current-buffer buffer
;;         (setq url-current-object url
;;               mode-line-format "%b [%s]")

;;         (dolist (var '(url-scgi-connection-opened
;;                        url-callback-function
;;                        url-callback-arguments))
;;           (set (make-local-variable var) nil))

;;         (setq url-callback-function callback
;;               url-callback-arguments cbargs
;;               url-scgi-connection-opened nil)

;;         (pcase (process-status connection)
;;           (`connect
;;            ;; Asynchronous connection
;;            (set-process-sentinel connection 'url-scgi-async-sentinel))
;;           (`failed
;;            ;; Asynchronous connection failed
;;            (error "Could not create connection to %s:%d" host port))
;;           (_
;;            (setq url-scgi-connection-opened t)
;;            (process-send-string connection (url-scgi-create-request))))))
;;     buffer))

;; TODO: do this with a bulk rpc and parse it
(defun mentor-download-update-this ()
  (let* ((tor (mentor-get-item-at-point))
         (hash (mentor-item-property 'hash tor))
         (methods (append mentor-rpc-volatile-d-methods mentor-rpc-t-methods))
         (values (cl-mapcar
                  (lambda (method)
                    (mentor-rpc-command method hash))
                  methods)))
    (let ((d-properties (mentor-rpc-methods-to-properties methods))
          (t-properties (mentor-rpc-methods-to-properties t-methods)))
      (mentor-download-update-from d-properties t-properties values))
    (mentor-redisplay-torrent)
    (mentor-goto-item-name-column)))

;; TODO: make compatible with emacs which don't have display-linue-numbers.
(defun mentor-reload-header-line ()
  (setq mentor--header-line
        (concat
         (make-string (1+ (line-number-display-width)) ?\s)
         (mentor-process-view-header-columns (eval mentor--columns-var)))))

;; TODO: file details... open dir in dired

;; TODO: define mentor-sort-by-property-promt to sort by Cmp, might be inspired
;; by sunrise commander sort


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust

(setq rust-indent-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help

(setq help-window-select t)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ansible Managed
