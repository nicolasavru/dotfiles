;; https://github.com/hans/dotfiles/blob/master/emacs.d/packages.el
(setq package-user-dir "~/.emacs.d/lisp/elpa")

(require 'package)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ;; ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
                         ))

(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun package--save-selected-packages (&rest opt) nil)

(setq package-selected-packages
      '(anaphora
        apache-mode
        async
        auctex
        auto-compile
        auto-complete-auctex
        auto-complete-c-headers
        auto-complete-chunk
        bind-key
        caml
        cargo
        clojure-mode
        color-theme
        company
        dash
        dash-functional
        dired-du
        dired-filetype-face
        dired-hacks-utils
        dired-isearch
        dired-launch
        dired-narrow
        dired-rainbow
        dired-ranger
        dired-sidebar
        dired-subtree
        diredfl
        doc-mode
        eglot
        elisp-refs
        elpy
        emms-player-mpv
        emms
        epl
        esxml
        f
        find-file-in-project
        flycheck
        flycheck-pycheckers
        flycheck-rust
        flymake
        frame-purpose
        fringe-helper
        fsm
        function-args
        ghub
        git-commit
        git-gutter
        git-gutter-fringe
        google-contacts
        haskell-mode
        helm
        helpful
        ht
        htmlize
        indicators
        ivy
        jabber
        julia-mode
        let-alist
        loccur
        lsp-mode
        lsp-python
        lsp-rust
        lsp-ui
        ly
        magit
        magit-popup
        markdown-mode
        matrix-client
        mentor
        mu4e-maildirs-extension
        ;mu4e-views
        nlinum
        oauth2
        offlineimap
        org-board
        org-plus-contrib
        php-mode
        pkg-info
        popup
        projectile
        py
        pyvenv
        quelpa
        quelpa-use-package
        rainbow-delimiters
        rainbow-identifiers
        request
        ;; racer?
        rust-mode
        rustfmt
        rustic
        s
        sauron
        scala-mode2
        shut-up
        smart-tab
        smartparens
        smartscan
        spinner
        srv
        sunrise-commander
        sunrise-x-buttons
        sunrise-x-checkpoints
        sunrise-x-loop
        sunrise-x-modeline
        sunrise-x-popviewer
        sunrise-x-tabs
        sunrise-x-tree
        swiper
        systemd
        toml-mode
        tracking
        transient
        treepy
        tuareg
        use-package
        visible-mark
        w3m
        with-editor
        xcscope
        xterm-color
        yaml-mode
        yasnippet
        yasnippet-snippets
        zotelo))

(package-install-selected-packages)

;;(package-initialize)

;(require 'quelpa-use-package)
;(use-package matrix-client
;  :quelpa ((matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
;                          :files (:defaults "logo.png" "matrix-client-standalone.el.sh"))))
