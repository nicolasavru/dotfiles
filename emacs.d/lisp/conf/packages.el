;; https://github.com/hans/dotfiles/blob/master/emacs.d/packages.el
(require 'package)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(apache-mode async auctex caml
                clojure-mode color-theme dash doc-mode google-contacts
                haskell-mode magit markdown-mode oauth2 scala-mode2 org php-mode rainbow-delimiters s
                sauron slime slime-clj smart-tab tuareg w3m yasnippet)
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))
