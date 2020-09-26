(deftheme nicolasavru-dark
  "Dark Emacs by Nicolas Avrutin (nicolasavru@gmail.com)
   Based on theme by Suvayu Ali:
   http://suvayu.wordpress.com/2010/10/11/color-theme-dark-emacs/
  ")

(custom-theme-set-variables
 'nicolasavru-dark
 '(org-todo-keyword-faces (quote (("IMP" :background "gold" :foreground "indianred3" :weight bold) ("CNCL" :background "snow3" :foreground "black" :weight bold)))))

(custom-theme-set-faces
 'nicolasavru-dark
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:weight bold :foreground "salmon"))))
 '(info-menu-header ((t (:family "Sans Serif" :foreground "tomato" :weight bold))))
 '(info-node ((t (:foreground "gold" :slant italic :weight bold))))
 '(info-xref ((t (:foreground "powder blue" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(woman-bold ((t (:weight bold :foreground "forest green"))))
 '(woman-italic ((t (:slant italic :foreground "salmon"))))
 '(cursor ((t (:background "red"))))

 ;; mode-line
 '(mode-line-buffer-id ((t (:weight bold :foreground "red"))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "slate gray" :background "#202020"))))
 '(mode-line-inactive ((t (:box (:line-width -1 :color nil :style released-button) :foreground "slate gray" :background "#404040"))))
 '(which-func ((t (:foreground "dark orange"))))

 ;; font-lock
 '(font-lock-warning-face ((t (:weight bold :foreground "Red1" :inherit (bold)))))
 '(font-lock-variable-name-face ((t (:foreground "orange" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "IndianRed3" :inherit (font-lock-builtin-face)))))
 '(font-lock-type-face ((t (:foreground "orchid" ))))
 '(font-lock-keyword-face ((t (:foreground "firebrick1" ))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "white" :inherit (default)))))
 '(font-lock-constant-face ((t (:foreground "LightGoldenrod1" :inherit (default)))))
 '(font-lock-string-face ((t (:foreground "lawn green" :inherit (default)))))
 '(font-lock-comment-face ((t (:foreground "dark cyan" :inherit (default)))))
 '(font-lock-builtin-face ((t (:foreground "goldenrod" :inherit (default)))))

 ;; show-paren
 '(show-paren-mismatch ((t (:foreground "white" :background "magenta"))))
 '(show-paren-match ((t (:background "SlateBlue1"))))

 ;; rainbow-delimeters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "magenta" :inherit (default)))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep sky blue" :inherit (default)))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "tomato" :inherit (default)))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "goldenrod" :inherit (default)))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "purple" :inherit (default)))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "green" :inherit (default)))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "gold" :inherit (default)))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "SpringGreen3" :inherit (default)))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark orange" :inherit (default)))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :inherit (default)))))

 ;; whitespace-mode
 '(whitespace-space ((t (:foreground "ivory4" :background "gray7"))))
 '(whitespace-newline ((t (:foreground "ivory4" :background "gray7"))))

 ;; erc
 '(erc-prompt-face ((t (:weight bold :background "#003500"))))
 '(erc-current-nick-face ((t (:weight bold :foreground "brown"))))

 ;; highlighing
 '(highlight-indent-face ((t (:foreground "ghost white" :background "gray9"))))
 '(highlight ((t (:background "#000040"))))
 '(region ((t (:background "#000060"))))
 '(secondary-selection ((t (:background "#000060"))))
 
 '(underline ((((supports :underline t)) (:underline t :foreground "green" :inherit (default)))))
 '(bold ((t (:weight bold :inherit (default)))))
 '(bold-italic ((t (:weight bold :inherit (italic)))))
 '(italic ((((supports :slant italic)) (:slant italic :foreground "blanched almond" :inherit (default)))))
 '(default ((t (:family "DejaVu Sans Mono" :foreground "ivory3" :background "black"))))

 ;; sunrise
 '(sr-directory-face ((t (:inherit default))))
 '(sr-symlink-face ((t (:foreground "cyan"))))
 '(sr-symlink-directory-face ((t (:foreground "cyan"))))
 '(sr-tabs-active-face ((t (:inherit variable-pitch :bold t :background "steel blue" :height 0.9))))
 '(sr-tabs-active-face ((t (:inherit variable-pitch :bold t :background "steel blue" :height 0.9))))
 '(sr-tabs-inactive-face ((t (:inherit variable-pitch :bold t :background "medium blue" :height 0.9))))
 '(sr-active-path-face ((t (:background "black" :foreground "yellow" :bold t :height 120))))
 '(sr-passive-path-face ((t (:background "black" :foreground "lightgray" :bold t :height 120))))
 '(dired-filetype-document ((t (:foreground "tomato3"))))

 '(matrix-client-date-header ((t (:foreground "tomato3"))))
 )

(provide-theme 'nicolasavru-dark)
