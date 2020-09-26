(require 'dired)
(require 'sunrise-commander)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings

(setq dired-auto-revert-buffer t)
(setq dired-listing-switches "-alhF")
(setq sr-listing-switches "-alhF")
(setq sr-virtual-listing-switches "-alhFd")

;; Some color customizations. The rest are in nicolasavru-dark-theme.el.
(require 'dired-filetype-face)
(deffiletype-face "directory" "DarkCyan")
(deffiletype-face-regexp directory :type-for-docstring "directory"
  :regexp "^  d")
(deffiletype-setup "directory" "directory")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Launcher

; Useful reference: https://github.com/thomp/dired-launch
(setq dired-launch-default-launcher '("xdg-open"))
(dired-launch-enable)
(setf dired-launch-extensions-map
      (list
       '("txt" (("emacs" find-file-other-frame)))))
(defun sr-find-regular-file (filename &optional wildcards)
  "Overwrite sr-find-regular-file to use dired-launch."
  (dired-launch-command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
(define-key sr-mode-map "\C-e"        'move-end-of-line)
(define-key sr-mode-map [backspace]   'sr-dired-prev-subdir)
(define-key sr-mode-map "\C-x\C-f"    'sr-find-file)

(setq dired-directory-face 'default
      dired-perm-write-face 'default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations


(load-file "~/.emacs.d/lisp/tmtxt-async-tasks/tmtxt-async-tasks.el")
(require 'tmtxt-async-tasks)
(load-file "~/.emacs.d/lisp/tmtxt-dired-async/tmtxt-dired-async.el")
(require 'tmtxt-dired-async)


(defun sr-setup-windows ()
  "Set up the Sunrise window configuration (two windows in `sr-mode')."
  (run-hooks 'sr-init-hook)
  ;;get rid of all windows except one (not any of the panes!)
  (sr-select-viewer-window)
  (delete-other-windows)
  (if (buffer-live-p other-window-scroll-buffer)
      (switch-to-buffer other-window-scroll-buffer)
    (sr-switch-to-nonpane-buffer))

  ;;now create the viewer window
  (unless (and sr-panes-height (< sr-panes-height (frame-height)))
    (setq sr-panes-height (sr-get-panes-size)))
  (if (and (<= sr-panes-height (* 2 window-min-height))
           (eq sr-window-split-style 'vertical))
      (setq sr-panes-height (* 2 window-min-height)))
  (split-window (selected-window))

  (case sr-window-split-style
    (horizontal (split-window-horizontally))
    (vertical   (split-window-vertically))
    (top        (ignore))
    (t (error "Unrecognised `sr-window-split-style' value: %s"
              sr-window-split-style)))

  (sr-setup-visible-panes)

  ;;select the correct window
  (sr-select-window sr-selected-window)
  (sr-restore-panes-width)
  (run-hooks 'sr-start-hook))


(defun sunrise-start ()
  "Invokes SC the Guido's way ;-)"
  (interactive)
  (unless sr-running
    (sunrise-cd)
    (sr-select-viewer-window)
    (delete-window)
    ;; (sr-lock-panes 'max)
    ))


;; (defun sr-tabs-add ()
;;   "Redefine sr-tabs-add to strip (Sunrise) off of the default name.

;;   Assign the current buffer to exactly one tab in the active pane.
;;   If a tab for the current buffer already exists, invoke `sr-tabs-rename'."
;;   (interactive)
;;   (let* ((buf-name (buffer-name))
;;          (tab-name
;;           (save-match-data
;;             (if (string-match "\\(.+\\) \(Sunrise\)$" buf-name)
;;                 (match-string 1 buf-name)
;;               buf-name)))
;;          (tab-set (assq sr-selected-window sr-tabs)))
;;       (if (member tab-name (cdr tab-set))
;;           (call-interactively 'sr-tabs-rename)
;;         (setcdr tab-set (cons tab-name (cdr tab-set)))))
;;   (sr-tabs-refresh))

;; (defun sr-tabs-add ()
;;   "Assign the current buffer to exactly one tab in the active pane.
;; If a tab for the current buffer already exists, invoke `sr-tabs-rename'."
;;   (interactive)
;;   (let ((tab-name (buffer-name))
;;         (tab-set (assq sr-selected-window sr-tabs)))
;;       (if (member tab-name (cdr tab-set))
;;           (call-interactively 'sr-tabs-rename)
;;         (setcdr tab-set (cons tab-name (cdr tab-set)))))

(defun my-sr-tabs-add ()
  "Redefine sr-tabs-add to strip (Sunrise) off of the default name.

  Assign the current buffer to exactly one tab in the active pane.
  If a tab for the current buffer already exists, invoke `sr-tabs-rename'."
  (interactive)
  (let ((buf-name (buffer-name)))
    (sr-tabs-add)
    (save-match-data
      (if (string-match "\\(.+\\) \(Sunrise\)$" buf-name)
          (sr-tabs-rename (match-string 1 buf-name))))))

(define-key sr-tabs-mode-map (kbd "C-j") 'my-sr-tabs-add)

(defun tda/verify (dest)
  ""
  ;; (interactive)
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Verify to:" (dired-dwim-target-directory)))))
  (let* ((files (sr-get-marked-files))
         (this-files (mapcar (lambda (s) (concat sr-this-directory s)) files))
         (other-files (mapcar (lambda (s) (concat sr-other-directory s)) files))
         command
         command1
         command2)
    (setq command
          (concat "md5deep" " " "-rbe" " "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file this-files)
      (setq command1 (concat command (shell-quote-argument file) " ")))
    (dolist (file other-files)
      (setq command2 (concat command (shell-quote-argument file) " ")))
    ;; append the destination to the rsync command
    ;; (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async command1 "md5deep")))

(load-file "~/.emacs.d/lisp/sunrise-async.el")
(define-key sr-mode-map (kbd "C") 'sr-rsync)
(define-key sr-mode-map (kbd "R") 'sr-mv)

;; sunrise-commander.el adds sr-lock-window to window-size-change-functions, but
;; that breaks the popup output buffer window from tmtxt-async-tasks, so clear
;; the hook. I haven't yet seen a case that this breaks.
(setq window-size-change-functions nil)
