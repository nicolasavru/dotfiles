;;; -*- lexical-binding: t; -*-

(defun tat/execute-async (command command-name &optional callbacks)
  "Execute the async shell command.
    command: the command to execute
    command-name: just the name for the output buffer
    handler-function: the function for handling process
    arguments: the arguments for passing into handler-function
    handler function must have one argument, that is the process of the the async task

    Create a new window at the bottom, execute the command and print
    the output to that window. After finish execution, print the message to that
    window and close it after x seconds"
  (let ((window-before-execute (selected-window))
        (output-buffer
         (concat "*" command-name "*" " at " (current-time-string))))

    ;; make a new window
    (select-window (tat/create-window))
    ;; not allow popup
    (add-to-list 'same-window-buffer-names output-buffer)
    ;; run async command
    (async-shell-command command output-buffer)
    ;; set event handler for the async process
    (set-process-sentinel
     (get-buffer-process output-buffer)
     (lambda (process event)
       (when (equal (process-status process) 'exit)
         (dolist (callback callbacks)
           (funcall callback))
         (tat/close-window-handler process event))))
    ;; add the new async buffer to the buffer list
    (add-to-list 'tat/buffers-list output-buffer)
    ;; switch the the previous window
    (select-window window-before-execute)))

(defun sr-rsync (dest)
  "Asynchronously copy file using Rsync for dired.
   This function runs only on Unix-based system.
   Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat tda/rsync-command-name " " tda/rsync-arguments " "))
    ;; add all selected file names as arguments to the rsync command
    (setq command (concat command " " (mapconcat 'shell-quote-argument files " ") " "))
    ;; append the destination to the rsync command
    (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async
     command "rsync"
     (list (lambda ()
             (let ((names (mapcar #'file-name-nondirectory files))
                   (inhibit-read-only t))
               (sr-in-other
                (progn
                  (revert-buffer)
                  (when (memq major-mode '(sr-mode sr-virtual-mode))
                    (dired-mark-remembered
                     (mapcar (lambda (x) (cons (expand-file-name x) ?C)) names))
                    (sr-focus-filename (car names)))))))))))

(defun sr-unrar ()
  "Asynchronously decompress the zip file at point"
  (interactive)

  (let (command
        (file (dired-get-filename 'verbatim)))

    ;; the unzip command
    (setq command (concat "unrar x "))
    ;; append the file name
    (setq command
          (concat command
                  (shell-quote-argument file) " "))

    ;; execute the command asynchronously
    (tat/execute-async
     command "unrar"
     (list (lambda ()
             (let ((inhibit-read-only t))
             ; TODO: mark extracted files, possibly via buffer diff or via
             ; separate unrar list command
               (progn
                 (sr-select-window (sr-this))
                 (revert-buffer))))))))


(defun sr-mv (dest)
  "Asynchronously mv file using Rsync for dired.
   This function runs only on Unix-based system.
   Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Move to:" (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat "mv " "-v" " "))
    ;; add all selected file names as arguments to the rsync command
    (setq command (concat command " " (mapconcat 'shell-quote-argument files " ") " "))
    ;; append the destination to the rsync command
    (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async
     command "mv"
     (list (lambda ()
             (let ((names (mapcar #'file-name-nondirectory files))
                   (inhibit-read-only t))
               (sr-select-window (sr-this))
               (revert-buffer)
               (sr-in-other
                (progn
                  (revert-buffer)
                  (when (memq major-mode '(sr-mode sr-virtual-mode))
                    (dired-mark-remembered
                     (mapcar (lambda (x) (cons (expand-file-name x) ?R)) names))
                    (sr-focus-filename (car names)))))))))))

(defun sr-du ()
  "Asynchronously mv file using Rsync for dired.
   This function runs only on Unix-based system.
   Usage: same as normal dired copy function."
  (interactive)
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat "du -hsx "))
    ;; add all selected file names as arguments to the rsync command
    (setq command (concat command " " (mapconcat 'shell-quote-argument files " ") " "))
    ;; execute the command asynchronously
    (tat/execute-async
     command "du")))
