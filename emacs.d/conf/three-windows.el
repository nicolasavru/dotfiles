;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ThreeWindows
; http://www.emacswiki.org/emacs/ThreeWindows

;split-window-4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             window layout related               ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+

(defun split-window-4()
  "Splite window into 4 sub-window"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn (split-window-vertically)
             (split-window-horizontally)
             (other-window 2)
             (split-window-horizontally)
             (other-window 2)
             )
    )
  )


;split-window-3

;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +                       +----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+


(defun split-window-3()
  "Splite window into 3 sub-window"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn (split-window-horizontally)
             (other-window 1)
             (split-window-vertically)
             (other-window 2)
             )
    )
  )


;split-v

;  +----------------------+                 +----------- +-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------------------+   +-------+/    |            |           |
;  |                      |           /     |            |           |
;  |                      |                 |            |           |
;  +----------------------+                 +----------- +-----------+

(defun split-v ()
  (interactive)
  (if (= 2 (length (window-list)))
      (let (( thisBuf (window-buffer))
            ( nextBuf (progn (other-window 1) (buffer-name))))
        (progn   (delete-other-windows)
                 (split-window-horizontally)
                 (set-window-buffer nil thisBuf)
                 (set-window-buffer (next-window) nextBuf)
                 ))
    )
  )


;split-h

;  +----------- +-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            |           |    +-------+/    +----------------------+
;  |            |           |            /     |                      |
;  |            |           |                  |                      |
;  +----------- +-----------+                  +----------------------+

(defun split-h ()
  (interactive)
  (if (= 2 (length (window-list)))
      (let (( thisBuf (window-buffer))
            ( nextBuf (progn (other-window 1) (buffer-name))))
        (progn   (delete-other-windows)
                 (split-window-vertically)
                 (set-window-buffer nil thisBuf)
                 (set-window-buffer (next-window) nextBuf)
                 ))
    )
  )


;split-v-3

;  +----------------------+                 +----------- +-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------------------+   +-------+/    |            |-----------|
;  |          |           |           /     |            |           |
;  |          |           |                 |            |           |
;  +----------------------+                 +----------- +-----------+


(defun split-v-3 ()
  "Change 3 window style from horizontal to vertical"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stBuf (window-buffer (car winList)))
              (2ndBuf (window-buffer (car (cdr winList))))
              (3rdBuf (window-buffer (car (cdr (cdr winList))))))
          (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
          (delete-other-windows)
          (split-window-horizontally)
          (set-window-buffer nil 1stBuf)
          (other-window 1)
          (set-window-buffer nil 2ndBuf)
          (split-window-vertically)
          (set-window-buffer (next-window) 3rdBuf)
          (select-window (get-largest-window))
          )
        )
    )
  )


;split-h-3

;  +----------- +-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            |-----------|    +-------+/    +----------------------+
;  |            |           |            /     |           |          |
;  |            |           |                  |           |          |
;  +----------- +-----------+                  +----------------------+


(defun split-h-3 ()
  "Change 3 window style from vertical to horizontal"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stBuf (window-buffer (car winList)))
              (2ndBuf (window-buffer (car (cdr winList))))
              (3rdBuf (window-buffer (car (cdr (cdr winList))))))
          (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
          (delete-other-windows)
          (split-window-vertically)
          (set-window-buffer nil 1stBuf)
          (other-window 1)
          (set-window-buffer nil 2ndBuf)
          (split-window-horizontally)
          (set-window-buffer (next-window) 3rdBuf)
          (select-window (get-largest-window))
          )
        )
    )
  )


;roll-v-3

;  +----------- +-----------+                    +----------- +-----------+
;  |            |     C     |            \       |            |     A     |
;  |            |           |    +-------+\      |            |           |
;  |     A      |-----------|    +-------+/      |     B      |-----------|
;  |            |     B     |            /       |            |     C     |
;  |            |           |                    |            |           |
;  +----------- +-----------+                    +----------- +-----------+
;
;  +------------------------+                     +------------------------+
;  |           A            |           \         |           B            |
;  |                        |   +-------+\        |                        |
;  +------------------------+   +-------+/        +------------------------+
;  |     B     |     C      |           /         |     C     |     A      |
;  |           |            |                     |           |            |
;  +------------------------+                     +------------------------+


(defun roll-v-3 ()
  "Rolling 3 window buffers clockwise"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stWin (car winList))
              (2ndWin (car (cdr winList)))
              (3rdWin (car (cdr (cdr winList)))))
          (let ((1stBuf (window-buffer 1stWin))
                (2ndBuf (window-buffer 2ndWin))
                (3rdBuf (window-buffer 3rdWin))
                )
            (set-window-buffer 1stWin 3rdBuf)
            (set-window-buffer 2ndWin 1stBuf)
            (set-window-buffer 3rdWin 2ndBuf)
            )
          )
        )
    )
  )


;change-split-type-2

;‘split-v’ and ‘split-h’ may be merged into one function, that automatically detects split type and changes vertical to horizontal or vice-versa, like this:

;;  +----------------------+                +---------- +----------+
;;  |                      |          \     |           |          |
;;  |                      |  +-------+\    |           |          |
;;  +----------------------+  +-------+/    |           |          |
;;  |                      |          /     |           |          |
;;  |                      |                |           |          |
;;  +----------------------+                +---------- +----------+
;;
;;  +--------- +-----------+                +----------------------+
;;  |          |           |          \     |                      |
;;  |          |           |  +-------+\    |                      |
;;  |          |           |  +-------+/    +----------------------+
;;  |          |           |          /     |                      |
;;  |          |           |                |                      |
;;  +--------- +-----------+                +----------------------+

(defun change-split-type-2 ()
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive)
  (if (= 2 (length (window-list)))
      (let ((thisBuf (window-buffer))
            (nextBuf (progn (other-window 1) (buffer-name)))
            (split-type (if (window-full-width-p)
                            'split-window-horizontally
                          'split-window-vertically)))
        (progn
          (delete-other-windows)
          (funcall split-type)
          (set-window-buffer nil thisBuf)
          (set-window-buffer (next-window) nextBuf)))))


;change-split-type-3

;‘split-v-3’ and ‘split-h-3’ may also be merged into one function, that automatically detects split type and changes vertical to horizontal or vice-versa, like this:

;  +----------------------+                 +----------- +-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------------------+   +-------+/    |            |-----------|
;  |          |           |           /     |            |           |
;  |          |           |                 |            |           |
;  +----------------------+                 +----------- +-----------+

;  +----------- +-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            |-----------|    +-------+/    +----------------------+
;  |            |           |            /     |           |          |
;  |            |           |                  |           |          |
;  +----------- +-----------+                  +----------------------+

(defun change-split-type-3 ()
  "Change 3 window style from horizontal to vertical and vice-versa"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stBuf (window-buffer (car winList)))
              (2ndBuf (window-buffer (car (cdr winList))))
              (3rdBuf (window-buffer (car (cdr (cdr winList)))))
              (split-3
               (lambda(1stBuf 2ndBuf 3rdBuf split-1 split-2)
                 "change 3 window from horizontal to vertical and vice-versa"
                 (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
                 (delete-other-windows)
                 (funcall split-1)
                 (set-window-buffer nil 1stBuf)
                 (other-window 1)
                 (set-window-buffer nil 2ndBuf)
                 (funcall split-2)
                 (set-window-buffer (next-window) 3rdBuf)
                 (select-window (get-largest-window))
                 ))
              (split-type-1 nil)
              (split-type-2 nil)
              )
          (if (window-full-width-p)
              (setq split-type-1 'split-window-horizontally split-type-2 'split-window-vertically)
            (setq split-type-1 'split-window-vertically  split-type-2 'split-window-horizontally))
          (funcall split-3 1stBuf 2ndBuf 3rdBuf split-type-1 split-type-2)
          ))))

;change-split-type

;'change-split-type-2' and 'change-split-type-3' can be further merged into one function that automatically detects split type and changes vertical to horizontal or vice-versa, like this:

(defun change-split-type ()
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive)
  (if (= 2 (length (window-list)))
      (change-split-type-2)
    (change-split-type-3)
    ))


(global-set-key (kbd "C-x C-4") 'split-window-4)
(global-set-key (kbd "C-x C-3") 'split-window-3)
(global-set-key (kbd "C-x C-SPC") 'change-split-type)
(global-set-key (kbd "C-x C-j") 'roll-v-3)
