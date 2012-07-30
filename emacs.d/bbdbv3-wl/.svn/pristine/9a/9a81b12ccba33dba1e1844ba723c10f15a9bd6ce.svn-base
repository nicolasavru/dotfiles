;;; bbdbV3-wl-syncbuffer.el --- 
;; 
;; Filename: bbdbV3-wl-syncbuffer.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: lun feb 20 11:41:41 2012 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Functions to use with the Synchronization buffer.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defconst bbdb-wl-syncbuffer-name "*BBDBV3-Wl Sync Buffer*"
  "Name of the synchronization buffer.")

(defvar bbdb-wl-syncbuffer-buffer nil
  "Variable that contains the buffer object for synchronization buffer.")

(defvar bbdb-wl-syncbuffer-amount-taken 0
  "How mutch items has been taken for udpating/inserting/doing something?")

(defun bbdb-wl-syncbuffer ()
  "Show Synchronization buffer."
  (interactive)
  (bbdb-wl-syncbuffer-create-buffer)
  (switch-to-buffer-other-window bbdb-wl-syncbuffer-buffer)
  )

(defun bbdb-wl-syncbuffer-init ()
  "Init everything needed for using syncbuffer."
  (bbdb-wl-syncbuffer-create-buffer t)
  (setq bbdb-wl-syncbuffer-amount-taken 0)
  )

(defun bbdb-wl-syncbuffer-create-buffer (&optional empty)
  "Create Synchronization buffer if necessary.
If empty is true, restart buffer deleting contents."
  (setq bbdb-wl-syncbuffer-buffer (get-buffer-create bbdb-wl-syncbuffer-name))
  (when empty
    (with-current-buffer bbdb-wl-syncbuffer-buffer
      (delete-region (point-min) (point-max))
      )
    )
  )
  
(defun bbdb-wl-syncbuffer-show (&rest texts)
  "Write text into the synchronization buffer."
  (bbdb-wl-syncbuffer-create-buffer)
  (with-current-buffer bbdb-wl-syncbuffer-buffer
    (insert "\n")
    (dolist (i texts)
      (insert i " ")
      )
    (insert "\n")
    )
  )

(defun bbdb-wl-syncbuffer-addrmgr-taking (addrmgr-elt)
  "Show the message \"-> Taking address manager item ...\""
  (bbdb-wl-syncbuffer-create-buffer)
  (with-current-buffer bbdb-wl-syncbuffer-buffer
    (insert "\n-> *Taking* address manager item: Name: "
	    (nth 2 addrmgr-elt)
	    " - Petname: "
	    (nth 1 addrmgr-elt)
	    " - Mail: "
	    (nth 0 addrmgr-elt)
	    "\n")
    )
  (setq bbdb-wl-syncbuffer-amount-taken 
	(+ 1 bbdb-wl-syncbuffer-amount-taken))
	 
  )

(defun bbdb-wl-syncbuffer-ending ()
  "Write the ending message."
  (insert "\n*Summary*:\nAmount of addresses taken from Wanderlust's Address manager:"
	  bbdb-wl-syncbuffer-amount-taken
	  "\n")
  )


(provide 'bbdbV3-wl-syncbuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-wl-syncbuffer.el ends here
