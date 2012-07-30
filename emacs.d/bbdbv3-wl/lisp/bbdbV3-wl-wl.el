;;; bbdbV3-wl-wl.el --- 
;; 
;; Filename: bbdbV3-wl-wl.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: dom feb 19 18:08:59 2012 (-0300)
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
;; This file add functions to work with Wanderlust. 
;; For example, getting data from wanderlust's buffers, adding keymaps or hooks, etc.
;; 
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


(require 'wl)
(require 'bbdb)

(require 'bbdbV3-wl-bbdb)

(defun bbdb-wl-get-sender-data ()
  "Get all the data located in the header about the sender."
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^From:[[:blank:]]*" nil t) 
    (search-forward-regexp "[^[:blank:]].*$" nil t)
    (match-string-no-properties 0)
    )
  )

(defun bbdb-wl-get-sender-name ()
  "Return the sender name(just the name)"
  ;; Search for sender mail...
  (let ((usrmail (bbdb-wl-get-sender-data)))
    ;; Extract real name
    (wl-address-header-extract-realname usrmail)
    )
  )

(defun bbdb-wl-get-sender-address ()
  "Return the sender petname(just that)"
  (let ((usrmail (bbdb-wl-get-sender-data)))
    (wl-address-header-extract-address usrmail)
    )
  )

(defun bbdb-wl-query-create-sender ()
  "Query the user if we can create the sender. If we can, create the sender with its data.
If we cannot... just ignore this mail."
  (when (y-or-n-p (format "Record %s doesn't exists. Create it?" (bbdb-wl-get-sender-name)))
    ;; Answers "y"... create it.
    (bbdb-wl-create-sender)
    )	       
  )

(defun bbdb-wl-create-sender ()
  "Create the sender BBDB record and add it to BBDB."
  ;; Avoid duplication!
  ;;(let ((bbdb-no-duplicates t))
    (if
	(bbdb-create-internal (bbdb-wl-get-sender-name) ;; name
			      nil ;; affix
			      nil ;; aka
			      nil ;; organizations
			      (cons (bbdb-wl-get-sender-address) nil);; mail
			      nil ;; phones
			      nil ;; addresses 
			      nil ;; notes
			      )
	(message "%s" "Record Added: Done")
      (message "%s" "Record not added... Possibly it is already in the database")
      )
	
  ;;  )
  )

(defun bbdb-wl-get-update-record ()
  "Function ideally of `wl-message-redisplay-hook'.
Find all data from the sender and reciever(the 'get' part) and query to update if necessary(the 'update' part)."  
  (interactive)

  ;; Find names!
  (let ((sender-name (bbdb-wl-get-sender-name)))
    ;; is it in BBDB?    
    (setq records (bbdb-wl-find-and-show sender-name))
    (if records
	;; yes, exists...	
	;; should we update it?
	(bbdb-wl-check-update record)
      ;;Nop, doesn't exists...
      (progn
	(bbdb-wl-query-create-sender)
	(bbdb-wl-find-and-show sender-name)
	)
      )
    )
  )

(defun bbdb-wl-check-update (record)
  "Check if this BBDB record and the information in the mail are different.
If they are, query the user if she/he want to update the BBDB record.
If she/he wants, update it.
If not, well, do nothing!"
  ;; TODO
  )



(provide 'bbdbV3-wl-wl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-wl-wl.el ends here
