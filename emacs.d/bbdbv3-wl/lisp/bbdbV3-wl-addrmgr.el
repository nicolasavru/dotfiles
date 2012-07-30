;;; bbdbV3-wl-addrmgr.el --- 
;; 
;; Filename: bbdbV3-wl-addrmgr.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: dom feb 19 18:14:52 2012 (-0300)
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
;; This file adds more functionallity to Wandelust's address manager.
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

(defconst bbdb-wl-addrmgr-temp-buffer " *bbdb-wl-addrmgr-tmp*"
  "Name of the temporary buffer for this module.")

(defun bbdb-wl-insert-into-file (entries &optional save-after-insert)
  "Insert all the new entries in the `wl-address-file'. This insertion is temporary until you call `bbdb-wl-addrmgr-save' or you pass a t value into the \"save-after-insert\" parameter.

Each entry, as usual, is a list composed by three elements in this order: the email address, the petname and the realname.

Remember that the parameter \"entries\" is a list of entries.

If no-init is set to t, then don't call `wl-address-init'. This is useful when you want to save multiple entries faster: you don't initialize
the address list every time you insert each entry!

If save-after-insert is t, then save the buffer after inserting each entry. Also, you can use `bbdb-wl-addrmgr-save' to save the file."

  ;; Part of this code was extracted from wl-address.el, a file that comes with Wanderlust.
  (let ((output-coding-system
	 (mime-charset-to-coding-system wl-mime-charset)))
    (with-current-buffer (get-buffer-create bbdb-wl-addrmgr-temp-buffer)
      (if (file-exists-p wl-address-file)
	  (insert-file-contents wl-address-file))
      (dolist (entry entries)
	(insert (format "%s\t%s\t%s\n"
			(nth 0 entry) ;; address
			(prin1-to-string (nth 1 entry)) ;; petname
			(prin1-to-string (nth 2 entry)) ;; realname
			)
		)
	)

      (when save-after-insert
	(bbdb-wl-addrmgr-save)
	(wl-address-init)
	)
      )	      
    )
  )

(defun bbdb-wl-addrmgr-save ()
  "Save the temporary buffer that contains the contents of the .address file.

This file is the container of all information of Wanderlust's Address Manager. 
If you inserted some entry with the function `bbdb-wl-insert-into-file' you can save what you inserted with this function.
"

  (let ((buff (get-buffer bbdb-wl-addrmgr-temp-buffer)))
    (when buff
      (with-current-buffer buff
  	(write-region (point-min) (point-max)
		      wl-address-file nil 'no-msg)
	)
      )
    )
  )


(defvar bbdb-wl-addrmgr-test-email-var nil
  "This variable is used internally. 
You need to set this variable to use `bbdb-wl-addrmgr-test-email' function.
This email will be compared with the entry parameter in that function.")

(defun bbdb-wl-addrmgr-test-email (entry)
  "This is an internal function for use with `find-if'.
This function test if the email in the entry is equal to the email stored in `bbdb-wl-addrmgr-test-email-var'.
Please, set that variable first using let to give the email to compare with."
  (string-equal (car entry) bbdb-wl-addrmgr-test-email-var)
  )

(defun bbdb-wl-addrmgr-search-email (email)
  "Return the position where is the email given in the variable `wl-address-list'.

I need the `bbdb-wl-addrmgr-test-mail' function."
  (let ((bbdb-wl-addrmgr-test-email-var email))
    (position-if 'bbdb-wl-addrmgr-test-email wl-address-list)
    )
  )

(provide 'bbdbV3-wl-addrmgr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-wl-addrmgr.el ends here
