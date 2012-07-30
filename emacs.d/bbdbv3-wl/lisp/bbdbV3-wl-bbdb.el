;;; bbdbV3-wl-bbdb.el --- 
;; 
;; Filename: bbdbV3-wl-bbdb.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: dom feb 19 18:11:41 2012 (-0300)
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
;; This file acts as interface between BBDBV3-WL and BBDB itself. 
;; So, add more functionallity to BBDB so we can work comfortable.
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

(require 'bbdb)

(defun bbdb-wl-insert-if-not-exists (data-list &optional update)
  "Insert the data into BBDB. 

If no-update is t then don't update if there is another record with the same name.

The data is a alist of elements:
name        => A String
affix       
aka         => A list of Strings
org         => A list of Strings
mail        => A comma separated list or a list of strings
phones      => A list of vectors with the form [\"label\" areacode prefix suffix extension-or-nil]
addresses   => A list of vectors with the form [\"label\" (\"line1\" \"line2\" ...) \"City\" \"State\" \"Postcode\" \"Country\"]
notes       => A list of associating symbols with strings

If name already exists, it only updates the information."

  (let ((record (bbdb-wl-find-name (cdr (assoc 'name data-list)))))
    (if (and record update)
	(bbdbl-wl-update-record record data-list) 	;; Record founded... update it.
      (bbdb-create-internal (cdr (assoc 'name data-list))      ;; name
			    (cdr (assoc 'affix data-list))     ;; affix
			    (cdr (assoc 'aka data-list))       ;; akas
			    (cdr (assoc 'org data-list))       ;; organizations
			    (cdr (assoc 'mail data-list))      ;; mails
			    (cdr (assoc 'phones data-list))    ;; phones
			    (cdr (assoc 'addresses data-list)) ;; addresses
			    (cdr (assoc 'notes data-list))     ;; notes
			    )
      )
    )
  )


(defun bbdb-wl-return-list (string-or-list)
  "Return a list no matter if the parameter is string or list.
If is a String return a list containing only the string.
If is a list just return it."
  (if (stringp string-or-list)
      (list string-or-list)
    string-or-list)
  )

(defun bbdb-wl-update-record (record data-list)
  "Update the information from the data-list to the record.

\"Update\" in this case means replace all!

Data list is an alist like this:
name        => A String
affix       
aka         => A list of Strings
org         => A list of Strings
mail        => A comma separated list or a list of strings
phones      => A list of vectors with the form [\"label\" areacode prefix suffix extension-or-nil]
addresses   => A list of vectors with the form [\"label\" (\"line1\" \"line2\" ...) \"City\" \"State\" \"Postcode\" \"Country\"]
notes       => A list of associating symbols with strings"
  (let ((name (cdr (assoc 'name data-list)))      ;; name
	(affix (cdr (assoc 'affix data-list)))    ;; affix
	(aka (cdr (assoc 'aka data-list)))       ;; akas	     
	(org (cdr (assoc 'org data-list)))       ;; organizations
	(mail (cdr (assoc 'mail data-list)))      ;; mails
	(phones (cdr (assoc 'phones data-list)))    ;; phones
	(addresses (cdr (assoc 'addresses data-list))) ;; addresses
	(notes (cdr (assoc 'notes data-list))))     ;; notes
    (let ((name (bbdb-wl-return-list name))
	  (affix (bbdb-wl-return-list affix))
	  (aka (bbdb-wl-return-list aka))
	  (org (bbdb-wl-return-list org))
	  (mail (bbdb-wl-return-list mail))
	  (phones (bbdb-wl-return-list phones))
	  (addresses (bbdb-wl-return-list addresses))
	  (notes (bbdb-wl-return-list notes))
	  )
		     
      (when name ;; there is a name
	(bbdb-record-set-name name))
      (when affix
	(bbdb-record-set-affix affix))
      (when aka
	(bbdb-record-set-aka aka))
      (when mail
	(bbdb-record-set-mail mail))
      (when phones 
	(bbdb-record-set-phone phones))
      (when addresses
	(bbdb-record-set-address addresses))
      (when notes
	(bbdb-record-set-notes notes))  
      )
    )
  )

(defun bbdb-wl-find-name (name)
  "Find this name in the BBDB. 
If it does exists return the record.
If nothing founds, return nil."
  (bbdb-search (bbdb-records) name)
  )

(defun bbdb-wl-find-and-show (name)
  "Look for the name in the BBDB and show it! 
Return the records of the BBDB found.
If no records are found, just return nil.

If name is an empty string, to avoid showing all the people in BBDB this function will return nil."
  (bbdb-wl-find name t)
  )

(defun bbdb-wl-find (name &optional show)
  "Look for the name in the BBDB.

If show is t then show it!
 
Return the records of the BBDB found.
If no records are found, just return nil.

If name is an empty string, to avoid showing all the people in BBDB this function will return nil."
  (if (string= name "")      
      (progn
	(bbdb-display-records nil) ;; Display nothing, erase last display.
	nil
	)
    (progn
      (let ((records (bbdb-search (bbdb-records) name)))
	(when show
	  (bbdb-display-records records)
	  )
	records
	)
      )
    )
  )




(provide 'bbdbV3-wl-bbdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-wl-bbdb.el ends here
