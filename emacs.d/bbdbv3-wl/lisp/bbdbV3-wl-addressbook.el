;;; bbdbV3-addressbook.el --- 
;; 
;; Filename: bbdbV3-addressbook.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: sáb feb 18 02:20:59 2012 (-0300)
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
;; Functions for work with the integration of the wanderlast's addressbook 
;; and BBDB Version 3.
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

(require 'wl-addrmgr)

(require 'bbdbV3-wl-addrmgr)
(require 'bbdbV3-wl-common)
(require 'bbdbV3-wl-syncbuffer)

(defun bbdb-wl-sinchronize-addressbook ()
  "Sinchronize BBDB with Wanderlust's Addressbook."
  (interactive)
  ;; We need that the address buffer has been visited...
  (let ((buff (get-buffer "Address")))
    (when buff      
      ;; it has been visited...
      (with-current-buffer buff
	;; For each item in the addressbook add it into the BBDB(if don't exists)
	(dolist (person wl-addrmgr-list)
	  (unless (bbdb-wl-find-name (nth 2 person))
	    ;; It doesn't exists... create it
	    (message "%s %s %s %s" "Adding into BBDB: " (nth 0 person) (nth 1 person) (nth 2 person))
	    (bbdb-create-internal (nth 2 person) ;; name
				  nil ;; affix
				  (cons (nth 1 person) nil) ;; aka
				  nil ;; organizations
				  (cons (nth 0 person) nil);; mail
				  nil ;; phones
				  nil ;; addresses 
				  nil ;; notes
				  )
	    )
	  )
	)
      )
    )
  )


					; ****************************************
					; Getting information from the Addressbook

(defun bbdb-wl-get-records-names (records &optional separator count)
  "Return a string with all the names taken from the records.

Use separator for divide between names, if it is not given or is nil then use \"\n\".

If count is t then put a number before the name."
  (let ((aux "")
	(num 0)
	(sep (if separator
		 separator
	       "\n")))
    (dolist (i records)
      (setq num (+ 1 num))
      (setq aux (concat 
		 aux
		 ;; Number first
		 (when count
		   (number-to-string num)) 
		 ": "
		 ;; Name in the record
		 (bbdb-record-name i)
		 ;; Separator
		 sep))
      )
    aux
    )  
  )


(defun bbdb-wl-ask-what-to-update (records addrbook-entry)
  "Ask the user what record tu update with the addrbook-entry.
Show the data to the user so he will understand which record to choice and show the options available.

Return the position number of the selected record or nil if user doesn't selected one. "
  (let ((msg
	 (concat "I have found two candidate to update, both with simmilar names.
Wich one I have to update?
What that will be updated:\n"
		 "Email: " (nth 0 addrbook-entry)
		 " - Petname: " (nth 1 addrbook-entry)
		 "\nThe candidates are\n"
		 (bbdb-wl-get-records-names records "\n" t)
		 "\nPlease, select one by its number."))
	(selected 0))
    (while (or (> selected (length records))
	       (< selected 1))
      (setq selected (read-number msg ))		      
      )
    selected
    )   
  )

(defun bbdb-wl-update-if-necessary (addrbook-entry)
  "Insert or update this addrbook-entry into BBDB if it is not there.
If the addrbook-entry is not in BBDB, insert the name, aka(petname) and mail.

Checks emails and aka
If the addrbook-entry is in the BBDB but hasn't that email insert it
If the addrbook-entry is in the BBDB but hasn't that aka insert it

If already exists all that information, do nothing."
  ;; Look for the contacts, if exists check the info.
  (let ((records (bbdb-wl-find (nth 2 addrbook-entry))))
    (cond ((eq (length records) 1) ;; Exists and is the only one contact 
	   (progn
	     (bbdb-wl-syncbuffer-show "One matching record founded... updating info.")
	     (bbdb-wl-update-record-with-addrbook (car records) addrbook-entry))
	   )
	   
	  
	  ((eq (length records) 0) ;; Doesn't exists.. create it
	  (progn
	    (bbdb-wl-syncbuffer-show "Record doesnt exists... creating it!")
	    (bbdb-wl-insert-if-not-exists (cons
					   (cons 'mails (nth 0 addrbook-entry))
					   (cons 'aka (nth 1 addrbook-entry))
					   (cons 'name (nth 2 addrbook-entry))
					   nil)))
	  )

	  ((> (length records) 1) ;; There are more records to update!!! Ask user!
	   (progn 
	     (bbdb-wl-syncbuffer-show "There are various matches for record... asking user.")
	     (let ((selected (- 
			      (bbdb-wl-ask-what-to-update records addrbook-entry)
			      1)))
	       (when selected
		 ;; User selected one... update it!
		 (let ((record-selected (nth selected records)))
		   (bbdb-wl-syncbuffer-show "User selects " 
					    (bbdb-record-name record-selected))
		   (bbdb-wl-update-record-with-addrbook record-selected
							addrbook-entry)))))
	   )

	  )
    )
  )






(defun bbdb-wl-take-data-from-addressbook ()
  "Sinchronize in one way: from the addressbook to the BBDB."
  (interactive)

  (bbdb-wl-syncbuffer-init)
  (bbdb-wl-syncbuffer)
  (message "Starting Syncrhonization from the Wanderlust's Addressbook...
Please wait...")  
  (bbdb-wl-syncbuffer-show "Starting Syncrhonization from the Wanderlust's Addressbook...")
  
  ;; Get the list of people
  (let ((lst (wl-addrmgr-local-list t)))
    ;; process each element adding to the BBDB if necessary
    (dolist (i lst)

      (bbdb-wl-syncbuffer-addrmgr-taking i)

      (bbdb-wl-update-if-necessary i)
      )
    )

  (message "Synchronization done. Check *Sync buffer* to see results.")
  (bbdb-wl-syncbuffer-show "Synchronization done.")
  )


					; ****************************************
					; Giving information to the addressbook

(defun bbdb-wl-give-data-to-addressbook ()
  "Sinchronize in one way: from the BBDB to the addressbook."
  (interactive)
  (let ((list (bbdb-records)))
    ;; For each element save info
    (dolist (elt list)
      (let ((name (bbdb-record-name elt))
	    (petname (bbdb-record-aka elt)) ;; Take only one petname!
	    (lst-emails (bbdb-record-mail elt)) ;; Take all emails!
	    )
	;; For each email address save info...
	(dolist (email lst-emails)
	  ;; Check if already exists
	  (unless (bbdb-wl-addrmgr-search-email email)
	    ;; It doesn't exists... Add it!
	    (bbdb-wl-insert-into-file (list email petname name))	  
	    )
	  )
	)
      )
    )
  )

;; `bbdb-wl-update-if-necessary' cannot be used here: it goes from Addrmgr to BBDB!
(defun bbdb-wl-update-addrmgr-if-necessary (new-addrmgr-entry)
  "Try to look into the address manager if the email entry exists:
* If it doesn't => add it!
* If it exists and it differs => Ask to update!
* If it exists and doesn't differs => do nothing!

new-addrmgr-entry is a typical address manager entry(is a list with three elements: *email*, *petname*, *realname*)."
  (let ((entry-pos (bbdb-wl-addrmgr-search-email (car new-addrmgr-entry)))
	(new-petname (nth 1 new-addrmgr-entry))
	(new-name (nth 2 new-addrmgr-entry))
	)
    (let ((entry (nth entry-pos wl-address-list)))
      ;; If exists then compare if differs; else add it!
      (if entry
	  (unless (and (string-equal (nth 1 entry) new-petname) 
		       (string-equal (nth 2 entry) new-name))
	    ;; Exists but is different! Ask if I have to update!
	    (let ((update-entry (bbdb-wl-ask-to-update-addrmgr-entry new-addrmgr-entry entry)))
	      (if (and update-entry (listp update-entry)) 
		  (bbdb-wl-edit-in-file entry-pos update-entry)	;; User request to update something
		(bbdb-wl-insert-into-file new-entry) ;; User requested to insert new-entry as new
		)
	      )
	    )
	(bbdb-wl-insert-into-file new-addrmgr-entry) ;; it doesn't exists: add it!
	)
      )
    )
  )

(defun bbdb-wl-ask-to-update-addrmgr-entry (new-entry old-entry)
  "Ask the user if the old-entry must be updated with the new entry or just add a new one.

This return the following depending on the user's choice:

* nil if the user has pressed any other key.
* t if it has to be added as a new one.
* An entry if it has to be updated. This entry is how the user want's to update the old one."
  
  ;; Ask if he wants to add it as new or update.  
  (let ((msg (concat 
	      "I found a contact with the same email.\n
Field - New Entry - Old Entry"
	      "\nEmail: " (car new-entry) " - " (car old-entry)
	      "\nPetname: " (nth 1 new-entry) " - " (nth 1 old-entry)
	      "\nName: " (nth 2 new-entry) " - " (nth 2 old-entry)
"\n\nWhat should I do? 
SPACE: add it as new.
p: update the old petname only.
n: update the old name only.
a: update the old petname and name.
other key: skip."))
	)
    (let ((choice (read-key msg)))
      (cond ((= choice 32)
	     t)
	    ((= choice ?a)
	     new-entry)
	    ((= choice ?p)
	     (list (car old-entry) (nth 1 new-entry) (nth 2 old-entry)))
	    ((= choice ?n)	     
	     (list (car old-entry) (nth 1 old-entry) (nth 2 new-entry)))     
	    )
      )
    )
  )

(provide 'bbdbV3-wl-addressbook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-addressbook.el ends here
