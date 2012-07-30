;;; bbdb3-wl-common.el --- 
;; 
;; Filename: bbdb3-wl-common.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: sáb feb 18 02:38:01 2012 (-0300)
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
;; Common functions for manipulating bbdb, and addrbook together.
;; Also add more functionallity to bbdb and addrbook by requiring two 
;; libraries: bbdbV3-wl-bbdb and bbdbV3-wl-addrmgr.
;;
;; Here you can find those functions that need both, BBDB and AddrMgr.
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

(require 'bbdbV3-wl-bbdb)
(require 'bbdbV3-wl-addrmgr)


(defun bbdb-wl-update-record-with-addrbook (record addrbook-entry)
  "Update the information from the addrbook-entry to the record.
Update aka and mail information checking if they are not there already.

This will be saved temporally in the BBDB. For saving to the database files use `bbdb-save'.
"
  (let ((aka (nth 1 addrbook-entry))
	(mail (nth 0 addrbook-entry)))
    ;; The record exists(and is only one!), update data...
    (unless (member aka (bbdb-record-aka record))
      ;; The petname is not in the record! add it...
      (bbdb-record-set-aka record 
			   (cons aka (bbdb-record-aka record)))
      
      )
    (unless (member mail (bbdb-record-mail record))
      ;; The mail is not there! add it...
      (bbdb-record-set-mail record
			    (cons mail (bbdb-record-mail record)))
      )    
    record
    )
  )
   

(provide 'bbdbV3-wl-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb3-wl-common.el ends here
