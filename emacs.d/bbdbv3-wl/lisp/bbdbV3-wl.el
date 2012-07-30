;;; bbdbV3-wl.el --- 
;; 
;; Filename: bbdbV3-wl.el
;; Description: 
;; Author: Christian Nelson Giménez
;; Maintainer: 
;; Created: vie oct  7 11:28:06 2011 (-0300)
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
;; I decide to make a bbdb-wl interface from scratch.
;; This decition cames because BBDB V3.x changes a lot from his last. 
;; Almost everything for e-mails was implemented, and every function 
;; has changed their names or even does not exist anymore.
;;
;; So, there's another version for this BBDB and Wanderlust 2011.
;; 
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


;; This is from bbdb-wl for BBDB V2.x
(eval-and-compile
  (add-hook 'wl-message-redisplay-hook 'bbdb-wl-get-update-record)
;;   (add-hook 'wl-summary-exit-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-message-window-deleted-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-exit-hook 'bbdb-wl-exit)
;;   (add-hook 'wl-save-hook '(bbdb-save t))
;;   (add-hook 'wl-summary-toggle-disp-off-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-summary-toggle-disp-folder-on-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-summary-toggle-disp-folder-off-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-summary-toggle-disp-folder-message-resumed-hook
;;             'bbdb-wl-show-bbdb-buffer)
;;   (add-hook 'wl-summary-mode-hook
;;             (function
;;              (lambda ()
;;                (define-key (current-local-map) ":" 'bbdb-wl-show-sender)
;;                (define-key (current-local-map) ";" 'bbdb-wl-edit-notes))))
;;   (add-hook 'wl-summary-exit-hook 'bbdb-flush-all-caches)
;;   (add-hook 'wl-summary-exec-hook 'bbdb-flush-all-caches)
;;   (add-hook 'wl-mail-setup-hook
;;             (function
;;              (lambda ()
;; ;;;            (local-set-key "\M-\t" 'bbdb-complete-name)
;;                (define-key (current-local-map) "\M-\t" 'bbdb-complete-name))))
;;;  (define-key mime-view-mode-default-map ":" 'bbdb-wl-get-update-record)
  )

(require 'bbdb)
(require 'bbdb-com)
(require 'wl)

(require 'bbdbV3-wl-common)
(require 'bbdbV3-wl-wl)


(provide 'bbdbV3-wl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-wl.el ends here
