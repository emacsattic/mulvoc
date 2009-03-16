;;; csv.el --- Functions for reading and parsing CSV files.

;;  Copyright (C) 2001-2004 by Ulf Jasper

;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   csv.el
;;  Created:    August 19 2001
;;  Keywords:   util
;;  CVS:        $Id$
;;  Time-stamp: "6. November 2004, 17:06:05 (ulf)"

(defconst csv-version 1.2 "Version number of csv.el.")

;; ======================================================================

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; ======================================================================

;;; Commentary:

;;  Main routine is `csv-parse-buffer' which takes a buffer containing a
;;  CSV (Comma Separated Value) file and converts its contents into a list
;;  of alists.  The first line of the CSV file is interpreted as the list of
;;  keys.  For example:

;;  Key1,Key 2,"Key3"
;;  Value1a,Value1b,"Value1c"
;;  Value2a,Value2b,"Very long Value
;;  2c"

;;  gets translated into

;;  ((("Key1" "Value1a") ("Key 2" "Value1b") ("Key3" "Value1c"))
;;   (("Key1" "Value2a") ("Key 2" "Value2b") ("Key3" "Very long Value
;;    2c")))

;;  The function `csv-insert-contents' demonstrates how to use
;;  `csv-parse-buffer'.

;; ======================================================================

;;; History:

;;  1.2 Introduced csv-(un)?quoted-entry-regexp, as suggested by John
;;      Sturdy.
;;      Tested on Emacs 21.3.50.

;;  1.1 First.first version.
;;      automatically add missing entries for lines that are too short.
;;      Tested on Emacs 20.7.1 and XEmacs 21.1.12.


;;  1.0 First version

;; ======================================================================

;;; Code:

(defvar csv-quoted-entry-regexp
  "\\(^\\|,\\)\"\\(\\([^\"]\\|\n\\|\\\\\"\\)*\\)\"\\(,\\|,?$\\)"
  "Regexp defining a quoted entry in a CSV file.

Modifying this expression allows for automatically modifying CSV
entries while they are being read.  The actual CSV entry is read
from the second matching part.  The following expression for
example strips off leading and trailing whitespaces:

\\(^\\|,\\)[\\t ]*\"\\(\\([^\"]\\|\\n\\|\\\\\"\\)*\\)\"[\\t ]*\\(,\\|,?$\\)")

(defvar csv-unquoted-entry-regexp
  "\\(^\\|,\\)\\([^,\n]*\\)\\(,\\|,?$\\)"
  "Regexp defining an unquoted entry in a CSV file.

Modifying this expression allows for automatically modifying CSV
entries while they are being read. The actual CSV entry is read
from the second matching part.  The following expression for
example strips off leading and trailing whitespaces:

\\(^\\|,\\)[\\t ]*\\(\[^,\\n\]*\\)\[\\t \]*\\(,\\|,?$\\)")


(defun csv-parse-buffer (&optional buffer coding-system)
  "Parse a buffer containing CSV data, return data as a list of alists.

If BUFFER is non-nil it gives the buffer to be parsed.  If it is
nil the current buffer is parsed.
CODING-SYSTEM gives the coding-system for reading the buffer."
  (interactive)
  (let ((result nil)
	(keylist nil)
	(go-ahead t)
	(coding-system-for-read coding-system)
	(line 1)
	(line-format (format "%%d/%d" (count-lines (point-min) (point-max)))))
    (save-window-excursion
      (if buffer
	  (switch-to-buffer buffer)
	(setq buffer (current-buffer)))
      (beginning-of-buffer)
      ;; get the header line
      (setq keylist (csv-read-line))
      (next-line 1)
      ;; get all the content lines
      (while go-ahead
	(setq result (cons (csv-read-line keylist) result))
	;; (message "Got %S" (car result))
	(next-line 1)
	(end-of-line)
	(setq line (1+ line))
	(message line-format line)
	(if (eobp)
	    (setq go-ahead nil))))
    (setq result (reverse result))
    ;;(csv-force-complete-lines keylist result)
    result))

(defvar csv-modify-line-functions nil
  "Functions to call to modify the result of `csv-read-line'.
This lets you add file positions, etc.")

(defun csv-read-line (&optional keylist)
  "Parse a single CSV line.
If KEYLIST is not nil an alist is returned, using the keys from the keylist.
Otherwise just the list of entries is returned."
  (let ((line-contents nil)
	(match1 "")
	(match2 "")
	(match "")
	(matchstart1 0)
	(matchstart2 0)
	(matchend1 0)
	(matchend2 0)
	(index 0)
	(go-ahead t))
    (beginning-of-line)
    (setq line-contents nil)
    (while go-ahead
      (setq matchstart1 nil
	    matchstart2 nil)
      ;; try for quoted entry
      (save-excursion
	(when (re-search-forward
	       csv-quoted-entry-regexp
	       nil t)
	  (setq matchstart1 (match-beginning 0))
	  (setq matchend1 (+ 1 (match-end 2)))
	  (setq match1 (match-string-no-properties 2))))
      ;; try unquoted
      (save-excursion
	(when (re-search-forward csv-unquoted-entry-regexp
				 nil t)
	  (setq matchstart2 (match-beginning 0))
	  (setq matchend2 (match-end 2))
	  (setq match2 (match-string-no-properties 2))))
      ;; check whether quoted or unquoted fits better
      (setq match nil)
      ;; (message "matchstart1=%S matchstart2=%S" matchstart1 matchstart2)
      (if matchstart1
	  (if matchstart2
	      (if (<= matchstart1 matchstart2)
		  (progn
		    (setq match match1)
		    (goto-char matchend1))
		(setq match match2)
		(goto-char matchend2))
	    (setq match match1)
	    (goto-char matchend1))
	(when matchstart2
	  (setq match match2)
	  (goto-char matchend2)))
      ;; check whether we found something
      (if (not match)
	  (setq go-ahead nil)
	(if (not keylist)
	    (setq line-contents (cons match line-contents))
	  (let ((key (nth index keylist)))
	    (setq line-contents (cons (cons key match) line-contents))))
	)
      (setq index (+ 1 index))
      (if (eolp) (setq go-ahead nil)))
    ;; fill up
    (while (< index (length keylist))
      (let ((key (nth index keylist)))
	(setq line-contents (cons (cons key "") line-contents)))
      (setq index (+ 1 index)))
    ;; finally reverse result -- for readability
    (dolist (filter csv-modify-line-functions)
      (setq line-contents (funcall filter line-contents)))
    (reverse line-contents)))

;; (defun csv-force-complete-lines (keylist contentlist)
;;   (mapcar (lambda (line)
;; 	    (mapcar (lambda (key)
;; 		      (or (assoc key line)
;; 			  (cons key "not found")))
;; 		    keylist))
;; 	  contentlist))


(defun csv-insert-contents (contentlist)
  "Insert the contents of a CSV file -- sample for using `csv-parse-buffer'.
CONTENTLIST gives a list of alists as returned by `csv-parse-buffer'."
  (interactive)
  (message "hallo")
   (mapcar (lambda (line)
	     (insert "-----\n")
	     (mapcar (lambda (i)
		       (insert (format "\"%s\" = \"%s\"\n" (car i) (cdr i)))
		       )
		     line))
	   contentlist))
  
(defun csv-test ()
  "Test routine -- don't care."
  (interactive)
  (let* ((b (current-buffer))
	 (tb (get-buffer-create "*csv*")))
    (switch-to-buffer-other-window tb)
    (insert "asdf")
    (erase-buffer)
    (beginning-of-buffer)
    (csv-insert-contents (csv-parse-buffer b))
    (switch-to-buffer tb)))


(provide 'csv)

;;; csv.el ends here
