;;;; language-codes.el -- map between language names and language codes
;;; Time-stamp: <2007-06-13 21:50:43 jcgs>

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

(provide 'language-codes)

(defvar language-codes-file-name (expand-file-name "LanguageCodes.tab" mulvoc-etc-directory)
  "The file containing the language codes file.
The contents for this can be downloaded from
http://www.ethnologue.com/codes/LanguageCodes.tab
This data is from SIL International.")

(add-to-list 'file-coding-system-alist
	     (cons (concat mulvoc-etc-directory
			   ".+\\.csv")
		   'utf-8-unix))

(defvar language-codes nil
  "Alist caching lookups in language-codes-file-name.")

(defmacro with-language-code-file (&rest forms)
  "Execute FORMS with the language code file."
  `(save-window-excursion
     (unless (file-readable-p language-codes-file-name)
       (with-output-to-temp-buffer "*File required"
	 (princ "Please download http://www.ethnologue.com/codes/LanguageCodes.tab\n")
	 (princ "and put it into a file pointed to by language-codes-file-name\n")
	 (princ (format "(which is currently set to %s).\n" language-codes-file-name)))
       (error "Language codes file not present"))
     (find-file-read-only language-codes-file-name)
     (save-excursion
       (goto-char (point-min))
       (prog1
	   (progn
	     ,@forms)
	 (bury-buffer)))))

(defun language-code (language-name &optional country-code standard-only)
  "Return the language code for LANGUAGE-NAME.
If LANGUAGE-NAME is not recognized, it is returned unchanged.
With optional second argument, return the country code instead."
  (if (and (let ((case-fold-search nil))
	     (string-match "^[A-Z][A-Z][A-Z]$" language-name))
	   (rassoc language-name language-codes))
      language-name
    (let ((pair (assoc language-name language-codes)))
      (if pair
	  (if country-code
	      (cddr pair)
	    (cadr pair))
	(with-language-code-file
	 (let ((case-fold-search t)
	       (search-pattern-standard (format "^\\([A-Z][A-Z][A-Z]\\)\t\\([A-Z][A-Z]\\)\t[A-Z]\t%s\\(, Standard\\)?$"
						language-name))
	       (search-pattern-any (format "^\\([A-Z][A-Z][A-Z]\\)\t\\([A-Z][A-Z]\\)\t[A-Z]\t%s\\(,.*\\)?$"
					   language-name)))
	   ;; (message "Searching for %S or %S" search-pattern-standard search-pattern-any)
	   (if (or (re-search-forward search-pattern-standard (point-max) t)
		   (re-search-forward search-pattern-any (point-max) t))
	       (let ((code (cons (match-string-no-properties 1) (match-string-no-properties 2))))
		 (setq language-codes
		       (cons (cons language-name
				   code)
			     language-codes))
		 (if country-code
		     (cdr code)
		   (car code)))
	     language-name)))))))

(defun assocn (n elt list)
  "Like ASSOC, but using the Nth element instead of the car."
  (catch 'found
    (while list
      (if (equal (nth n (car list)) elt)
	  (throw 'found (car list))
	(setq list (cdr list))))
    nil))

(defun language-name (language-code)
  "Return the language name for LANGUAGE-CODE.
If LANGUAGE-CODE is not recognized, it is returned unchanged."
  (when (symbolp language-code) (setq language-code (symbol-name language-code)))
  (or (car (assocn 1 language-code language-codes))
      (with-language-code-file
       (let ((case-fold-search t)
	     (search-pattern (format "^%s\t\\([A-Z][A-Z]\\)\t[A-Z]\t\\(.+\\)$"
				     language-code)))
	 ;; (message "Searching for %s" search-pattern)
	 (if (re-search-forward search-pattern
				(point-max) t)
	     (let ((country (match-string-no-properties 1))
		   (name (match-string-no-properties 2)))
	       (setq language-codes
		     (cons (cons name
				 (cons language-code country))
			   language-codes))
	       name)
	   nil)))))

(defvar language-codes-to-countries nil
  "Cache for language-code-to-country.")

(defun language-code-to-country (three)
  "Convert a 3-letter language code to the 2-letter equivalent."
  (let ((pair (assoc three language-codes-to-countries)))
    (if pair
	(cdr pair)
      (with-language-code-file
       (let ((case-fold-search t)
	     (search-pattern (format "^\\(%s\\)\t\\([A-Z][A-Z]\\)\t" three)))
	 ;; (message "Searching for %s" search-pattern)
	 (if (re-search-forward search-pattern
				(point-max) t)
	     (let ((code (cons (match-string 1) (match-string 2))))
	       (setq language-codes-to-countries
		     (cons code language-codes-to-countries))
	       (cdr code))
	   three))))))

;;; end of language-codes.el
