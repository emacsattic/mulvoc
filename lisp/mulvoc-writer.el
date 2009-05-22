;;;; mulvoc-writer.el -- write mulvoc data structures out to file
;;; Time-stamp: <2009-05-22 17:30:00 jcgs>

;; Copyright (C) 2007, 2008, 2009, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2006?
;; Keywords: languages

;; This file is NOT part of GNU Emacs.

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

;; (require 'mulvoc-merge)
;; (provide 'mulvoc-writer)

;; ;;;###autoload
;; (defun mulvoc-write-dictionary (file &optional dictionary words languages writer synonym-separator sort-key)
;;   "Write the dictionary into FILE.
;; If optional second argument given, that is the dictionary to write (as an obarray).
;; Third argument is an obarray: write only the words interned in it (but use the second argument,
;; or the main dictionary obarray, for the data to write).
;; Fourth argument is a list of language names: write only the languages in it.
;; The names are as strings in whatever form they are in the incoming vocabulary files.
;; Fifth argument is a function to call to do the writing.
;; It should take a list of alists, a coding system, and a list of languages.
;; Sixth argument is string to separate synonyms."
;;   (interactive
;;    (let* ((filename (read-file-name "File to save dictionary in: "))
;; 	  (original-dictionary-name (completing-read "Write words that came from dictionary: "
;; 						     mulvoc-words-from-files))
;; 	  (languages (mulvoc-read-languages "Languages to write words in: "))
;; 	  (sort-key (mulvoc-read-language "Sort by: " '(("#TYPE")))))
;;      (list filename
;; 	   nil
;; 	   (cdr (assoc original-dictionary-name mulvoc-words-from-files))
;; 	   languages
;; 	   nil
;; 	   nil
;; 	   sort-key))
;;    )
;;   (when (null dictionary) (setq dictionary mulvoc-words))
;;   (when (or (null languages) (eq languages t))
;;     (setq languages (nreverse (mapcar 'car mulvoc-languages))))
;;   (when (null synonym-separator)
;;     (setq synonym-separator ", "))
;;   (let ((meanings nil))
;;     (mapatoms
;;      (lambda (word-atom)
;;        (when (and word-atom
;; 		  (boundp word-atom)
;; 		  (or (null words) ; if WORDS specified, check word-atom is in it
;; 		      (intern-soft (symbol-name word-atom) words)))
;; 	 (when mulvoc-debug
;; 	   (message "adding %S"
;; 		    word-atom)
;; 	   (message "   with definition-->%S to collection for writing"
;; 		    (if (and word-atom (boundp word-atom))
;; 			(symbol-value word-atom)
;; 		      "<none>")))
;; 	 (dolist (def (symbol-value word-atom))
;; 	   (let ((data (cadr def)))
;; 	     (when mulvoc-debug
;; 	       (message "  Adding meaning %S" data)
;; 	       (when (member data meanings) (message "  Already got that one"))
;; 	       )
;; 	     (pushnew data meanings :test 'equal)
;; 	     ))))
;;      dictionary)
    
;;     (when t mulvoc-debug
;;       (message "%d meanings collected" (length meanings))
;;       (message "meanings are %S" meanings)
;;       (message "languages are %S" languages))

;;     (let ((dict-as-list 
;; 	   (mapcar (lambda (meaning)
;; 		     ;; (message "Looking at meaning %S" meaning)
;; 		     (cons (cons "#TYPE"
;; 				 (cond
;; 				  ((symbolp (car meaning))
;; 				   (symbol-name (car meaning)))
;; 				  ((consp (car meaning))
;; 				   (symbol-name (caar meaning)))))
;; 			   (mapcar (lambda (language)
;; 				     ;; (message "  Looking at language %S" language)
;; 				     (let ((pair (assoc language meaning)))
;; 				       (if pair
;; 					   (cons (car pair)
;; 						 (if (stringp (cdr pair))
;; 						     (cdr pair)
;; 						   (mapconcat 'identity
;; 							      (cdr pair)
;; 							      synonym-separator)))
;; 					 nil)))
;; 				   languages)))
;; 		   meanings)))
;;       (message "Made dict-as-list %S" dict-as-list)
;;       (setq dict-as-list (sort dict-as-list
;; 			       (function
;; 				(lambda (a b)
;; 				  (safe-string< (cdr (assoc sort-key a))
;; 						(cdr (assoc sort-key b)))))))
;;       (funcall (or writer 'csv-write-data-to-file)
;; 	       file dict-as-list 'utf-8-unix (append '("#TYPE" "#SENSE") languages)))))

;; (defun safe-string< (a b)
;;   "Compare strings A and B.
;; If they're not both strings, return whether the first one is a string.
;; This way, all strings should sort before any non-strings."
;;   (if (and (stringp a) (stringp b))
;;       (string< a b)
;;     (stringp a)))

;; (defun mulvoc-writer-worthy-row (row languages &optional n-required)
;;   "Return whether ROW is worth writing out."
;;   (if (eq n-required t)
;;       t
;;     (let ((count 0))
;;       (dolist (language languages)
;; 	(when (assoc language row)
;; 	  (incf count)))
;;       (>= count (if n-required n-required (length languages))))))

;; (defun mulvoc-html-writer-0 (file dict-as-list coding-system languages)
;;   "Helper function for mulvoc-write-html and others."
;;   (let ((empty (if (assoc 'empty-cell-text options)
;; 		   (cdr (assoc 'empty-cell-text options))
;; 		 ""))
;; 	(cell-opts (if (assoc 'cell-options options)
;; 		       (cdr (assoc 'cell-options options))
;; 		     "")))
;;     (save-excursion 
;;       (find-file file)
;;       (erase-buffer)
;;       (insert "<html><head>\n")
;;       (insert "<title>Glossary</title>")
;;       (insert "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n")
;;       (insert "</head>\n<body>\n")
;;       (insert "<table" (if (assoc 'table-options options)
;; 			   (cdr (assoc 'table-options options))
;; 			 "")
;; 	      ">\n")
;;       (insert "  <tr>\n")
;;       (dolist (language languages)
;; 	(insert "    <th>" 
;; 		(cond
;; 		 ((stringp language)
;; 		  language)
;; 		 ((symbolp language)
;; 		  (symbol-name language))
;; 		 (t (prin1-to-string language)))
;; 		"</th>\n"))
;;       (insert "  </tr>\n")
;;       (dolist (row dict-as-list)
;; 	(when (mulvoc-writer-worthy-row row languages n-required)
;; 	  (insert "  <tr>\n")
;; 	  (dolist (cell row)
;; 	    (if (null (cdr cell))
;; 		(insert "    <td" cell-opts ">" empty "</td>\n")
;; 	      (let* ((language (car cell))
;; 		     (language-name (cond
;; 				     ((stringp language) language)
;; 				     ((symbolp language) (symbol-name language))
;; 				     (t (prin1-to-string language)))))
;; 		(insert "    <td" cell-opts " lang=\""
;; 			;; (language-code-3-to-2 language-name)
;; 			language-name
;; 			"\">"))
;; 	      (insert (cdr cell))
;; 	      (insert "</td>\n")))
;; 	  (insert "  </tr>\n")))
;;       (insert "</table>\n")
;;       (insert "<body>")
;;       (basic-save-buffer))))

;; (defun mulvoc-write-html (file &optional dictionary words languages n-required options)
;;   "Write the dictionary into FILE as HTML.
;; If optional second argument given, that is the dictionary to write (as an obarray).
;; Third argument is a DICTIONARY obarray: write only the words interned in it.
;; Fourth argument is a list of LANGUAGES names: write only the languages in it.
;; The names are as strings in whatever form they are in the incoming vocabulary files.
;; Fifth argument is how many languages must be present in a row for the
;; row to be included. If nil, it means all of LANGUAGES must be in that row.
;; If t, all rows are output.
;; OPTIONS is an alist of extra text to output in the table header etc.
;; Valid keys are table-options, empty-cell-text, synonym-separator, and cell-options."

;;   ;; todo: remove INPUT-METHODS row(s)
;;   ;; todo: put LANGUAGE-NAMES row at top
;;   ;; todo: treat #SENSE column specially
;;   ;; todo: sort data, including handling SEQUENCENUMBER column specially

;;   (interactive
;;    (let* ((file (read-file-name "File to save dictionary in: "))
;; 	  (languages (mulvoc-read-languages "Language to include: "))
;; 	  (number (string-to-int (read-from-minibuffer "Minimum number of languages per row: ")))
;; 	  (table-opts (read-from-minibuffer "Table options: " " cellpadding=\"2\""))
;; 	  (synonym-sep (read-from-minibuffer "Synonym separator: " ",<br>"))
;; 	  (empty (read-from-minibuffer "Empty cell text: " ""))
;; 	  (cell-opts (read-from-minibuffer "Cell options: " " align=\"center\"")))
;;      (list file nil nil languages number (list (cons 'table-options table-opts)
;; 					       (cons 'synonym-separator synonym-sep)
;; 					       (cons 'empty-cell-text empty)
;; 					       (cons 'cell-options cell-opts)))))
;;   (mulvoc-write-dictionary
;;    file dictionary words languages
;;    'mulvoc-html-writer-0
;;    (if (assoc 'synonym-separator options)
;;        (cdr (assoc 'synonym-separator options))
;;      ",<br>")))

;; (defun mulvoc-convert-dict-csv-to-html (csv-file html-file)
;;   "Convert CSV-FILE dictionary to HTML-FILE."
;;   (interactive "fCSV dictionary input file:
;; FHTML dictionary output file: ")
;;   (let* ((options nil)
;; 	 (n-required 2)
;; 	 (dict-as-list (csv-parse-file csv-file))
;; 	 (languages (mapcar 'car (car dict-as-list))))
;;     (mulvoc-html-writer-0 html-file
;; 			  dict-as-list
;; 			  'utf-8-unix
;; 			   languages)))

;; (defun mulvoc-test-writer ()
;;   (interactive)
;;   (let ((mulvoc-debug t)
;; 	;; (mulvoc-dictionaries-directories (list (expand-file-name "test" (car mulvoc-dictionaries-directories))))
;; 	)
;;     ;; (mulvoc-clear-dictionary)
;;     (setq message-log-max t)
;;     (switch-to-buffer "*Messages*")
;;     (erase-buffer)
;;     (mulvoc-write-dictionary "~/tmp/mulvoc-write-test.csv" nil nil '("ENG" "GER" "DUT" "NRR"))))

;; (defun mulvoc-test-html-writer ()
;;   (interactive)
;;   (let ((mulvoc-debug t)
;; 	;; (mulvoc-dictionaries-directories (list (expand-file-name "test" (car mulvoc-dictionaries-directories))))
;; 	)
;;     ;; (mulvoc-clear-dictionary)
;;     (setq message-log-max t)
;;     (switch-to-buffer "*Messages*")
;;     (erase-buffer)
;;     (mulvoc-write-html "~/tmp/mulvoc-write-test.html" nil nil '("ENG" "GER" "DUT" "NRR") 3)))

;; (defun mulvoc-test-reader-writer ()
;;   (interactive)
;;   (switch-to-buffer "*Messages*")
;;   (setq message-log-max t)
;;   (erase-buffer)
;;   (let ((mulvoc-debug nil)
;; 	(stack-trace-on-error t)
;; 	(mulvoc-dictionaries-directories
;; 	 (if t
;; 	     mulvoc-dictionaries-directories
;; 	   (list (expand-file-name "test" (car mulvoc-dictionaries-directories))))))
;;     (mulvoc-clear-dictionary)
;;     (mulvoc-list-dictionary))
;;   (mulvoc-write-dictionary "/tmp/dict.csv"))

;;; end of mulvoc-writer.el
