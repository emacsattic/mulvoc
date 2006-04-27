;;;; mulvoc-listing.el -- listing of mulvoc data structures
;;; Time-stamp: <2006-04-18 14:35:20 john>

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

(provide 'mulvoc-listing)

(defun mulvoc-print-meaning (meaning-pair)
  "Print MEANING-PAIR to stdout.
It is a pair of language to string or to list of strings."
  (unless (eq (car meaning-pair) 'origins)
    (if (stringp (car meaning-pair))
	(princ (format " %s[%s]" (mulvoc-word-as-string (cdr meaning-pair) t) (car meaning-pair)))
      (princ (format " {%s:%s}" (car meaning-pair) (mulvoc-word-as-string (cdr meaning-pair)))))))

(defun mulvoc-print-type-meaning (type-meaning)
  "Print TYPE-MEANING to stdout.
It should be a pair of a type and the meaning associated with that type."
  ;; (message "Printing type-meaning pair %S" type-meaning)
  (let ((type (car type-meaning)))
    (cond
     ((or (symbolp type) (stringp type))
      (princ (format "(%S)" type)))
     ((consp type)
      (princ (format "(%S (%S))" (car type) (cdr type))))
     (t (error "bad type field: %S" type))))
  ;; (message "Printing meaning %S" (cdr type-meaning))
  (mapcar 'mulvoc-print-meaning (cdr type-meaning))
  (when t
    (let ((origins (cdr (assoc 'origins (cdr type-meaning)))))
      (princ "<")
      (princ (mapconcat
	      (function
	       (lambda (origin)
		 (if (and t (markerp origin))
		     (format "%s:%d"
			     (file-name-nondirectory (buffer-file-name (marker-buffer origin)))
			     (marker-position origin))
		   (format "non-marker %S" origin))))
	      origins
	      ", "))
      (princ ">")))
  (princ "; "))

(defun mulvoc-print-language-word (language-word)
  "Print LANGUAGE-WORD to stdout.

LANGUAGE-WORD is an alist of (type . meaning), where type is the type
of word (aka part of speech) such as Verb or Noun, and meaning is a
list of (language . string) pairs.

Made for use inside mulvoc-list-dictionary."
  ;; (princ (format "[type=%s]" (car language-word)))
  (when (listp language-word)
    (mapcar 'mulvoc-print-type-meaning language-word)))

(defun mulvoc-print-atom-language-definition (language-definition)
  "Print LANGUAGE-DEFINITION to stdout.

LANGUAGE-DEFINITION is the definition, for a language, of all words of
a particular spelling. It takes the form of a pair, of
  (language . definition)
where definition is a list of pairs of type and meaning (see
mulvoc-print-language-word).

Made for use inside mulvoc-list-dictionary.
Expects atom and mulvoc-listing-label-format to be bound."
  (princ (format mulvoc-listing-label-format atom
		 (car language-definition)))
  (mapcar 'mulvoc-print-type-meaning ; 'mulvoc-print-language-word
	  (cdr language-definition))
  ;; (when t (princ (format "; %S" language-definition)))
  (princ "\n"))

(defun mulvoc-unlisted-atom (atom)
  "Return t if ATOM should not be listed in the dictionary, etc."
  (let ((name (symbol-name atom)))

    (or (string-match "^[0-9]+$" name)
	(string-match " " name))))

(defun mulvoc-print-atom (atom)
  "Print ATOM's mulvoc value to stdout.
The value should be an alist of language to definitions in that language.
Made for use inside mulvoc-list-dictionary.
Expects atom and mulvoc-listing-label-format to be bound."
  (unless (mulvoc-unlisted-atom atom)
    ;; watch out for it being from a different obarray
    (mapcar 'mulvoc-print-atom-language-definition (symbol-value atom))))

;;;###autoload
(defun mulvoc-list-dictionary (&optional dictionary-buffer-name)
  "List the dictionary. With optional argument, use that as the listing buffer name.
Loads the dictionary if needed, unless a buffer name is given (this is used in debugging loading of the dictionary.)"
  (interactive)
  (unless (or mulvoc-loaded dictionary-buffer-name) (mulvoc-setup))
  ;; (mulvoc-dump-dict)
  (let ((atoms nil)
	;; this is making it fall over on windows
	;; (temp-buffer-setup-hook '(lambda () (set-buffer-file-coding-system 'utf8)))
	)
    ;; first gather up all the atoms into a list, so I can sort them
    (mapatoms (lambda (atom)
		(when (and atom (boundp atom))
		    (push atom atoms)))
	      mulvoc-words)
    (with-output-to-temp-buffer (if dictionary-buffer-name
				    dictionary-buffer-name
				  "*Dictionary*")
      (let ((mulvoc-listing-label-format (format "%% %ds[%%s]: "
						(min mulvoc-dictionary-left-column-limit
						     (apply 'max
							    (mapcar (lambda (atom)
								      (length (symbol-name atom)))
								    atoms))))))
	;; now show the sorted list
	(mapcar 'mulvoc-print-atom
		(sort atoms (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))))

(defun mulvoc-dump-dict ()
  "Dump the dictionary, in a fairly basic way."
  (interactive)
  (message "[dict:")
  (mapatoms
   (lambda (word-atom)
     (when (and word-atom (boundp word-atom))
       ;; (message "  %S:" word-atom)
       ;; (message " %S" (symbol-value word-atom))
       (message "  %S: %S" word-atom (symbol-value word-atom))
       ))
   mulvoc-words)
  (message "]"))

(defun mulvoc-display-file-data ()
  "Display what we know about what came from which file."
  (interactive)
  (with-output-to-temp-buffer "*Mulvoc per-file data*"
    (dolist (file mulvoc-words-from-files)
      (princ (format "\n%s words:\n" (car file)))
      (mapatoms (lambda (atom)
		  ;; (princ (format "%S\n" atom))
		  (if (and atom
			   ;; (boundp atom)
			   ;; (stringp (symbol-value atom))
			   )
		      (princ (format "  %s\n" (symbol-name atom)))))
		(cdr file)))
    (dolist (file mulvoc-languages-from-files)
      (princ (format "\n%s languages:\n" (car file)))
      (mapatoms (lambda (atom)
		  (if (and atom
			   ;; (boundp atom)
			   )
		      (princ (format "  %S\n" (symbol-name atom)))))
		(cdr file)))))

(defun mulvoc-list-phrases ()
  "List the known phrases."
  (interactive)
  (let ((phrases nil))
    (mapatoms (lambda (symbol)
		(when (boundp symbol)
		  (let ((value (symbol-value symbol)))
		    (when value
		      (push (cons symbol value) phrases)))))
	      mulvoc-phrase-ending-words)
    (with-output-to-temp-buffer "*Phrases*"
      (dolist (phrase phrases)
	(princ (format "%s: [%s] <- %s\n"
		       (cadr phrase)
		       (car phrase)
		       (mapconcat 'identity
				  (cddr phrase)
				  " <- ")))))))

(defun mulvoc-list-files-word (word)
  "List the files supplying WORD."
  (let ((result nil))
    (dolist (file-words-pair mulvoc-words-from-files)
      ;; Alist of file truenames to obarrays of words defined in that file
      (when (intern-soft word (cdr file-words-pair))
	(push (car file-words-pair) result)))
    result))

(defun mulvoc-list-possible-puns ()
  "List possible multilingual puns."
  (interactive)
  (let ((result nil))
    (mapatoms (lambda (sym)
		(when (boundp sym)
		  (let ((val (symbol-value sym)))
		    (when (cdr val)
		      (push (cons sym val) result)))))
	      mulvoc-words)
    (with-output-to-temp-buffer "*Punnable words*"
      (princ (format "%d words defined in more than one language\n" (length result)))
      (dolist (word result)
	(princ (format "%s %s\n" (car word) (mapconcat 'car (cdr word) ",")))))))

;;; end of mulvoc-listing.el
