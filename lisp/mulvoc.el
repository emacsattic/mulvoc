;;;; mulvoc.el -- multi-lingual vocabulary
;;; Time-stamp: <2007-08-21 13:56:43 jcgs>

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

;;; Written by John C G Sturdy in October 2004
;;; IPR declaration: entirely my original work

(require 'thingatpt)
(require 'csv)
(require 'language-codes)

(require 'cl)
(require 'time-date)

(require 'mulvoc-custom)
(require 'mulvoc-data)
(require 'mulvoc-mode)
(require 'mulvoc-listing)	    ; should probably become autoloads
(require 'mulvoc-interactive-edit)   ; should probably become autoloads
(require 'mulvoc-writer)		    ; should probably become autoloads
(require 'mulvoc-flashcards)	    ; should probably become autoloads

(autoload 'csv-write-data-to-file "csv-writer.el"
  ;; from http://www.cb1.com/~john/computing/emacs/lisp/data-structures/csv-writer.el
  "Into FILE write DATA as comma separated values.
If optional third argument is given and non-nil, it is the coding system to use.
Optional fourth argument is the column headings to use (instead of choosing them
automatically -- see csv-insert-data for details).
See csv-insert-data for more detail.")

;; todo: default parts of speech?
;; todo: tidy up the defcustoms

(defvar mulvoc-latest-word nil
  "The latest word whose translation has been displayed.")

(mapcar 'make-variable-buffer-local '(mulvoc-cursor-active mulvoc-latest-word))

(defvar mulvoc-loaded nil
  "Whether we have yet run mulvoc-setup.")

(defun mulvoc-count-words ()
  "Count the words mulvoc knows."
  (interactive)
  (let ((count 0))
    (mapatoms (lambda (atom)
		(incf count))
	      mulvoc-words)
    (when (interactive-p)
      (message "Mulvoc knows %d words" count))
    count))

(defun mulvoc-count-language-words (language)
  "Count the words mulvoc knows in LANGUAGE."
  (interactive (list (mulvoc-read-language "Count words in language: ")))
  (when (symbolp language) (setq language (symbol-name language)))
  (let ((count 0))
    (mapatoms (lambda (atom)
		(when (and (boundp atom)
			   (assoc language (symbol-value atom)))
		  (incf count)))
	      mulvoc-words)
    (when (interactive-p)
      (message "Mulvoc knows %d %s words" count language))
    count))

(defun mulvoc-count-languages ()
  "Count how many languages mulvoc knows."
  (length mulvoc-languages))

(defun mulvoc-report-counts ()
  "Report on how many languages and words are known."
  (interactive)
  (let* ((words-per-language (sort (mapcar (lambda (language)
					     (cons (car language)
						   (mulvoc-count-language-words (car language))))
					   mulvoc-languages)
				   (lambda (a b) (> (cdr a) (cdr b)))))
	 (report (concat (format "%d words in %d languages: "
				 (mulvoc-count-words)
				 (mulvoc-count-languages))
			 (mapconcat (lambda (pair) (format "%s: %d" (car pair) (cdr pair)))
				    words-per-language
				    ", "))))
    (message report)
    words-per-language))

(defun mulvoc-word-as-string (word &optional further-info)
  "Return WORD as a string."
  ;; transitional, while I go over to making them all lists:
  (cond
   ((null word) nil)
   ((stringp word)
    (if (and further-info (get-text-property 0 'gender word))
	(format "%s<%S>" word (get-text-property 0 'gender word))
    word))
   ((consp word)
    (mapconcat 'identity word mulvoc-alias-separator))
   (t (error "Unknown word format: %S" word))))

(defun mulvoc-word-languages (word)
  "List the languages in which WORD is known to occur.
The result is an alist, of the language name dotted with the
translations from that language/word combination."
  (symbol-value (intern-soft word mulvoc-words)))

;;;###autoload
(defun mulvoc-translate-word (word)
  "Show (and return) translations of WORD."
  ;; on old system
  (interactive
   (progn
     (mulvoc-ensure-loaded)
     (list
      (completing-read "Translate word: " mulvoc-words
		       nil t))))
  (mulvoc-ensure-loaded)
  (let ((ob (symbol-value (intern-soft word mulvoc-words))))
    (if (interactive-p)
	(if ob
	    (message "%S" ob)
	  (message "No translation for %S" word))
      ob)))

;;;###autoload
(defun mulvoc-translate-word-to-language (word type from-language to-language)
  "Show (and return) translation of WORD of TYPE from FROM-LANGUAGE to TO-LANGUAGE."
  (interactive
   (progn
     (mulvoc-ensure-loaded)
     (let* ((word (completing-read "Translate word: " mulvoc-words
				   nil t))
	    (occurring-languages (mulvoc-word-languages word))
	    (from-language (if (null (cdr occurring-languages))
			       (caar occurring-languages)
			     (completing-read "From language: " occurring-languages nil t)))
	    (type			; todo: get this
	     )
	    (translated-languages (cdr (assoc from-language occurring-languages)))
	    (to-language (completing-read "To language: "
					  translated-languages
					  nil t)))
       (list word type from-language to-language))))
  (mulvoc-ensure-loaded)
  (let* ((meaning (if from-language
		      (cdr (assoc from-language (mulvoc-word-languages word)))
		    (cdr (car (mulvoc-word-languages word)))))
	 (translated-word (mulvoc-word-as-string (cdr (assoc to-language meaning)))))
    (if (interactive-p)
	(if translated-word
	    (message "\"%s\" in %s is \"%s\" in %s" word from-language translated-word to-language)
	  (message "No translation for %S" word))
      translated-word)))

(defun mulvoc-setup-hook-function ()
  (add-hook 'post-command-hook 'mulvoc-command-hook-function nil t))

(add-hook 'text-mode-hook 'mulvoc-setup-hook-function)

(add-to-list 'auto-coding-regexp-alist
	     (cons "content=\"text/html; charset=utf-8\""
		   'utf-8-unix))

(defun mulvoc-clear-dictionary ()
  "Clear the dictionary.
Meant for debugging loading of the dictionary."
  (interactive)
  (setq mulvoc-loaded nil
	mulvoc-words (make-vector (length mulvoc-words) nil)
	mulvoc-phrase-ending-words (make-vector 1511 nil)
	mulvoc-languages nil
	mulvoc-flashcard-meanings nil))

(defvar mulvoc-parts-of-speech
  '(("Noun") ("Verb")
    ("Adjective") ("Adverb")
    ("Preposition") ("Conjunction")
    ("Phrase"))
  "All the parts of speech currently known in the system.
Stored as an alist, for giving to completing-read.")

(defvar mulvoc-languages nil
  "All the languages currently known in the system.
This is an alist. The cdr of each element can be an alist of
properties of that language, such as input method.")

(defun mulvoc-part-of-speech-hook-function (language word)
  "Hook function for setting part of speech."
  (if (string-match "#type: \\(.+\\)$" word)
      (cons 'type
	    (intern (match-string 1 word)))
    nil))

(defun mulvoc-input-method-hook-function (language word)
  "Hook function for setting input method."
  (message "mulvoc-input-method-hook-function(%S,%S)" language word)
  (if (string-match "#input-method: \\(.+\\)$" language)
      (let ((lang-def (assoc language mulvoc-languages)))
	(rplacd lang-def
		(cons (cons 'input-method (intern (match-string 1)))
		      (cdr lang-def)))))
  nil)

(add-hook 'mulvoc-comment-hook 'mulvoc-part-of-speech-hook-function)
(add-hook 'mulvoc-comment-hook 'mulvoc-input-method-hook-function)

(defconst mulvoc-gender-pattern "\\(.+\\) *(\\([mfncx][0-9]\\))"
  "Pattern for recognizing that a word is given with its gender.")

(defconst mulvoc-gender-strings
  '(("m" . masculine)
    ("f" . feminine)
    ("n" . neuter)
    ("c" . common)			; for Dutch, Swedish
    ("x" . mixed)			; for Polish
    )
  "Alist of gender strings to genders.")

(defun mulvoc-gender-from-string (string)
  "Convert STRING to a gender."
  (cdr (assoc (substring string 0 1) mulvoc-gender-strings)))

;;;###autoload
(defun mulvoc-ensure-loaded (&optional originals-only)
  "Ensure that mulvoc has loaded its dictionaries."
  (unless mulvoc-loaded
    (mulvoc-setup originals-only)))

(defvar mulvoc-file-word-array nil
  "Where to collect this file's definitions. Rebound in reading code.")

(defvar mulvoc-file-language-array nil
  "Where to collect this file's languages. Rebound in reading code.")

(defvar mulvoc-words-from-files nil
  "Alist of file truenames to obarrays of words defined in that file.")

(defvar mulvoc-languages-from-files nil
  "Alist of file truenames to obarrays of languages defined in that file.")

(defun mulvoc-read-directory (dict-directory)
  "Read the vocabulary files in DICT-DIRECTORY."
  (interactive "DRead vocabulary from files in directory: ")
  (mapcar 'mulvoc-read-csv-file (directory-files dict-directory t mulvoc-dictionaries-pattern t)))

(defconst mulvoc-origin-string "#ORIGIN"
  "String we use to mark origin data.")

(defun mulvoc-add-csv-marker (cells)
  "Return CELLS with a marker consed onto the front."
  ;; (message "In mulvoc-add-csv-marker %S" cells)
  (let ((start-marker (make-marker)))
    (set-marker start-marker (line-beginning-position) (current-buffer))
    ;; (message "Made origin marker %S" start-marker)
    (cons (cons mulvoc-origin-string
		start-marker)
	  cells)))

(defvar mulvoc-file-columns nil
  "The column headings for this file.")

(make-variable-buffer-local 'mulvoc-file-columns)

(defun mulvoc-read-csv-file (dict-file &optional fast)
  "Read the vocabulary from comma separate values DICT-FILE.
With optional FAST, do not merge with existing vocabulary.
Use FAST only when you know there will be no overlaps,
e.g. on the first file."
  (interactive "fRead vocabulary from csv file: ")
  (unless (interactive-p) (message "Reading vocabulary from file %s" dict-file))
  (save-window-excursion
    (let* ((csv-unquoted-entry-regexp  "\\(^\\|,\\)[\t ]*\\([^,\n]*\\)[\t ]*\\(,\\|,?$\\)")
	   (csv-quoted-entry-regexp "\\(^\\|,\\)[\t ]*\"\\(\\([^\"]\\|\n\\|\\\\\"\\)*\\)\"[\t ]*\\(,\\|,?$\\)")
	   (was-visited (get-file-buffer dict-file))
	   (truename (file-truename dict-file))
	   (word-array-pair (assoc truename mulvoc-words-from-files))
	   (mulvoc-file-word-array (cdr word-array-pair))
	   (language-array-pair (assoc truename mulvoc-languages-from-files))
	   (mulvoc-file-language-array (cdr language-array-pair))
	   )
      (unless word-array-pair
	(setq mulvoc-file-word-array (make-vector 1511 nil)
	      word-array-pair (cons truename mulvoc-file-word-array)
	      mulvoc-words-from-files (cons word-array-pair mulvoc-words-from-files)))
      (unless language-array-pair
	(setq mulvoc-file-language-array (make-vector 1511 nil)
	      language-array-pair (cons truename mulvoc-file-language-array)
	      mulvoc-languages-from-files (cons language-array-pair mulvoc-languages-from-files)))
      (find-file dict-file)
      (let* ((csv-read-line-hooks '(mulvoc-add-csv-marker))
	     (dict (csv-parse-buffer))
	     (rows (length dict))
	     (rows-per-percent (if (> rows 100)
				   (/ (length dict) 100)
				 1))
	     (i 0))
	(if mulvoc-debug
	    (message "Reading %s and adding %S" dict-file dict)
	  (message "Adding data from %s" dict-file))
	(message "%d rows, %d per percent" (length dict) rows-per-percent)
	(setq mulvoc-file-columns (mapcar 'car (car dict)))
	(dolist (meaning dict)
	  (setq i (1+ i))
	  (when (zerop (% i rows-per-percent))
	    (message "Adding %s: %d%%" dict-file (/ (* i 100) (length dict))))
	  (mulvoc-define-meaning meaning fast)))
      (unless (or mulvoc-keep-dictionary-buffers
		  was-visited)
	(kill-buffer nil)))))

(defvar mulvoc-in-setup nil
  "Whether we are currently in mulvoc-setup.
Used to prevent recursion, in the case of mulvoc-setup (or
mulvoc-ensure-loaded) being called from a mode hook function that gets
used in the csv files that are read during mulvoc-setup.")

;;;###autoload
(defun mulvoc-setup (&optional force)
  "Set up the MUlti-Lingual VOcabulary system.
With optional argument, ignore the cached vocabulary file and get the original data."
  (interactive "P")
  (unless mulvoc-in-setup
    (let ((mulvoc-in-setup t)
	  (setup-started (current-time)))
      (run-hooks 'mulvoc-setup-hook)
      (if (and (not force)
	       (stringp mulvoc-cache-file)
	       (file-exists-p mulvoc-cache-file))
	  (if (and mulvoc-load-while-idle
		   (fboundp 'load-lisp-while-idle))
	      (load-lisp-while-idle mulvoc-cache-file)
	    (load-file mulvoc-cache-file))
	(message "Mulvoc loading dictionaries matching %s from %s"
		 mulvoc-dictionaries-pattern
		 (mapconcat 'identity mulvoc-dictionaries-directories ", "))
	(dolist (dict-directory mulvoc-dictionaries-directories)
	  (message "Setting coding system for files in %s to be utf-8-unix" dict-directory)
	  (add-to-list 'file-coding-system-alist
		       (cons (concat dict-directory
				     "/.+\\.csv")
			     'utf-8-unix)))
	(mapcar 'mulvoc-read-directory mulvoc-dictionaries-directories)
	(when (and (stringp mulvoc-cache-file)
		   (file-directory-p (file-name-directory mulvoc-cache-file)))
	  (mulvoc-dump-all-meanings mulvoc-cache-file)))
      (run-hooks 'mulvoc-post-setup-hook)
      (let* ((setup-ended (current-time))
	     (setup-duration (subtract-time setup-ended setup-started)))
	(message "Reading vocabulary took %S seconds... got %d words in %d languages"
		 setup-duration
		 (mulvoc-count-words) (mulvoc-count-languages)))))
  (setq mulvoc-loaded t
	mulvoc-abbrev-setup-done nil	; in case it got triggered recursively while loading vocab data
	))

(provide 'mulvoc)

;;; end of mulvoc.el
