;;;; mulvoc-data.el -- main data handling for mulvoc
;;; Time-stamp: <2008-10-04 19:04:56 jcgs>

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

(require 'cl)

(defvar mulvoc-words (make-vector 1511 nil)
  "Obarray for mulvoc functions.
Each atom in it has a value which is a word definition.

A word definition is an alist of language to language-word-definition.

A language-word-definition is an alist of type to meaning, where type is the part of speech.

A meaning is an alist of language to string.

For example, the word \"fear\", which is a noun and a verb in English,
and a noun (with the same meaning as the English word \"man\" in
Irish), could have the following value on its symbol:

\((ENG . (noun . <alist of translations of the noun fear>)
        (verb . <alist of translations of the verb fear>))
 (IRL . (noun . (ENG . \"man\") (IRL \"fear\"))))

Synonyms replace the string (at the lowest level above) with a list of
strings."
  ;; todo: update this documentation to cover senses
  )

(defvar mulvoc-phrase-ending-words (make-vector 1511 nil)
  "Obarray for mulvoc phrase lookup.
Each symbol is the last word of a recognized phrase, and it is bound
to a cons of the string of the whole phrase, and the preceding words
of the phrase, in reverse order, i.e. the word just before the ending
word comes first in the list.")

(defun mulvoc-add-to-meaning (language word old-meaning)
  "Add LANGUAGE WORD to OLD-MEANING. Return the new meaning."
  (let* ((lang-def (assoc language old-meaning)))
    (if lang-def
	(progn
	  (rplacd lang-def word)
	  lang-def)
      (cons (cons language word) old-meaning))))

(defun mulvoc-get-meaning (language type word)
  "Get the meaning of LANGUAGE TYPE WORD."
  (if (symbolp word) (setq word (symbol-name word)))
  (let ((symbol (intern-soft word mulvoc-words)))
    (when mulvoc-debug
      (message "  Looking up (%S %S %S)-->%S" language type word symbol)
      (when (boundp symbol)
	(message "    value is %S" (symbol-value symbol))))
    (if (and symbol (boundp symbol))
	(cdr (assoc type (cdr (assoc language (symbol-value symbol)))))
      nil)))

(defun mulvoc-set-symbol-language-type-meaning (word-or-symbol language type meaning)
  "Set WORD-OR-SYMBOL's LANGUAGE meaning for TYPE to be MEANING."
  (if (stringp word-or-symbol) (setq word-or-symbol (intern word-or-symbol mulvoc-words)))
  (when mulvoc-debug (message "    Defining word-or-symbol=%S language=%S type=%S meaning=%S" word-or-symbol language type meaning))
  (mulvoc-define-phrase word-or-symbol)
  (let* ((language-definitions (if (boundp word-or-symbol)
				   (symbol-value word-or-symbol)
				 nil))
	 (this-language-definition-pair (assoc language language-definitions)))
    (if this-language-definition-pair
	(let* ((this-language-definition (cdr this-language-definition-pair))
	       (this-old-meaning-pair (assoc type this-language-definition)))
	  (if this-old-meaning-pair
	      (let* ((this-old-meaning (cdr this-old-meaning-pair)))
		(when mulvoc-debug
		  (message "        %S: Language %S already has definitions for type %S: %S %S and we are looking at putting %S there instead"
			   word-or-symbol language type this-old-meaning-pair this-old-meaning meaning))
		;; We already had a definition; overwrite it. The new
		;; definition should include all the old one, anyway.
		(rplacd this-old-meaning-pair meaning)
		)
	    (rplacd this-language-definition-pair
		    (cons
		     (cons type meaning)
		     this-language-definition))))
      (set word-or-symbol (cons (cons language
				      (list (cons type
						  meaning)))
				language-definitions)))))

(defvar mulvoc-decline-pattern "\\([a-z0-9]+\\): ?\\(.+\\)"
  "Pattern for declensions, cases etc.")

(defun mulvoc-define-meaning (new-meaning &optional fast)
  "Record the definitions of NEW-MEANING which is an alist of language.word pairs.
This is the form in which csv-parse-buffer returns each row of the
input file.
With optional FAST, do not merge with existing vocabulary.
Use FAST only when you know there will be no overlaps,
e.g. on the first file."
  (when mulvoc-debug
    (message "  Defining meaning-group %S" new-meaning)
    (message "type-pair is %S" (assoc "#TYPE" new-meaning)))
  ;; We do this in two phases: first we "gather the meaning", then we
  ;; "set the meaning".

  ;; A "meaning" is an association list of language to word. (In our
  ;; overall data structure, this is inside a list of meanings for a
  ;; particular word-language combination, as the same word might, for
  ;; example, be both a noun and a verb in the same language. This
  ;; list, in turn, is part of a list of words of the same spelling in
  ;; various languages. For example, the string "fear" is a noun and a
  ;; verb in English, and a noun in Irish (Gaelic)).

  ;; This is the form in which our argument, new-meaning, comes. We
  ;; must look up, in our existing data, each word that occurs in
  ;; new-meaning, to see whether it is part of any existing meanings,
  ;; as reached through any of the languages in the meaning so far. If
  ;; it is, we collect up all the language-word pairs from that
  ;; meaning too, and make them part of our new meaning.

  ;; Having done that, we then go through all the words which have
  ;; contributed to this collected meaning, and set them to use this
  ;; meaning (for that particular language and part of speech).

  ;; Let's clarify this by example:

  ;; If our new-meaning is telling us that the English word "Father"
  ;; and the German word "Vater" are equivalent, and our grand
  ;; collection of words also has a meaning that tells that the German
  ;; word "Vater" and the Dutch word "Vader" are equivalent, we can
  ;; pick up "Vader" and include it in the meaning we are currently
  ;; gathering. Having gathered all the bits of the meaning, we now
  ;; have to go along all the contributing words (not just the ones in
  ;; new-meaning) and set them to use this gathered meaning; so, in
  ;; the example, the language-type-word triplets
  ;; "English:Noun:Father", "German:Noun:Vater" and "Dutch:Noun:Vader"
  ;; all get set to use the meaning
  ;; (("English" . "Father") ("German" . "Vater") ("Dutch" . "Vader"))

  (let* ((gathered-meaning nil)
	 (origins nil)
	 (type-pair (assoc "#TYPE" new-meaning))
	 (sense-pair (assoc "#SENSE" new-meaning)))
    (cond
     ((null type-pair)
	(setq type-pair (cons "#TYPE" "unspecified")))
     ((equal (cdr type-pair) "")
      (rplacd type-pair "unspecified")))
    (let* ((type (if type-pair
		     (if (and sense-pair
			      (stringp (cdr sense-pair))
			      (not (string= (cdr sense-pair) "")))
			 (cons (intern (cdr type-pair))
			       (intern (cdr sense-pair)))
		       (intern (cdr type-pair)))
		   '*unknown-type*))
	   (read-languages (or mulvoc-read-languages
			       mulvoc-displayed-languages))
	   (slow nil))
      (unless (assoc (cdr type-pair) mulvoc-parts-of-speech)
	(setq mulvoc-parts-of-speech
	      (cons (list (cdr type-pair))
		    mulvoc-parts-of-speech)))
      ;; First a loop to gather the meaning
      (dolist (language-word new-meaning)
	(when mulvoc-debug (message "   language-word=%S" language-word))
	(if (null (car language-word))
	    (message "Warning: unspecified language for %S" (cdr language-word))
	  (let* ((raw-data (cdr language-word))
		 (language-string (if (stringp (car language-word))
				      (upcase (car language-word))
				    (symbol-name (car language-word))))
		 (first-form-of-word nil))
	    (cond
	     ((equal raw-data "") nil)
	     ((= (aref language-string 0) ?#)
	      (when mulvoc-debug (message "    Column pragma %s:%S" language-string raw-data))
	      (cond
	       ((and (string= language-string mulvoc-origin-string)
		     (not (member raw-data origins)))
		;; (message "Got raw origin data %S" raw-data)
		(setq origins (cons raw-data origins))
		;; (message "Origins are now %S" origins)
		)))
	     ((= (aref raw-data 0) ?#)
	      (when mulvoc-debug (message "    Row pragma %S" raw-data))
	      (let ((hook-result (run-hook-with-args-until-success 'mulvoc-comment-hook raw-data language-string)))
		(cond
		 ((eq hook-result t) nil)
		 ((consp hook-result)
		  (push hook-result gathered-meaning) ; todo: skip these when displaying etc
		  )
		 ((stringp hook-result)
		  ))))
	     ((or (eq read-languages t)
		  (member language-string read-languages))
	      (let* ((words-string (downcase raw-data))
		     (words-list (split-string words-string mulvoc-alias-separator-pattern)))
		(dolist (gendered-word-string words-list)
		  (let* ((gendered (string-match mulvoc-gender-pattern gendered-word-string))
			 (gender (if gendered
				     (mulvoc-gender-from-string (match-string 2 gendered-word-string))))
			 (word-string (if gendered
					  (propertize (match-string 1 gendered-word-string)
						      'gender gender)
					gendered-word-string))
			 (declined (string-match mulvoc-decline-pattern word-string))
			 (declension (if declined (match-string 1 word-string))))
		    (when declined
		      (setq word-string (propertize (match-string 2 word-string)
						    'declension declension)))
		    (when mulvoc-debug (message "    Gathering existing meanings linked through %S:%s" language-word word-string))
		    (if declined
			(when (string-match "^[-~]" word-string)
			  (when mulvoc-debug
			    (message "prepending first form of word %S to %S form %S"
				     first-form-of-word declension word-string))
			  (setq word-string (propertize (concat first-form-of-word (substring word-string 1))
							'declension (get-text-property 0 'declension word-string)
							'gender (get-text-property 0 'gender first-form-of-word))))
		      ;; if this was not a declined form, remember it as the base form
		      (setq first-form-of-word word-string))
		    (when (and (> (length word-string) 0)
			       (> (length language-string) 0))
		      (when mulvoc-file-word-array
			(intern word-string mulvoc-file-word-array))
		      (when mulvoc-file-language-array
			(intern language-string mulvoc-file-language-array))
		      (when mulvoc-debug
			(when gendered (message "got gendered word %S" word-string))
			(when declined (message "got declined word %S:%S, and first form was %S"
						declension word-string first-form-of-word)))
		      (let* ((language (intern (language-code language-string))))
			(rplacd language-word
				(if (null (cdr words-list))
				    ;; I think it's saying that if there's
				    ;; only one word in this alias group,
				    ;; keep it as a string, otherwise make a
				    ;; list of them
				    word-string
				  words-list))
			;; if we do not yet have this language, add it to
			;; the known languages
			(unless (assoc language mulvoc-languages)
			  (push (list language) mulvoc-languages))
			(let* ((word (intern word-string mulvoc-words))
			       (old-meaning (mulvoc-get-meaning language type word)))
			  ;; (message "old-meaning of (%S %S %S) is %S" language type word old-meaning)
			  (when old-meaning (setq slow t))
			  (dolist (this-old-language-word-pair old-meaning)
			    (if (eq (car this-old-language-word-pair) 'origins)
				(dolist (this-origin (cdr this-old-language-word-pair))
				  ;; (message "getting old origin %S" this-origin)
				  (pushnew this-origin origins :test 'equal))
			      ;; todo: make this merge multiple words in the same language, into a list, just as though they were read as a comma-separated list in one file -- this will be more efficient later as well, when re-reading dumped data
			      (pushnew this-old-language-word-pair gathered-meaning :test 'equal)))
			  (pushnew (cons language word-string) gathered-meaning :test 'equal))))))))))))
      ;; and now a loop to update all the words that contributed to the new meaning,
      ;; so they all use it
      (setq gathered-meaning (cons (cons 'origins origins) gathered-meaning))
      (when mulvoc-debug (message "Gathered meanings are %S" gathered-meaning))
      (dolist (language-word (cdr gathered-meaning))
	(let ((language (car language-word))
	      (word (cdr language-word)))
	  (when mulvoc-debug (message " Defining %S %S %S to be %S" word language type gathered-meaning))
	  (if (or (stringp language)
		  (symbolp language))
	      (if (stringp word)
		  (mulvoc-set-symbol-language-type-meaning
		   word
		   language
		   type
		   gathered-meaning)
		(dolist (this-word word)
		  (mulvoc-set-symbol-language-type-meaning
		   this-word
		   language
		   type
		   gathered-meaning)))
	    (message "Warning: non-string, non-symbol as language: %S" language))))
      gathered-meaning)))

(defun mulvoc-define-phrase (phrase)
  "Define PHRASE in the phrase obarray, to point to PHRASE in the words obarray."
  (when (symbolp phrase) (setq phrase (symbol-name phrase)))
  (when (string-match " " phrase)
    (let ((words (nreverse (split-string phrase))))
      (when mulvoc-debug
	(message "Defining phrase-ender %S with preceding words %S"
		 (car words)
		 (cdr words)))
      (set (intern (car words) mulvoc-phrase-ending-words)
	   (cons phrase (cdr words))))))

(defun mulvoc-all-meanings ()
  "Return a list of all meanings."
  (let ((meanings-hash (make-hash-table :test 'equal))
	(meanings-list nil))
    (mapatoms
     (lambda (word-atom)
       (when (and word-atom (boundp word-atom))
	 (let ((word-languages (symbol-value word-atom)))
	   ;; (message "  word-languages=%S" word-languages)
	   (dolist (language-meanings word-languages)
	     ;; (message "    language-meanings=%S" language-meanings)
	     (dolist (meaning (cdr language-meanings))
	       ;; (message "      meaning=%S" meaning)
	       (puthash meaning meaning meanings-hash))))))
     mulvoc-words)
    (maphash (lambda (key value)
	       (setq meanings-list (cons key meanings-list)))
	     meanings-hash)
    meanings-list))

(defun dm (m)
  "Define meaning M.
This is for use in the file produced by mulvoc-dump-all-meanings."
  (mulvoc-define-meaning
   (cons (cons "#TYPE"
	       (symbol-name (if (consp (car m))
				(caar m)
			      (car m))))
	 (cons (cons "#SENSE"
		     (if (consp (car m))
			 (symbol-name (cdar m))
		       nil))
	       (cdr m)))))

(defun mulvoc-compile-vocabulary-cache (file)
  "Compile a vocabulary cache file."
  (interactive "FFile to store vocabulary meanings in: ")
  (mulvoc-clear-dictionary)
  (mulvoc-dump-all-meanings file t))

(defun mulvoc-dump-all-meanings (file &optional rebuild data)
  "Put a list of all known meanings into FILE.
With optional REBUILD, force a re-load from the CSV files.
With optional DATA, use that instead of the loaded data."
  (interactive "FFile to store vocabulary meanings in: ")
  (unless data
    (mulvoc-ensure-loaded rebuild))
  (find-file file)
  (set-buffer-file-coding-system 'utf-8)
  (erase-buffer)
  (insert ";; -*- coding: utf-8 -*-\n"
	  ";; mulvoc meanings dumped at " (current-time-string)
	  " by " user-mail-address
	  " on " (system-name) "\n")
  (let* ((meanings (if data
		       (mulvoc-convert-meanings data)
		     (mulvoc-all-meanings)))
	 (n-meanings (length meanings))
	 (per-percent (/ n-meanings 100))
	 (standard-output (current-buffer))
	 (blank-symbol (intern ""))
	 (i 1)
	 (i-percent 0))
    (message "saving %d meanings to file %s" n-meanings file)
    (dolist (meaning meanings)
      (when (eq (caadr meaning) 'origins)
	(rplacd meaning (cddr meaning)))
      (princ "(dm '")
      (prin1 meaning)
      (princ (format ") ; %d\n" i))
      (setq i (1+ i))
      (when (> (/ i 100) i-percent)
	(setq i-percent (/ i 100))
	(princ (format "(message \"loading vocab ... %d%%%%\")\n" i-percent))))
    (message "%d meanings saved to file %s" n-meanings file))
  (insert "\n(setq mulvoc-loading-backgrounded nil)\n"
	  ";; end of meanings dump\n")
  (basic-save-buffer))

(provide 'mulvoc-data)

;;; end of mulvoc-data.el
