;;;; mulvoc-interactive-edit.el -- interactive vocabulary editing for mulvoc
;;; Time-stamp: <2008-11-23 22:22:54 jcgs>

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

(provide 'mulvoc-interactive-edit)

(defun mulvoc-read-language (prompt &optional extra)
  "Ask the user for a language, prompting with PROMPT.
Optional second argument gives extra things that can be read as well as languages."
  (let ((completion-ignore-case t))
    (completing-read prompt
		     (append extra
			     (mapcar (lambda (langdescr)
				       (list (symbol-name (car langdescr))))
				     mulvoc-languages))
		     nil
		     t)))

(defun mulvoc-read-languages (prompt)
  "Read a series of language names."
  (catch 'done
    (let ((languages nil)
	  (language nil)
	  (usual-full-prompt (concat prompt "(Q to quit) "))
	  (full-prompt (concat prompt "(Q to quit, ALL for all) ")))
      (while (not (string= (setq language (mulvoc-read-language
					   full-prompt
					   '(("Q") ("ALL"))))
			   "Q"))
	(setq full-prompt usual-full-prompt)
	(when (and (null languages)	; allow for the first one only
		   (string= (downcase language) "all"))
	  (throw 'done t))
	(push language languages))
      (nreverse languages))))

(defvar mulvoc-input-methods-for-languages
  '(("JPN" . "japanese-hiragana")
    ("CMN" . "chinese-tonepy")
    ("KHK" . "mongolian-cyrillic"))
  "Input methods for languages.")

(defun mulvoc-input-method-for-language (language)
  "Select a suitable input method for LANGUAGE."
  (message "Want input method for %S" language)
  (let ((pair (assoc language mulvoc-input-methods-for-languages)))
    (if pair
	(cdr pair)
      nil)))

(defun read-word-in-language (prompt original-contents language)
  "Using PROMPT and offering ORIGINAL-CONTENTS, read a word in LANGUAGE.
The input method is set to one appropriate for LANGUAGE."
  (let* ((om nil)
	 (im (mulvoc-input-method-for-language language))
	 (minibuffer-setup-hook (lambda ()
				  (setq om current-input-method)
				  (set-input-method im)))
	 (minibuffer-exit-hook (lambda ()
				 (set-input-method om))))
    (save-window-excursion
      (quail-show-keyboard-layout)	; todo: cache this display
      (read-from-minibuffer prompt
			    original-contents
			    nil		     ; keymap
			    nil		     ; read as lisp
			    nil		     ; history
			    nil		     ; default-value
			    t		     ; inherit-input-method
			    ))))

;;;###autoload
(defun mulvoc-edit-translation (from-lang from-word to-lang to-word)
  "Read a translation from FROM-LANG of FROM-WORD, into TO-LANG, which is TO-WORD"
  (interactive
   (progn
     (mulvoc-ensure-loaded)
     (let* ((from-word (completing-read "Translate word: " mulvoc-words nil t))
	    (from-word-data (symbol-value (intern (downcase from-word) mulvoc-words)))
	    (from-lang (if (null (cdr from-word-data))
			   (caar from-word-data)
			 (completing-read "From language: "
					  from-word-data
					  nil t)))
	    (to-lang (mulvoc-read-language (format "Translate %s from %s to language: "
						  from-word from-lang)))
	    (old-word (assoc to-lang from-word-data))
	    (to-word (read-word-in-language
		      (format "%s in %s is what in %s? " from-word from-lang to-lang)
		      (cdr old-word)	; initial-contents
		      to-lang)))
       (list from-lang from-word to-lang to-word))))

  (when mulvoc-debug (message "Got %S" to-word))
  (mulvoc-define-meaning
   (list (cons from-lang from-word)
	 (cons to-lang to-word)))
  )

;;;###autoload
(defun mulvoc-read-word-pairs (from-language to-language)
  "Read pairs of words in FROM-LANGUAGE and TO-LANGUAGE."
  (interactive
   (let* ((from-language (mulvoc-read-language "First language: "))
	  (to-language (mulvoc-read-language "Second language: ")))
     (list from-language to-language)))
  (let ((prompt-a (format "Word in %s: " from-language))
	(prompt-b (format "Word in %s: " to-language))
	(sense nil)
	(word-a  nil)
	(word-b nil)
	(type nil))
    (while (not (and (string= word-a "")
		     (string= word-b "")))
      (setq word-a (read-word-in-language prompt-a nil from-language)
	    type (completing-read "Type: " mulvoc-parts-of-speech)
	    ;; todo: read sense
	    word-b (read-word-in-language prompt-b
					  ;; todo: find and use old content
					  nil
					  to-language))
      (let* ((mulvoc-debug t)
	     (new-meaning (mulvoc-define-meaning
			   (list
			    (cons "#TYPE" type
				  ;; todo: include sense
				  )
			    (cons from-language word-a)
			    (cons to-language word-b))))
	     )
	(message "new-meaning is %S" new-meaning)
	))))

(defun assoc-sublist-tails (key list)
  "Like assoc, but compares KEY with the tails of the members of LIST."
  (catch 'found
    (while list
      (let ((sub-result (assoc key (cdar list))))
	(when sub-result
	  (throw 'found sub-result))
	(setq list (cdr list))))
    nil))

(defun mulvoc-present-in-A-missing-from-B (a b)
  "Prepare a sorted list of words defined in language A but not in B."
  (let ((atoms nil))
    (mapatoms
     (function
      (lambda (atom)
	(when (and atom (boundp atom))
	  (let ((value (symbol-value atom)))
	    (let ((pair (assoc a value)))
	      (when (and pair
			 (not (assoc-sublist-tails b (cdr pair))))
		(push atom atoms)))))))
     mulvoc-words)
    (sort atoms 'string<)))

(defvar mulvoc-missing-format "%s\n"
  "Format for indicating missing items.")

;;;###autoload
(defun mulvoc-list-missing-words (present-in absent-from)
  "List the words defined in PRESENT-IN that are not defined in ABSENT-FROM."
  (interactive
   (progn
     (mulvoc-ensure-loaded)
     (let ((a (mulvoc-read-language "Present in: "))
	   (b (mulvoc-read-language "Absent from: ")))
       (list a b))))
  (mulvoc-ensure-loaded)
  (with-output-to-temp-buffer (format "*Missing in %s but present in %s*" absent-from present-in)
    (mapcar (lambda (w)
	      (princ (format mulvoc-missing-format (symbol-name w))))
	    (mulvoc-present-in-A-missing-from-B present-in absent-from))))

;;; todo: mulvoc-make-missing-words-spreadsheet

;;;###autoload
(defun mulvoc-make-missing-words-table (file present-in absent-from)
  "List, as an HTML table in FILE, the words defined in PRESENT-IN that are not defined in ABSENT-FROM.
This produces a page you can go through with a dictionary, looking up the further words you need to learn."
  (interactive
   (progn
     (mulvoc-ensure-loaded)
     (let ((f (read-file-name "Make missing list in file: "))
	   (a (mulvoc-read-language "Present in: "))
	   (b (mulvoc-read-language "Absent from: ")))
       (list f a b))))
  (mulvoc-ensure-loaded)
  (find-file file)
  (erase-buffer)
  (insert "<html><head><title>Words to look up in dictionary</title></head><body><table border width=\"100%\">\n")
  (insert "<tr><th>" present-in
	  "</th><th width=\"60%\">" absent-from
	  "</th></tr>")
  (let ((mulvoc-missing-format "<tr><td align=\"right\">%s</td><td>&nbsp;</td></tr>\n"))
    (mapcar (lambda (w)
	      (insert (format mulvoc-missing-format w)))
	    (mulvoc-present-in-A-missing-from-B present-in absent-from)))
  (insert "</table></body>\n")
  (basic-save-buffer))

;;;###autoload
(defun mulvoc-read-missing-words (present-in absent-from)
  "Read the words defined in PRESENT-IN that are not defined in ABSENT-FROM.
They are asked in alphabetical order in PRESENT-IN, so you can work your way
through a dictionary adding to your vocabulary."
  (interactive
   (progn
     (mulvoc-ensure-loaded)
     (let ((a (mulvoc-read-language "Present in: "))
	   (b (mulvoc-read-language "Absent from: ")))
       (list a b))))
  (mulvoc-ensure-loaded)
  (let ((words (mulvoc-present-in-A-missing-from-B present-in absent-from)))
    (dolist (word words)
      (let ((translation
	     (read-word-in-language
	      (format "%s in %s is what in %s? " word present-in absent-from)
	      ""
	      absent-from)))
	(unless (string= translation "")
	  ;; todo: define in absent-from
	  (when mulvoc-debug (message "updating %s-->%s" word translation))
	    (mulvoc-define-meaning
	     (list (cons present-in (symbol-name word))
		   (cons absent-from translation))))))))

;;; end of mulvoc-interactive-edit.el
