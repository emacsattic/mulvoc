;;;; mulvoc-custom.el -- custom definitions for mulvoc
;;; Time-stamp: <2006-04-21 08:58:47 jcgs>

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

(provide 'mulvoc-custom)

(defgroup mulvoc nil
  "MUlti-Lingual VOcabulary."
  :prefix "mulvoc-"
  :group 'mulvoc)

(defcustom mulvoc-cursor-active nil
  "When non-nil, translations of the word at point are displayed after each command.
This happens in buffers in which mulvoc-setup-hook-function has been run."
  :type 'boolean
  :group 'mulvoc)

(defcustom mulvoc-abbrev-active t
  "When non-nil, abbreviation expansion is used to display translations of known words as they are entered."
  :type 'boolean
  :group 'mulvoc)

(defcustom mulvoc-dictionaries-directories
  (list (substitute-in-file-name "~/dictionaries"))
  "*Where to find the dictionary CSV files."
  :group 'mulvoc)

(defcustom mulvoc-dictionaries-pattern ".csv$"
  "*Pattern describing which directories in mulvoc-dictionaries-directories to load."
  :type 'regexp
  :group 'mulvoc)

(defcustom mulvoc-etc-directory "i:/common/open-projects/mulvoc/etc/"
  "Directory containing language-related data other than the dictionaries."
  :type 'directory
  :group 'mulvoc)

(defcustom mulvoc-use-tooltips t
  "Whether to use tooltips, if they are available."
  :type 'boolean
  :group 'mulvoc)

(defcustom mulvoc-displayed-languages '("DUT" "GER" "NRR" "GLI")
  "Which languages to display in.
If nil, all known languages are used."
  :group 'mulvoc)

(defcustom mulvoc-setup-hook nil
  "*Functions to run just before setting up mulvoc data structures."
  :type 'hook
  :group 'mulvoc)

(defcustom mulvoc-post-setup-hook nil
  "*Functions to run just after setting up mulvoc data structures."
  :type 'hook
  :group 'mulvoc)

(defcustom mulvoc-alias-separator "/"
  "String to separate aliases."
  :type 'string
  :group 'mulvoc)

(defcustom mulvoc-alias-separator-pattern "\\(/\\)\\|\\(, *\\)"
  "Pattern to separate aliases."
  :type 'regexp
  :group 'mulvoc)

(defcustom mulvoc-use-overlays 3
  "Experimental feature.
If nil, mulvoc does not use overlays.
If a number, it uses that many of them, per buffer.
If anything else, it uses any number of them."
  :group 'mulvoc)

(defcustom mulvoc-dictionary-left-column-limit 18
  "*The maximum width for the leftmost column in the dictionary."
  :group 'mulvoc
  :type 'integer)

(defcustom mulvoc-debug nil
  "Controls printing of messages from mulvoc-setup and possibly other functions."
  :group 'mulvoc
  :type 'boolean)

(defcustom mulvoc-comment-hook nil
  "List of functions to try on comments in the file. Called with word and language as args."
  :group 'mulvoc)

(defcustom mulvoc-keep-dictionary-buffers t
  "Whether to leave the dictionary buffers instead of deleting them.
This allows them to be updated."
  :group 'mulvoc
  :type 'boolean)

;;; end of mulvoc-custom.el
