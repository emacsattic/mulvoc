/* mulvoc.c
   Time-stamp: <2009-03-15 19:16:22 jcgs>
   C definitions for MuLVoc data (multi-lingual vocabulary)

   Copyright J. C. G. Sturdy 2009
   
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3 of the License, or (at your
   option) any later version.
   
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

 */

#define MAX_CODE 8
#define MAX_NAME 24

typedef struct vocabulary_language {
  char code[MAX_CODE];
  char name[MAX_NAME];
  int language_number;
} vocabulary_language;

/* 
   This is the chain unit within a `meaning', representing one word in
   a set of words in different languages that have the same meaning,
   that is, that are translations of each other.
 */
typedef struct vocabulary_word {
  struct vocabulary_word *next;
  /* vocabulary_language *language; */
  int language;
  char *text;
} vocabulary_word;

/* 
   This is the head for a chain of words of the same meaning in
   several languages.
 */
typedef struct vocabulary_meaning {
  /* todo: record {type, sense, form} as part of the meaning; output this in both output formats from vocmerge */
  struct vocabulary_meaning *next;
  vocabulary_word *words;
  int part_of_speech;
  int sense_index;
  int form_index;
} vocabulary_meaning;

/* 
   This links a form (person, tense, case, etc) of a word with the
   text of that word.  There probably won't usually be a chain of them
   for the same word; on the other hand, if your file(s) contain all
   the persons and/or tenses of a verb, for example, there might be
   lots of these for each word.
 */

typedef struct form_chain_unit {
  struct form_chain_unit *next;
  int form_index;
  vocabulary_meaning *meaning;
} form_chain_unit;

/* 
   This chains together all the senses of a word, in a particular
   language and a particular part of speech.  For example, with the
   string "fear", and having picked "English" and "verb", we could
   have different senses for the common use and for the use as in
   "God-fearing".
 */
typedef struct sense_chain_unit {
  struct sense_chain_unit *next;
  int sense_index;
  form_chain_unit *forms;
} sense_chain_unit;

/*
  This chains together all the parts of speech which a particular word
  may be in a particular language; for example, the string "fear" in
  English may be a noun or a verb.
 */
typedef struct part_of_speech_chain_unit {
  struct part_of_speech_chain_unit *next;
  int part_of_speech;
  sense_chain_unit *senses;
} part_of_speech_chain_unit;

/*
  This chains together all the languages in which a `word' (character
  sequence) has a meaning.  For example, the string "fear" is a noun
  in Irish, and either a noun or a verb in English.
 */
typedef struct language_chain_unit {
  struct language_chain_unit *next;
  int language;
  part_of_speech_chain_unit *parts_of_speech;
} language_chain_unit;

/*
  This is the chain unit for the main hash table.  It is the starting
  point for all information about a particular `word' in the sense of
  a sequence of characters.
*/
typedef struct hash_chain_unit {
  struct hash_chain_unit *next;
  char *text;
  language_chain_unit *languages;
} hash_chain_unit;

/* bit flags for table.tracing */
#define TRACE_READ 1
#define TRACE_HEADERS 2
#define TRACE_PRAGMATA 3
#define TRACE_COMMENTS 4

/*
  The `table' is our top-level structure.

  It contains a hash table of words, which maps from a sequence of
  characters to the possible meanings of that sequence of characters.
 
  Each entry in the table is a chain of words with that hash, mapping
  to an `association list' (yes, MuLVoc started in Lisp) of languages
  in which that character sequence has a meaning, to the meanings in
  each language.

  The `meanings in each language' is a list mapping parts of speech to
  `senses', where a `sense' is a list mapping `forms' to `meanings',
  where a `meaning' is list mapping senses a collection of words in
  many languages of the same meaning.
 */
typedef struct vocabulary_table {
  int n_languages;
  int languages_table_size;	/* allocated size */
  vocabulary_language **languages;

  int n_parts_of_speech;
  int parts_of_speech_table_size; /* allocated size */
  char **parts_of_speech;

  int n_senses;
  int senses_table_size;	/* allocated size */
  char **senses;

  int n_forms;
  int forms_table_size;		/* allocated size */
  char **forms;

  int hash_max;
  hash_chain_unit **hash_table;

  int n_meanings;
  vocabulary_meaning *meanings;

  int tracing;
  unsigned int bytes_read;
  unsigned int bytes_allocated;
} vocabulary_table;


extern void mulvoc_initialize_table(vocabulary_table *table,
				    int hash_size,
				    int misc_table_size,
				    int tracing_flags);

extern int read_vocab_file(const char *filename,
			   vocabulary_table *table);

extern vocabulary_word *
find_language_word_in_meaning(vocabulary_meaning *meaning, int lang_index);

extern char *get_word_translations_string(vocabulary_table *table,
					  char *as_text,
					  char *result_section_format,
					  char *result_space,
					  int result_size);

extern void show_table_metadata(FILE *stream,
				vocabulary_table *table);

extern void show_table_data(FILE *stream,
			    vocabulary_table *table,
			    char *unspecified);

extern void mulvoc_output_html(FILE *output_stream,
			       vocabulary_table *table,
			       char *table_opts,
			       char *blank);

extern void mulvoc_output_csv(FILE *output_stream,
			      vocabulary_table *table);

/* mulvoc.h ends here */
