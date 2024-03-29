/* mulvoc.c
   Time-stamp: <2009-06-24 09:23:30 jcgs>
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

typedef struct language_property {
  struct language_property *next;
  int key;
  char *value;
} language_property;

#define MAX_CODE 8
#define MAX_NAME 24

typedef struct vocabulary_language {
  char code[MAX_CODE];
  char name[MAX_NAME];
  int language_number;
  language_property *properties;
} vocabulary_language;

/* 
   This is the chain unit within a `meaning', representing one word in
   a set of words in different languages that have the same meaning,
   that is, that are translations of each other.
 */
typedef struct vocabulary_word {
  struct vocabulary_word *next;
  int language;
  char *text;
} vocabulary_word;

typedef struct extra_column_cell {
  struct extra_column_cell *next;
  int extra_column_index;
  char *value;
} extra_column_cell;

typedef struct vocabulary_location {
  struct vocabulary_location *next;
  char *file;
  int line;
} vocabulary_location;

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
  extra_column_cell *extra_columns;
  vocabulary_location *whence;
#ifdef debug
  int meaning_id;
#endif
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

/* 
   This is used for sorting a table ready for output sorted by a
   particular key language.
 */

typedef struct sorted_vocab_word {
  char *as_text;
  vocabulary_meaning *meaning;
} sorted_vocab_word;

/* bit flags for table.tracing */
#define TRACE_READ 1
#define TRACE_HEADERS 2
#define TRACE_PRAGMATA 4
#define TRACE_COMMENTS 8
#define TRACE_MERGE 16
#define TRACE_ORIGINS 32	/* not an immediate output, but saves extra data */

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

  int n_parts_of_speech;	  /* used size */
  int parts_of_speech_table_size; /* allocated size */
  char **parts_of_speech;

  int n_senses;			/* used size */
  int senses_table_size;	/* allocated size */
  char **senses;

  int n_forms;			/* used size */
  int forms_table_size;		/* allocated size */
  char **forms;

  /* `properties' are attached to language descriptions.  They come
     from the header rows of the file.
  */
  int n_properties;		/* used size */
  int property_table_size;	/* allocated size */
  char **properties;

  int n_extra_columns;		/* used size */
  int extra_column_table_size;	/* allocated size */
  char **extra_column_names;

  /* This is the actual vocabulary data, organized as a hash table
     from word texts.
  */
  int hash_max;
  hash_chain_unit **hash_table;

  /* This is the actual vocabulary data, organized as `meanings' (I
     think that professionals in this field may call them something
     like `terms').
  */
  int n_meanings;
  vocabulary_meaning *meanings;

  /* The files we got the data from (not that it necessarily came from
     files).  This is the same format of data as the corresponding
     information for individual meanings, but here we use the `line
     number' to give the number of lines in the file.  The
     vocabulary_location data in the meanings uses the filename
     strings in this list for its filenames; this list is here
     primarily to manage the allocation of those names. */
  vocabulary_location *files_used;

#ifdef debug
  int next_meaning_id;
#endif

  int n_keyed;
  sorted_vocab_word *keyed;

  int tracing;
  unsigned int bytes_read;
  unsigned int bytes_allocated;

  char *location_format;
} vocabulary_table;


extern void mulvoc_initialize_table(vocabulary_table *table,
				    int hash_size,
				    int misc_table_size,
				    int tracing_flags);

extern void mulvoc_destroy_table(vocabulary_table *table);

extern int read_vocab_file(const char *filename,
			   vocabulary_table *table);

extern vocabulary_word *find_language_word_in_meaning(vocabulary_meaning *meaning,
						      int lang_index);

extern int count_language_words(vocabulary_table *table,
				int language_index,
				int all_synonyms);

extern int list_language_words(vocabulary_table *table,
			       int language_index,
			       char ***word_array_ptr);

extern int language_indices(vocabulary_table *table,
			    char *languages_string,
			    int **languages);

extern int vocabulary_keyed_by_language(vocabulary_table *table,
					int key_language_index,
					int all_synonyms,
					int sorted);


extern char *get_word_translations_string(vocabulary_table *table,
					  char *as_text,
					  int language_in,
					  int language_out,
					  int pos_in,
					  int sense_in,
					  int form_in,
					  char *result_section_format,
					  char *result_space,
					  int result_size);

extern void show_meaning(FILE *stream, vocabulary_table *table, vocabulary_meaning *meaning, char *label);
extern void show_meanings(FILE *stream, vocabulary_table *table, char *start_label, char *end_label, char *row_label);

extern void show_table_metadata(FILE *stream,
				vocabulary_table *table);

extern void show_table_data(FILE *stream,
			    vocabulary_table *table,
			    char *unspecified);

extern int part_of_speech_index(vocabulary_table *table, char *as_text);
extern int sense_index(vocabulary_table *table, char *as_text);
extern int form_index(vocabulary_table *table, char *as_text);
extern int property_index(vocabulary_table *table, char *as_text);
extern int extra_column_index(vocabulary_table *table, char *as_text, int text_length);

extern char *language_property_string(vocabulary_table *table,
				      int language_index,
				      int property_index);

extern extra_column_cell *find_extra_cell(vocabulary_meaning *meaning, int cell_type);

extern hash_chain_unit *get_word_data(vocabulary_table *table,
				      char *as_text);

extern language_chain_unit *get_word_language_data(vocabulary_table *table,
						   hash_chain_unit *word_data,
						   int language);
extern part_of_speech_chain_unit *get_word_language_type_data(vocabulary_table *table,
							      language_chain_unit* word_language_data,
							      int type_index);
extern sense_chain_unit *get_word_language_type_sense_data(vocabulary_table *table,
							   part_of_speech_chain_unit* word_language_type_data,
							   int sense_index);
extern form_chain_unit* get_word_language_type_sense_form_data(vocabulary_table *table,
							       sense_chain_unit* word_language_type_sense_data,
							       int form_index);

extern void mulvoc_output_html(FILE *output_stream,
			       vocabulary_table *table,
			       int *languages,
			       int n_languages,
			       int key_idx,
			       char *table_opts,
			       char *blank,
			       int table_controlled);

extern void mulvoc_output_csv(FILE *output_stream,
			      vocabulary_table *table,
			      int *languages,
			      int n_languages,
			      int key_idx);

extern void mulvoc_output_data(FILE *output_stream,
			       vocabulary_table *table,
			       int *languages,
			       int n_languages,
			       int key_idx);

extern void mulvoc_output_delimited(FILE *output_stream,
				    vocabulary_table *table,
				    int *languages,
				    int n_languages,
				    int key_idx,
				    char *row_start,
				    char *between_cells,
				    char *row_end);

extern unsigned int count_meaning_words(vocabulary_table *table, vocabulary_meaning *meaning, int verbose);
extern int check_table_data(vocabulary_table *table, int *char_count_ptr, int verbose);

/* mulvoc.h ends here */
