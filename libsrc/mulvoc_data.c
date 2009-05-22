/* mulvoc_data.c
   Time-stamp: <2009-05-17 20:54:24 jcgs>
   Read and manage MuLVoc data (multi-lingual vocabulary)

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

/* the next few for stat(2) */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#define _GNU_SOURCE
#include <string.h>
#include <ctype.h>

/* If you want the debugging enabled, you should turn it on in the
   header, too (i.e. before the include of the header), as it uses
   extra structure fields. */
/* #define debug 1 */

#include "mulvoc.h"

void *
mulvoc_malloc(vocabulary_table *table, unsigned int size)
{
  table->bytes_allocated += size;
  return malloc(size);
}

void
mulvoc_free(vocabulary_table *table, void *mem)
{
  free(mem);
}

void
mulvoc_initialize_table(vocabulary_table *table,
			int hash_size,
			int misc_table_size,
			int tracing_flags)
{
  int i;
  hash_chain_unit **hash;

  table->tracing = tracing_flags;
  table->bytes_read = 0;
  table->bytes_allocated = sizeof(struct vocabulary_table);

  table->languages_table_size = misc_table_size;
  table->languages = (vocabulary_language**)mulvoc_malloc(table, table->languages_table_size
						  * sizeof(vocabulary_language*));
  table->n_languages = 0;

  table->parts_of_speech_table_size = misc_table_size;
  table->parts_of_speech = (char**)mulvoc_malloc(table, table->parts_of_speech_table_size
					 * sizeof(char*));
  table->n_parts_of_speech = 0;

  table->senses_table_size = misc_table_size;
  table->senses = (char**)mulvoc_malloc(table, table->senses_table_size
					 * sizeof(char*));
  table->n_senses = 0;

  table->forms_table_size = misc_table_size;
  table->forms = (char**)mulvoc_malloc(table, table->forms_table_size
					 * sizeof(char*));
  table->n_forms = 0;

  table->n_properties = 0;
  table->property_table_size = misc_table_size;
  table->properties = (char**)mulvoc_malloc(table, table->property_table_size
					    * sizeof(char*));

  table->n_extra_columns = 0;
  table->extra_column_table_size = misc_table_size;
  table->extra_column_names = (char**)mulvoc_malloc(table, table->extra_column_table_size
					    * sizeof(char*));

  table->hash_max = hash_size;
  table->hash_table = hash = (hash_chain_unit**)mulvoc_malloc(table, table->hash_max * sizeof(hash_chain_unit*));

  for (i = hash_size - 1; i >= 0; i--) {
    hash[i] = NULL;
  }

  table->n_meanings = 0;
  table->meanings = NULL;
#ifdef debug
  table->next_meaning_id = 0;
#endif

  table->n_keyed = 0;
  table->keyed = NULL;
}

int
language_index(vocabulary_table *table,
	       char *language_code,
	       int code_length)
{
  int i;
  int n = table->n_languages;

  if (code_length > MAX_CODE) {
    code_length = MAX_CODE;
  }

  for (i = 0; i < n; i++) {
    vocabulary_language *lang = table->languages[i];
    if (strncmp(language_code,
		table->languages[i]->code,
		code_length) == 0) {
      return i;
    }
  }

  {
    struct vocabulary_language *new_language = (vocabulary_language*)mulvoc_malloc(table, sizeof(vocabulary_language));
    char *code = (char*)(&(new_language->code[0]));
    char *name = (char*)(&(new_language->name[0]));
    strncpy(code, language_code, code_length);
    code[code_length] = (char)'\0';
    name[0] = (char)'\0';

    if (table->n_languages == table->languages_table_size) {
      int new_table_size = table->languages_table_size * 2;
      vocabulary_language **new_table =
	(vocabulary_language**)mulvoc_malloc(table, new_table_size
					     * sizeof(vocabulary_language*));
      // fprintf(stderr, "enlarging language table from %d\n", table->languages_table_size);
      for (i = 0; i < n; i++) {
	// fprintf(stderr, " copying %d\n", i);
	new_table[i] = table->languages[i];
      }
      for (i; i < new_table_size; i++) {
	// fprintf(stderr, " nulling %d\n", i);
	new_table[i] = NULL;
      }
      mulvoc_free(table, table->languages);
      table->languages = new_table;
      table->languages_table_size = new_table_size;
    }

    // fprintf(stderr, "setting %d to new language\n", n);
    new_language->properties = NULL;
    new_language->language_number = n;
    table->languages[n] = new_language;
    table->n_languages = n + 1;
    return n;
  }
}

vocabulary_language*
get_language(vocabulary_table *table,
	     char *language_code,
	     int code_length)
{
  /* The natural code to write here hits what I suspect is a compiler
 bug in the case when the table has been doubled in size by
 language_index(); if I split it to use an intermediate variable, it
 works OK. */
#if 0
  return table->languages[language_index(table, language_code, code_length)];
#else
  int idx = language_index(table, language_code, code_length);
  // fprintf(stderr, "get_language looking for %.*s, got index %d\n", code_length, language_code, idx);
  return table->languages[idx];
#endif
}

void
show_languages(FILE *stream,
	       vocabulary_table *table)
{
  int i;
  int n = table->n_languages;

  for (i = 0; i < n; i++) {
    vocabulary_language *lang = table->languages[i];
    language_property *props = lang->properties;
    fprintf(stream, "% 3d: %s (%s)\n", lang->language_number, &(lang->code[0]), &(lang->name[0]));
    for (; props != NULL; props = props->next) {
      fprintf(stream, "     %s = %s\n", table->properties[props->key], props->value);
    }
  }
}

void
show_types(FILE *stream,
	   vocabulary_table *table)
{
  int i;
  int n = table->n_parts_of_speech;
  char **parts_of_speech = table->parts_of_speech;

  for (i = 0; i < n; i++) {
    fprintf(stream, "% 3d: %s\n", i, parts_of_speech[i]);
  }
}

void
show_senses(FILE *stream,
	    vocabulary_table *table)
{
  int i;
  int n = table->n_senses;
  char **senses = table->senses;

  for (i = 0; i < n; i++) {
    fprintf(stream, "% 3d: %s\n", i, senses[i]);
  }
}

void
show_forms(FILE *stream,
	   vocabulary_table *table)
{
  int i;
  int n = table->n_forms;
  char **forms = table->forms;

  for (i = 0; i < n; i++) {
    fprintf(stream, "% 3d: %s\n", i, forms[i]);
  }
}

void
show_properties(FILE *stream,
		vocabulary_table *table)
{
  int i;
  int n = table->n_properties;
  char **properties = table->properties;

  for (i = 0; i < n; i++) {
    fprintf(stream, "% 3d: %s\n", i, properties[i]);
  }
}

void
show_extra_columns(FILE *stream,
		   vocabulary_table *table)
{
  int i;
  int n = table->n_extra_columns;
  char **names = table->extra_column_names;

  for (i = 0; i < n; i++) {
    fprintf(stream, "% 3d: %s\n", i, names[i]);
  }
}

void
show_table_metadata(FILE *stream,
		    vocabulary_table *table)
{
  fprintf(stream, "%d bytes read to make table\n", table->bytes_read);
  fprintf(stream, "%d bytes allocated in table (%f times file size)\n",
	  table->bytes_allocated,
	  ((float)table->bytes_allocated) / ((float)table->bytes_read));
  fprintf(stream, "Languages:\n");
  show_languages(stream, table);
  fprintf(stream, "Types:\n");
  show_types(stream, table);
  fprintf(stream, "Senses:\n");
  show_senses(stream, table);
  fprintf(stream, "Forms:\n");
  show_forms(stream, table);
  fprintf(stream, "Properties:\n");
  show_properties(stream, table);
  fprintf(stream, "Extra columns:\n");
  show_extra_columns(stream, table);
}

void
show_meaning(FILE *stream, vocabulary_table *table, vocabulary_meaning *meaning, char *label)
{
  vocabulary_word *word;
  extra_column_cell *x;
  fprintf(stream, label);
  if (meaning == NULL) {
    fprintf(stream, "<empty meaning>");
  } else {
#ifdef debug
    fprintf(stream, "[%05d] ", meaning->meaning_id);
#endif
    for (word = meaning->words;
	 word != NULL;
	 word=word->next) {
      fprintf(stream, "%s: %s; ", table->languages[word->language]->code, word->text);
    }
  }
  for (x = meaning->extra_columns; x != NULL; x = x->next) {
    fprintf(stream, "; {%s=%s}", table->extra_column_names[x->extra_column_index], x->value);
  }
  fprintf(stream, "\n");
}

void
show_meanings(FILE *stream, vocabulary_table *table, char *start_label, char *end_label, char *row_label)
{
  vocabulary_meaning *meaning;
  int n = 0;
  fprintf(stream, start_label);
  for (meaning = table->meanings;
       meaning != NULL;
       meaning=meaning->next) {
    show_meaning(stream, table, meaning, row_label);
    n++;
  }
  fprintf(stream, end_label, n);
}

void
show_table_data(FILE *stream,
		vocabulary_table *table,
		char *unspecified)
{
  int n = table->hash_max;
  int i;

  for (i = 0; i < n; i++) {
    hash_chain_unit *chain_link;
    for (chain_link = table->hash_table[i];
	 chain_link != NULL;
	 chain_link = chain_link->next) {
      language_chain_unit *language;
      for (language = chain_link->languages;
	   language != NULL;
	   language = language->next) {
	part_of_speech_chain_unit *part_of_speech;
	for (part_of_speech = language->parts_of_speech;
	     part_of_speech != NULL;
	     part_of_speech = part_of_speech->next) {
	  int p_o_s_index = part_of_speech->part_of_speech;
	  sense_chain_unit *sense;
	  char *p_o_s_descr = ((p_o_s_index >= 0)
			       ? table->parts_of_speech[p_o_s_index]
			       : unspecified);
	  for (sense = part_of_speech->senses;
	       sense != NULL;
	       sense=sense->next) {
	    int s_index = sense->sense_index;
	    form_chain_unit *form;
	    char *sense_descr = ((s_index >= 0)
				 ? table->senses[s_index]
				 : unspecified);
	    for (form = sense->forms;
		 form != NULL;
		 form = form->next) {
	      int f_index = form->form_index;
	      vocabulary_meaning *meaning = form->meaning;
	      vocabulary_word *word;
	      fprintf(stream, "% 20.20s: % 10.10s: % 10.10s: % 10.10s: % 10.10s: ",
		      chain_link->text,
		      table->languages[language->language]->code,
		      p_o_s_descr,
		      sense_descr,
		      f_index >= 0 ? table->forms[f_index] : unspecified);
	      show_meaning(stream, table, meaning, "");
	    }
	  }
	}
      }
    }
  }
}

unsigned int
count_meaning_words(vocabulary_table *table,
		    vocabulary_meaning *meaning,
		    int verbose)
{
  unsigned int count = 0;
  
  vocabulary_word *word;
  if (meaning == NULL) {
  } else {
    for (word = meaning->words;
	 word != NULL;
	 word=word->next) {
      if (verbose) { fprintf(stderr, "            %lx: %s --> %lx\n", (unsigned long)word, word->text, (unsigned long)(word->next)); }
      count++;
    }
  }
  return count;
}

unsigned int
check_table_data(vocabulary_table *table,
		 int verbose)
{
  int words_in_meanings = 0;
  int n = table->hash_max;
  int i;

  if (verbose) { fprintf(stderr, "checking table at %lx\n", (unsigned long)table); }

  for (i = 0; i < n; i++) {
    hash_chain_unit *chain_link;
    for (chain_link = table->hash_table[i];
	 chain_link != NULL;
	 chain_link = chain_link->next) {
      if (verbose) { fprintf(stderr, "  chain_link[%d]=%lx \"%s\"\n", i, (unsigned long)chain_link, chain_link->text); }
      language_chain_unit *language;
      for (language = chain_link->languages;
	   language != NULL;
	   language = language->next) {
	part_of_speech_chain_unit *part_of_speech;
	if (verbose) { fprintf(stderr, "    language=%lx\n", (unsigned long)language); }
	for (part_of_speech = language->parts_of_speech;
	     part_of_speech != NULL;
	     part_of_speech = part_of_speech->next) {
	  sense_chain_unit *sense;
	  if (verbose) { fprintf(stderr, "      part_of_speech=%lx\n", (unsigned long)part_of_speech); }
	  for (sense = part_of_speech->senses;
	       sense != NULL;
	       sense=sense->next) {
	    form_chain_unit *form;
	    if (verbose) { fprintf(stderr, "        sense=%lx\n", (unsigned long)sense); }
	    for (form = sense->forms;
		 form != NULL;
		 form = form->next) {
	      if (verbose ) { fprintf(stderr, "          form=%lx\n", (unsigned long)form); }
	      words_in_meanings += count_meaning_words(table, form->meaning, verbose);
	    }
	  }
	}
      }
    }
  }
  return words_in_meanings;
}

int
part_of_speech_index(vocabulary_table *table, char *as_text)
{
  int i;
  int n = table->n_parts_of_speech;
  char **parts_of_speech = table->parts_of_speech;
  char *new_text;

  if ((as_text == NULL) || (as_text[0] == '\0')) {
    return -1;
  }

  for (i = 0; i < n; i++) {
    if (strcmp(as_text, parts_of_speech[i]) == 0) {
      return i;
    }
  }
  if (n == table->parts_of_speech_table_size) {
    int new_size = table->parts_of_speech_table_size * 2;
    char **new_table = (char**)mulvoc_malloc(table, new_size * sizeof(char*));
    for (i = 0; i < n; i++) {
      new_table[i] = parts_of_speech[i];
    }
    mulvoc_free(table, parts_of_speech);
    table->parts_of_speech = new_table;
    table->parts_of_speech_table_size = new_size;
  }
  new_text = (char*)mulvoc_malloc(table, strlen(as_text)+1);
  strcpy(new_text, as_text);
  table->parts_of_speech[n] = new_text;
  table->n_parts_of_speech = n+1;
  return n;
}

int
sense_index(vocabulary_table *table,
	    char *as_text)
{
  int i;
  int n = table->n_senses;
  char **senses = table->senses;
  char *new_text;

  if ((as_text == NULL) || (as_text[0] == '\0')) {
    return -1;
  }

  for (i = 0; i < n; i++) {
    if (strcmp(as_text, senses[i]) == 0) {
      return i;
    }
  }

  if (n == table->senses_table_size) {
    int new_size = table->senses_table_size * 2;
    char **new_table = (char**)mulvoc_malloc(table, new_size * sizeof(char*));
    for (i = 0; i < n; i++) {
      new_table[i] = senses[i];
    }
    mulvoc_free(table, senses);
    table->senses = new_table;
    table->senses_table_size = new_size;
  }
  new_text = (char*)mulvoc_malloc(table, strlen(as_text)+1);
  strcpy(new_text, as_text);
  table->senses[n] = new_text;
  table->n_senses = n+1;
  return n;
}

int
form_index(vocabulary_table *table,
	   char *as_text)
{
  int i;
  int n = table->n_forms;
  char **forms = table->forms;
  char *new_text;

  if ((as_text == NULL) || (as_text[0] == '\0')) {
    return -1;
  }

  for (i = 0; i < n; i++) {
    if (strcmp(as_text, forms[i]) == 0) {
      return i;
    }
  }
  if (n == table->forms_table_size) {
    int new_size = table->forms_table_size * 2;
    char **new_table = (char**)mulvoc_malloc(table, new_size * sizeof(char*));
    for (i = 0; i < n; i++) {
      new_table[i] = forms[i];
    }
    mulvoc_free(table, forms);
    table->forms = new_table;
    table->forms_table_size = new_size;
  }
  new_text = (char*)mulvoc_malloc(table, strlen(as_text)+1);
  strcpy(new_text, as_text);
  table->forms[n] = new_text;
  table->n_forms = n+1;
  return n;
}

int
property_index(vocabulary_table *table,
	   char *as_text)
{
  int i;
  int n = table->n_properties;
  char **properties = table->properties;
  char *new_text;

  if ((as_text == NULL) || (as_text[0] == '\0')) {
    return -1;
  }

  for (i = 0; i < n; i++) {
    if (strcmp(as_text, properties[i]) == 0) {
      return i;
    }
  }
  if (n == table->property_table_size) {
    int new_size = table->property_table_size * 2;
    char **new_table = (char**)mulvoc_malloc(table, new_size * sizeof(char*));
    for (i = 0; i < n; i++) {
      new_table[i] = properties[i];
    }
    mulvoc_free(table, properties);
    table->properties = new_table;
    table->property_table_size = new_size;
  }
  new_text = (char*)mulvoc_malloc(table, strlen(as_text)+1);
  strcpy(new_text, as_text);
  table->properties[n] = new_text;
  table->n_properties = n+1;
  return n;
}

int
extra_column_index(vocabulary_table *table,
		   char *as_text,
		   int text_length)
{
  int i;
  int n = table->n_extra_columns;
  char **columns = table->extra_column_names;
  char *new_text;

  if ((as_text == NULL) || (as_text[0] == '\0')) {
    return -1;
  }

  for (i = 0; i < n; i++) {
    if (strncmp(as_text, columns[i], text_length) == 0) {
      return i;
    }
  }
  if (n == table->extra_column_table_size) {
    int new_size = table->extra_column_table_size * 2;
    char **new_table = (char**)mulvoc_malloc(table, new_size * sizeof(char*));
    for (i = 0; i < n; i++) {
      new_table[i] = columns[i];
    }
    mulvoc_free(table, columns);
    table->extra_column_names = new_table;
    table->extra_column_table_size = new_size;
  }
  new_text = (char*)mulvoc_malloc(table, text_length+1);
  strncpy(new_text, as_text, text_length);
  new_text[text_length] = '\0';
  table->extra_column_names[n] = new_text;
  table->n_extra_columns = n+1;
  return n;
}

extra_column_cell *
find_extra_cell(vocabulary_meaning *meaning, int cell_type)
{
  extra_column_cell *x;
  for (x = meaning->extra_columns; x != NULL; x = x->next) {
    if (x->extra_column_index == cell_type) {
      return x;
    }
  }
  return NULL;
}

char *
language_property_string(vocabulary_table *table,
			 int language_index,
			 int property_index)
{
  vocabulary_language *language = table->languages[language_index];
  language_property *p;

  for (p = language->properties;
       p != NULL;
       p = p->next) {
    if (p->key = property_index) {
      return p->value;
    }
  }
  return NULL;
}

hash_chain_unit*
get_word_data(vocabulary_table *table,
	      char *as_text)
{
  int ih;
  char c;
  unsigned int hash = 0;
  hash_chain_unit* chain;

  for (ih = 0; (c = as_text[ih]) != '\0'; ih++) {
    hash += c;
  }
  hash %= table->hash_max;

  for (chain = table->hash_table[hash];
       chain != NULL;
       chain = chain->next) {
    if (strcmp(as_text, chain->text) == 0) {
      return chain;
    }
  }

  chain = (hash_chain_unit*)mulvoc_malloc(table, sizeof(hash_chain_unit));
  chain->languages = NULL;
  chain->text = as_text;
  chain->next = table->hash_table[hash];
  table->hash_table[hash] = chain;

  return chain;
}

language_chain_unit*
get_word_language_data(vocabulary_table *table,
		       hash_chain_unit *word_data,
		       int language)
{
  language_chain_unit *lang;

  if (word_data == NULL) {
    return NULL;
  }

  for (lang = word_data->languages;
       lang != NULL;
       lang = lang->next) {
    if (lang->language == language) {
      return lang;
    }
  }

  lang = (language_chain_unit*)mulvoc_malloc(table, sizeof(language_chain_unit));
  lang->language = language;
  lang->parts_of_speech = NULL;
  lang->next = word_data->languages;
  word_data->languages = lang;
  return lang;
}

part_of_speech_chain_unit*
get_word_language_type_data(vocabulary_table *table,
			    language_chain_unit* word_language_data,
			    int type_index)
{
  part_of_speech_chain_unit *posu;

  if (word_language_data == NULL) {
    return NULL;
  }

  for (posu = word_language_data->parts_of_speech;
       posu != NULL;
       posu = posu->next) {
    if (posu->part_of_speech == type_index) {
      return posu;
    }
  }

  posu = (part_of_speech_chain_unit*)mulvoc_malloc(table, sizeof(part_of_speech_chain_unit));
  posu->part_of_speech = type_index;
  posu->senses = NULL;
  posu->next = word_language_data->parts_of_speech;
  word_language_data->parts_of_speech = posu;
  return posu;
}

sense_chain_unit*
get_word_language_type_sense_data(vocabulary_table *table,
				  part_of_speech_chain_unit* word_language_type_data,
				  int sense_index)
{
  sense_chain_unit *sense;

  if (word_language_type_data == NULL) {
    return NULL;
  }

  for (sense = word_language_type_data->senses;
       sense != NULL;
       sense = sense->next) {
    if (sense->sense_index == sense_index) {
      return sense;
    }
  }

  sense = (sense_chain_unit*)mulvoc_malloc(table, sizeof(sense_chain_unit));
  sense->sense_index = sense_index;
  sense->forms = NULL;
  sense->next = word_language_type_data->senses;
  word_language_type_data->senses = sense;
  return sense;
}

form_chain_unit*
get_word_language_type_sense_form_data(vocabulary_table *table,
				       sense_chain_unit* word_language_type_sense_data,
				       int form_index)
{
  form_chain_unit *form;

  if (word_language_type_sense_data == NULL) {
    return NULL;
  }

  for (form = word_language_type_sense_data->forms;
       form != NULL;
       form = form->next) {
    if (form->form_index == form_index) {
      return form;
    }
  }

  form = (form_chain_unit*)mulvoc_malloc(table, sizeof(form_chain_unit));
  form->form_index = form_index;
  form->meaning = NULL;
  form->next = word_language_type_sense_data->forms;
  word_language_type_sense_data->forms = form;
  return form;
}

vocabulary_word*
find_language_word_in_meaning(vocabulary_meaning *meaning, int lang_index)
{
  vocabulary_word *word;

  for (word = meaning->words; word != NULL; word = word->next) {
    if (word->language == lang_index) {
      return word;
    }
  }
  return NULL;
}

#define MAX_TYPE 256
#define MAX_PRAGMA 256

int
read_vocab_file(const char *filename,
		vocabulary_table *table)
{
  struct stat stat_buf;
  int vocab_fd, vocab_file_size;
  char *vocab_file_buf_start;
  char *vocab_file_buf_end;
  int n_columns = 1;
  int n_langs = 0;
  int type_column = -1;
  int form_column = -1;
  int sense_column = -1;
  vocabulary_language** language_columns = NULL;
  int *extra_columns = NULL;
  char c, *p;

  if (stat(filename, &stat_buf) != 0) {
    exit(errno);
  }

  vocab_file_size = stat_buf.st_size;
  table->bytes_read += vocab_file_size;

  if ((vocab_fd = open(filename, O_RDONLY)) == -1) {
    exit(errno);
  }

  vocab_file_buf_start = mmap(NULL, vocab_file_size,
			      PROT_READ, MAP_SHARED,
			      vocab_fd, 0);
  if (vocab_file_buf_start == (void*)(-1)) {
    perror("Could not map vocab file");
    exit(errno);
  }

  if (table->tracing & TRACE_READ) {
    fprintf(stderr, "Starting to read file \"%s\"\n", filename);
  }

  vocab_file_buf_end = vocab_file_buf_start + vocab_file_size;

  /* Count the number of columns */
  {
    int in_quotes = 0;
    int i;
    char prev_c = '\0';

    for (p = vocab_file_buf_start;
	 ((c = *p) != '\n');
	 p++) {
      if (c == '"') {
	in_quotes = !in_quotes;
      } else {
	if (in_quotes) {
	  if ((prev_c == '"') && (c != '#')) {
	    n_langs++;
	  }
	} else {
	  if (c == ',') {
	    n_columns++;
	  }
	}
      }
      prev_c = c;
    }
    if (table->tracing & TRACE_HEADERS) {
      fprintf(stderr, "%d columns found\n", n_columns);
    }
  }

  /* Allocate the column data.  This is for the duration of
     read_vocab_file, and is not part of the relatively enduring
     vocabulary_table structure.
  */
  {
    int i;
    language_columns = (vocabulary_language**)mulvoc_malloc(table, sizeof(vocabulary_language*) * n_columns);
    extra_columns = (int*)mulvoc_malloc(table, sizeof(int) * n_columns);
    for (i = 0; i < n_columns; i++) {
      language_columns[i] = NULL;
      extra_columns[i] = -1;
    }
  }

  /* Look for the special (non-language) columns. */
  {
    char prev_c = '\0';
    int column = 0;
    int in_quotes = 0;

    for (p = vocab_file_buf_start;
	 ((c = *p) != '\n');
	 p++) {
      if (c == '"') {
	in_quotes = !in_quotes;
      } else {
	if (in_quotes) {
	  if (prev_c == '"') {
	    char *code_end = strchr(p, '"');
	    int code_length = code_end - p;
	    if (c == '#') {
	      if (strncmp("#TYPE", p, code_length) == 0) {
		type_column = column;
	      } else if (strncmp("#SENSE", p, code_length) == 0) {
		sense_column = column;
	      } else if (strncmp("#FORM", p, code_length) == 0) {
		form_column = column;
	      } else {
		extra_columns[column] = extra_column_index(table, p, code_length);
	      }
	    } else {
	      language_columns[column] = get_language(table, p, code_length);
	      if (table->tracing & TRACE_HEADERS) {
		fprintf(stderr, "using %#lx as header for column %d\n", language_columns[column], column);
	      }
	    }
	  }
	} else {
	  if (c == ',') {
	    column++;
	  }
	}
      }
      prev_c = c;
    }
  }

  /* Now the actual data reader. */
  {
    int column = 0;
    int in_quotes = 0;
    char prev_c = '\0';
    int doing_language_names;
    /* We fill these (with copies of the cell data) in as we find them on each row. */
    char row_type[MAX_TYPE];
    char row_sense[MAX_TYPE];
    char row_form[MAX_TYPE];
    char current_pragma_name[MAX_PRAGMA];
    int current_pragma_index = -1;
    /* These are indices into the arrays of type, sense and form. */
    int row_type_index, row_sense_index, row_form_index;

    /* Loop to read the rows of the file */
    while (p < vocab_file_buf_end) {
      vocabulary_meaning *row_meaning = NULL;
      column = 0;		/* spreadsheet column, not character column */
      doing_language_names = 0;	/* non-zero if on the language-names row */
      row_type[0] = row_sense[0] = row_form[0] = current_pragma_name[0] = '\0';
      row_type_index = -1;
      row_sense_index = -1;
      row_form_index = -1;

      /* Loop to read the cells of the row */
      while ((c = *++p) != '\n') {
	if (c == '"') {
	  in_quotes = !in_quotes;
	} else {
	  if (in_quotes) {
	    if (column < n_columns) {
	      if (prev_c == '"') {
		char *cell_end = strchr(p, '"');
		int cell_length = cell_end - p;

		/* Handle comments, pragmata etc */
		if (c == '#') {
		  if (column == 0) {
		    int pragma_length = cell_length;
		    if ((strncmp(p, "#languagename", pragma_length) == 0) ||
			(strncmp(p, "#LANGUAGENAME", pragma_length) == 0)){
		      doing_language_names = 1;
		    } else {
		      int i;
		      if (pragma_length > MAX_PRAGMA) {
			pragma_length = MAX_PRAGMA;
		      }
		      // strncpy(current_pragma_name, p+1, pragma_length-1);
		      for (i = 0; i < pragma_length; i++) {
			current_pragma_name[i] = tolower(p[i+1]);
		      }
		      current_pragma_name[pragma_length-1] = '\0';
		      current_pragma_index = property_index(table, current_pragma_name);
		    }
		    if (table->tracing & TRACE_PRAGMATA) {
		      printf("got pragma %s\n", current_pragma_name);
		    }
		  } else {
		    if (table->tracing & TRACE_COMMENTS) {
		      printf("got comment %.*s in column %d\n", cell_length, p, column);
		    }
		    fprintf(stderr, "got comment %.*s in column %d\n", cell_length, p, column);
		  }

		  /*
		    A normal data cell (neither pragma nor comment).
		    Look to see whether it is one of the special columns.
		  */
		} else {
		  if (column == type_column) {
		    int length = (cell_length > MAX_TYPE) ? MAX_TYPE : cell_length;
		    strncpy(row_type, p, length);
		    row_type[length] = '\0';
		    row_type_index = part_of_speech_index(table, row_type);
		  } else if (column == sense_column) {
		    int length = (cell_length > MAX_TYPE) ? MAX_TYPE : cell_length;
		    strncpy(row_sense, p, length);
		    row_sense[length] = '\0';
		    row_sense_index = sense_index(table, row_sense);
		  } else if (column == form_column) {
		    int length = (cell_length > MAX_TYPE) ? MAX_TYPE : cell_length;
		    strncpy(row_form, p, length);
		    row_form[length] = '\0';
		  } else {
		    /* Not in a special column, this could well be a word -- unless we are in a special row */
		    vocabulary_language *word_lang = language_columns[column];
		    char *lang_name = (word_lang != NULL) ? (&((word_lang->code)[0])) : "?";
		    if (doing_language_names) {
		      if (language_columns[column] != NULL) {
			char *name = &(language_columns[column]->name[0]);
			strncpy(name, p, cell_length);
			name[cell_length] = '\0';
		      }
		    } else if (current_pragma_index >= 0) {
		      if (language_columns[column] != NULL) {
			language_property *prop;
			int got_it = 0;
			for (prop = language_columns[column]->properties;
			     prop != NULL;
			     prop = prop->next) {
			  if (prop->key == current_pragma_index) {
			    got_it = 1;
			    break;
			  }
			}
			
			if (!got_it) {
			  language_property *new_prop = (language_property*)mulvoc_malloc(table, sizeof(language_property));
			  char *new_val = (char*)mulvoc_malloc(table, 1+cell_length);
			  strncpy(new_val, p, cell_length);
			  new_val[cell_length] = '\0';
			  new_prop->key = current_pragma_index;
			  new_prop->value = new_val;
			  new_prop->next = language_columns[column]->properties;
			  language_columns[column]->properties = new_prop;
			}
		      }
		    } else {
		      /* We are probably in a normal cell of a normal row */
		      hash_chain_unit* word_data;
		      language_chain_unit* word_language_data;
		      part_of_speech_chain_unit *word_type_data;
		      sense_chain_unit *word_type_sense_data;
		      form_chain_unit *word_type_sense_form_data;
		      if (word_lang == NULL) {
		      } else {
			char *q = p;
			int more_synonyms = 1;
			/* Iterate over comma/slash separated synonyms within the cell */
			while (more_synonyms && (q < cell_end)) {
			  char *next_comma = strchr(q, ',');
			  char *next_slash = strchr(q, '/');
			  char *word_end = cell_end;
			  int word_length;
			  if ((next_comma == NULL) && (next_slash == NULL)) {
			    more_synonyms = 0;
			  }
			  if ((next_comma != NULL) && (next_comma < word_end)) {
			    word_end = next_comma;
			  }
			  if ((next_slash != NULL) && (next_slash < word_end)) {
			    word_end = next_slash;
			  }
			  if (*q == ' ') {
			    q++;
			  }
			  word_length = word_end - q;

			  {
			    char *new_word_text = (char*)mulvoc_malloc(table, word_length + 1);
			    /* Now we really have got a word, so store it */
			    vocabulary_word *word_in_chain;
			    vocabulary_meaning *existing_meaning;
			    int link_word = 0;

			    strncpy(new_word_text, q, word_length);
			    new_word_text[word_length] = '\0';

			    if (table->tracing & TRACE_READ) {
			      fprintf(stderr, "got word %s in column %d which is %s(%d):%s:%s in %s\n", new_word_text, column, row_type, row_type_index, row_form, row_sense, lang_name);
			    }

			    word_data = get_word_data(table,
						      new_word_text);
			    word_language_data = get_word_language_data(table,
									word_data,
									word_lang->language_number);
			    word_type_data = get_word_language_type_data(table,
									 word_language_data,
									 row_type_index);
			    word_type_sense_data = get_word_language_type_sense_data(table,
										     word_type_data,
										     row_sense_index);
			    word_type_sense_form_data = get_word_language_type_sense_form_data(table,
											       word_type_sense_data,
											       row_form_index);

			    /* Get the existing meaning, if there is one */
			    existing_meaning = word_type_sense_form_data->meaning;

			    if ((row_meaning != NULL) && (existing_meaning == row_meaning)) {
			      /* nothing needs to be done */
			    } else {
			      if (row_meaning == NULL) {
				/* There are no previous occupied word cells
				   on this row */
				if (existing_meaning != NULL) {
				  /* The word already has a meaning we can
				     use, and we haven't yet started a new
				     one, so just tag on to the old one */
				  link_word = 1;
				  row_meaning = existing_meaning;
				} else {
				  /* We are on the first occupied word cell
				     of this row, and there is no
				     existing_meaning for this word+language
				     combination; so now we allocate a
				     meaning for this row */
				  row_meaning = (vocabulary_meaning*)mulvoc_malloc(table,
										   sizeof(vocabulary_meaning));
#ifdef debug
				  row_meaning->meaning_id = table->next_meaning_id++;
#endif
				  row_meaning->next = NULL;
				  row_meaning->words = NULL;
				  row_meaning->extra_columns = NULL;
				  row_meaning->part_of_speech = row_type_index;
				  row_meaning->sense_index = row_sense_index;
				  row_meaning->form_index = row_form_index;
				}
			      } else {
				/* We have already started to build a row,
				   and must now merge it with the existing
				   row that we've just found */
				if ((existing_meaning != NULL)
				    && (existing_meaning->words !=NULL)
				    && (existing_meaning != row_meaning)
				    ) {
				  /* We have a new meaning and an old one, so must merge them */
				  vocabulary_word *scanning_word;
				  if (table->tracing & TRACE_MERGE) {
				    fprintf(stderr, "    really merging meanings, on account of %s:%s:%s\n", word_lang->code, row_type, new_word_text);
				    show_meaning(stderr, table, row_meaning, "    row meaning: ");
				    show_meaning(stderr, table, existing_meaning, "    old meaning: ");
				    show_meanings(stderr, table, "meanings before merging\n", "%d meanings (before merging)\n", "\t");
				  }
				  link_word = 1;

				  /* find end of existing_meaning words */
				  for (scanning_word = existing_meaning->words;
				       scanning_word->next != NULL;
				       scanning_word = scanning_word->next) {
				  }
				  scanning_word->next = row_meaning->words;
				  if (table->tracing & TRACE_MERGE) {
				    show_meaning(stderr, table, existing_meaning, "    combined meaning: ");
				    show_meanings(stderr, table, "meanings after merging\n", "%d meanings (after merging)\n", "\t");
				  }

				  /* Remove any pointers to the unmerged meaning: */
				  for (scanning_word = row_meaning->words;
				       scanning_word != NULL;
				       scanning_word = scanning_word->next) {
				    int word_language = scanning_word->language;
				    
				    form_chain_unit *fcu = 
				      get_word_language_type_sense_form_data
				      (table,
				       get_word_language_type_sense_data
				       (table,
					get_word_language_type_data
					(table,
					 get_word_language_data
					 (table,
					  get_word_data(table, scanning_word->text),
					  word_language),
					 row_type_index),
					row_sense_index),
				       row_form_index);

				    if (fcu->meaning == row_meaning) {
				      fcu->meaning = existing_meaning;
				    }
				  }

				  /* Now switch to using the combined meaning from now on (and fixup old references): */
				  {
				    vocabulary_meaning *m;
				    int i = 0;
				    for (m = table->meanings; m != NULL; m = m->next) {
				      if (m->next == row_meaning) {
					m->next = m->next->next;
				      }
				      i++;
				    }
				  }
				  table->n_meanings--;

				  mulvoc_free(table, row_meaning); /* todo: these are still being referred to, because our fixup system only fixes up references in the current row, whereas we may be joining a previous row which knows nothing of these early sixties sitcoms of which we speak */

				  if (table->meanings == row_meaning) {
				    table->meanings = existing_meaning;
				  }
				  row_meaning = existing_meaning;
				}
			      }
			    }

			    word_type_sense_form_data->meaning = row_meaning;
			    
			    if (!link_word) {
			      /* Now add the word to the meaning */
			      word_in_chain = (vocabulary_word*)mulvoc_malloc(table, sizeof(vocabulary_word));
			      word_in_chain->text = new_word_text;
			      word_in_chain->language = word_lang->language_number;

			      word_in_chain->next = row_meaning->words;
			      row_meaning->words = word_in_chain;
			    }
			  }
			  q = word_end+1;
			}
		      }
		    }
		  }
		}
		p += cell_length - 1;
	      }
	    }
	  } else {
	    if (c == ',') {
	      column++;
	    } else {
	      int column_extra_id = extra_columns[column];
	      if (column_extra_id >= 0) {
		char *next_comma = strchr(p, ',');
		int cell_length = next_comma - p;
		extra_column_cell *cell = (extra_column_cell*)mulvoc_malloc(table, sizeof(extra_column_cell));
		cell->extra_column_index = column_extra_id;
		cell->value = (char*)mulvoc_malloc(table, cell_length+1);
		strncpy(cell->value, p, cell_length);
		cell->value[cell_length] = '\0';
#if 0
		fprintf(stderr, "got unquoted %s in column %d\n", cell->value, column);
#endif
		if (row_meaning == NULL) {
		  row_meaning = (vocabulary_meaning*)mulvoc_malloc(table,
								   sizeof(vocabulary_meaning));
#ifdef debug
		  row_meaning->meaning_id = table->next_meaning_id++;
#endif
		  row_meaning->next = NULL;
		  row_meaning->words = NULL;
		  row_meaning->extra_columns = NULL;
		  row_meaning->part_of_speech = row_type_index;
		  row_meaning->sense_index = row_sense_index;
		  row_meaning->form_index = row_form_index;
		}
		cell->next = row_meaning->extra_columns;
		row_meaning->extra_columns = cell;
		p += cell_length - 1;
	      }
	    }
	  }
	}
	prev_c = c;
      }
      current_pragma_name[0] = '\0';
      current_pragma_index = -1;

      /* We've finished reading the row */

      if (row_meaning != NULL) {
	int got_already = 0;
	vocabulary_meaning *m;
	for (m = table->meanings; m != NULL; m = m->next) {
	  if (m == row_meaning) {
	    got_already = 1;
	    break;
	  }
	}
	if (!got_already) {
	  row_meaning->next = table->meanings;
	  table->meanings = row_meaning;
	  table->n_meanings++;
	}
      }

      if (table->tracing & TRACE_READ) {
	fprintf(stderr, "\n");
      }
    }

  }
  munmap(vocab_file_buf_start, vocab_file_size);
  mulvoc_free(table, language_columns);

  if (table->tracing & TRACE_READ) {
    fprintf(stderr, "Finished reading file \"%s\"\n", filename);
  }

  close(vocab_fd);
}

char *
get_word_translations_string(vocabulary_table *table,
			     char *as_text,
			     int language_in,
			     int language_out,
			     int pos_in,
			     int sense_in,
			     int form_in,
			     char *result_section_format,
			     char *result_space,
			     int result_size)
{
  char *result_so_far = result_space;
  int length_remaining = result_size - 1;
  int format_length = strlen(result_section_format);
  hash_chain_unit *chain_link = get_word_data(table, as_text);
  language_chain_unit *language;

  for (language = chain_link->languages;
       language != NULL;
       language = language->next) {
    if ((language_in < 0)
	|| (language->language == language_in)) {
      part_of_speech_chain_unit *part_of_speech;
      for (part_of_speech = language->parts_of_speech;
	   part_of_speech != NULL;
	   part_of_speech = part_of_speech->next) {
	if ((pos_in < 0) || (part_of_speech->part_of_speech == pos_in)) {
	  int p_o_s_index = part_of_speech->part_of_speech;
	  sense_chain_unit *sense;
	  for (sense = part_of_speech->senses;
	       sense != NULL;
	       sense=sense->next) {
	    if ((sense_in < 0) || (sense->sense_index == sense_in)) {
	      int s_index = sense->sense_index;
	      form_chain_unit *form;
	      for (form = sense->forms;
		   form != NULL;
		   form = form->next) {
		if ((form_in < 0) || (form->form_index == form_in)) {
		  int f_index = form->form_index;
		  vocabulary_meaning *meaning = form->meaning;
		  vocabulary_word *word;

		  for (word = meaning->words;
		       word != NULL;
		       word=word->next) {
		    if ((language_out < 0) || (language_out == word->language)) {
		      int this_length = format_length + strlen(table->languages[word->language]->code) + strlen(word->text);
		      if ((length_remaining - this_length) < 0) {
			return NULL;
		      }
		      sprintf(result_so_far, result_section_format, table->languages[word->language]->code, word->text);
		      this_length = strlen(result_so_far);
		      result_so_far += this_length;
		      length_remaining -= this_length;
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
  return result_space;
}

int
count_language_words(vocabulary_table *table,
		     int language_index,
		     int all_synonyms)
{
  int n = 0;

  vocabulary_meaning *m;

  for (m = table->meanings; m != NULL; m = m->next) {
    vocabulary_word *w;
    // show_meaning(stderr, table, m, "?\t");
    for (w = m->words; w != NULL; w = w->next) {
      if (w->language == language_index) {
	n++;
	if (!all_synonyms) {
	  break;
	}
      }
    }
  }

  return n;
}

/* 
   Construct an array of all the words in a given language.
   Homographs appear as many times as they have meanings.  The third
   argument points to a location to fill in with the address of the
   resulting array.  This array has pointers to the strings of the
   internal data of the table.  The function result is the size of the
   array.
 */
int
list_language_words(vocabulary_table *table,
		    int language_index,
		    char ***word_array_ptr)
{
  int n_words = count_language_words(table, language_index, 1);
  char **word_array = malloc(n_words * sizeof(char*));
  int fill_index = 0;
  vocabulary_meaning *m;

  for (m = table->meanings; m != NULL; m = m->next) {
    vocabulary_word *w;
    for (w = m->words; w != NULL; w = w->next) {
      if (w->language == language_index) {
	word_array[fill_index++] = w->text;
      }
    }
  }

  if (fill_index != n_words) {
    fprintf(stderr, "Internal problem: wrong number of words!! Was %d, should have been %d\n", fill_index, n_words);
  }

  *word_array_ptr = word_array;

  return fill_index;
}

int
sort_word_order(const void *a, const void *b)
{
  sorted_vocab_word *wa = (sorted_vocab_word*)a;
  sorted_vocab_word *wb = (sorted_vocab_word*)b;
  return strcmp(wa->as_text, wb->as_text);
}

int
vocabulary_keyed_by_language(vocabulary_table *table,
			     int key_language_index,
			     int all_synonyms,
			     int sorted)
{
  int n_words = count_language_words(table, key_language_index, all_synonyms);
  sorted_vocab_word *next;
  vocabulary_meaning *m;

#if 0
  fprintf(stderr, "%d words in %d(%s)\n", n_words, key_language_index, table->languages[key_language_index]->code);
#endif

  if (table->keyed != NULL) { mulvoc_free(table, table->keyed); }
  table->keyed = (sorted_vocab_word*)mulvoc_malloc(table, n_words * sizeof(sorted_vocab_word));

  next = table->keyed;

  for (m = table->meanings; m != NULL; m = m->next) {
    vocabulary_word *w;
    for (w = m->words; w != NULL; w = w->next) {
      if (w->language == key_language_index) {
	next->as_text = w->text;
	next->meaning = m;
	next++;
	if (!all_synonyms) {
	  break;
	}
      }
    }
  }

  if (sorted) {
    qsort(table->keyed,
	  n_words,
	  sizeof(sorted_vocab_word),
	  sort_word_order);
  }

  return n_words;
}

#define LANG_SPEC_SEPARATOR ','

/* List the indices of the languages whose codes are given in languages_string.

   The second argument is a string of language codes, separated by
   LANG_SPEC_SEPARATOR, which is normally a comma.

   The third argument is a pointer to the location to fill in with the
   address of the indices array, which is newly allocated.
*/
int
language_indices(vocabulary_table *table,
		 char *languages_string,
		 int **languages)
{
  if ((languages_string != NULL) && (languages_string[0] != '\0')) {
    /* pick specific languages */
    int n = 1;
    int i;
    int *l;
    char c;
    char *s = languages_string;
    while ((c = *s++) != '\0') {
      if (c == LANG_SPEC_SEPARATOR) {
	n++;
      }
    }
    l = (int*)malloc(n * sizeof(int));
    *languages = l;
    s = languages_string;
    for (i = 0; i < n; i++) {
      /* strchrnul is meant to be declared when you define _GNU_SOURCE and include string.h
       but on the development system (debian) it didn't work, hence the cast. */
      char *s1 = (char*) strchrnul(s, LANG_SPEC_SEPARATOR);
      l[i] = language_index(table, s, s1 - s);
      s = s1 + 1;
    }
    return n;
  } else {
    /* use all the languages */
    int n = table->n_languages;
    int *l = (int*)malloc(n * sizeof(int));
    int i;
    *languages = l;
    for (i = 0; i < n; i++) {
      l[i] = table->languages[i]->language_number;
    }
    return n;
  }
}

void
start_tracing(vocabulary_table *table, int flags)
{
  table->tracing |= flags;
}

void
stop_tracing(vocabulary_table *table, int flags)
{
  table->tracing &= ~flags;
}

/* mulvoc_data.c ends here */
