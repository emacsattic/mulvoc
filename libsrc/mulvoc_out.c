/* mulvoc_data.c
   Time-stamp: <2009-05-17 20:58:11 jcgs>
   Output MuLVoc data (multi-lingual vocabulary) as CSV or HTML

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

#include <stdio.h>

#include "mulvoc.h"

void
mulvoc_output_html(FILE *output_stream,
		   vocabulary_table *table,
		   int *languages,
		   int n_languages,
		   int key_idx,
		   char *table_opts,
		   char *blank,
		   int table_controlled)
{
  int i_lang;
  int rows = vocabulary_keyed_by_language(table, key_idx, 0, 1);
  int i_row;
  int color_index = table_controlled ? property_index(table, "color") : -1;
  int bgcolor_index = table_controlled ? property_index(table, "background") : -1;

  if (languages == NULL) {
    n_languages = language_indices(table, NULL, &languages);
  }

  /* todo: output the rest of the header row(s) */

  fprintf(output_stream, "<table%s%s>\n",
	  table_opts != NULL ? " " : "",
	  table_opts != NULL ? table_opts : "");
  fprintf(output_stream, "  <tr><td>#TYPE</td><td>#FORM</td><td>#SENSE</td>");

  for (i_lang = 0; i_lang < n_languages; i_lang++) {
    fprintf(output_stream, "<td>%s</td>", table->languages[languages[i_lang]]->code);
  }
  fprintf(output_stream, "</tr>\n  <tr><td></td><td></td><td></td>");
  for (i_lang = 0; i_lang < n_languages; i_lang++) {
    fprintf(output_stream, "<td>%s</td>", table->languages[languages[i_lang]]->name);
  }
  fprintf(output_stream, "  </tr>\n");

  for (i_row = 0; i_row < rows; i_row++) {
    vocabulary_meaning *meaning = table->keyed[i_row].meaning;
    int pos_index = meaning->part_of_speech;
    int sense_index = meaning->sense_index;
    int form_index = meaning->form_index;
    fprintf(output_stream, "  <tr><td class=\"pos\">%s</td><td%s>%s</td><td%s>%s</td>\n",
	    pos_index >= 0 ? table->parts_of_speech[pos_index] : "",
	    sense_index >= 0 ? " class=\"sense\"" : "",
	    sense_index >= 0 ? table->senses[sense_index] : "",
	    form_index >= 0 ? " class=\"form\"" : "",
	    form_index >= 0 ? table->forms[form_index] : "");
    for (i_lang = 0; i_lang < n_languages; i_lang++) {
      vocabulary_word *word = find_language_word_in_meaning(meaning, languages[i_lang]);
      if (word != NULL) {
	char *color_string = language_property_string(table,
						      i_lang,
						      color_index);
	char *bgcolor_string = language_property_string(table,
						      i_lang,
						      bgcolor_index);
	fprintf(output_stream, "    <td lang=\"%s\"",
		table->languages[word->language]->code);
	if (color_string) {
	  fprintf(output_stream, " text=\"%s\"", color_string);
	}	
	if (bgcolor_string) {
	  fprintf(output_stream, " bgcolor=\"%s\"", bgcolor_string);
	}	
	fprintf(output_stream, ">%s</td>\n",
		word->text);
      } else {
	if (blank != NULL) {
	  fprintf(output_stream, "    <td>%s</td>\n", blank);
	} else {
	  fprintf(output_stream, "    <td></td>\n");
	}
      }
    }
    fprintf(output_stream, "  </tr>\n");
  }

  fprintf(output_stream, "</table>\n");
}

void
mulvoc_output_meaning(FILE *output_stream,
		      vocabulary_table *table,
		      int *languages,
		      int n_languages,
		      vocabulary_meaning *meaning)
{
  int i;

  fprintf(output_stream, "\"%s\",\"%s\",\"%s\"",
	  table->parts_of_speech[meaning->part_of_speech],
	  table->senses[meaning->sense_index],
	  table->forms[meaning->form_index]);

  for (i = 0; i < n_languages; i++) {
    int l = languages[i];
    int got_some = 0;
    vocabulary_word *w;

    for (w = meaning->words;
	 w != NULL;
	 w = w->next) {
      if (w->language == l) {
	if (got_some) {
	  fputs(", ", output_stream);
	} else {
	  fputs(",\"", output_stream);
	  got_some = 1;
	}
	fputs(w->text, output_stream);
      }
    }
    if (got_some) {
      putc('\"', output_stream);
    } else {
      putc(',', output_stream);
    }
  }
  putc('\n', output_stream);
}

void
mulvoc_output_csv(FILE *output_stream,
		  vocabulary_table *table,
		  int *languages,
		  int n_languages,
		  int key_idx)
{
  int rows = vocabulary_keyed_by_language(table, key_idx, 0, 1);
  int i_row, i_lang;

  fprintf(output_stream, "\"#TYPE\",\"#FORM\",\"#SENSE\"");

  for (i_lang = 0; i_lang < table->n_extra_columns; i_lang++) {
    fprintf(output_stream, ",\"%s\"", table->extra_column_names[i_lang]);
  }

  for (i_lang = 0; i_lang < n_languages; i_lang++) {
    fprintf(output_stream, ",\"%s\"", table->languages[languages[i_lang]]->code);
  }
  fprintf(output_stream, "\n\"#languagename\",,");
  for (i_lang = 0; i_lang < table->n_extra_columns; i_lang++) {
    putc(',', output_stream);
  }
  for (i_lang = 0; i_lang < n_languages; i_lang++) {
    fprintf(output_stream, ",\"%s\"", table->languages[languages[i_lang]]->name);
  }

  putc('\n', output_stream);

  for (i_row = 0;
       i_row < table->n_properties;
       i_row++) {
    fprintf(output_stream, "\"#%s\",,", table->properties[i_row]);
    for (i_lang = 0; i_lang < table->n_extra_columns; i_lang++) {
      putc(',', output_stream);
    }
    for (i_lang = 0; i_lang < n_languages; i_lang++) {
      char *prop_text = language_property_string(table, languages[i_lang], i_row);
      if (prop_text == NULL) {
	putc(',', output_stream);
      } else {
	fprintf(output_stream, ",\"%s\"", prop_text);
      }
    }
    putc('\n', output_stream);
  }

  for (i_row = 0; i_row < rows; i_row++) {
    vocabulary_meaning *meaning = table->keyed[i_row].meaning;
    int pos_index = meaning->part_of_speech;
    int sense_index = meaning->sense_index;
    int form_index = meaning->form_index;
    fprintf(output_stream, "\"%s\",\"%s\",\"%s\"",
	    pos_index >= 0 ? table->parts_of_speech[pos_index] : "",
	    form_index >= 0 ? table->forms[form_index] : "",
	    sense_index >= 0 ? table->senses[sense_index] : "");
    for (i_lang = 0; i_lang < table->n_extra_columns; i_lang++) {
      extra_column_cell *xcc = find_extra_cell(meaning, i_lang);
      if (xcc) {
	fprintf(output_stream, ",\"%s\"", xcc->value);
      } else {
	putc(',', output_stream);
      }
    }
    for (i_lang = 0; i_lang < n_languages; i_lang++) {
      vocabulary_word *word = find_language_word_in_meaning(meaning, languages[i_lang]);
      if (word != NULL) {
	fprintf(output_stream, ",\"%s\"", word->text);
      } else {
	putc(',', output_stream);
      }
    }
    putc('\n', output_stream);
  }
}

void
mulvoc_output_data(FILE *output_stream,
		   vocabulary_table *table,
		   int *languages,
		   int n_languages,
		   int key_idx)
{
  int rows = vocabulary_keyed_by_language(table, key_idx, 0, 1);
  int i_row, i_lang;

  for (i_row = 0; i_row < rows; i_row++) {
    vocabulary_meaning *meaning = table->keyed[i_row].meaning;
    int first = 1;
#if 0
    int pos_index = meaning->part_of_speech;
    int sense_index = meaning->sense_index;
    int form_index = meaning->form_index;
    /* might want to put something like these in, optionally, later */
    fprintf(output_stream, "\"%s\",\"%s\",\"%s\"",
	    pos_index >= 0 ? table->parts_of_speech[pos_index] : "",
	    form_index >= 0 ? table->forms[form_index] : "",
	    sense_index >= 0 ? table->senses[sense_index] : "");
#endif
    vocabulary_word *word = find_language_word_in_meaning(meaning, key_idx);
    fprintf(output_stream, "%s: ", word->text);
    
    for (i_lang = 0; i_lang < n_languages; i_lang++) {
      if (i_lang != key_idx) {
	word = find_language_word_in_meaning(meaning, languages[i_lang]);
	if (word != NULL) {
	  if (first) {
	    first = 0;
	  } else {
	    putc(',', output_stream); putc(' ', output_stream);
	  }
	  fprintf(output_stream, "%s(%s)",
		  word->text,
		  table->languages[word->language]->code);
	}
      }
    }
    putc('\n', output_stream);
  }
}

/* mulvoc_out.c ends here */
