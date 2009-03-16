/* mulvoc_data.c
   Time-stamp: <2009-03-15 19:16:22 jcgs>
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
		   char *table_opts,
		   char *blank)
{
  int n_langs = table->n_languages;
  vocabulary_language **t_langs = table->languages;
  int i_lang;

  vocabulary_meaning *meaning;

  fprintf(output_stream, "<table%s%s>\n",
	  table_opts != NULL ? " " : "",
	  table_opts != NULL ? table_opts : "");
  fprintf(output_stream, "  <tr><td>#TYPE</td><td>#FORM</td><td>#SENSE</td>");

  for (i_lang = 0; i_lang < n_langs; i_lang++) {
    fprintf(output_stream, "<td>%s</td>", t_langs[i_lang]->code);
  }
  fprintf(output_stream, "</tr>\n  <tr><td></td><td></td><td></td>");
  for (i_lang = 0; i_lang < n_langs; i_lang++) {
    fprintf(output_stream, "<td>%s</td>", t_langs[i_lang]->name);
  }
  fprintf(output_stream, "  </tr>\n");
  
  for (meaning = table->meanings;
       meaning != NULL;
       meaning = meaning->next) {
    int pos_index = meaning->part_of_speech;
    int sense_index = meaning->sense_index;
    int form_index = meaning->form_index;
    fprintf(output_stream, "  <tr><td class=\"pos\">%s</td><td%s>%s</td><td%s>%s</td>\n",
	    pos_index >= 0 ? table->parts_of_speech[pos_index] : "",
	    sense_index >= 0 ? " class=\"sense\"" : "",
	    sense_index >= 0 ? table->senses[sense_index] : "",
	    form_index >= 0 ? " class=\"form\"" : "",
	    form_index >= 0 ? table->forms[form_index] : "");
    for (i_lang = 0; i_lang < n_langs; i_lang++) {
      vocabulary_word *word = find_language_word_in_meaning(meaning, i_lang);
      if (word != NULL) {
	fprintf(output_stream, "    <td lang=\"%s\">%s</td>\n",
		table->languages[word->language]->code,
		word->text);
      } else {
	if (blank) {
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
mulvoc_output_csv(FILE *output_stream,
		  vocabulary_table *table)
{
}

/* mulvoc_out.c ends here */
