/* voclist.c
   Time-stamp: <2009-05-15 21:45:17 jcgs>
   Vocabulary listing program in the MuLVoc suite.
 */

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include "../libsrc/mulvoc.h"

#define START_SIZE 24

void
show_usage(char *program)
{
  fprintf(stderr,
	  "Usage: %s dictionary language\n\n",
	  program);
}

int
main(int argc, char **argv)
{
  vocabulary_table table;

  int language_in = -1;

  int n_words;
  char **word_array = NULL;
  int i;

  if (argc != 3) {
    show_usage(argv[0]);
    exit(0);
  }

  mulvoc_initialize_table(&table, 1511, START_SIZE, 0);

  read_vocab_file(argv[1], &table);

  language_in = language_index(&table, argv[2], strlen(argv[2]));

  n_words = list_language_words(&table,
				language_in,
				&word_array);

  for (i = 0; i < n_words; i++) {
    puts(word_array[i]);
  }

  exit(0);
}
