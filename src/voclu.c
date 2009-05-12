/* voclu.c
   Time-stamp: <2009-05-12 14:25:56 jcgs>
 */

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include "../libsrc/mulvoc.h"

#define START_SIZE 24

#define BUFFER_SIZE (256*256)

static const char *short_options = "d:f:l:s:t:v";

static struct option long_options[] = {
  {"dictionary", required_argument, 0, 'd'},
  {"form", required_argument, 0, 'f'},
  {"language", required_argument, 0, 'l'},
  {"sense", required_argument, 0, 's'},
  {"type", required_argument, 0, 't'},
  {"verbose", no_argument, 0, 'v'},
  {0, 0, 0, 0}
};

void
show_usage(char *program)
{
  fprintf(stderr,
	  "Usage: %s [options] [words]\n\n",
	  program);
  fprintf(stderr, "  Does vocabulary lookup using CSV files.\n");
  fprintf(stderr, "  Options are:\n");
  fprintf(stderr, "    --dictionary (-d) file \n");
  fprintf(stderr, "    --language (-l) language\n");
  fprintf(stderr, "    --type (-t) part-of-speech \n");
  fprintf(stderr, "    --sense (-s) sense\n");
  fprintf(stderr, "    --form (-f) form \n");
  fprintf(stderr, "    --verbose\n");
  fprintf(stderr, "  If no --dictionary is given, the first non-option argument is the dictionary.\n");
  fprintf(stderr, "  If words are given on the command line, they are translated;\n");
  fprintf(stderr, "  Otherwise, standard input is read for words to translate.\n");
}

int
main(int argc, char **argv)
{
  vocabulary_table table;

  int verbose = 0;

  char *dictionary = NULL;
  char *language_in_code = NULL;
  char *type_name = NULL;
  char *sense_name = NULL;
  char *form_name = NULL;
  int language_in = -1;
  int type =  -1;
  int sense = -1;
  int form = -1;

  int words_on_command_line = 0;

  if (argc < 2) {
    show_usage(argv[0]);
    exit(0);
  }

  while (1) {
    int option_index = 0;
    char opt = getopt_long(argc, argv,
			   short_options, long_options,
			   &option_index);

    if (opt == -1) {
      break;
    }
    switch (opt) {
    case 'd': dictionary = optarg; break;
    case 'f': form_name = optarg; break;
    case 'l': language_in_code = optarg; break;
    case 's': sense_name = optarg; break;
    case 't': type_name = optarg; break;
    case 'v': verbose = 1; break;
    default: show_usage(argv[0]); exit(1); break;
    }
  }

  mulvoc_initialize_table(&table, 1511, START_SIZE, 0);

  if (dictionary == NULL) {
    dictionary = argv[optind++];
  }

  if (verbose) {
    fprintf(stderr, "Reading dictionary %s\n", dictionary);
  }

  read_vocab_file(dictionary, &table);

  if (language_in_code != NULL) {
    language_in = language_index(&table, language_in_code, strlen(language_in_code));
  }

  if (type_name != NULL) {
    type = part_of_speech_index(&table, type_name);
  }

  if (sense_name != NULL) {
    sense = sense_index(&table, sense_name);
  }

  if (form_name != NULL) {
    form = form_index(&table, form_name);
  }

  for (; optind < argc; optind++) {
    char buf[BUFFER_SIZE];
    printf("%s: %s\n",
	   argv[optind],
	   get_word_translations_string(&table, argv[optind],
					language_in,
					type, sense, form,
					"%s: %s; ",
					buf, BUFFER_SIZE));
    words_on_command_line = 1;
  }

  if (!words_on_command_line) {
    char in_buf[BUFFER_SIZE];
    char out_buf[BUFFER_SIZE];

    while (fgets(in_buf, BUFFER_SIZE, stdin)) {
      char *p;
      out_buf[0] = '\0';

      for (p = in_buf; (*p != '\n') && (*p != '\r') && (*p != '\0'); p++) {
      }

      *p = '\0';

      printf("%s: %s\n",
	     in_buf,
	     get_word_translations_string(&table, in_buf,
					  language_in,
					  type, sense, form,
					  "%s: %s; ",
					  out_buf, BUFFER_SIZE));
    }
  }

  exit(0);
}


