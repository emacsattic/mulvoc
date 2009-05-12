/* vocmerge.c
   Time-stamp: <2009-05-10 18:22:23 jcgs>
 */

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include "../libsrc/mulvoc.h"

#define START_SIZE 24

#define BUFFER_SIZE (256*256)

static const char *short_options = "a:b:cHhl:o:ps:t:v";

static struct option long_options[] = {
  {"attributes", required_argument, 0, 'a'},
  {"blank", required_argument, 0, 'b'},
  {"controlled", no_argument, 0, 'c'},
  {"help", no_argument, 0, 'H'},
  {"html", no_argument, 0, 'h'},
  {"html-page", no_argument, 0, 'p'},
  {"languages", required_argument, 0, 'l'},
  {"sort", required_argument, 0, 's'},
  {"output", required_argument, 0, 'o'},
  {"title", required_argument, 0, 't'},
  {"verbose", no_argument, 0, 'v'},
  {0, 0, 0, 0}
};

void
print_usage(char *progname)
{
  fprintf(stderr, "Usage: %s [options] file ... file\n", progname);
  fprintf(stderr, "\nOptions:\n");
  fprintf(stderr, "  --languages A,B,C,D      output columns A B C D, in that order\n");
  fprintf(stderr, "  --sort (-s) LANGUAGE     sort output by LANGUAGE\n");
  fprintf(stderr, "  --output (-o) FILE       output to FILE (otherwise stdout)\n");
  fprintf(stderr, "  --html (-h)              output bare html table code\n");
  fprintf(stderr, "  --html-page (-p)         output table with page surround\n");
  fprintf(stderr, "  --title (-t) TITLE       use TITLE for html page\n");
  fprintf(stderr, "  --attributes (-a) ATTRS  use ATTRS for html table attributes\n");
  fprintf(stderr, "  --blank (-b) TEXT        use TEXT for blank cells\n");
  fprintf(stderr, "  --controlled (-c)        use colours etc from input\n");
  fprintf(stderr, "  --verbose (-b)           turn on debugging\n");
  fprintf(stderr, "\n--title implies --html-page\n");
  fprintf(stderr, "--attributes or --blank or --controlled imply --html\n");
  fprintf(stderr, "--output whatever.htm[l] implies --html\n");
  fprintf(stderr, "\nIf html is not selected, output is as csv.\n");
}

int
main(int argc, char **argv)
{
  int html = 0;
  int page = 0;
  char *title = "Vocabulary";
  char *attributes = NULL;
  char *blank = NULL;
  char *output_file_name = NULL;
  FILE *output_stream = stdout;
  char *language_code = "ENG";
  char *languages_string = NULL;
  int verbose = 0;
  int table_controlled = 0;

  vocabulary_table table;

  if (argc < 2) {
    print_usage(argv[0]);
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
    case 'H': print_usage(argv[0]); exit(0); break;
    case 'a': html = 1; attributes = optarg; break;
    case 'b': html = 1; blank = optarg; break;
    case 'c': table_controlled = 1; html = 1; break;
    case 'h': html = 1; break;
    case 'l': languages_string = optarg; break;
    case 'o': output_file_name = optarg;
      if ((strcmp(output_file_name + strlen(output_file_name) - 5, ".html") == 0)
	  || (strcmp(output_file_name + strlen(output_file_name) - 4, ".htm") == 0)) {
	html = 1;
      }
      break;
    case 'p': page = html = 1; break;
    case 's': language_code = optarg; break;
    case 't': page = html = 1; title = optarg; break;
    case 'v': verbose = 1; break;
    default: print_usage(argv[0]); exit(1); break;
    }
  }

  mulvoc_initialize_table(&table, 1511, START_SIZE, verbose ? -1 : 0);

  for (; optind < argc; optind++) {
    read_vocab_file(argv[optind], &table);
  }

  if (output_file_name != NULL) {
    output_stream = fopen(output_file_name, "w");
  }

  {
    int key_idx = language_index(&table, language_code, strlen(language_code));
    int *languages = NULL;
    int n_languages = language_indices(&table, languages_string, &languages);

    if (html) {
      if (page) {
	fprintf(output_stream,
		"<html>\n<head>\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head>\n<body>\n",
		title);
      }

      mulvoc_output_html(output_stream,
			 &table,
			 languages, n_languages,
			 key_idx,
			 attributes,
			 blank,
			 table_controlled);

      if (page) {
	fprintf(output_stream, "</body>\n</html>\n");
      }
    } else {
      mulvoc_output_csv(output_stream,
			&table,
			languages, n_languages,
			key_idx);
    }
  }
  if (output_file_name != NULL) {
    fclose(output_stream);
  }

  exit(0);
}


