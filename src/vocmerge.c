/* vocmerge.c
   Time-stamp: <2009-03-15 19:16:50 jcgs>
 */

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include "../libsrc/mulvoc.h"

#define START_SIZE 24

#define BUFFER_SIZE (256*256)

static const char *short_options = "ho:";

static struct option long_options[] = {
  {"attributes", required_argument, 0, 'a'},
  {"blank", required_argument, 0, 'b'},
  {"help", no_argument, 0, 'H'},
  {"html", no_argument, 0, 'h'},
  {"html-page", no_argument, 0, 'p'},
  {"output", required_argument, 0, 'o'},
  {"title", required_argument, 0, 't'},
  {0, 0, 0, 0}
};

void
print_usage(char *progname)
{
  fprintf(stderr, "Usage: %s [options] file ... file\n", progname);
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  --html (-h)         output bare table code\n");
  fprintf(stderr, "  --html-page (-p)    output table with page surround\n");
  fprintf(stderr, "  --output (-o) FILE  output to FILE (otherwise stdout)\n");
  fprintf(stderr, "  --title TITLE       use TITLE for html page\n");
  fprintf(stderr, "  --attributes ATTRS  use ATTRS for html table attributes\n");
  fprintf(stderr, "  --blank TEXT        use TEXT for blank cells\n");
  fprintf(stderr, "\n--title implies --html-page\n");
  fprintf(stderr, "--attributes or --blank imply --html\n");
}

int
main(int argc, char **argv)
{
  int html = 0;
  int page = 0;
  char *title = "Vocabulary";
  char *attributes = NULL;
  char *blank;
  char *output_file_name = NULL;
  FILE *output_stream = stdout;

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
    case 'h': html = 1; break;
    case 'o': output_file_name = optarg; break;
    case 'p': page = html = 1; break;
    case 't': page = html = 1; title = optarg; break;
    default: print_usage(argv[0]); exit(0); break;
    }
  }

  mulvoc_initialize_table(&table, 1511, START_SIZE, 0);

  for (; optind < argc; optind++) {
    read_vocab_file(argv[optind], &table);
  }

  if (output_file_name != NULL) {
    output_stream = fopen(output_file_name, "w");
  }

  if (html) {
    if (page) {
      fprintf(output_stream,
	      "<html>\n<head>\n<title>%s</title></head>\n<body>\n",
	      title);
    }
    mulvoc_output_html(output_stream, &table, attributes, blank);
    if (page) {
      fprintf(output_stream, "</body>\n</html>\n");
    }
  } else {
    mulvoc_output_csv(output_stream, &table);
  }

  if (output_file_name != NULL) {
    fclose(output_stream);
  }

  exit(0);
}


