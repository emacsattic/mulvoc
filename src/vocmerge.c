/* vocmerge.c
   Time-stamp: <2009-06-24 09:28:42 jcgs>
   Vocabulary file merge program in the MuLVoc suite.
 */

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include "../libsrc/mulvoc.h"

#if 1
#define ENABLE_TRACE_MERGE 1
#endif

#define START_SIZE 24

#define BUFFER_SIZE (256*256)

static const char *short_options = "a:b:cd:E:gHhl:mo:prS:s:t:v";

static struct option long_options[] = {
  {"attributes", required_argument, 0, 'a'},
  {"blank", required_argument, 0, 'b'},
  {"controlled", no_argument, 0, 'c'},
  {"delimited", required_argument, 0, 'd'},
  {"help", no_argument, 0, 'H'},
  {"html", no_argument, 0, 'h'},
  {"html-page", no_argument, 0, 'p'},
  {"languages", required_argument, 0, 'l'},
  {"locate", no_argument, 0, 'g'},
  {"output", required_argument, 0, 'o'},
  {"raw", no_argument, 0, 'r'},
  {"row-end", required_argument, 0, 'E'},
  {"row-start", required_argument, 0, 'S'},
  {"sort", required_argument, 0, 's'},
  {"title", required_argument, 0, 't'},
#ifdef ENABLE_TRACE_MERGE
  {"trace-merge", no_argument, 0, 'm'},
#endif
  {"verbose", no_argument, 0, 'v'},
  {0, 0, 0, 0}
};

void
print_usage(char *progname)
{
  fprintf(stderr, "Usage: %s [options] file ... file\n", progname);
  fprintf(stderr, "\nOptions:\n");
  fprintf(stderr, "  --languages (-l) A,B,C   output columns A B C, in that order\n");
  fprintf(stderr, "  --sort (-s) LANGUAGE     sort output by LANGUAGE\n");
  fprintf(stderr, "  --output (-o) FILE       output to FILE (otherwise stdout)\n");
  fprintf(stderr, "  --html (-h)              output bare html table code\n");
  fprintf(stderr, "  --html-page (-p)         output table with page surround\n");
  fprintf(stderr, "  --title (-t) TITLE       use TITLE for html page\n");
  fprintf(stderr, "  --attributes (-a) ATTRS  use ATTRS for html table attributes\n");
  fprintf(stderr, "  --blank (-b) TEXT        use TEXT for blank cells\n");
  fprintf(stderr, "  --controlled (-c)        use colours etc from input\n");
  fprintf(stderr, "  --locate (-g)            show filenames and line numbers\n");
  fprintf(stderr, "  --delimited (-d) DELIM   output plain text with DELIM between cells\n");
  fprintf(stderr, "  --row-start (-S) STRING  use STRING at start of each line with -d\n");
  fprintf(stderr, "  --row-end (-S) STRING    use STRING at end of each line with -d\n");
  fprintf(stderr, "  --verbose (-v)           turn on debugging\n");
#ifdef ENABLE_TRACE_MERGE
  fprintf(stderr, "  --trace-merge (-m)       turn on merge debugging\n");
#endif
  fprintf(stderr, "\n--title implies --html-page\n");
  fprintf(stderr, "--attributes or --blank or --controlled imply --html\n");
  fprintf(stderr, "--output whatever.htm[l] implies --html\n");
  fprintf(stderr, "\nIf no other format is selected, output is as csv.\n");
}

int
main(int argc, char **argv)
{
  int html = 0;
  int page = 0;
  int raw = 0;
  char *title = "Vocabulary";
  char *attributes = NULL;
  char *blank = NULL;
  char *output_file_name = NULL;
  FILE *output_stream = stdout;
  char *language_code = "ENG";
  char *languages_string = NULL;
  char *delimiter = NULL;
  char *row_start = NULL;
  char *row_end = "\n";
  int verbose = 0;
#ifdef ENABLE_TRACE_MERGE
  int trace_merge = 0;
#endif
  int table_controlled = 0;
  int show_locations = 0;
  int files_read = 0;

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
    case 'g': show_locations = 1; break;
    case 'h': html = 1; break;
    case 'l': languages_string = optarg; break;
#ifdef ENABLE_TRACE_MERGE
    case 'm': trace_merge = 1; break;
#endif
    case 'o': output_file_name = optarg;
      if ((strcmp(output_file_name + strlen(output_file_name) - 5, ".html") == 0)
	  || (strcmp(output_file_name + strlen(output_file_name) - 4, ".htm") == 0)) {
	html = 1;
      }
      break;
    case 'p': page = html = 1; break;
    case 'r': raw = 1; break;
    case 's': language_code = optarg; break;
    case 't': page = html = 1; title = optarg; break;
    case 'v': verbose = 1; break;
#ifndef ENABLE_TRACE_MERGE
    case 'm':
#endif
    default: print_usage(argv[0]); exit(1); break;
    }
  }

  mulvoc_initialize_table(&table, 1511, START_SIZE,
			  ((verbose ? (TRACE_READ
				       | TRACE_HEADERS
				       | TRACE_PRAGMATA
				       | TRACE_COMMENTS) : 0)
			   | (show_locations ? TRACE_ORIGINS : 0)
#ifdef ENABLE_TRACE_MERGE
			   | (trace_merge ? TRACE_MERGE : 0)
#endif
			   ));

  for (; optind < argc; optind++) {
    read_vocab_file(argv[optind], &table);
    files_read++;
  }

  if (files_read == 1) {
    /* If only one file, print only line numbers, and not filenames,
       for locations */
    table.location_format = "%2$d";
  }

  if (output_file_name != NULL) {
    output_stream = fopen(output_file_name, "w");
  }

  {
    int key_idx = language_index(&table,
				 language_code,
				 strlen(language_code));
    int *languages = NULL;
    int n_languages = language_indices(&table,
				       languages_string,
				       &languages);

    if (delimiter != NULL) {
      mulvoc_output_delimited(output_stream,
			      &table,
			      languages,
			      n_languages,
			      key_idx,
			      row_start,
			      delimiter,
			      row_end);
    } else if (raw) {
      mulvoc_output_data(output_stream,
			 &table,
			 languages, n_languages,
			 key_idx);
    } else if (html) {
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


