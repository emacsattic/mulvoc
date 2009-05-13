/* voctest.c
   Time-stamp: <2009-05-12 20:20:50 jcgs>
 */

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include "../libsrc/mulvoc.h"
#include <sys/time.h>
#include <time.h>

#define START_SIZE 24

#define BUFFER_SIZE (256*256)

static const char *short_options = "dmt:vh";

static struct option long_options[] = {
  {"data", no_argument, 0, 'd'},
  {"help", no_argument, 0, 'h'},
  {"meta", no_argument, 0, 'm'},
  {"translate", optional_argument, 0, 't'},
  {"verbose", no_argument, 0, 'v'},
  {0, 0, 0, 0}
};

void
show_usage(char *program)
{
  fprintf(stderr,
	  "Usage: %s [--verbose] [--meta] [--data] [--translate word] file ... file\n",
	  program);
}

int
main(int argc, char **argv)
{
  int trace_flags = 0;
  int print_metadata = 0;
  int print_data = 0;
  int translate = 0;
  char *word_in;

  vocabulary_table table;
  int files_read = 0;

  clock_t start_time = clock();
  clock_t end_time;

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
    case 'd': print_data = 1; break;
    case 'h': show_usage(argv[0]); exit(0); break;
    case 'm': print_metadata = 1; break;
    case 't': translate = 1; word_in = optarg; break;
    case 'v': trace_flags = TRACE_READ | TRACE_HEADERS | TRACE_PRAGMATA | TRACE_COMMENTS; break;
    default: show_usage(argv[0]); exit(1); break;
    }
  }

  /* note: the table stores its tracing state, so the `trace_flags'
     setting we give here will affect later library calls too. */
  mulvoc_initialize_table(&table, 1511, START_SIZE, trace_flags);

  for (; optind < argc; optind++) {
    read_vocab_file(argv[optind], &table);
    files_read++;
  }

  end_time = clock();

  printf("Read %d %s in %f seconds\n",
	 files_read, (files_read == 1) ? "file" : "files",
	   ((float)(end_time - start_time)) / (float)CLOCKS_PER_SEC);

  if (print_metadata) {
    show_table_metadata(stdout, &table);
  }

  if (print_data) {
    show_table_data(stdout, &table, "-");
  }

  if (translate) {
    char buf[BUFFER_SIZE];
    printf("%s: %s\n",
	   word_in,
	   get_word_translations_string(&table, word_in,
					-1, -1, -1, -1, -1,
					"%s: %s; ",
					buf, BUFFER_SIZE));
  }

  exit(0);
}


