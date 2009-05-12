# Top-level Makefile.am for MuLVoc
# Time-stamp: <2009-05-11 22:02:56 jcgs>

SUBDIRS = libsrc src texinfo
dist_doc_DATA = README

bin_PROGRAMS = voctest vocmerge voclu
voctest_SOURCES = src/voctest.c libsrc/mulvoc_data.c
vocmerge_SOURCES = src/vocmerge.c libsrc/mulvoc_data.c libsrc/mulvoc_out.c
voclu_SOURCES = src/voclu.c libsrc/mulvoc_data.c

info_TEXINFOS = texinfo/mulvoc.texinfo
grevo_TEXINFOS = fdl.texi

