# Top-level Makefile.am for MuLVoc
# Time-stamp: <2009-05-15 21:27:54 jcgs>

SUBDIRS = libsrc src texinfo
dist_doc_DATA = README

bin_PROGRAMS = voctest vocmerge voclu voclist
voctest_SOURCES = src/voctest.c libsrc/mulvoc_data.c
vocmerge_SOURCES = src/vocmerge.c libsrc/mulvoc_data.c libsrc/mulvoc_out.c
voclu_SOURCES = src/voclu.c libsrc/mulvoc_data.c
voclist_SOURCES = src/voclist.c libsrc/mulvoc_data.c

info_TEXINFOS = texinfo/mulvoc.texinfo
grevo_TEXINFOS = fdl.texi

