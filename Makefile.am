# Top-level Makefile.am for MuLVoc
# Time-stamp: <2009-03-14 21:18:12 jcgs>

SUBDIRS = libsrc src texinfo
dist_doc_DATA = README

bin_PROGRAMS = voctest vocmerge
voctest_SOURCES = src/voctest.c libsrc/mulvoc_data.c
vocmerge_SOURCES = src/vocmerge.c libsrc/mulvoc_data.c libsrc/mulvoc_out.c

info_TEXINFOS = texinfo/mulvoc.texinfo
grevo_TEXINFOS = fdl.texi

