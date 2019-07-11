## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

info_TEXINFOS		= doc/mmck-infix.texi
doc_mmck_infix_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_INFIX_MODULES		=

# All the object files ".o" that must be linked in the extension library.
#
MMCK_INFIX_RUNTIME_OBJECTS	=

# All the source files ".import.scm" of all the modules' import libraries.
#
MMCK_INFIX_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.infix.import.$(MMCK_SOEXT)
#
MMCK_INFIX_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the shared library "(mmck  infix)".  We need to build and
# install all of them.
#
MMCK_INFIX_IMPORT_SHARED_LIBS	=

# This is the "(mmck infix)" shared library file pathname; in addition
# we also build the helpers library.
#
MMCK_INFIX_RUNTIME_SHARED_LIBS	= \
	lib/mmck.infix.$(MMCK_SOEXT)		\
	lib/mmck.infix.helpers.$(MMCK_SOEXT)

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_INFIX_TEMP_FILES		=


#### library (mmck infix): module version

MMCK_INFIX_MODULES		+= lib/mmck.infix.version.scm
MMCK_INFIX_RUNTIME_OBJECTS	+= lib/mmck.infix.version.$(OBJEXT)
MMCK_INFIX_IMPORT_SOURCES	+= lib/mmck.infix.version.import.scm
MMCK_INFIX_IMPORT_DEPS		+= lib/mmck.infix.version.import.$(MMCK_SOEXT)
MMCK_INFIX_IMPORT_SHARED_LIBS	+= lib/mmck.infix.version.import.$(MMCK_SOEXT)
MMCK_INFIX_TEMP_FILES		+= \
	lib/mmck.infix.version.temp				\
	lib/mmck.infix.version.lock				\
	lib/mmck.infix.version.stamp

# A compiler  invokation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.infix.version.stamp: lib/mmck.infix.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.infix.version.temp
	@touch lib/mmck.infix.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.infix.version.$(OBJEXT) $(<) -prologue lib/config.scm
	@$(MV) mmck.infix.version.import.scm lib/mmck.infix.version.import.scm
	@mv -f lib/mmck.infix.version.temp $@

lib/mmck.infix.version.import.scm lib/mmck.infix.version.$(OBJEXT): lib/mmck.infix.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.infix.version.lock lib/mmck.infix.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.infix.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.infix.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.infix.version.stamp; \
	    $$dry rmdir lib/mmck.infix.version.lock; \
	  else \
	    while test -d lib/mmck.infix.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.infix.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.infix.version.import.$(MMCK_SOEXT): lib/mmck.infix.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck infix): module helpers

MMCK_INFIX_MODULES		+= lib/mmck.infix.helpers.scm
MMCK_INFIX_RUNTIME_OBJECTS	+= lib/mmck.infix.helpers.$(OBJEXT)
MMCK_INFIX_IMPORT_SOURCES	+= lib/mmck.infix.helpers.import.scm
MMCK_INFIX_IMPORT_DEPS		+= lib/mmck.infix.helpers.import.$(MMCK_SOEXT)
MMCK_INFIX_IMPORT_SHARED_LIBS	+= lib/mmck.infix.helpers.import.$(MMCK_SOEXT)
MMCK_INFIX_TEMP_FILES		+= \
	lib/mmck.infix.helpers.temp				\
	lib/mmck.infix.helpers.lock				\
	lib/mmck.infix.helpers.stamp

lib/mmck.infix.helpers.stamp: lib/mmck.infix.helpers.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.infix.helpers.temp
	@touch lib/mmck.infix.helpers.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.infix.helpers.$(OBJEXT) $(<)
	@$(MV) mmck.infix.helpers.import.scm lib/mmck.infix.helpers.import.scm
	@mv -f lib/mmck.infix.helpers.temp $@

lib/mmck.infix.helpers.import.scm lib/mmck.infix.helpers.$(OBJEXT): lib/mmck.infix.helpers.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.infix.helpers.lock lib/mmck.infix.helpers.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.infix.helpers.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.infix.helpers.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.infix.helpers.stamp; \
	    $$dry rmdir lib/mmck.infix.helpers.lock; \
	  else \
	    while test -d lib/mmck.infix.helpers.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.infix.helpers.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.infix.helpers.import.$(MMCK_SOEXT): lib/mmck.infix.helpers.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

# We build this libraries to use  its facilities at expand-time in the
# other modules; we would like not to install it!
#
# But this  library is also needed  to run the test  suite; this means
# the  rule "make  installcheck" will  work  only if  we install  this
# library too.  So we do it.  In the end it is a very small library...
#
# Maybe we could do some trick with GNU Automake's substitution symbol
# @MAINTAINER_MODE_TRUE@ which (it is my understanding):
#
# * Expands to the empty string when maintainer mode is enabled.
#
# * Expands to the comment character when maintainer mode is disabled.
#
# With this we could include/exclude  the installation of this library
# depending on the mode.  We are not doing it right now.
#
# Life is hard!  (Marco Maggi; May 30, 2019)
#
lib/mmck.infix.helpers.$(MMCK_SOEXT): lib/mmck.infix.helpers.$(OBJEXT)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### library (mmck infix): module funcs

MMCK_INFIX_MODULES		+= lib/mmck.infix.funcs.scm
MMCK_INFIX_RUNTIME_OBJECTS	+= lib/mmck.infix.funcs.$(OBJEXT)
MMCK_INFIX_IMPORT_SOURCES	+= lib/mmck.infix.funcs.import.scm
MMCK_INFIX_IMPORT_DEPS		+= lib/mmck.infix.funcs.import.$(MMCK_SOEXT)
MMCK_INFIX_IMPORT_SHARED_LIBS	+= lib/mmck.infix.funcs.import.$(MMCK_SOEXT)
MMCK_INFIX_TEMP_FILES		+= \
	lib/mmck.infix.funcs.temp				\
	lib/mmck.infix.funcs.lock				\
	lib/mmck.infix.funcs.stamp

MMCK_INFIX_FUNCS_DEPS = \
	lib/mmck.infix.helpers.import.$(MMCK_SOEXT)		\
	$(MMCK_OBJECTS_DEPS)					\
	\
	lib/mmck.infix.helpers.$(MMCK_SOEXT)

lib/mmck.infix.funcs.stamp: lib/mmck.infix.funcs.scm $(MMCK_INFIX_FUNCS_DEPS)
	@rm -f lib/mmck.infix.funcs.temp
	@touch lib/mmck.infix.funcs.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.infix.funcs.$(OBJEXT) $(<)
	@$(MV) mmck.infix.funcs.import.scm lib/mmck.infix.funcs.import.scm
	@mv -f lib/mmck.infix.funcs.temp $@

lib/mmck.infix.funcs.import.scm lib/mmck.infix.funcs.$(OBJEXT): lib/mmck.infix.funcs.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.infix.funcs.lock lib/mmck.infix.funcs.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.infix.funcs.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.infix.funcs.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.infix.funcs.stamp; \
	    $$dry rmdir lib/mmck.infix.funcs.lock; \
	  else \
	    while test -d lib/mmck.infix.funcs.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.infix.funcs.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.infix.funcs.import.$(MMCK_SOEXT): lib/mmck.infix.funcs.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck infix): module core

MMCK_INFIX_MODULES		+= lib/mmck.infix.core.scm
MMCK_INFIX_RUNTIME_OBJECTS	+= lib/mmck.infix.core.$(OBJEXT)
MMCK_INFIX_IMPORT_SOURCES	+= lib/mmck.infix.core.import.scm
MMCK_INFIX_IMPORT_DEPS		+= lib/mmck.infix.core.import.$(MMCK_SOEXT)
MMCK_INFIX_IMPORT_SHARED_LIBS	+= lib/mmck.infix.core.import.$(MMCK_SOEXT)
MMCK_INFIX_TEMP_FILES		+= \
	lib/mmck.infix.core.temp				\
	lib/mmck.infix.core.lock				\
	lib/mmck.infix.core.stamp

MMCK_INFIX_CORE_DEPS = \
	lib/mmck.infix.helpers.import.$(MMCK_SOEXT)		\
	lib/mmck.infix.funcs.import.$(MMCK_SOEXT)		\
	$(MMCK_OBJECTS_DEPS)					\
	\
	lib/mmck.infix.helpers.$(MMCK_SOEXT)

lib/mmck.infix.core.stamp: lib/mmck.infix.core.scm $(MMCK_INFIX_CORE_DEPS)
	@rm -f lib/mmck.infix.core.temp
	@touch lib/mmck.infix.core.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.infix.core.$(OBJEXT) $(<)
	@$(MV) mmck.infix.core.import.scm lib/mmck.infix.core.import.scm
	@mv -f lib/mmck.infix.core.temp $@

lib/mmck.infix.core.import.scm lib/mmck.infix.core.$(OBJEXT): lib/mmck.infix.core.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.infix.core.lock lib/mmck.infix.core.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.infix.core.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.infix.core.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.infix.core.stamp; \
	    $$dry rmdir lib/mmck.infix.core.lock; \
	  else \
	    while test -d lib/mmck.infix.core.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.infix.core.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.infix.core.import.$(MMCK_SOEXT): lib/mmck.infix.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)



#### library (mmck infix): main module

MMCK_INFIX_MODULES		+= lib/mmck.infix.scm
MMCK_INFIX_RUNTIME_OBJECTS	+= lib/mmck.infix.$(OBJEXT)
MMCK_INFIX_IMPORT_SOURCES	+= lib/mmck.infix.import.scm
MMCK_INFIX_IMPORT_SHARED_LIBS	+= lib/mmck.infix.import.$(MMCK_SOEXT)
MMCK_INFIX_TEMP_FILES		+= \
	lib/mmck.infix.temp				\
	lib/mmck.infix.lock				\
	lib/mmck.infix.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.infix.stamp: lib/mmck.infix.scm $(MMCK_INFIX_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.infix.temp
	@touch lib/mmck.infix.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.infix.$(OBJEXT) $(<)
	@$(MV) mmck.infix.import.scm lib/mmck.infix.import.scm
	@mv -f lib/mmck.infix.temp $@

lib/mmck.infix.import.scm lib/mmck.infix.$(OBJEXT): lib/mmck.infix.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.infix.lock lib/mmck.infix.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.infix.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.infix.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.infix.stamp; \
	    $$dry rmdir lib/mmck.infix.lock; \
	  else \
	    while test -d lib/mmck.infix.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.infix.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.infix.import.$(MMCK_SOEXT): lib/mmck.infix.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck infix): wrapping up

EXTRA_DIST	+= $(MMCK_INFIX_MODULES)

CLEANFILES	+= \
	$(MMCK_INFIX_RUNTIME_SHARED_LIBS)	\
	$(MMCK_INFIX_RUNTIME_OBJECTS)		\
	$(MMCK_INFIX_IMPORT_SHARED_LIBS)	\
	$(MMCK_INFIX_IMPORT_SOURCES)		\
	$(MMCK_INFIX_TEMP_FILES)

lib/mmck.infix.$(MMCK_SOEXT): $(MMCK_INFIX_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_INFIX_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_INFIX_RUNTIME_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_SHARED_LIBS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST		+= \
	tests/test-version.scm		\
	tests/test-funcs.scm		\
	tests/test-core.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)	\
	tests/test-funcs.$(OBJEXT)	\
	tests/test-core.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe		\
	tests/test-funcs.exe		\
	tests/test-core.exe

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

MMCK_CHECK_DEPS	+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-funcs.$(OBJEXT): tests/test-funcs.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-funcs.exe: tests/test-funcs.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-core.$(OBJEXT): tests/test-core.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-core.exe: tests/test-core.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
