## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

info_TEXINFOS		= doc/mmck-infix.texi
doc_mmck_infix_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi


#### library build targets

# All the source files of the extension library.
#
MMCK_INFIX_MODULES		= \
	lib/mmck.infix.core.scm		\
	lib/mmck.infix.version.scm		\
	lib/mmck.infix.scm

# All the object files that must be linked in the extension library.
#
MMCK_INFIX_RUNTIME_OBJECTS	= \
	lib/mmck.infix.core.$(OBJEXT)	\
	lib/mmck.infix.version.$(OBJEXT)	\
	lib/mmck.infix.$(OBJEXT)

# All the source files of all the modules' import libraries.
#
MMCK_INFIX_IMPORT_SOURCES	= \
	lib/mmck.infix.core.import.scm	\
	lib/mmck.infix.version.import.scm	\
	lib/mmck.infix.import.scm

# The module's import libraries needed to compile main module's object
# file.  It does not include the main import library:
#
#   lib/mmck.infix.import.$(MMCK_SOEXT)
#
MMCK_INFIX_IMPORT_DEPS	= \
	lib/mmck.infix.core.import.$(MMCK_SOEXT)	\
	lib/mmck.infix.version.import.$(MMCK_SOEXT)

# The import  libraries needed  to compile new  code using  the shared
# library  "(mmck infix)".   We need  to build  and install  all of
# them.
#
MMCK_INFIX_IMPORT_SHARED_LIBS	= \
	lib/mmck.infix.core.import.$(MMCK_SOEXT)	\
	lib/mmck.infix.version.import.$(MMCK_SOEXT)	\
	lib/mmck.infix.import.$(MMCK_SOEXT)

# This is the "(mmck infix)" shared library file pathname.
#
MMCK_INFIX_RUNTIME_SHARED_LIBS	= lib/mmck.infix.$(MMCK_SOEXT)

MMCK_INFIX_TEMP_FILES	= \
	lib/mmck.infix.version.temp			\
	lib/mmck.infix.core.temp			\
	lib/mmck.infix.temp				\
	\
	lib/mmck.infix.version.lock			\
	lib/mmck.infix.core.lock			\
	lib/mmck.infix.lock				\
	\
	lib/mmck.infix.version.stamp			\
	lib/mmck.infix.core.stamp			\
	lib/mmck.infix.stamp

## --------------------------------------------------------------------

EXTRA_DIST	+= $(MMCK_INFIX_MODULES)

CLEANFILES	+= \
	$(MMCK_INFIX_RUNTIME_SHARED_LIBS)	\
	$(MMCK_INFIX_RUNTIME_OBJECTS)	\
	$(MMCK_INFIX_IMPORT_SHARED_LIBS)	\
	$(MMCK_INFIX_IMPORT_SOURCES)		\
	$(MMCK_INFIX_TEMP_FILES)

## --------------------------------------------------------------------

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

## --------------------------------------------------------------------

lib/mmck.infix.core.stamp: lib/mmck.infix.core.scm $(MMCK_OBJECTS_DEPS)
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

## --------------------------------------------------------------------

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

## --------------------------------------------------------------------

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
	tests/test-core.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)	\
	tests/test-core.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe		\
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

tests/test-core.$(OBJEXT): tests/test-core.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-core.exe: tests/test-core.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
