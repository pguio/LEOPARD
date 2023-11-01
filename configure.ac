# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.71])

AC_INIT([LEOPARD],[1.0],[patrick.guio@gmail.com])

AC_CONFIG_AUX_DIR([config])
AC_CONFIG_SRCDIR([main.f90])

AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE([foreign 1.9 dist-bzip2 subdir-objects])

AM_MAINTAINER_MODE

AC_MSG_NOTICE([

Configuring $PACKAGE_STRING for $target

])


# Checks for programs.
AC_PROG_FC([gfortran])
AC_PROG_CC([gcc])
AC_PROG_RANLIB

AC_LANG(Fortran)

dnl Checks for libraries.
AC_FC_LIBRARY_LDFLAGS

dnl Checks for header files.
AC_CHECK_HEADER([mpfr.h], [AC_DEFINE(HAVE_MPFR_H)])
AM_CONDITIONAL([MPFR], [test "x$ac_cv_header_mpfr_h" = xyes])

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

dnl Config makefile
AC_CONFIG_FILES([Makefile])

AC_OUTPUT

