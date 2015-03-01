AC_INIT(grail, 3.1.3, Richard.Moot@labri.fr)

dnl Check for SWI Prolog/XPCE
AC_PATH_PROG(XPCE, [xpce], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(XPCE)

dnl Check for Tclsh
AC_PATH_PROG(TCLSH, [tclsh], NULL, [path=$PATH$PATH_SEPARATOR/usr/bin$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(TCLSH)

dnl Check for Dot
AC_PATH_PROG(DOT, [dot], NULL, [path=$PATH$PATH_SEPARATOR/Applications/Graphviz.app/Contents/MacOS])
GRAPHVIZ=`AS_DIRNAME([$DOT])`
AC_SUBST(GRAPHVIZ)

dnl Check for PdfLaTeX
AC_PATH_PROG(PDFLATEX, [pdflatex], NULL, [path=$PATH$PATH_SEPARATOR/usr/texbin/])
AC_SUBST(PDFLATEX)

dnl Check for mv
AC_PATH_PROG(MV, [mv], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(MV)

dnl Check for sed
AC_PATH_PROG(SED, [sed], NULL, [path=$PATH$PATH_SEPARATOR/usr/bin$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(SED)

dnl Check for gv
AC_PATH_PROGS(GV, [gv open], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(GV)

dnl Check for pdf viewer
AC_PATH_PROGS(PDF, [open acrobat acroread gv], NULL)
AC_SUBST(PDF)

AC_SUBST(GRAIL_YEAR, `date +%Y`)

AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(bindir)

AC_CONFIG_FILES([Makefile Grail/Makefile Grail/source/insertdot Grail/source/g3])
AC_OUTPUT
