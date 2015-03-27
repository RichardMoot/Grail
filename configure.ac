AC_INIT(grail, 3.2.0, Richard.Moot@labri.fr)

AC_ARG_WITH(html, [ --with-html  enable HTML help], [], [with_html="yes"])
if test "$with_html" == "yes"
then
	AC_MSG_NOTICE([HTML help enabled])
        AC_SUBST(HTML, "xpce_html.")
elif test "$with_html" == "no"
then
	AC_MSG_NOTICE([HTML help disabled])
        AC_SUBST(HTML, "% xpce_html.")
else
	AC_MSG_NOTICE([HTML help enabled])
        AC_SUBST(HTML, "xpce_html.")
fi

dnl Check for SWI Prolog/XPCE
AC_PATH_PROG(XPCE, [xpce], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(XPCE)
AC_PATH_PROG(SWIPL, [swipl], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin])
AC_SUBST(SWIPL)

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
AC_PATH_PROGS(GV, [gv evince Preview], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin$PATH_SEPARATOR/Applications/Preview.app/Contents/MacOS/Preview])
AC_SUBST(GV)

dnl Check for pdf viewer
AC_PATH_PROGS(PDF, [Preview acrobat acroread gv evince], NULL, [path=$PATH$PATH_SEPARATOR/opt/local/bin$PATH_SEPARATOR/Applications/Preview.app/Contents/MacOS/Preview])
AC_SUBST(PDF)

AC_SUBST(GRAIL_YEAR, `date +%Y`)

AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(bindir)

AC_CONFIG_FILES([Makefile source/insertdot source/g3])
AC_OUTPUT
