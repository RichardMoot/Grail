
prefix      = /Users/moot/grailexec
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
datarootdir = ${prefix}/share
datadir     = ${datarootdir}/Grail

execfiles   = source/insertdot source/g3
resources   = resources/help.html resources/grail3.ps resources/grail3.dot
images      = images/arthur.jpg images/represses.jpg images/kundry_grail.jpg \
              images/monsalvat2.gif images/par_left.xpm images/par_right.xpm \
              images/par_up.xpm images/u_par_down.xpm images/u_par_up.xpm \
              images/tensor_down.xpm images/tensor_left.xpm images/ffwd.xpm \
              images/tensor_right.xpm  images/u_tensor_down.xpm \
              images/u_tensor_up.xpm images/undo.gif images/undo.xpm \
              images/next.xpm images/stop.xpm images/previous.xpm \
	      images/play.xpm \
              images/someone.jpg images/tensor_left.gif images/tensor_right.gif
grammarfiles = grammars/dutch.html grammars/engproncase1.pl \
               grammars/engproncase2.pl grammars/grail0.pl \
               grammars/hitchhiker.pl grammars/film_db.pl \
               grammars/q.pl grammars/semantics.pl grammars/dutch.pl \
               grammars/grail0.html grammars/q.html grammars/pentus.pl \
               grammars/savateev.pl grammars/drt.pl grammars/itipy.pl \
               grammars/dynamics.pl grammars/clitics.pl \
	       grammars/drt_assoc.pl grammars/clitics_simple.pl \
               grammars/cgn.pl \
               grammars/README grammars/LICENSE

package_version = 3.2.0
grail_year = 2015

rel_date := $(shell date +"%e %B %Y (%H:%M:%S %Z)" | tr -s " " | sed 's/^[ ]//g')
swi_version := $(shell swipl -v)

# perform substitutions explicitly
edit = /usr/bin/sed \
        -e 's,@graildatadir\@,$(datadir),g' \
        -e 's,@grailbindir\@,$(bindir),g'

dev2usr = /usr/bin/sed \
        -e 's,@GRAIL_VERSION\@,$(package_version),g' \
        -e 's,@COPYRIGHT_YEAR\@,$(grail_year),g' \
        -e 's,@RELEASE_DATE\@,$(rel_date),g' \
        -e 's,@SWI_VERSION\@,$(swi_version),g'
addf = /usr/bin/sed \
	-e 's,~/checkout/Grail/grammars/big_french_drt.pl,../grammars/big_french_drt,g'

tense_y = -e 's/^\%TENSE_Y //g' \
	  -e '/^\%TENSE_N.*/ d'
tense_n = -e 's/^\%TENSE_N //g' \
	  -e '/^\%TENSE_Y.*/ d'

lefff_n = -e '/^\%LEFFF_A.*/ d' \
	  -e '/^\%LEFFF_B.*/ d' \
	  -e 's/^\%LEFFF_N //g'
lefff_a = -e '/^\%LEFFF_N.*/ d' \
	  -e '/^\%LEFFF_B.*/ d' \
	  -e 's/^\%LEFFF_A //g'
lefff_b = -e '/^\%LEFFF_A.*/ d' \
	  -e '/^\%LEFFF_N.*/ d' \
	  -e 's/^\%LEFFF_B //g'


.PHONY: all install grail user release clean realclean

# the script create_exec.pl creates an executable `grail3' in the current
# directory

all:
	-cd source ; $(edit) g3 > g3.tmp
	-cd source ; mv -f g3.tmp g3
	-mkdir $(datarootdir)
	-mkdir $(datadir)
	cp -f $(images) $(datadir)
	cd source ; chmod a+x g3

#

user: source/g3.in.devel resources/grail3.html.devel README.devel
	-cd source ; $(dev2usr) g3.in.devel > g3.in
	-cd resources ; $(dev2usr) grail3.html.devel > grail3.html
	-$(dev2usr) README.devel > README

devel/Makefile.in: ../Makefile.in
	cp -f ../Makefile.in devel/Makefile.in
devel/Makefile: ../Makefile
	cp -f ../Makefile devel/Makefile
devel/configure.ac: ../configure.ac
	cp -f ../configure.ac devel/configure.ac
devel/configure: ../configure
	cp -f ../configure devel/configure

devel: devel/Makefile.in devel/Makefile devel/configure.ac devel/configure

# create grail3.ps from grail3.dot if necessary

grail3.ps: resources/grail3.dot
	cd resources ; /usr/local/bin/dot -Tps2 grail3.dot > grail3.ps

# create configure from configure.ac if necessary

../configure: ../configure.ac
	cd .. ; autoconf configure.ac > configure
configure: configure.ac
	autoconf configure.ac > configure

# move everything to the right place, creating new directories where
# necessary

install:
	-mkdir $(bindir)
	cp -f source/insertdot $(bindir)
	chmod a+x $(bindir)/insertdot
	cp -f $(resources) $(datadir)
	cp -f source/grail3 $(bindir)/g3

grammars.tgz: $(grammarfiles)
	tar cfz grammars.tgz $(grammarfiles)

# Grail light files

source/chart_var_lp.pl: source/chart_var.pl
	cd source ; /usr/bin/sed -e 's/%LP//g' chart_var.pl | tail +2 > chart_var_lp.pl
source/chart_var_cr.pl: source/chart_var.pl
	cd source ; /usr/bin/sed -e 's/%CR//g' chart_var.pl | tail +2 > chart_var_cr.pl

source/grail_light.pl: source/chart_var.pl
	-cd source ; $(addf) chart_var_lp.pl > grail_light.pl ; chmod a+x grail_light.pl
	-cd source ; $(addf) chart_var_cr.pl > grail_light_cr.pl ; chmod a+x grail_light_cr.pl

# grammars

grammars: grammars/big_french_drt_yn.pl grammars/big_french_drt_ya.pl grammars/big_french_drt_yb.pl grammars/big_french_drt_nn.pl grammars/big_french_drt_na.pl grammars/big_french_drt_nb.pl

grammars/big_french_drt_nn.pl: grammars/big_french_drt.pl
	/usr/bin/sed -e '/^.* \% DEFAULT/ d' $(tense_n) $(lefff_n) grammars/big_french_drt.pl > grammars/big_french_drt_nn.pl
grammars/big_french_drt_na.pl: grammars/big_french_drt.pl
	/usr/bin/sed -e '/^.* \% DEFAULT/ d' $(tense_n) $(lefff_a) grammars/big_french_drt.pl > grammars/big_french_drt_na.pl
grammars/big_french_drt_nb.pl: grammars/big_french_drt.pl
	/usr/bin/sed -e '/^.* \% DEFAULT/ d' $(tense_n) $(lefff_b) grammars/big_french_drt.pl > grammars/big_french_drt_nb.pl
grammars/big_french_drt_yn.pl: grammars/big_french_drt.pl
	/usr/bin/sed -e '/^.* \% DEFAULT/ d' $(tense_y) $(lefff_n) grammars/big_french_drt.pl > grammars/big_french_drt_yn.pl
grammars/big_french_drt_ya.pl: grammars/big_french_drt.pl
	/usr/bin/sed -e '/^.* \% DEFAULT/ d' $(tense_y) $(lefff_a) grammars/big_french_drt.pl > grammars/big_french_drt_ya.pl
grammars/big_french_drt_yb.pl: grammars/big_french_drt.pl
	/usr/bin/sed -e '/^.* \% DEFAULT/ d' $(tense_y) $(lefff_b) grammars/big_french_drt.pl > grammars/big_french_drt_yb.pl

# only auxiliary dot files are deleted right now; postscript files and
# the Grail 3 source files will stay where they are. In principle, all
# files - except those in $(datadir) and $(bindir) - can be deleted.

clean:
	$(RM) -f grail_log texlog *.dot *.tmp *.ps *.tex *.aux *.log *.dvi *.pdf *~

realclean:
	$(RM) -f grail_log texlog config.status *.tgz *.tar *.dot *.tmp *.ps *.tex *.aux *.log *.dvi *.pdf *~
	$(RM) -rf autom4te.cache

