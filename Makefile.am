
bin_PROGRAMS = dsolve dsolve20 dsolvempfr test test20 testmpfr

noinst_LIBRARIES = libmpfun.a libmpfun20.a libmpfunmpfr.a

mpfun_srcdir = $(top_srcdir)/mpfun2015

mpfun_src = \
$(mpfun_srcdir)/mpfuna.f90 \
$(mpfun_srcdir)/mpfunbq.f90 \
$(mpfun_srcdir)/mpfunc.f90 \
$(mpfun_srcdir)/mpfund.f90 \
$(mpfun_srcdir)/mpfune.f90 \
$(mpfun_srcdir)/mpfunf.f90 \
$(mpfun_srcdir)/mpfungq2.f90 \
$(mpfun_srcdir)/second.f90 \
$(mpfun_srcdir)/mpmodule.f90 

mpfun20_srcdir = $(top_srcdir)/mpfun2020

mpfun20_src = \
$(mpfun20_srcdir)/mpfuna.f90 \
$(mpfun20_srcdir)/mpfunb.f90 \
$(mpfun20_srcdir)/mpfunc.f90 \
$(mpfun20_srcdir)/mpfund.f90 \
$(mpfun20_srcdir)/mpfune.f90 \
$(mpfun20_srcdir)/mpfunf.f90 \
$(mpfun20_srcdir)/mpfung2.f90 \
$(mpfun20_srcdir)/mpfunh2.f90 \
$(mpfun20_srcdir)/mpmask13.f90 \
$(mpfun20_srcdir)/second.f90 \
$(mpfun20_srcdir)/mpmodule.f90 

mpfunmpfr_srcdir = $(top_srcdir)/mpfunmpfr

mpfunmpfr_src = \
$(mpfunmpfr_srcdir)/mpfuna.f90 \
$(mpfunmpfr_srcdir)/mpfunf.f90 \
$(mpfunmpfr_srcdir)/mpfungq2.f90 \
$(mpfunmpfr_srcdir)/mpfunhq2.f90 \
$(mpfunmpfr_srcdir)/second.f90 \
$(mpfunmpfr_srcdir)/mpmodule.f90 \
$(mpfunmpfr_srcdir)/second.f90 \
$(mpfunmpfr_srcdir)/mpinterface.c 

dsolve_SOURCES = param_mod.f90 main.f90 disp_det.f90 \
Z_func.f90 dZ_func.f90 integrator.f90 muller.f90 polyfit.f90 \
read_data.f90 read_distr.f90 spline_interpol.f90 \
exp_Bessel_In.f90 exp_dBessel_In.f90 cerror.f90 cont_frac.f90 \
Bessel_int.f90 int_para.f90 int_para_mpfun2015.f90 \
F12.f90 F12_mpfun.f90 F23.f90 F23_mpfun.f90 \
gamma_func.f90 acc_F.f90 acc_Kvpa.f90 get_splinecoeff.f90 \
exp_Bessel_In_mpfun.f90 fort_Bes.f90
dsolve_FCFLAGS = -fdefault-real-8 -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -J$(mpfun_srcdir)
dsolve_DEPENDENCIES = libmpfun.a 
dsolve_LDADD = libmpfun.a

# A fib -- the real dependency is int_para_mpfun2015.o: mpmodule.mod
dsolve-int_para_mpfun2015.o: libmpfun.a int_para_mpfun2015.f90
	 $(AM_V_FC)$(FC) $(dsolve_FCFLAGS) $(FCFLAGS) -c -o dsolve-int_para_mpfun2015.o `if test -f 'int_para_mpfun2015.f90'; then $(CYGPATH_W) 'int_para_mpfun2015.f90'; else $(CYGPATH_W) '$(srcdir)/int_para_mpfun2015.f90'; fi`


dsolve20_SOURCES = param_mod.f90 main.f90 disp_det.f90 \
Z_func.f90 dZ_func.f90 integrator.f90 muller.f90 polyfit.f90 \
read_data.f90 read_distr.f90 spline_interpol.f90 \
exp_Bessel_In.f90 exp_dBessel_In.f90 cerror.f90 cont_frac.f90 \
Bessel_int.f90 int_para.f90 int_para_mpfun2015.f90 \
F12.f90 F12_mpfun.f90 F23.f90 F23_mpfun.f90 \
gamma_func.f90 acc_F.f90 acc_Kvpa.f90 get_splinecoeff.f90 \
exp_Bessel_In_mpfun.f90 fort_Bes.f90
dsolve20_FCFLAGS = -fdefault-real-8 -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -J$(top_srcdir)/mpfun2020
dsolve20_DEPENDENCIES = libmpfun20.a
dsolve20_LDADD = libmpfun20.a

# A fib -- the real dependency is int_para_mpfun2015.o: mpmodule.mod
dsolve20-int_para_mpfun2015.o: libmpfun20.a int_para_mpfun2015.f90
	 $(AM_V_FC)$(FC) $(dsolve20_FCFLAGS) $(FCFLAGS) -c -o dsolve20-int_para_mpfun2015.o `if test -f 'int_para_mpfun2015.f90'; then $(CYGPATH_W) 'int_para_mpfun2015.f90'; else $(CYGPATH_W) '$(srcdir)/int_para_mpfun2015.f90'; fi`


dsolvempfr_SOURCES = param_mod.f90 main.f90 disp_det.f90 \
Z_func.f90 dZ_func.f90 integrator.f90 muller.f90 polyfit.f90 \
read_data.f90 read_distr.f90 spline_interpol.f90 \
exp_Bessel_In.f90 exp_dBessel_In.f90 cerror.f90 cont_frac.f90 \
Bessel_int.f90 int_para.f90 int_para_mpfun2015.f90 \
F12.f90 F12_mpfun.f90 F23.f90 F23_mpfun.f90 \
gamma_func.f90 acc_F.f90 acc_Kvpa.f90 get_splinecoeff.f90 \
exp_Bessel_In_mpfun.f90 fort_Bes.f90
dsolvempfr_FCFLAGS = -fdefault-real-8 -fno-underscoring -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -J$(top_srcdir)/mpfunmpfr
dsolvempfr_DEPENDENCIES = libmpfunmpfr.a
dsolvempfr_LDADD = libmpfunmpfr.a -lmpfr

# A fib -- the real dependency is int_para_mpfun2020.o: mpmodule.mod
dsolvempfr-int_para_mpfun2015.o: libmpfunmpfr.a int_para_mpfun2015.f90
	 $(AM_V_FC)$(FC) $(dsolvempfr_FCFLAGS) $(FCFLAGS) -c -o dsolvempfr-int_para_mpfun2015.o `if test -f 'int_para_mpfun2015.f90'; then $(CYGPATH_W) 'int_para_mpfun2015.f90'; else $(CYGPATH_W) '$(srcdir)/int_para_mpfun2015.f90'; fi`


test_SOURCES = test.f90
test_FCFLAGS = -fdefault-real-8 -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -J$(top_srcdir)/mpfun2015
test_DEPENDENCIES = libmpfun.a
test_LDADD = libmpfun.a

test20_SOURCES = test.f90
test20_FCFLAGS = -fdefault-real-8 -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -J$(top_srcdir)/mpfun2020
test20_DEPENDENCIES = libmpfun20.a
test20_LDADD = libmpfun20.a

testmpfr_SOURCES = test.f90
testmpfr_FCFLAGS = -fdefault-real-8 -fno-underscoring -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -J$(top_srcdir)/mpfunmpfr
testmpfr_DEPENDENCIES = libmpfunmpfr.a
testmpfr_LDADD = libmpfunmpfr.a -lmpfr

libmpfun_a_SOURCES = $(mpfun_src)
libmpfun_a_FCFLAGS = -J$(mpfun_srcdir)

libmpfun20_a_SOURCES = $(mpfun20_src)
libmpfun20_a_FCFLAGS = -J$(mpfun20_srcdir)

libmpfunmpfr_a_SOURCES = $(mpfunmpfr_src)
libmpfunmpfr_a_FCFLAGS = -fno-underscoring -J$(mpfunmpfr_srcdir)
libmpfunmpfr_a_CFLAGS = 

CLEANFILES = *.mod $(mpfun_srcdir)/*.mod $(mpfun20_srcdir)/*.mod $(mpfunmpfr_srcdir)/*.mod
