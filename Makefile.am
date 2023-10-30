
bin_PROGRAMS = dsolve dsolve20

noinst_LIBRARIES = libmpfun.a libmpfun20.a

mpfun_srcdir = $(top_srcdir)/mpfun2015

mpfun_src = \
$(mpfun_srcdir)/mpfuna.f90 \
$(mpfun_srcdir)/mpfunbq.f90 \
$(mpfun_srcdir)/mpfunc.f90 \
$(mpfun_srcdir)/mpfund.f90 \
$(mpfun_srcdir)/mpfune.f90 \
$(mpfun_srcdir)/mpfunf.f90 \
$(mpfun_srcdir)/mpfungq2.f90 \
$(mpfun_srcdir)/mpmodule.f90 

mpfun20_srcdir = $(top_srcdir)/mpfun2020

mpfun20_src = \
$(mpfun20_srcdir)/mpfuna.f90 \
$(mpfun20_srcdir)/mpfunb.f90 \
$(mpfun20_srcdir)/mpfunc.f90 \
$(mpfun20_srcdir)/mpfund.f90 \
$(mpfun20_srcdir)/mpfune.f90 \
$(mpfun20_srcdir)/mpfunf.f90 \
$(mpfun20_srcdir)/mpfung1.f90 \
$(mpfun20_srcdir)/mpmodule.f90 \
$(mpfun20_srcdir)/mpmask13.f90 

dsolve_SOURCES = param_mod.f90 main.f90 disp_det.f90 \
Z_func.f90 dZ_func.f90 integrator.f90 muller.f90 polyfit.f90 \
read_data.f90 read_distr.f90 spline_interpol.f90 \
exp_Bessel_In.f90 exp_dBessel_In.f90 cerror.f90 cont_frac.f90 \
Bessel_int.f90 int_para.f90 int_para_mpfun2015.f90 \
F12.f90 F12_mpfun.f90 F23.f90 F23_mpfun.f90 \
gamma_func.f90 acc_F.f90 acc_Kvpa.f90 get_splinecoeff.f90 \
exp_Bessel_In_mpfun.f90 fort_Bes.f90
dsolve_FCFLAGS = -fdefault-real-8 -J$(top_srcdir) -I$(top_srcdir)
dsolve_DEPENDENCIES = libmpfun.a 
dsolve_LDADD = libmpfun.a

# A fib -- the real dependency is int_para_mpfun2015.o: mpmodule.mod
dsolve-int_para_mpfun2015.o: libmpfun.a 
	 $(AM_V_FC)$(FC) $(dsolve_FCFLAGS) $(FCFLAGS) -c -o dsolve-int_para_mpfun2015.o `if test -f 'int_para_mpfun2015.f90'; then $(CYGPATH_W) 'int_para_mpfun2015.f90'; else $(CYGPATH_W) '$(srcdir)/int_para_mpfun2015.f90'; fi`


dsolve20_SOURCES = param_mod.f90 main.f90 disp_det.f90 \
Z_func.f90 dZ_func.f90 integrator.f90 muller.f90 polyfit.f90 \
read_data.f90 read_distr.f90 spline_interpol.f90 \
exp_Bessel_In.f90 exp_dBessel_In.f90 cerror.f90 cont_frac.f90 \
Bessel_int.f90 int_para.f90 int_para_mpfun2020.f90 \
F12.f90 F12_mpfun.f90 F23.f90 F23_mpfun.f90 \
gamma_func.f90 acc_F.f90 acc_Kvpa.f90 get_splinecoeff.f90 \
exp_Bessel_In_mpfun.f90 fort_Bes.f90
dsolve20_FCFLAGS = -fdefault-real-8 -J$(top_srcdir) -I$(top_srcdir)
dsolve20_DEPENDENCIES = libmpfun20.a
dsolve20_LDADD = libmpfun20.a

# A fib -- the real dependency is int_para_mpfun2020.o: mpmodule.mod
dsolve20-int_para_mpfun2020.o: libmpfun20.a 
	 $(AM_V_FC)$(FC) $(dsolv20_FCFLAGS) $(FCFLAGS) -c -o dsolve20-int_para_mpfun2020.o `if test -f 'int_para_mpfun2020.f90'; then $(CYGPATH_W) 'int_para_mpfun2020.f90'; else $(CYGPATH_W) '$(srcdir)/int_para_mpfun2020.f90'; fi`


libmpfun_a_SOURCES = $(mpfun_src)
libmpfun_a_FCFLAGS = -J$(top_srcdir)

libmpfun20_a_SOURCES = $(mpfun20_src)
libmpfun20_a_FCFLAGS = -J$(top_srcdir)

CLEANFILES = *.mod
