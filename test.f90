program main
  use mpmodule
  implicit none

  integer :: nwds=4, ndp=30
  real(kind=8) :: r = 6.6e6, x, y
  type(mp_real) :: mp_r

  complex(kind=8) :: c = (6.6e6,5.5e5), c1
  type(mp_complex) :: mp_c
  complex, parameter :: i=(0.0,1.d0)

  type(mp_real) :: vpa2
  integer :: ipara=1
  integer :: iarb=1
  integer :: npara_max=255
  integer :: narb=1
  real, allocatable, dimension (:,:) :: vpara

  ! test real
  write(*,*)'r=',r
  mp_r = mpreald(r,nwds)
  mp_r = mpreal(r,nwds)

  ! test complex
  write(*,*)'c=',c
  mp_c = mpcmplxdc(c,nwds)
  mp_c = mpcmplx(c,nwds)

  c1 = complex(real(c),aimag(c))
  mp_c = mpcmplxdc(c1,nwds)
  mp_c = mpcmplxdc(complex(real(c1),aimag(c1)),nwds)

  x = c
  y = aimag(c)
  write(*,*)'x=',x,'y=',y

  c = mp_c

  mp_r = mp_c
  c = qreal(mp_r)+i*qreal(aimag(mp_c))
  write(*,*)'c=',c

  ! This code snippet is for the second problem with MPDMC and mpfun20-fort-v05
  allocate(vpara(npara_max,narb))
  write(*,*)vpara(ipara+1,iarb)
  vpa2=mpreald(vpara(ipara+1,iarb),nwds)
  deallocate(vpara)

end program main

