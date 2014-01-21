      subroutine rrgg2gght13
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt13
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt13
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhardt13s1e1  
      doubleprecision rrgg2gghhardt13s1e0  
      doubleprecision rrgg2gghhardt13s1em1  
      doubleprecision rrgg2gghhardt13s1em2  
      doubleprecision rrgg2gghhardt13s1em3  
      doubleprecision rrgg2gghhardt13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt13s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * x1
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = x3 * s * t7
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t13 = t10 * t7
      t14 = t1 ** 2
      t15 = t14 * wd
      t16 = x4 * pi
      t17 = cos(t16)
      t18 = t17 ** 2
      t20 = x1 * z
      t21 = -z - x1 + t20
      t24 = Sqrt(x3 * t21 * t9)
      t25 = t24 ** 2
      t26 = 0.1D1 / t21
      t28 = 0.1D1 / x2
      t32 = t18 * t25
      t41 = x1 ** 2
      t42 = x3 * t41
      t43 = Sin(t16)
      t44 = t43 ** 2
      t46 = z ** 2
      t49 = t6 ** 2
      t54 = log(0.4D1 * t42 * t44 / t46 * t26 * t49 * t9)
      t64 = -0.18D2 * t15 * t18 * t25 * t26 * t28 - (0.180D3 * t32 * t14
     # * t26 + (-0.180D3 * t32 * t14 * lh - 0.90D2 * t32 * t14 - 0.90D2 
     #* t54 * t18 * t25 * t14) * t26) * wd / 0.5D1
      t65 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t64)
      t67 = x3 * z
      t68 = t3 * z
      t69 = x2 * x3
      t70 = t69 * z
      t71 = t3 * x2
      t72 = x2 * z
      t73 = t3 * t72
      t74 = sqrt(x2)
      t82 = Sqrt(-x3 * (-0.1D1 + t74) * (t74 + 0.1D1) * t21 * t9)
      t84 = 0.2D1 * t17 * t74 * t82
      t89 = x1 * x2
      t90 = t89 * z
      t91 = z + x1 - t20 - t72 - t89 + t90 - t67 - t3 + t68 + t70 + t71 
     #- t73 + t69 + t84
      t100 = t21 ** 2
      t101 = 0.1D1 / t100
      t103 = t41 * t74
      t105 = t74 * x2
      t106 = t105 * x1
      t107 = t74 * t46
      t108 = t105 * t41
      t109 = t74 * z
      t110 = x1 * t74
      t115 = t41 * t46
      t116 = x3 * t74
      t120 = z * t17 * t82
      t123 = t17 * t82
      t130 = t46 * x3
      t135 = 0.2D1 * t103 - t106 + t107 - t108 + t109 + t110 + 0.6D1 * t
     #3 * t107 + 0.8D1 * t42 * t109 - 0.4D1 * t115 * t116 - 0.4D1 * t89 
     #* t120 + 0.4D1 * t89 * t123 + 0.4D1 * t20 * t123 - 0.4D1 * t108 * 
     #t67 - 0.2D1 * t106 * t130 + 0.2D1 * t108 * t130
      t166 = -0.4D1 * t3 * t109 - 0.4D1 * t103 * z - 0.3D1 * t110 * t46 
     #+ 0.2D1 * t115 * t74 - 0.2D1 * t107 * x3 + 0.2D1 * t108 * x3 + 0.2
     #D1 * t106 * x3 + 0.2D1 * t108 * z + t106 * t46 - t108 * t46 + 0.2D
     #1 * t110 * z - 0.2D1 * t116 * z - 0.2D1 * t3 * t74 - 0.4D1 * t42 *
     # t74 - 0.4D1 * t120 - 0.4D1 * x1 * t17 * t82
      t168 = (t135 + t166) ** 2
      t170 = 0.1D1 / (z + x1 - t20 - t89 + t90)
      t175 = FJET(XB1, XB2, s, t2 * x1 * (-t67 - t3 + t68 + t70 + t71 - 
     #t73 - x2 + t69 + t84) * t26, -t8, -t2 * x1 * t91 * t26, t13, s * t
     #14 * x2 * x1 * t6 * t26, -0.9D1 / 0.8D1 * t15 * t101 * t168 * t170
     # * t28)
      rrgg2gghhardt13s1e1 = t65 * t64 - 0.9D1 / 0.8D1 * t175 * t14 * wd 
     #* t101 * t168 * t170 * t28

      end function



      doubleprecision function rrgg2gghhardt13s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t7 = t1 * (-0.1D1 + x1)
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t14 = t1 ** 2
      t17 = cos(x4 * pi)
      t18 = t17 ** 2
      t20 = -z - x1 + x1 * z
      t23 = Sqrt(x3 * t20 * t9)
      t24 = t23 ** 2
      t27 = t18 * t24 / t20
      t30 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, -0.18D2 * t14 * wd * t27)
      rrgg2gghhardt13s1e0 = -0.18D2 * t30 * t14 * wd * t27

      end function



      doubleprecision function rrgg2gghhardt13s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt13s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt13s1em4 = 0.0D0

      end function
  
      subroutine rrgg2gghsoftt13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt13s1e1  
      doubleprecision rrgg2gghsoftt13s1e0  
      doubleprecision rrgg2gghsoftt13s1em1  
      doubleprecision rrgg2gghsoftt13s1em2  
      doubleprecision rrgg2gghsoftt13s1em3  
      doubleprecision rrgg2gghsoftt13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt13s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt13s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt13s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt13s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt13s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt13s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt13s1em4 = 0.0D0

      end function
