  
      subroutine RVbbargHren0n
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision RVbbargHren0n1e1  
      doubleprecision RVbbargHren0n1e0  
      doubleprecision RVbbargHren0n1em1  
      doubleprecision RVbbargHren0n1em2  
      doubleprecision RVbbargHren0n1em3  
      doubleprecision RVbbargHren0n1em4  
      doubleprecision RVbbargHren0n2e1  
      doubleprecision RVbbargHren0n2e0  
      doubleprecision RVbbargHren0n2em1  
      doubleprecision RVbbargHren0n2em2  
      doubleprecision RVbbargHren0n2em3  
      doubleprecision RVbbargHren0n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=RVbbargHren0n1e1(s, XB1, XB2, z, lh, wd, nf, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargHren0n2e1(s, XB1, XB2, z, lh, wd, nf, x1)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=RVbbargHren0n1e0(s, XB1, XB2, z, lh, wd, nf, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargHren0n2e0(s, XB1, XB2, z, lh, wd, nf, x1)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=RVbbargHren0n1em1(s, XB1, XB2, z, lh, wd, nf, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargHren0n2em1(s, XB1, XB2, z, lh, wd, nf, x1)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=RVbbargHren0n1em2(s, XB1, XB2, z, lh, wd, nf, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargHren0n2em2(s, XB1, XB2, z, lh, wd, nf, x1)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=RVbbargHren0n1em3(s, XB1, XB2, z, lh, wd, nf, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargHren0n2em3(s, XB1, XB2, z, lh, wd, nf, x1)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=RVbbargHren0n1em4(s, XB1, XB2, z, lh, wd, nf, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargHren0n2em4(s, XB1, XB2, z, lh, wd, nf, x1)  
      end if  
      end if  
      end subroutine

      doubleprecision function RVbbargHren0n1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.57D2 + 0.2D1 * nf
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t8 = t7 * wd
      t9 = 0.1D1 / z
      t10 = t9 * x1
      t11 = log(t10)
      t12 = t11 ** 2
      t17 = -0.1D1 + 0.2D1 * z - t6
      t19 = lh * 0.3141592653589793D1
      t20 = t4 * t7
      t24 = (-0.12D2 * t5 * t17 + 0.12D2 * t19 * t20) * wd
      t26 = t4 * t17
      t29 = lh ** 2
      t30 = 0.6D1 * t29
      t31 = 0.3141592653589793D1 ** 2
      t32 = -t30 + t31
      t36 = (0.12D2 * t19 * t26 + t32 * 0.3141592653589793D1 * t20) * wd
      t38 = 0.1D1 / x1
      t40 = log(t9)
      t43 = t40 ** 2
      t62 = -(0.6D1 * t5 * t8 * t12 + t24 * t11 - t36) * t38 / 0.36D2 + 
     #((-t30 + t31 - 0.12D2 * t40 * lh - 0.6D1 * t43) * 0.31415926535897
     #93D1 * t26 + (0.2D1 * t29 * lh + 0.4808227612638376D1 - lh * t31 -
     # t40 * t32 + 0.6D1 * t43 * lh + 0.2D1 * t43 * t40) * 0.31415926535
     #89793D1 * t20) * wd / 0.36D2
      t63 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t62)
      t65 = -0.1D1 + x1
      t69 = log(-t10 * t65)
      t70 = t69 ** 2
      t75 = -0.6D1 * t5 * t8 * t70 - t24 * t69 + t36
      t78 = FJET(XB1, XB2, s, -t2 * t65, t2 * x1, 0.0D0, 0.0D0, 0.0D0, -
     #t75 * t38 / 0.36D2)
      RVbbargHren0n1e1 = t63 * t62 - t78 * t75 * t38 / 0.36D2

      end function



      doubleprecision function RVbbargHren0n1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.57D2 + 0.2D1 * nf
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t8 = t7 * wd
      t9 = 0.1D1 / z
      t10 = t9 * x1
      t11 = log(t10)
      t16 = -0.1D1 + 0.2D1 * z - t6
      t19 = t4 * t7
      t23 = (-0.12D2 * t5 * t16 + 0.12D2 * lh * 0.3141592653589793D1 * t
     #19) * wd
      t25 = 0.1D1 / x1
      t27 = log(t9)
      t33 = lh ** 2
      t35 = 0.3141592653589793D1 ** 2
      t38 = t27 ** 2
      t46 = -(-0.12D2 * t5 * t8 * t11 - t23) * t25 / 0.36D2 + ((0.12D2 *
     # lh + 0.12D2 * t27) * 0.3141592653589793D1 * t4 * t16 + (-0.6D1 * 
     #t33 + t35 - 0.12D2 * t27 * lh - 0.6D1 * t38) * 0.3141592653589793D
     #1 * t19) * wd / 0.36D2
      t47 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t46)
      t49 = -0.1D1 + x1
      t53 = log(-t10 * t49)
      t57 = 0.12D2 * t5 * t8 * t53 + t23
      t60 = FJET(XB1, XB2, s, -t2 * t49, t2 * x1, 0.0D0, 0.0D0, 0.0D0, -
     #t57 * t25 / 0.36D2)
      RVbbargHren0n1e0 = t47 * t46 - t60 * t57 * t25 / 0.36D2

      end function



      doubleprecision function RVbbargHren0n1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.57D2 + 0.2D1 * nf
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t10 = t7 * wd / x1
      t12 = t5 * t10 / 0.3D1
      t18 = log(0.1D1 / z)
      t27 = -t12 + (-0.12D2 * t5 * (-0.1D1 + 0.2D1 * z - t6) + (0.12D2 *
     # lh + 0.12D2 * t18) * 0.3141592653589793D1 * t4 * t7) * wd / 0.36D
     #2
      t28 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t27)
      t33 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, t12)
      RVbbargHren0n1em1 = t28 * t27 + t33 * 0.3141592653589793D1 * t4 * 
     #t10 / 0.3D1

      end function



      doubleprecision function RVbbargHren0n1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t4 = -0.57D2 + 0.2D1 * nf
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t11 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t4 * t7 * wd / 0.3D1)
      RVbbargHren0n1em2 = -t11 * 0.3141592653589793D1 * t4 * t7 * wd / 0
     #.3D1

      end function



      doubleprecision function RVbbargHren0n1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargHren0n1em3 = 0.0D0

      end function



      doubleprecision function RVbbargHren0n1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargHren0n1em4 = 0.0D0

      end function


      doubleprecision function RVbbargHren0n2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.57D2 + 0.2D1 * nf
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t8 = t7 * wd
      t9 = 0.1D1 / z
      t10 = t9 * x1
      t11 = log(t10)
      t12 = t11 ** 2
      t17 = -0.1D1 + 0.2D1 * z - t6
      t19 = lh * 0.3141592653589793D1
      t20 = t4 * t7
      t24 = (-0.12D2 * t5 * t17 + 0.12D2 * t19 * t20) * wd
      t26 = t4 * t17
      t29 = lh ** 2
      t30 = 0.6D1 * t29
      t31 = 0.3141592653589793D1 ** 2
      t32 = -t30 + t31
      t36 = (0.12D2 * t19 * t26 + t32 * 0.3141592653589793D1 * t20) * wd
      t38 = 0.1D1 / x1
      t40 = log(t9)
      t43 = t40 ** 2
      t62 = -(0.6D1 * t5 * t8 * t12 + t24 * t11 - t36) * t38 / 0.36D2 + 
     #((-t30 + t31 - 0.12D2 * t40 * lh - 0.6D1 * t43) * 0.31415926535897
     #93D1 * t26 + (0.2D1 * t29 * lh + 0.4808227612638376D1 - lh * t31 -
     # t40 * t32 + 0.6D1 * t43 * lh + 0.2D1 * t43 * t40) * 0.31415926535
     #89793D1 * t20) * wd / 0.36D2
      t63 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t62)
      t66 = -0.1D1 + x1
      t69 = log(-t10 * t66)
      t70 = t69 ** 2
      t75 = -0.6D1 * t5 * t8 * t70 - t24 * t69 + t36
      t78 = FJET(XB1, XB2, s, t2 * x1, -t2 * t66, 0.0D0, 0.0D0, 0.0D0, -
     #t75 * t38 / 0.36D2)
      RVbbargHren0n2e1 = t63 * t62 - t78 * t75 * t38 / 0.36D2

      end function



      doubleprecision function RVbbargHren0n2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.57D2 + 0.2D1 * nf
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t8 = t7 * wd
      t9 = 0.1D1 / z
      t10 = t9 * x1
      t11 = log(t10)
      t16 = -0.1D1 + 0.2D1 * z - t6
      t19 = t4 * t7
      t23 = (-0.12D2 * t5 * t16 + 0.12D2 * lh * 0.3141592653589793D1 * t
     #19) * wd
      t25 = 0.1D1 / x1
      t27 = log(t9)
      t33 = lh ** 2
      t35 = 0.3141592653589793D1 ** 2
      t38 = t27 ** 2
      t46 = -(-0.12D2 * t5 * t8 * t11 - t23) * t25 / 0.36D2 + ((0.12D2 *
     # lh + 0.12D2 * t27) * 0.3141592653589793D1 * t4 * t16 + (-0.6D1 * 
     #t33 + t35 - 0.12D2 * t27 * lh - 0.6D1 * t38) * 0.3141592653589793D
     #1 * t19) * wd / 0.36D2
      t47 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t46)
      t50 = -0.1D1 + x1
      t53 = log(-t10 * t50)
      t57 = 0.12D2 * t5 * t8 * t53 + t23
      t60 = FJET(XB1, XB2, s, t2 * x1, -t2 * t50, 0.0D0, 0.0D0, 0.0D0, -
     #t57 * t25 / 0.36D2)
      RVbbargHren0n2e0 = t47 * t46 - t60 * t57 * t25 / 0.36D2

      end function



      doubleprecision function RVbbargHren0n2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.57D2 + 0.2D1 * nf
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t10 = t7 * wd / x1
      t12 = t5 * t10 / 0.3D1
      t18 = log(0.1D1 / z)
      t27 = -t12 + (-0.12D2 * t5 * (-0.1D1 + 0.2D1 * z - t6) + (0.12D2 *
     # lh + 0.12D2 * t18) * 0.3141592653589793D1 * t4 * t7) * wd / 0.36D
     #2
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t27)
      t33 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, t12)
      RVbbargHren0n2em1 = t28 * t27 + t33 * 0.3141592653589793D1 * t4 * 
     #t10 / 0.3D1

      end function



      doubleprecision function RVbbargHren0n2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t4 = -0.57D2 + 0.2D1 * nf
      t6 = z ** 2
      t7 = 0.1D1 + t6
      t11 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, -0.3141592653589793D1 * t4 * t7 * wd / 0.3D1)
      RVbbargHren0n2em2 = -t11 * 0.3141592653589793D1 * t4 * t7 * wd / 0
     #.3D1

      end function



      doubleprecision function RVbbargHren0n2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargHren0n2em3 = 0.0D0

      end function



      doubleprecision function RVbbargHren0n2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargHren0n2em4 = 0.0D0

      end function
