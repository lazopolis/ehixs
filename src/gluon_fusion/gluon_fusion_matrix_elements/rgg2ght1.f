  
      subroutine rgg2ght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision rgg2ght1s1e1  
      doubleprecision rgg2ght1s1e0  
      doubleprecision rgg2ght1s1em1  
      doubleprecision rgg2ght1s1em2  
      doubleprecision rgg2ght1s1em3  
      doubleprecision rgg2ght1s1em4  
      doubleprecision rgg2ght1s2e1  
      doubleprecision rgg2ght1s2e0  
      doubleprecision rgg2ght1s2em1  
      doubleprecision rgg2ght1s2em2  
      doubleprecision rgg2ght1s2em3  
      doubleprecision rgg2ght1s2em4  
     

      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rgg2ght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rgg2ght1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rgg2ght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rgg2ght1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rgg2ght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rgg2ght1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rgg2ght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rgg2ght1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rgg2ght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rgg2ght1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rgg2ght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rgg2ght1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rgg2ght1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = t3 * wd
      t5 = x1 * t3
      t6 = log(t5)
      t8 = z ** 2
      t10 = (t8 - z + 0.1D1) ** 2
      t13 = lh * t3
      t14 = wd * t10
      t18 = 0.1D1 / x1
      t20 = lh ** 2
      t22 = 0.3141592653589793D1 ** 2
      t23 = log(t3)
      t26 = t23 ** 2
      t34 = -(0.12D2 * t4 * (-t6 + 0.1D1) * t10 - 0.12D2 * t13 * t14) * 
     #t18 / 0.4D1 + (-0.6D1 * t20 + t22 - 0.12D2 * t23 * lh - 0.6D1 * t2
     #6 + 0.12D2 * lh + 0.12D2 * t23 - 0.12D2) * t3 * t14 / 0.4D1
      t35 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t34)
      t37 = -0.1D1 + x1
      t40 = x1 * z
      t42 = x1 ** 2
      t43 = t42 * x1
      t45 = 0.8D1 * t43 * z
      t47 = 0.12D2 * t43 * t8
      t48 = t8 * z
      t50 = 0.8D1 * t43 * t48
      t51 = t8 * x1
      t53 = t48 * x1
      t55 = t8 ** 2
      t57 = 0.2D1 * t55 * x1
      t58 = t42 ** 2
      t60 = 0.4D1 * t58 * z
      t62 = 0.6D1 * t58 * t8
      t64 = 0.4D1 * t58 * t48
      t66 = 0.2D1 * t43 * t55
      t67 = t58 * t55
      t68 = -0.1D1 - 0.10D2 * t40 - t45 + t47 - t50 + 0.16D2 * t51 - 0.1
     #0D2 * t53 + t57 + t60 - t62 + t64 + t66 - t67
      t69 = z * t42
      t71 = t8 * t42
      t73 = t48 * t42
      t76 = 0.3D1 * t55 * t42
      t78 = log(-t5 * t37)
      t83 = 0.1D1 + 0.8D1 * t40 + t45 - t47 + t50 - 0.12D2 * t51 + 0.8D1
     # * t53 - t57 - t60 + t62 - t64 - t66
      t87 = 0.3D1 * t8
      t88 = 0.2D1 * z
      t89 = 0.2D1 * x1
      t90 = 0.2D1 * t43
      t91 = 0.2D1 * t48
      t92 = 0.3D1 * t42
      t93 = t67 - 0.12D2 * t69 + 0.18D2 * t71 - 0.12D2 * t73 + t76 + t87
     # - t88 - t89 - t90 - t91 + t55 + t58 + t92
      t94 = t83 + t93
      t96 = 0.14D2 * t69 - 0.22D2 * t71 + 0.14D2 * t73 - t76 + (0.2D1 - 
     #t78) * t94 - t87 + t88 + t89 + t90 + t91 - t55 - t58 - t92
      t102 = -0.12D2 * t4 * (t68 + t96) + 0.12D2 * t13 * wd * t94
      t105 = FJET(XB1, XB2, s, -t2 * t37, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #-t102 * t18 / 0.4D1)
      rgg2ght1s1e1 = t35 * t34 - t105 * t102 * t18 / 0.4D1

      end function



      doubleprecision function rgg2ght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = t3 * wd
      t5 = z ** 2
      t7 = (t5 - z + 0.1D1) ** 2
      t8 = 0.1D1 / x1
      t12 = log(t3)
      t19 = -0.3D1 * t4 * t7 * t8 + (0.12D2 * lh + 0.12D2 * t12 - 0.12D2
     #) * t3 * t7 * wd / 0.4D1
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t19)
      t27 = x1 ** 2
      t28 = t27 * x1
      t33 = t5 * z
      t40 = t5 ** 2
      t43 = t27 ** 2
      t52 = 0.1D1 + 0.8D1 * x1 * z + 0.8D1 * t28 * z - 0.12D2 * t28 * t5
     # + 0.8D1 * t28 * t33 - 0.12D2 * t5 * x1 + 0.8D1 * t33 * x1 - 0.2D1
     # * t40 * x1 - 0.4D1 * t43 * z + 0.6D1 * t43 * t5 - 0.4D1 * t43 * t
     #33 - 0.2D1 * t28 * t40
      t68 = t43 * t40 - 0.12D2 * z * t27 + 0.18D2 * t5 * t27 - 0.12D2 * 
     #t33 * t27 + 0.3D1 * t40 * t27 + 0.3D1 * t5 - 0.2D1 * t28 - 0.2D1 *
     # t33 + t40 + t43 + 0.3D1 * t27 - 0.2D1 * z - 0.2D1 * x1
      t69 = t52 + t68
      t73 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, 0.3D1 * t4 * t69 * t8)
      rgg2ght1s1e0 = t20 * t19 + 0.3D1 * t73 * t3 * wd * t69 * t8

      end function



      doubleprecision function rgg2ght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = 0.1D1 / z
      t5 = z ** 2
      t7 = (t5 - z + 0.1D1) ** 2
      t10 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, -0.3D1 * t3 * wd * t7)
      rgg2ght1s1em1 = -0.3D1 * t10 * t3 * wd * t7

      end function



      doubleprecision function rgg2ght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgg2ght1s1em2 = 0.0D0

      end function



      doubleprecision function rgg2ght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgg2ght1s1em3 = 0.0D0

      end function



      doubleprecision function rgg2ght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgg2ght1s1em4 = 0.0D0

      end function


      doubleprecision function rgg2ght1s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = t3 * wd
      t5 = x1 * t3
      t6 = log(t5)
      t8 = z ** 2
      t10 = (t8 - z + 0.1D1) ** 2
      t13 = lh * t3
      t14 = wd * t10
      t18 = 0.1D1 / x1
      t20 = lh ** 2
      t22 = 0.3141592653589793D1 ** 2
      t23 = log(t3)
      t26 = t23 ** 2
      t34 = -(0.12D2 * t4 * (-t6 + 0.1D1) * t10 - 0.12D2 * t13 * t14) * 
     #t18 / 0.4D1 + (-0.6D1 * t20 + t22 - 0.12D2 * t23 * lh - 0.6D1 * t2
     #6 + 0.12D2 * lh + 0.12D2 * t23 - 0.12D2) * t3 * t14 / 0.4D1
      t35 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t34)
      t38 = -0.1D1 + x1
      t40 = x1 * z
      t42 = x1 ** 2
      t43 = t42 * x1
      t45 = 0.8D1 * t43 * z
      t47 = 0.12D2 * t43 * t8
      t48 = t8 * z
      t50 = 0.8D1 * t43 * t48
      t51 = t8 * x1
      t53 = t48 * x1
      t55 = t8 ** 2
      t57 = 0.2D1 * t55 * x1
      t58 = t42 ** 2
      t60 = 0.4D1 * t58 * z
      t62 = 0.6D1 * t58 * t8
      t64 = 0.4D1 * t58 * t48
      t66 = 0.2D1 * t43 * t55
      t67 = t58 * t55
      t68 = -0.1D1 - 0.10D2 * t40 - t45 + t47 - t50 + 0.16D2 * t51 - 0.1
     #0D2 * t53 + t57 + t60 - t62 + t64 + t66 - t67
      t69 = z * t42
      t71 = t8 * t42
      t73 = t48 * t42
      t76 = 0.3D1 * t55 * t42
      t78 = log(-t5 * t38)
      t83 = 0.1D1 + 0.8D1 * t40 + t45 - t47 + t50 - 0.12D2 * t51 + 0.8D1
     # * t53 - t57 - t60 + t62 - t64 - t66
      t87 = 0.2D1 * z
      t88 = 0.2D1 * x1
      t89 = 0.3D1 * t8
      t90 = 0.2D1 * t43
      t91 = 0.2D1 * t48
      t92 = 0.3D1 * t42
      t93 = t67 - 0.12D2 * t69 + 0.18D2 * t71 - 0.12D2 * t73 + t76 - t87
     # - t88 + t89 - t90 - t91 + t55 + t58 + t92
      t94 = t83 + t93
      t96 = 0.14D2 * t69 - 0.22D2 * t71 + 0.14D2 * t73 - t76 + (0.2D1 - 
     #t78) * t94 + t87 + t88 - t89 + t90 + t91 - t55 - t58 - t92
      t102 = -0.12D2 * t4 * (t68 + t96) + 0.12D2 * t13 * wd * t94
      t105 = FJET(XB1, XB2, s, t2 * x1, -t2 * t38, 0.0D0, 0.0D0, 0.0D0, 
     #-t102 * t18 / 0.4D1)
      rgg2ght1s2e1 = t35 * t34 - t105 * t102 * t18 / 0.4D1

      end function



      doubleprecision function rgg2ght1s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = t3 * wd
      t5 = z ** 2
      t7 = (t5 - z + 0.1D1) ** 2
      t8 = 0.1D1 / x1
      t12 = log(t3)
      t19 = -0.3D1 * t4 * t7 * t8 + (0.12D2 * lh + 0.12D2 * t12 - 0.12D2
     #) * t3 * t7 * wd / 0.4D1
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t19)
      t27 = x1 ** 2
      t28 = t27 * x1
      t33 = t5 * z
      t40 = t5 ** 2
      t43 = t27 ** 2
      t52 = 0.1D1 + 0.8D1 * x1 * z + 0.8D1 * t28 * z - 0.12D2 * t28 * t5
     # + 0.8D1 * t28 * t33 - 0.12D2 * t5 * x1 + 0.8D1 * t33 * x1 - 0.2D1
     # * t40 * x1 - 0.4D1 * t43 * z + 0.6D1 * t43 * t5 - 0.4D1 * t43 * t
     #33 - 0.2D1 * t28 * t40
      t68 = t43 * t40 - 0.12D2 * z * t27 + 0.18D2 * t5 * t27 - 0.12D2 * 
     #t33 * t27 + 0.3D1 * t40 * t27 + 0.3D1 * t5 - 0.2D1 * t28 - 0.2D1 *
     # t33 + t40 + t43 + 0.3D1 * t27 - 0.2D1 * z - 0.2D1 * x1
      t69 = t52 + t68
      t73 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, 0.3D1 * t4 * t69 * t8)
      rgg2ght1s2e0 = t20 * t19 + 0.3D1 * t73 * t3 * wd * t69 * t8

      end function



      doubleprecision function rgg2ght1s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = 0.1D1 / z
      t5 = z ** 2
      t7 = (t5 - z + 0.1D1) ** 2
      t10 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, -0.3D1 * t3 * wd * t7)
      rgg2ght1s2em1 = -0.3D1 * t10 * t3 * wd * t7

      end function



      doubleprecision function rgg2ght1s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgg2ght1s2em2 = 0.0D0

      end function



      doubleprecision function rgg2ght1s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgg2ght1s2em3 = 0.0D0

      end function



      doubleprecision function rgg2ght1s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rgg2ght1s2em4 = 0.0D0

      end function
