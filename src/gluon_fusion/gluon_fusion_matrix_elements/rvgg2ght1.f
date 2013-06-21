  
      subroutine rvgg2ght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision rvgg2ght1s1e1  
      doubleprecision rvgg2ght1s1e0  
      doubleprecision rvgg2ght1s1em1  
      doubleprecision rvgg2ght1s1em2  
      doubleprecision rvgg2ght1s1em3  
      doubleprecision rvgg2ght1s1em4  
      doubleprecision rvgg2ght1s2e1  
      doubleprecision rvgg2ght1s2e0  
      doubleprecision rvgg2ght1s2em1  
      doubleprecision rvgg2ght1s2em2  
      doubleprecision rvgg2ght1s2em3  
      doubleprecision rvgg2ght1s2em4  
      doubleprecision rvgg2ght1s3e1  
      doubleprecision rvgg2ght1s3e0  
      doubleprecision rvgg2ght1s3em1  
      doubleprecision rvgg2ght1s3em2  
      doubleprecision rvgg2ght1s3em3  
      doubleprecision rvgg2ght1s3em4  
      doubleprecision rvgg2ght1s4e1  
      doubleprecision rvgg2ght1s4e0  
      doubleprecision rvgg2ght1s4em1  
      doubleprecision rvgg2ght1s4em2  
      doubleprecision rvgg2ght1s4em3  
      doubleprecision rvgg2ght1s4em4  
      doubleprecision rvgg2ght1s5e1  
      doubleprecision rvgg2ght1s5e0  
      doubleprecision rvgg2ght1s5em1  
      doubleprecision rvgg2ght1s5em2  
      doubleprecision rvgg2ght1s5em3  
      doubleprecision rvgg2ght1s5em4  
      doubleprecision rvgg2ght1s6e1  
      doubleprecision rvgg2ght1s6e0  
      doubleprecision rvgg2ght1s6em1  
      doubleprecision rvgg2ght1s6em2  
      doubleprecision rvgg2ght1s6em3  
      doubleprecision rvgg2ght1s6em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rvgg2ght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rvgg2ght1s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rvgg2ght1s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rvgg2ght1s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rvgg2ght1s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rvgg2ght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rvgg2ght1s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rvgg2ght1s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rvgg2ght1s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rvgg2ght1s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rvgg2ght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rvgg2ght1s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rvgg2ght1s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rvgg2ght1s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rvgg2ght1s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then
c      print*,"\n[inside rvgg2ght1] s=",s," x1=",XB1,
c     &    "x2=",XB2,
c     &    "z=",z,"lh=",lh,"wd=",wd,"nf=",nf,
c     &    "l1=",x1,"l2=",x2,"l3=",x3,"l4=",x4,
c     &    "sector=",sector
      if(sector.eq.1)then
         fff=rvgg2ght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rvgg2ght1s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rvgg2ght1s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rvgg2ght1s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rvgg2ght1s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rvgg2ght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rvgg2ght1s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rvgg2ght1s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rvgg2ght1s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rvgg2ght1s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rvgg2ght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rvgg2ght1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rvgg2ght1s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rvgg2ght1s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rvgg2ght1s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rvgg2ght1s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rvgg2ght1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = log(z)
      t9 = -t1
      t10 = 0.1D1 / t9
      t12 = 0.1D1 - (0.1D1 - z + t7) * t10
      t13 = 0.1D1 / x1
      t17 = polylog(2, t9)
      t19 = -0.2D1 * t7 - 0.2D1 * t17
      t23 = x1 ** 2
      t24 = 0.1D1 / z
      t25 = t23 * t24
      t26 = t1 ** 2
      t27 = t26 * t1
      t29 = log(-t25 * t27)
      t30 = t29 * t5
      t31 = -t5 - t30
      t33 = -t5 * t19 * t10 / 0.2D1 - t31 * t12
      t36 = 0.720D3 * lh
      t37 = 0.360D3 * t7
      t39 = (t36 - t37 + 0.360D3) * wd
      t40 = t5 * t12
      t47 = polylog(3, t9)
      t49 = (0.1D1 - z - t47) * t10
      t50 = (0.1D1 - z - t17) * t10 - t49
      t55 = t29 ** 2
      t64 = 0.3141592653589793D1 ** 2
      t65 = 0.60D2 * t64
      t66 = lh ** 2
      t67 = 0.720D3 * t66
      t68 = t7 * lh
      t69 = 0.720D3 * t68
      t70 = t7 ** 2
      t71 = 0.180D3 * t70
      t73 = (t65 - t67 + t69 - t71 - t36 + t37 - 0.360D3) * wd
      t81 = log(-t24 * t27)
      t82 = 0.360D3 * t81
      t88 = polylog(4, t9)
      t94 = t81 * lh
      t99 = t3 ** 2
      t102 = t3 * z
      t105 = t81 * t7
      t122 = 0.720D3 * t94
      t123 = 0.360D3 * t105
      t132 = -0.360D3 + 0.1440D4 * t94 * z - 0.2160D4 * t94 * t3 - 0.720
     #D3 * t94 * t99 + 0.1440D4 * t94 * t102 - 0.720D3 * t105 * z + 0.10
     #80D4 * t105 * t3 + 0.360D3 * t105 * t99 - 0.720D3 * t105 * t102 - 
     #0.1440D4 * t68 * z + 0.2160D4 * t68 * t3 + 0.720D3 * t68 * t99 - 0
     #.1440D4 * t68 * t102 + t69 - t122 + t123 + 0.1440D4 * t66 * z - 0.
     #2160D4 * t66 * t3 - 0.720D3 * t66 * t99 + 0.1440D4 * t66 * t102
      t133 = t81 ** 2
      t174 = 0.720D3 - t36 + t37
      t175 = t81 * t174
      t184 = -t133 * t174 / 0.2D1 + t81 * (-t65 + 0.720D3 * t7 + 0.1440D
     #4 - 0.1440D4 * lh + t67 - t69 + t71) + t82 + t122 - t123 - t65 + t
     #67 - t69 + t71 + t36 - t37
      t191 = 0.180D3 * t133
      t192 = -0.180D3 * t70 * t99 + 0.360D3 * t70 * t102 + (0.1513974627
     #033210D4 - 0.60D2 * t70 * t7 + 0.480D3 * t66 * lh - 0.720D3 * t7 *
     # t66 + 0.60D2 * t64 * t7 - 0.120D3 * t64 * lh + 0.360D3 * t70 * lh
     # + 0.540D3 * t133 + 0.60D2 * t133 * t81 - 0.2D1 * t175 + t184) * t
     #5 + 0.720D3 * z - 0.1080D4 * t3 + t65 - t67 - t71 + 0.720D3 * t102
     # - 0.360D3 * t99 - t191
      t223 = (-0.1080D4 * t81 + t175 - t191 + t65 - t67 + t69 - t71 - t3
     #6 + t37 - 0.360D3) * t5 + t36 - t37 + t82 - 0.1440D4 * lh * z + 0.
     #2160D4 * lh * t3 + 0.720D3 * lh * t99 - 0.1440D4 * lh * t102 + 0.7
     #20D3 * t7 * z - 0.1080D4 * t7 * t3 - 0.360D3 * t7 * t99 + 0.720D3 
     #* t7 * t102 - 0.720D3 * t81 * z + 0.1080D4 * t81 * t3 + 0.360D3 * 
     #t81 * t99 - 0.720D3 * t81 * t102
      t231 = -(0.1080D4 * wd * t5 * t12 * t13 + 0.2D1 * (-0.360D3 * wd *
     # t33 - t39 * t40) * t13 + (-0.360D3 * wd * (-t5 * t50 - t31 * t19 
     #* t10 / 0.2D1 - (t30 + t55 * t5 / 0.2D1) * t12) + t39 * t33 - t73 
     #* t40) * t13) * t24 / 0.80D2 + (((t36 - t37 + t82 + 0.360D3) * t5 
     #- 0.360D3 * t5) * t50 - 0.360D3 * t5 * (-t49 + (0.1D1 - z - t88) *
     # t10) + (t132 + 0.360D3 * t133 * z - 0.540D3 * t133 * t3 - 0.180D3
     # * t133 * t99 + 0.360D3 * t133 * t102 - 0.120D3 * t64 * z + 0.180D
     #3 * t64 * t3 + 0.60D2 * t64 * t99 - 0.120D3 * t64 * t102 + 0.360D3
     # * t70 * z - 0.540D3 * t70 * t3 + t192) * t12 + t223 * t19 * t10 /
     # 0.2D1) * wd * t24 / 0.160D3
      t232 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t231)
      t234 = -0.1D1 + x1
      t237 = 0.2D1 * z
      t238 = 0.2D1 * x1
      t239 = 0.3D1 * t3
      t240 = 0.3D1 * t23
      t241 = 0.2D1 * t102
      t242 = x1 * z
      t244 = x1 * t3
      t246 = x1 * t102
      t249 = 0.2D1 * x1 * t99
      t250 = t23 * z
      t252 = 0.1D1 - t237 - t238 + t239 + t240 - t241 + t99 + 0.8D1 * t2
     #42 - 0.12D2 * t244 + 0.8D1 * t246 - t249 - 0.12D2 * t250
      t253 = t23 * t3
      t255 = t23 * t102
      t258 = 0.3D1 * t23 * t99
      t259 = t23 * x1
      t261 = 0.8D1 * t259 * z
      t263 = 0.12D2 * t259 * t3
      t265 = 0.8D1 * t259 * t102
      t267 = 0.2D1 * t259 * t99
      t268 = t23 ** 2
      t270 = 0.4D1 * t268 * z
      t272 = 0.6D1 * t268 * t3
      t274 = 0.4D1 * t268 * t102
      t275 = t268 * t99
      t276 = 0.2D1 * t259
      t277 = 0.18D2 * t253 - 0.12D2 * t255 + t258 + t261 - t263 + t265 -
     # t267 - t270 + t272 - t274 + t275 - t276 + t268
      t278 = t252 + t277
      t280 = t234 * t1
      t282 = log(0.1D1 - t280)
      t284 = 0.1D1 / t234
      t286 = 0.1D1 / t1
      t288 = 0.1D1 - (t280 + t282) * t284 * t286
      t292 = polylog(2, t280)
      t294 = -0.2D1 * t282 - 0.2D1 * t292
      t296 = t284 * t286
      t299 = 0.11D2 * t242
      t300 = 0.18D2 * t244
      t301 = 0.11D2 * t246
      t302 = 0.15D2 * t250
      t303 = 0.24D2 * t253
      t304 = -0.1D1 + t237 + t238 - t239 - t240 + t241 - t99 - t299 + t3
     #00 - t301 + t249 + t302 - t303
      t305 = 0.15D2 * t255
      t308 = log(t25 * t27 * t234)
      t310 = t305 - t258 - t261 + t263 - t265 + t267 + t270 - t272 + t27
     #4 - t275 - t308 * t278 + t276 - t268
      t311 = t304 + t310
      t313 = t278 * t294 * t296 / 0.2D1 + t311 * t288
      t316 = t278 * t288
      t324 = polylog(3, t280)
      t333 = -0.1D1 + t237 + t238 - t239 - t240 + t241 - t99 - t299 + t3
     #00 - t301 + t249 + t302
      t334 = -t303 + t305 - t258 - t261 + t263 - t265 + t267 + t270 - t2
     #72 + t274 - t275 + t276 - t268
      t337 = t308 ** 2
      t349 = -0.1080D4 * wd * t278 * t288 * t13 + 0.2D1 * (-0.360D3 * wd
     # * t313 + t39 * t316) * t13 + (-0.360D3 * wd * (t278 * ((t280 - t2
     #92) * t284 * t286 - (t280 - t324) * t284 * t286) + t311 * t294 * t
     #296 / 0.2D1 + (-t308 * (t333 + t334) + t337 * t278 / 0.2D1) * t288
     #) + t39 * t313 + t73 * t316) * t13
      t352 = FJET(XB1, XB2, s, -t2 * t234, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # -t349 * t24 / 0.80D2)
      rvgg2ght1s1e1 = t232 * t231 - t352 * t349 * t24 / 0.80D2

      end function



      doubleprecision function rvgg2ght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = log(z)
      t9 = -t1
      t10 = 0.1D1 / t9
      t12 = 0.1D1 - (0.1D1 - z + t7) * t10
      t13 = 0.1D1 / x1
      t17 = polylog(2, t9)
      t19 = -0.2D1 * t7 - 0.2D1 * t17
      t23 = x1 ** 2
      t24 = 0.1D1 / z
      t25 = t23 * t24
      t26 = t1 ** 2
      t27 = t26 * t1
      t29 = log(-t25 * t27)
      t36 = 0.720D3 * lh
      t37 = 0.360D3 * t7
      t39 = (t36 - t37 + 0.360D3) * wd
      t49 = polylog(3, t9)
      t56 = log(-t24 * t27)
      t57 = 0.360D3 * t56
      t68 = t56 ** 2
      t70 = 0.3141592653589793D1 ** 2
      t72 = lh ** 2
      t76 = t7 ** 2
      t84 = t3 ** 2
      t87 = t3 * z
      t106 = (-0.1080D4 * t56 + t56 * (0.720D3 - t36 + t37) - 0.180D3 * 
     #t68 + 0.60D2 * t70 - 0.720D3 * t72 + 0.720D3 * t7 * lh - 0.180D3 *
     # t76 - t36 + t37 - 0.360D3) * t5 + t36 - t37 + t57 - 0.1440D4 * lh
     # * z + 0.2160D4 * lh * t3 + 0.720D3 * lh * t84 - 0.1440D4 * lh * t
     #87 + 0.720D3 * t7 * z - 0.1080D4 * t7 * t3 - 0.360D3 * t7 * t84 + 
     #0.720D3 * t7 * t87 - 0.720D3 * t56 * z + 0.1080D4 * t56 * t3 + 0.3
     #60D3 * t56 * t84 - 0.720D3 * t56 * t87
      t112 = -(0.720D3 * wd * t5 * t12 * t13 + (-0.360D3 * wd * (-t5 * t
     #19 * t10 / 0.2D1 - (-t5 - t29 * t5) * t12) - t39 * t5 * t12) * t13
     #) * t24 / 0.80D2 + (-0.360D3 * t5 * ((0.1D1 - z - t17) * t10 - (0.
     #1D1 - z - t49) * t10) + ((t36 - t37 + t57 + 0.360D3) * t5 - 0.360D
     #3 * t5) * t19 * t10 / 0.2D1 + t106 * t12) * wd * t24 / 0.160D3
      t113 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t112)
      t115 = -0.1D1 + x1
      t118 = 0.2D1 * t87
      t119 = x1 * z
      t121 = x1 * t3
      t123 = x1 * t87
      t126 = 0.2D1 * x1 * t84
      t127 = t23 * z
      t129 = t23 * t3
      t131 = t23 * t87
      t134 = 0.3D1 * t23 * t84
      t135 = t23 * x1
      t137 = 0.8D1 * t135 * z
      t138 = 0.1D1 + t84 - t118 + 0.8D1 * t119 - 0.12D2 * t121 + 0.8D1 *
     # t123 - t126 - 0.12D2 * t127 + 0.18D2 * t129 - 0.12D2 * t131 + t13
     #4 + t137
      t140 = 0.12D2 * t135 * t3
      t142 = 0.8D1 * t135 * t87
      t144 = 0.2D1 * t135 * t84
      t145 = t23 ** 2
      t147 = 0.4D1 * t145 * z
      t149 = 0.6D1 * t145 * t3
      t151 = 0.4D1 * t145 * t87
      t152 = t145 * t84
      t153 = 0.2D1 * z
      t154 = 0.2D1 * x1
      t155 = 0.2D1 * t135
      t156 = 0.3D1 * t3
      t157 = 0.3D1 * t23
      t158 = -t140 + t142 - t144 - t147 + t149 - t151 + t152 - t153 - t1
     #54 - t155 + t145 + t156 + t157
      t159 = t138 + t158
      t161 = t115 * t1
      t163 = log(0.1D1 - t161)
      t165 = 0.1D1 / t115
      t167 = 0.1D1 / t1
      t169 = 0.1D1 - (t161 + t163) * t165 * t167
      t173 = polylog(2, t161)
      t186 = -0.1D1 - t84 + t118 - 0.11D2 * t119 + 0.18D2 * t121 - 0.11D
     #2 * t123 + t126 + 0.15D2 * t127 - 0.24D2 * t129 + 0.15D2 * t131 - 
     #t134 - t137 + t140
      t189 = log(t25 * t27 * t115)
      t191 = -t142 + t144 + t147 - t149 + t151 - t152 - t189 * t159 + t1
     #53 + t154 + t155 - t145 - t156 - t157
      t201 = -0.720D3 * wd * t159 * t169 * t13 + (-0.360D3 * wd * (t159 
     #* (-0.2D1 * t163 - 0.2D1 * t173) * t165 * t167 / 0.2D1 + (t186 + t
     #191) * t169) + t39 * t159 * t169) * t13
      t204 = FJET(XB1, XB2, s, -t2 * t115, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # -t201 * t24 / 0.80D2)
      rvgg2ght1s1e0 = t113 * t112 - t204 * t201 * t24 / 0.80D2

      end function



      doubleprecision function rvgg2ght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = log(z)
      t9 = -t1
      t10 = 0.1D1 / t9
      t12 = 0.1D1 - (0.1D1 - z + t7) * t10
      t13 = 0.1D1 / x1
      t15 = 0.1D1 / z
      t19 = polylog(2, t9)
      t27 = t1 ** 2
      t30 = log(-t15 * t27 * t1)
      t41 = -0.9D1 / 0.2D1 * wd * t5 * t12 * t13 * t15 + (-0.180D3 * t5 
     #* (-0.2D1 * t7 - 0.2D1 * t19) * t10 + ((0.720D3 * lh - 0.360D3 * t
     #7 + 0.360D3 * t30 + 0.360D3) * t5 - 0.360D3 * t5) * t12) * wd * t1
     #5 / 0.160D3
      t42 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t44 = -0.1D1 + x1
      t49 = x1 ** 2
      t51 = t49 * x1
      t53 = t49 ** 2
      t54 = t3 * z
      t56 = t3 ** 2
      t65 = 0.1D1 - 0.2D1 * z - 0.2D1 * x1 + 0.3D1 * t49 - 0.2D1 * t51 +
     # t53 - 0.2D1 * t54 + t56 + 0.8D1 * x1 * z - 0.12D2 * x1 * t3 + 0.8
     #D1 * x1 * t54 - 0.2D1 * x1 * t56
      t90 = -0.12D2 * t49 * z + 0.18D2 * t49 * t3 - 0.12D2 * t49 * t54 +
     # 0.3D1 * t49 * t56 + 0.8D1 * t51 * z - 0.12D2 * t51 * t3 + 0.8D1 *
     # t51 * t54 - 0.2D1 * t51 * t56 - 0.4D1 * t53 * z + 0.6D1 * t53 * t
     #3 - 0.4D1 * t53 * t54 + t53 * t56 + 0.3D1 * t3
      t91 = t65 + t90
      t93 = t44 * t1
      t95 = log(0.1D1 - t93)
      t103 = (0.1D1 - (t93 + t95) / t44 / t1) * t13 * t15
      t106 = FJET(XB1, XB2, s, -t2 * t44, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #0.9D1 / 0.2D1 * wd * t91 * t103)
      rvgg2ght1s1em1 = t42 * t41 + 0.9D1 / 0.2D1 * t106 * wd * t91 * t10
     #3

      end function



      doubleprecision function rvgg2ght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t6 = log(z)
      t11 = 0.1D1 + (0.1D1 - z + t6) / t1
      t13 = 0.1D1 / z
      t17 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1
     # / 0.4D1 * t5 * t11 * wd * t13)
      rvgg2ght1s1em2 = -0.9D1 / 0.4D1 * t17 * t5 * t11 * wd * t13

      end function



      doubleprecision function rvgg2ght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s1em3 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s1em4 = 0.0D0

      end function


      doubleprecision function rvgg2ght1s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = log(z)
      t9 = -t1
      t10 = 0.1D1 / t9
      t12 = 0.1D1 - (0.1D1 - z + t7) * t10
      t13 = 0.1D1 / x1
      t17 = polylog(2, t9)
      t19 = -0.2D1 * t7 - 0.2D1 * t17
      t23 = x1 ** 2
      t24 = 0.1D1 / z
      t25 = t23 * t24
      t26 = t1 ** 2
      t27 = t26 * t1
      t29 = log(-t25 * t27)
      t30 = t29 * t5
      t31 = -t5 - t30
      t33 = -t5 * t19 * t10 / 0.2D1 - t31 * t12
      t36 = 0.720D3 * lh
      t37 = 0.360D3 * t7
      t39 = (t36 - t37 + 0.360D3) * wd
      t40 = t5 * t12
      t47 = polylog(3, t9)
      t49 = (0.1D1 - z - t47) * t10
      t50 = (0.1D1 - z - t17) * t10 - t49
      t55 = t29 ** 2
      t64 = 0.3141592653589793D1 ** 2
      t65 = 0.60D2 * t64
      t66 = lh ** 2
      t67 = 0.720D3 * t66
      t68 = t7 * lh
      t69 = 0.720D3 * t68
      t70 = t7 ** 2
      t71 = 0.180D3 * t70
      t73 = (t65 - t67 + t69 - t71 - t36 + t37 - 0.360D3) * wd
      t81 = log(-t24 * t27)
      t82 = 0.360D3 * t81
      t88 = polylog(4, t9)
      t94 = t3 * z
      t96 = t3 ** 2
      t98 = t81 ** 2
      t99 = 0.180D3 * t98
      t101 = t81 * lh
      t110 = t81 * t7
      t127 = -0.360D3 + 0.720D3 * t94 - 0.360D3 * t96 - t99 - 0.1080D4 *
     # t3 + t65 - t67 - t71 + 0.1440D4 * t101 * z - 0.2160D4 * t101 * t3
     # - 0.720D3 * t101 * t96 + 0.1440D4 * t101 * t94 - 0.720D3 * t110 *
     # z + 0.1080D4 * t110 * t3 + 0.360D3 * t110 * t96 - 0.720D3 * t110 
     #* t94 - 0.1440D4 * t68 * z + 0.2160D4 * t68 * t3 + 0.720D3 * t68 *
     # t96 - 0.1440D4 * t68 * t94
      t128 = 0.720D3 * t101
      t129 = 0.360D3 * t110
      t178 = 0.720D3 - t36 + t37
      t179 = t81 * t178
      t188 = -t98 * t178 / 0.2D1 + t81 * (-t65 + 0.720D3 * t7 + 0.1440D4
     # - 0.1440D4 * lh + t67 - t69 + t71) + t82 + t128 - t129 - t65 + t6
     #7 - t69 + t71 + t36 - t37
      t192 = 0.360D3 * t98 * t94 - 0.120D3 * t64 * z + 0.180D3 * t64 * t
     #3 + 0.60D2 * t64 * t96 - 0.120D3 * t64 * t94 + 0.360D3 * t70 * z -
     # 0.540D3 * t70 * t3 - 0.180D3 * t70 * t96 + 0.360D3 * t70 * t94 + 
     #(0.1513974627033210D4 - 0.60D2 * t70 * t7 + 0.480D3 * t66 * lh - 0
     #.720D3 * t7 * t66 + 0.60D2 * t64 * t7 - 0.120D3 * t64 * lh + 0.360
     #D3 * t70 * lh + 0.540D3 * t98 + 0.60D2 * t98 * t81 - 0.2D1 * t179 
     #+ t188) * t5 + 0.720D3 * z
      t223 = (-0.1080D4 * t81 + t179 - t99 + t65 - t67 + t69 - t71 - t36
     # + t37 - 0.360D3) * t5 + t36 - t37 + t82 - 0.1440D4 * lh * z + 0.2
     #160D4 * lh * t3 + 0.720D3 * lh * t96 - 0.1440D4 * lh * t94 + 0.720
     #D3 * t7 * z - 0.1080D4 * t7 * t3 - 0.360D3 * t7 * t96 + 0.720D3 * 
     #t7 * t94 - 0.720D3 * t81 * z + 0.1080D4 * t81 * t3 + 0.360D3 * t81
     # * t96 - 0.720D3 * t81 * t94
      t231 = -(0.1080D4 * wd * t5 * t12 * t13 + 0.2D1 * (-0.360D3 * wd *
     # t33 - t39 * t40) * t13 + (-0.360D3 * wd * (-t5 * t50 - t31 * t19 
     #* t10 / 0.2D1 - (t30 + t55 * t5 / 0.2D1) * t12) + t39 * t33 - t73 
     #* t40) * t13) * t24 / 0.80D2 + (((t36 - t37 + t82 + 0.360D3) * t5 
     #- 0.360D3 * t5) * t50 - 0.360D3 * t5 * (-t49 + (0.1D1 - z - t88) *
     # t10) + (t127 + t69 - t128 + t129 + 0.1440D4 * t66 * z - 0.2160D4 
     #* t66 * t3 - 0.720D3 * t66 * t96 + 0.1440D4 * t66 * t94 + 0.360D3 
     #* t98 * z - 0.540D3 * t98 * t3 - 0.180D3 * t98 * t96 + t192) * t12
     # + t223 * t19 * t10 / 0.2D1) * wd * t24 / 0.160D3
      t232 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t231)
      t235 = -0.1D1 + x1
      t237 = 0.2D1 * t94
      t238 = 0.3D1 * t3
      t239 = 0.3D1 * t23
      t240 = t23 * x1
      t241 = 0.2D1 * t240
      t242 = t23 ** 2
      t243 = x1 * z
      t245 = x1 * t3
      t247 = x1 * t94
      t250 = 0.2D1 * x1 * t96
      t251 = t23 * z
      t253 = 0.1D1 - t237 + t96 + t238 + t239 - t241 + t242 + 0.8D1 * t2
     #43 - 0.12D2 * t245 + 0.8D1 * t247 - t250 - 0.12D2 * t251
      t254 = t23 * t3
      t256 = t23 * t94
      t259 = 0.3D1 * t23 * t96
      t261 = 0.8D1 * t240 * z
      t263 = 0.12D2 * t240 * t3
      t265 = 0.8D1 * t240 * t94
      t267 = 0.2D1 * t240 * t96
      t269 = 0.4D1 * t242 * z
      t271 = 0.6D1 * t242 * t3
      t273 = 0.4D1 * t242 * t94
      t274 = t242 * t96
      t275 = 0.2D1 * z
      t276 = 0.2D1 * x1
      t277 = 0.18D2 * t254 - 0.12D2 * t256 + t259 + t261 - t263 + t265 -
     # t267 - t269 + t271 - t273 + t274 - t275 - t276
      t278 = t253 + t277
      t280 = t235 * t1
      t282 = log(0.1D1 - t280)
      t284 = 0.1D1 / t235
      t286 = 0.1D1 / t1
      t288 = 0.1D1 - (t280 + t282) * t284 * t286
      t292 = polylog(2, t280)
      t294 = -0.2D1 * t282 - 0.2D1 * t292
      t296 = t284 * t286
      t299 = 0.11D2 * t243
      t300 = 0.18D2 * t245
      t301 = 0.11D2 * t247
      t302 = 0.15D2 * t251
      t303 = 0.24D2 * t254
      t304 = -0.1D1 + t237 - t96 - t238 - t239 + t241 - t242 - t299 + t3
     #00 - t301 + t250 + t302 - t303
      t305 = 0.15D2 * t256
      t308 = log(t25 * t27 * t235)
      t310 = t305 - t259 - t261 + t263 - t265 + t267 + t269 - t271 + t27
     #3 - t274 - t308 * t278 + t275 + t276
      t311 = t304 + t310
      t313 = t278 * t294 * t296 / 0.2D1 + t311 * t288
      t316 = t278 * t288
      t324 = polylog(3, t280)
      t333 = -0.1D1 + t237 - t96 - t238 - t239 + t241 - t242 - t299 + t3
     #00 - t301 + t250 + t302
      t334 = -t303 + t305 - t259 - t261 + t263 - t265 + t267 + t269 - t2
     #71 + t273 - t274 + t275 + t276
      t337 = t308 ** 2
      t349 = -0.1080D4 * wd * t278 * t288 * t13 + 0.2D1 * (-0.360D3 * wd
     # * t313 + t39 * t316) * t13 + (-0.360D3 * wd * (t278 * ((t280 - t2
     #92) * t284 * t286 - (t280 - t324) * t284 * t286) + t311 * t294 * t
     #296 / 0.2D1 + (-t308 * (t333 + t334) + t337 * t278 / 0.2D1) * t288
     #) + t39 * t313 + t73 * t316) * t13
      t352 = FJET(XB1, XB2, s, t2 * x1, -t2 * t235, 0.0D0, 0.0D0, 0.0D0,
     # -t349 * t24 / 0.80D2)
      rvgg2ght1s2e1 = t232 * t231 - t352 * t349 * t24 / 0.80D2

      end function



      doubleprecision function rvgg2ght1s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = log(z)
      t9 = -t1
      t10 = 0.1D1 / t9
      t12 = 0.1D1 - (0.1D1 - z + t7) * t10
      t13 = 0.1D1 / x1
      t17 = polylog(2, t9)
      t19 = -0.2D1 * t7 - 0.2D1 * t17
      t23 = x1 ** 2
      t24 = 0.1D1 / z
      t25 = t23 * t24
      t26 = t1 ** 2
      t27 = t26 * t1
      t29 = log(-t25 * t27)
      t36 = 0.720D3 * lh
      t37 = 0.360D3 * t7
      t39 = (t36 - t37 + 0.360D3) * wd
      t49 = polylog(3, t9)
      t56 = log(-t24 * t27)
      t57 = 0.360D3 * t56
      t68 = t56 ** 2
      t70 = 0.3141592653589793D1 ** 2
      t72 = lh ** 2
      t76 = t7 ** 2
      t84 = t3 ** 2
      t87 = t3 * z
      t106 = (-0.1080D4 * t56 + t56 * (0.720D3 - t36 + t37) - 0.180D3 * 
     #t68 + 0.60D2 * t70 - 0.720D3 * t72 + 0.720D3 * t7 * lh - 0.180D3 *
     # t76 - t36 + t37 - 0.360D3) * t5 + t36 - t37 + t57 - 0.1440D4 * lh
     # * z + 0.2160D4 * lh * t3 + 0.720D3 * lh * t84 - 0.1440D4 * lh * t
     #87 + 0.720D3 * t7 * z - 0.1080D4 * t7 * t3 - 0.360D3 * t7 * t84 + 
     #0.720D3 * t7 * t87 - 0.720D3 * t56 * z + 0.1080D4 * t56 * t3 + 0.3
     #60D3 * t56 * t84 - 0.720D3 * t56 * t87
      t112 = -(0.720D3 * wd * t5 * t12 * t13 + (-0.360D3 * wd * (-t5 * t
     #19 * t10 / 0.2D1 - (-t5 - t29 * t5) * t12) - t39 * t5 * t12) * t13
     #) * t24 / 0.80D2 + (-0.360D3 * t5 * ((0.1D1 - z - t17) * t10 - (0.
     #1D1 - z - t49) * t10) + ((t36 - t37 + t57 + 0.360D3) * t5 - 0.360D
     #3 * t5) * t19 * t10 / 0.2D1 + t106 * t12) * wd * t24 / 0.160D3
      t113 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t112)
      t116 = -0.1D1 + x1
      t118 = t23 * x1
      t119 = 0.2D1 * t118
      t120 = t23 ** 2
      t121 = 0.2D1 * z
      t122 = x1 * z
      t124 = x1 * t3
      t126 = x1 * t87
      t129 = 0.2D1 * x1 * t84
      t130 = t23 * z
      t132 = t23 * t3
      t134 = t23 * t87
      t137 = 0.3D1 * t23 * t84
      t138 = 0.1D1 - t119 + t120 - t121 + 0.8D1 * t122 - 0.12D2 * t124 +
     # 0.8D1 * t126 - t129 - 0.12D2 * t130 + 0.18D2 * t132 - 0.12D2 * t1
     #34 + t137
      t140 = 0.8D1 * t118 * z
      t142 = 0.12D2 * t118 * t3
      t144 = 0.8D1 * t118 * t87
      t146 = 0.2D1 * t118 * t84
      t148 = 0.4D1 * t120 * z
      t150 = 0.6D1 * t120 * t3
      t152 = 0.4D1 * t120 * t87
      t153 = t120 * t84
      t154 = 0.3D1 * t3
      t155 = 0.3D1 * t23
      t156 = 0.2D1 * t87
      t157 = 0.2D1 * x1
      t158 = t140 - t142 + t144 - t146 - t148 + t150 - t152 + t153 + t15
     #4 + t155 + t84 - t156 - t157
      t159 = t138 + t158
      t161 = t116 * t1
      t163 = log(0.1D1 - t161)
      t165 = 0.1D1 / t116
      t167 = 0.1D1 / t1
      t169 = 0.1D1 - (t161 + t163) * t165 * t167
      t173 = polylog(2, t161)
      t186 = -0.1D1 + t119 - t120 + t121 - 0.11D2 * t122 + 0.18D2 * t124
     # - 0.11D2 * t126 + t129 + 0.15D2 * t130 - 0.24D2 * t132 + 0.15D2 *
     # t134 - t137 - t140
      t189 = log(t25 * t27 * t116)
      t191 = t142 - t144 + t146 + t148 - t150 + t152 - t153 - t189 * t15
     #9 - t154 - t155 - t84 + t156 + t157
      t201 = -0.720D3 * wd * t159 * t169 * t13 + (-0.360D3 * wd * (t159 
     #* (-0.2D1 * t163 - 0.2D1 * t173) * t165 * t167 / 0.2D1 + (t186 + t
     #191) * t169) + t39 * t159 * t169) * t13
      t204 = FJET(XB1, XB2, s, t2 * x1, -t2 * t116, 0.0D0, 0.0D0, 0.0D0,
     # -t201 * t24 / 0.80D2)
      rvgg2ght1s2e0 = t113 * t112 - t204 * t201 * t24 / 0.80D2

      end function



      doubleprecision function rvgg2ght1s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t7 = log(z)
      t9 = -t1
      t10 = 0.1D1 / t9
      t12 = 0.1D1 - (0.1D1 - z + t7) * t10
      t13 = 0.1D1 / x1
      t15 = 0.1D1 / z
      t19 = polylog(2, t9)
      t27 = t1 ** 2
      t30 = log(-t15 * t27 * t1)
      t41 = -0.9D1 / 0.2D1 * wd * t5 * t12 * t13 * t15 + (-0.180D3 * t5 
     #* (-0.2D1 * t7 - 0.2D1 * t19) * t10 + ((0.720D3 * lh - 0.360D3 * t
     #7 + 0.360D3 * t30 + 0.360D3) * t5 - 0.360D3 * t5) * t12) * wd * t1
     #5 / 0.160D3
      t42 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t45 = -0.1D1 + x1
      t47 = x1 ** 2
      t49 = t47 * x1
      t51 = t47 ** 2
      t52 = t3 * z
      t54 = t3 ** 2
      t64 = 0.1D1 + 0.3D1 * t47 - 0.2D1 * t49 + t51 - 0.2D1 * t52 + t54 
     #+ 0.3D1 * t3 - 0.2D1 * z - 0.2D1 * x1 + 0.8D1 * x1 * z - 0.12D2 * 
     #x1 * t3 + 0.8D1 * x1 * t52
      t90 = -0.2D1 * x1 * t54 - 0.12D2 * t47 * z + 0.18D2 * t47 * t3 - 0
     #.12D2 * t47 * t52 + 0.3D1 * t47 * t54 + 0.8D1 * t49 * z - 0.12D2 *
     # t49 * t3 + 0.8D1 * t49 * t52 - 0.2D1 * t49 * t54 - 0.4D1 * t51 * 
     #z + 0.6D1 * t51 * t3 - 0.4D1 * t51 * t52 + t51 * t54
      t91 = t64 + t90
      t93 = t45 * t1
      t95 = log(0.1D1 - t93)
      t103 = (0.1D1 - (t93 + t95) / t45 / t1) * t13 * t15
      t106 = FJET(XB1, XB2, s, t2 * x1, -t2 * t45, 0.0D0, 0.0D0, 0.0D0, 
     #0.9D1 / 0.2D1 * wd * t91 * t103)
      rvgg2ght1s2em1 = t42 * t41 + 0.9D1 / 0.2D1 * t106 * wd * t91 * t10
     #3

      end function



      doubleprecision function rvgg2ght1s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t3 = z ** 2
      t5 = (t3 - z + 0.1D1) ** 2
      t6 = log(z)
      t11 = 0.1D1 + (0.1D1 - z + t6) / t1
      t13 = 0.1D1 / z
      t17 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, -0.9D1
     # / 0.4D1 * t5 * t11 * wd * t13)
      rvgg2ght1s2em2 = -0.9D1 / 0.4D1 * t17 * t5 * t11 * wd * t13

      end function



      doubleprecision function rvgg2ght1s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s2em3 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s2em4 = 0.0D0

      end function


      doubleprecision function rvgg2ght1s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = log(z)
      t7 = 0.720D3 - 0.720D3 * lh + 0.360D3 * t5
      t8 = t7 * t3
      t10 = 0.1D1 / z
      t11 = t1 ** 2
      t12 = t11 * t1
      t14 = log(-t10 * t12)
      t15 = t14 * t3
      t18 = t14 ** 2
      t19 = t18 * t3
      t22 = t5 ** 2
      t25 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t42 = t5 * lh
      t44 = 0.60D2 * t22 * t5 + 0.1726025372966790D4 + 0.720D3 * t5 * t2
     #5 - 0.120D3 * t28 - 0.2880D4 * lh - 0.360D3 * t22 * lh - 0.480D3 *
     # t25 * lh + 0.1440D4 * t5 + 0.360D3 * t22 + 0.120D3 * t28 * lh + 0
     #.1440D4 * t25 - 0.60D2 * t28 * t5 - 0.1440D4 * t42
      t56 = -0.60D2 * t28 + 0.720D3 * t5 + 0.1440D4 - 0.1440D4 * lh + 0.
     #720D3 * t25 - 0.720D3 * t42 + 0.180D3 * t22
      t71 = 0.48D2 * t14 - 0.36D2 * t14 * lh + 0.18D2 * t14 * t5 - 0.9D1
     # * t18 - 0.48D2 * t5 - 0.36D2 * t25 + 0.3D1 * t28 + 0.96D2 * lh - 
     #0.104D3 - 0.9D1 * t22 + 0.36D2 * t42
      t74 = t15 * t7
      t76 = t56 * t3
      t77 = 0.6D1 * lh
      t78 = 0.3D1 * t5
      t81 = (0.8D1 - t77 + t78 - 0.3D1 * t14) * t3
      t83 = 0.2D1 * t8 - 0.720D3 * t15 + 0.1680D4 * t3 + t19 * t7 / 0.6D
     #1 + t3 * t44 / 0.3D1 - 0.20D2 * t18 * t14 * t3 - t15 * t56 / 0.3D1
     # - 0.40D2 / 0.9D1 * t71 * t3 - t74 + 0.180D3 * t19 + t76 + 0.80D2 
     #* t81
      t85 = z ** 2
      t86 = t85 ** 2
      t87 = t5 * t86
      t89 = t85 * z
      t90 = t5 * t89
      t92 = t5 * z
      t94 = t5 * t85
      t96 = 0.18D2 * t5 + 0.18D2 * t87 - 0.36D2 * t90 - 0.36D2 * t92 + 0
     #.54D2 * t94
      t98 = 0.12D2 * z
      t100 = 0.6D1 * t85
      t101 = -t1
      t102 = polylog(2, t101)
      t104 = polylog(3, t101)
      t106 = polylog(4, t101)
      t112 = -0.6D1 + t98 - 0.96D2 * t5 - t100 - 0.138D3 * t102 + 0.84D2
     # * t104 - 0.18D2 * t106 + 0.192D3 * t92 - 0.288D3 * t94 - 0.96D2 *
     # t87 + 0.192D3 * t90
      t113 = t102 * t89
      t115 = t104 * t89
      t119 = t102 * t86
      t121 = t104 * t86
      t125 = t102 * t85
      t127 = t102 * z
      t129 = t104 * z
      t133 = t104 * t85
      t137 = 0.276D3 * t113 - 0.168D3 * t115 + 0.36D2 * t106 * t89 - 0.1
     #38D3 * t119 + 0.84D2 * t121 - 0.18D2 * t106 * t86 - 0.414D3 * t125
     # + 0.276D3 * t127 - 0.168D3 * t129 + 0.36D2 * t106 * z + 0.252D3 *
     # t133 - 0.54D2 * t106 * t85
      t141 = 0.440D3 * t3
      t142 = t8 / 0.3D1
      t145 = 0.2D1 * nf
      t146 = 0.138D3 * t5
      t147 = 0.3D1 * t85
      t148 = 0.3D1 * t89
      t149 = nf * t85
      t150 = 0.2D1 * t149
      t151 = 0.84D2 * t102
      t152 = 0.18D2 * t104
      t153 = 0.276D3 * t92
      t154 = 0.414D3 * t94
      t155 = 0.6D1 - t98 - t145 + t146 + t147 + t148 - t150 + t151 - t15
     #2 - t153 + t154
      t156 = 0.138D3 * t87
      t157 = 0.276D3 * t90
      t158 = 0.168D3 * t113
      t159 = 0.36D2 * t115
      t160 = 0.84D2 * t119
      t161 = 0.18D2 * t121
      t162 = 0.252D3 * t125
      t163 = 0.168D3 * t127
      t164 = 0.36D2 * t129
      t165 = 0.54D2 * t133
      t167 = 0.4D1 * nf * z
      t168 = t156 - t157 - t158 + t159 + t160 - t161 + t162 - t163 + t16
     #4 - t165 + t167
      t171 = 0.960D3 * t3
      t175 = t76 / 0.3D1
      t178 = 0.168D3 * t90
      t179 = 0.18D2 * t102
      t180 = 0.18D2 * t119
      t181 = 0.84D2 * t5
      t182 = 0.36D2 * t127
      t183 = nf * t89
      t184 = 0.252D3 * t94
      t185 = 0.84D2 * t87
      t186 = 0.168D3 * t92
      t187 = 0.36D2 * t113
      t188 = 0.54D2 * t125
      t189 = t178 - t149 - t179 + t147 - t148 - t180 - t181 + t182 + t18
     #3 - t184 - t185 + t186 + t187 - t188
      t196 = (t141 + t142) * wd
      t197 = x1 ** 2
      t198 = t197 * t10
      t200 = log(-t198 * t12)
      t202 = t178 - t149 - t179 + t147 - t148 - t180 - t181 + t182 + t18
     #3 - t184 - t185 + t186 + t187 - t188 - t200 * t96
      t208 = (t171 + t8 + t175 + 0.80D2 / 0.3D1 * (0.8D1 - t77 + t78) * 
     #t3) * wd
      t210 = t3 * wd
      t212 = 0.6D1 - t200 * t189 - t98 - t145 + t146 + t147 + t148 - t15
     #0 + t151 - t152 - t153 + t154
      t213 = t200 ** 2
      t216 = t156 - t157 - t158 + t159 + t160 - t161 + t162 - t163 + t16
     #4 - t165 + t167 + t213 * t96 / 0.2D1
      t221 = 0.1D1 / x1
      t225 = -(t83 * t96 + 0.120D3 * t3 * (t112 + t137) + (t141 + t142 -
     # 0.120D3 * t15) * (t155 + t168) + (t171 + t8 - 0.360D3 * t15 - t74
     # / 0.3D1 + 0.60D2 * t19 + t175 + 0.80D2 / 0.3D1 * t81) * t189) * w
     #d * t10 / 0.960D3 - (t196 * t202 + t208 * t96 + 0.120D3 * t210 * (
     #t212 + t216)) * t221 * t10 / 0.480D3
      t226 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t225)
      t228 = -0.1D1 + x1
      t231 = x1 * z
      t233 = x1 * t85
      t235 = x1 * t89
      t237 = x1 * t86
      t240 = 0.72D2 * t197 * z
      t241 = t197 * t85
      t243 = t197 * t89
      t245 = t197 * t86
      t247 = t197 * x1
      t248 = t247 * z
      t249 = 0.12D2 * t248
      t250 = t247 * t85
      t252 = t247 * t89
      t254 = t247 * t86
      t256 = t197 ** 2
      t257 = t256 * z
      t259 = t256 * t85
      t261 = t256 * t89
      t263 = t256 * t86
      t265 = t86 * z
      t266 = x1 * t265
      t268 = t197 * t265
      t270 = 0.36D2 * t231 - 0.108D3 * t233 + 0.84D2 * t235 + 0.9D1 * t2
     #37 - t240 + 0.162D3 * t241 - 0.30D2 * t243 - 0.138D3 * t245 + t249
     # + 0.42D2 * t250 - 0.198D3 * t252 + 0.222D3 * t254 + 0.9D1 * t257 
     #- 0.66D2 * t259 + 0.114D3 * t261 - 0.81D2 * t263 - 0.21D2 * t266 +
     # 0.78D2 * t268
      t271 = t247 * t265
      t272 = 0.78D2 * t271
      t273 = t256 * t265
      t274 = 0.21D2 * t273
      t275 = t256 * x1
      t277 = 0.15D2 * t275 * z
      t279 = 0.30D2 * t275 * t85
      t281 = 0.30D2 * t275 * t89
      t283 = 0.15D2 * t275 * t86
      t285 = 0.3D1 * t275 * t265
      t286 = nf * t275
      t287 = nf * t256
      t288 = nf * t86
      t289 = nf * t265
      t290 = t228 * t1
      t292 = log(0.1D1 - t290)
      t293 = t292 * t256
      t294 = 0.252D3 * t293
      t295 = t292 * t197
      t296 = 0.84D2 * t295
      t297 = t292 * t247
      t298 = 0.168D3 * t297
      t299 = t256 * t197
      t300 = t292 * t299
      t301 = 0.84D2 * t300
      t302 = t292 * t275
      t303 = 0.168D3 * t302
      t304 = t292 * t86
      t305 = 0.252D3 * t304
      t306 = t86 * t85
      t307 = t292 * t306
      t308 = 0.84D2 * t307
      t309 = t292 * t265
      t310 = 0.168D3 * t309
      t311 = -t272 + t274 + t277 - t279 + t281 - t283 + t285 + t286 - t2
     #87 - t288 + t289 - t294 - t296 + t298 - t301 + t303 - t305 - t308 
     #+ t310
      t313 = t292 * t85
      t314 = 0.84D2 * t313
      t315 = t292 * t89
      t316 = 0.168D3 * t315
      t317 = polylog(2, t290)
      t318 = t317 * t247
      t319 = 0.36D2 * t318
      t320 = t317 * t197
      t321 = 0.18D2 * t320
      t322 = t317 * t256
      t323 = 0.54D2 * t322
      t324 = t317 * t299
      t325 = 0.18D2 * t324
      t326 = t317 * t275
      t327 = 0.36D2 * t326
      t328 = t317 * t86
      t329 = 0.54D2 * t328
      t330 = t317 * t306
      t331 = 0.18D2 * t330
      t332 = t317 * t265
      t333 = 0.36D2 * t332
      t334 = t317 * t85
      t335 = 0.18D2 * t334
      t336 = t317 * t89
      t337 = 0.36D2 * t336
      t340 = log(t198 * t12 * t228)
      t356 = 0.18D2 * t235 - 0.36D2 * t237 + 0.36D2 * t241 - 0.126D3 * t
     #243 + 0.144D3 * t245 + 0.18D2 * t248 - 0.108D3 * t250 + 0.216D3 * 
     #t252 - 0.180D3 * t254 - 0.18D2 * t257 + 0.72D2 * t259 - 0.108D3 * 
     #t261 + 0.72D2 * t263 + 0.18D2 * t266 - 0.54D2 * t268
      t369 = t292 * x1
      t370 = t369 * z
      t372 = t369 * t306
      t374 = t369 * t85
      t376 = t369 * t89
      t378 = 0.54D2 * t271 - 0.18D2 * t273 + 0.54D2 * t293 + 0.18D2 * t2
     #95 - 0.36D2 * t297 + 0.18D2 * t300 - 0.36D2 * t302 + 0.54D2 * t304
     # + 0.18D2 * t307 - 0.36D2 * t309 + 0.18D2 * t313 - 0.36D2 * t315 +
     # 0.36D2 * t370 - 0.72D2 * t372 - 0.144D3 * t374 + 0.324D3 * t376
      t380 = t369 * t86
      t382 = t295 * z
      t384 = t295 * t85
      t386 = t295 * t89
      t388 = t295 * t86
      t390 = t297 * z
      t392 = t297 * t85
      t394 = t297 * t89
      t396 = t297 * t86
      t398 = t293 * z
      t400 = t293 * t85
      t402 = t293 * t89
      t404 = t293 * t86
      t406 = t369 * t265
      t408 = t295 * t265
      t410 = t297 * t265
      t412 = -0.396D3 * t380 - 0.144D3 * t382 + 0.558D3 * t384 - 0.1116D
     #4 * t386 + 0.1188D4 * t388 + 0.324D3 * t390 - 0.1116D4 * t392 + 0.
     #1944D4 * t394 - 0.1836D4 * t396 - 0.396D3 * t398 + 0.1188D4 * t400
     # - 0.1872D4 * t402 + 0.1638D4 * t404 + 0.252D3 * t406 - 0.648D3 * 
     #t408 + 0.900D3 * t410
      t413 = t293 * t265
      t415 = t295 * t306
      t417 = t297 * t306
      t419 = t293 * t306
      t421 = t302 * z
      t423 = t302 * t85
      t425 = t302 * t89
      t427 = t302 * t86
      t429 = t302 * t265
      t431 = t302 * t306
      t433 = t300 * z
      t435 = t300 * t85
      t437 = t300 * t89
      t439 = t300 * t86
      t441 = t300 * t265
      t443 = t300 * t306
      t445 = -0.756D3 * t413 + 0.144D3 * t415 - 0.180D3 * t417 + 0.144D3
     # * t419 + 0.252D3 * t421 - 0.720D3 * t423 + 0.1080D4 * t425 - 0.90
     #0D3 * t427 + 0.396D3 * t429 - 0.72D2 * t431 - 0.108D3 * t433 + 0.2
     #70D3 * t435 - 0.360D3 * t437 + 0.270D3 * t439 - 0.108D3 * t441 + 0
     #.18D2 * t443
      t447 = t356 + t378 + t412 + t445
      t449 = t317 * x1
      t450 = t449 * t306
      t451 = 0.72D2 * t450
      t452 = t449 * z
      t453 = 0.36D2 * t452
      t454 = t449 * t85
      t455 = 0.144D3 * t454
      t456 = t449 * t89
      t457 = 0.324D3 * t456
      t458 = t449 * t86
      t459 = 0.396D3 * t458
      t460 = -t314 + t316 + t319 - t321 - t323 - t325 + t327 - t329 - t3
     #31 + t333 - t335 + t337 - t340 * t447 + t451 - t453 + t455 - t457 
     #+ t459
      t461 = t320 * z
      t463 = t320 * t85
      t465 = t320 * t89
      t467 = t320 * t86
      t469 = t318 * z
      t471 = t318 * t85
      t473 = t318 * t89
      t475 = t318 * t86
      t477 = t322 * z
      t479 = t322 * t85
      t481 = t322 * t89
      t483 = t322 * t86
      t485 = t449 * t265
      t487 = t320 * t265
      t489 = t318 * t265
      t491 = t322 * t265
      t493 = t320 * t306
      t495 = t318 * t306
      t497 = t322 * t306
      t499 = 0.144D3 * t461 - 0.558D3 * t463 + 0.1116D4 * t465 - 0.1188D
     #4 * t467 - 0.324D3 * t469 + 0.1116D4 * t471 - 0.1944D4 * t473 + 0.
     #1836D4 * t475 + 0.396D3 * t477 - 0.1188D4 * t479 + 0.1872D4 * t481
     # - 0.1638D4 * t483 - 0.252D3 * t485 + 0.648D3 * t487 - 0.900D3 * t
     #489 + 0.756D3 * t491 - 0.144D3 * t493 + 0.180D3 * t495 - 0.144D3 *
     # t497
      t502 = t326 * z
      t504 = t326 * t85
      t506 = t326 * t89
      t508 = t326 * t86
      t510 = t326 * t265
      t512 = t326 * t306
      t514 = t324 * z
      t516 = t324 * t85
      t518 = t324 * t89
      t520 = t324 * t86
      t522 = t324 * t265
      t524 = t324 * t306
      t527 = nf * x1
      t528 = t527 * t89
      t530 = nf * t197
      t531 = t530 * t85
      t533 = t530 * t89
      t535 = t530 * t86
      t539 = -0.252D3 * t502 + 0.720D3 * t504 - 0.1080D4 * t506 + 0.900D
     #3 * t508 - 0.396D3 * t510 + 0.72D2 * t512 + 0.108D3 * t514 - 0.270
     #D3 * t516 + 0.360D3 * t518 - 0.270D3 * t520 + 0.108D3 * t522 - 0.1
     #8D2 * t524 - 0.168D3 * t370 - 0.2D1 * t528 - 0.2D1 * t531 + 0.8D1 
     #* t533 - 0.10D2 * t535 - 0.5D1 * t286 * z
      t547 = nf * t247
      t548 = t547 * z
      t550 = t547 * t85
      t552 = t547 * t89
      t554 = t547 * t86
      t556 = t287 * z
      t558 = t287 * t85
      t560 = t287 * t89
      t562 = t287 * t86
      t564 = t547 * t265
      t566 = t287 * t265
      t568 = t527 * t86
      t570 = t527 * t265
      t572 = t530 * t265
      t576 = 0.10D2 * t286 * t85 - 0.10D2 * t286 * t89 + 0.5D1 * t286 * 
     #t86 - t286 * t265 - 0.2D1 * t548 + 0.10D2 * t550 - 0.18D2 * t552 +
     # 0.14D2 * t554 + 0.7D1 * t556 - 0.18D2 * t558 + 0.22D2 * t560 - 0.
     #13D2 * t562 - 0.4D1 * t564 + 0.3D1 * t566 + 0.5D1 * t568 - 0.3D1 *
     # t570 + 0.4D1 * t572 + 0.336D3 * t372 + 0.672D3 * t374
      t596 = -0.1566D4 * t376 + 0.1956D4 * t380 + 0.672D3 * t382 - 0.271
     #2D4 * t384 + 0.5586D4 * t386 - 0.5976D4 * t388 - 0.1566D4 * t390 +
     # 0.5532D4 * t392 - 0.9720D4 * t394 + 0.9108D4 * t396 + 0.1902D4 * 
     #t398 - 0.5760D4 * t400 + 0.9060D4 * t402 - 0.7860D4 * t404 - 0.123
     #0D4 * t406 + 0.3186D4 * t408 - 0.4362D4 * t410 + 0.3582D4 * t413
      t612 = 0.3D1 * t86
      t613 = 0.3D1 * t256
      t614 = 0.3D1 * t275
      t615 = 0.3D1 * t265
      t616 = -0.672D3 * t415 + 0.840D3 * t417 - 0.672D3 * t419 - 0.1176D
     #4 * t421 + 0.3360D4 * t423 - 0.5040D4 * t425 + 0.4200D4 * t427 - 0
     #.1848D4 * t429 + 0.336D3 * t431 + 0.504D3 * t433 - 0.1260D4 * t435
     # + 0.1680D4 * t437 - 0.1260D4 * t439 + 0.504D3 * t441 - 0.84D2 * t
     #443 + t612 + t613 - t614 - t615
      t618 = t539 + t576 + t596 + t616
      t621 = (-z - x1 + t231) ** 2
      t622 = 0.1D1 / t621
      t636 = -t150 + 0.4D1 * t183 - 0.48D2 * t231 + 0.132D3 * t233 - 0.1
     #08D3 * t235 + 0.15D2 * t237 + t240 - 0.252D3 * t241 + 0.258D3 * t2
     #43 - 0.78D2 * t245 + t249 + 0.30D2 * t250
      t647 = -0.42D2 * t252 + 0.6D1 * t254 - 0.21D2 * t257 + 0.54D2 * t2
     #59 - 0.66D2 * t261 + 0.39D2 * t263 + 0.9D1 * t266 - 0.6D1 * t268 +
     # 0.6D1 * t271 - 0.9D1 * t273 - t277 + t279
      t658 = -t281 + t283 - t285 - 0.2D1 * t287 - 0.2D1 * t288 + 0.414D3
     # * t293 + 0.138D3 * t295 - 0.276D3 * t297 + 0.138D3 * t300 - 0.276
     #D3 * t302 + 0.414D3 * t304 + 0.138D3 * t307
      t672 = -0.276D3 * t309 + 0.138D3 * t313 - 0.276D3 * t315 - 0.168D3
     # * t318 + 0.84D2 * t320 + 0.252D3 * t322 + 0.84D2 * t324 - 0.168D3
     # * t326 + 0.252D3 * t328 + 0.84D2 * t330 - 0.168D3 * t332 + 0.84D2
     # * t334 - 0.168D3 * t336
      t687 = -0.2D1 * t530 + 0.4D1 * t547 - 0.336D3 * t450 + 0.168D3 * t
     #452 - 0.672D3 * t454 + 0.1566D4 * t456 - 0.1956D4 * t458 - 0.672D3
     # * t461 + 0.2712D4 * t463 - 0.5586D4 * t465 + 0.5976D4 * t467 + 0.
     #1566D4 * t469
      t701 = -0.5532D4 * t471 + 0.9720D4 * t473 - 0.9108D4 * t475 - 0.19
     #02D4 * t477 + 0.5760D4 * t479 - 0.9060D4 * t481 + 0.7860D4 * t483 
     #+ 0.1230D4 * t485 - 0.3186D4 * t487 + 0.4362D4 * t489 - 0.3582D4 *
     # t491 + 0.672D3 * t493 - 0.840D3 * t495
      t715 = 0.672D3 * t497 + 0.1176D4 * t502 - 0.3360D4 * t504 + 0.5040
     #D4 * t506 - 0.4200D4 * t508 + 0.1848D4 * t510 - 0.336D3 * t512 - 0
     #.504D3 * t514 + 0.1260D4 * t516 - 0.1680D4 * t518 + 0.1260D4 * t52
     #0 - 0.504D3 * t522
      t729 = 0.84D2 * t524 + 0.276D3 * t370 - 0.22D2 * t528 - 0.42D2 * t
     #531 + 0.50D2 * t533 - 0.28D2 * t535 - 0.22D2 * t548 + 0.48D2 * t55
     #0 - 0.52D2 * t552 + 0.28D2 * t554 + 0.10D2 * t556 - 0.20D2 * t558 
     #+ 0.20D2 * t560
      t745 = -0.10D2 * t562 - 0.6D1 * t564 + 0.2D1 * t566 + 0.12D2 * t56
     #8 - 0.2D1 * t570 + 0.6D1 * t572 - 0.552D3 * t372 - 0.1104D4 * t374
     # + 0.2682D4 * t376 - 0.3432D4 * t380 - 0.1104D4 * t382 + 0.4674D4 
     #* t384
      t759 = -0.9942D4 * t386 + 0.10692D5 * t388 + 0.2682D4 * t390 - 0.9
     #744D4 * t392 + 0.17280D5 * t394 - 0.16056D5 * t396 - 0.3234D4 * t3
     #98 + 0.9900D4 * t400 - 0.15540D5 * t402 + 0.13350D5 * t404 + 0.213
     #0D4 * t406 - 0.5562D4 * t408 + 0.7494D4 * t410
      t773 = -0.5994D4 * t413 + 0.1104D4 * t415 - 0.1380D4 * t417 + 0.11
     #04D4 * t419 + 0.1932D4 * t421 - 0.5520D4 * t423 + 0.8280D4 * t425 
     #- 0.6900D4 * t427 + 0.3036D4 * t429 - 0.552D3 * t431 - 0.828D3 * t
     #433 + 0.2070D4 * t435
      t783 = -0.2760D4 * t437 + 0.2070D4 * t439 - 0.828D3 * t441 + 0.138
     #D3 * t443 + t612 - 0.12D2 * t89 + t100 + 0.6D1 * t197 + t613 + t61
     #4 - 0.12D2 * t247 + t615 - 0.4D1 * t527 * z
      t790 = polylog(3, t290)
      t791 = t790 * x1
      t802 = t790 * t197
      t811 = t790 * t247
      t814 = 0.16D2 * t527 * t85 + 0.16D2 * t530 * z + 0.72D2 * t791 * t
     #306 - 0.36D2 * t791 * z + 0.144D3 * t791 * t85 - 0.324D3 * t791 * 
     #t89 + 0.396D3 * t791 * t86 + 0.144D3 * t802 * z - 0.558D3 * t802 *
     # t85 + 0.1116D4 * t802 * t89 - 0.1188D4 * t802 * t86 - 0.324D3 * t
     #811 * z
      t821 = t790 * t256
      t842 = 0.1116D4 * t811 * t85 - 0.1944D4 * t811 * t89 + 0.1836D4 * 
     #t811 * t86 + 0.396D3 * t821 * z - 0.1188D4 * t821 * t85 + 0.1872D4
     # * t821 * t89 - 0.1638D4 * t821 * t86 - 0.252D3 * t791 * t265 + 0.
     #648D3 * t802 * t265 - 0.900D3 * t811 * t265 + 0.756D3 * t821 * t26
     #5 - 0.144D3 * t802 * t306 + 0.180D3 * t811 * t306
      t846 = t790 * t275
      t859 = t790 * t299
      t870 = -0.144D3 * t821 * t306 - 0.252D3 * t846 * z + 0.720D3 * t84
     #6 * t85 - 0.1080D4 * t846 * t89 + 0.900D3 * t846 * t86 - 0.396D3 *
     # t846 * t265 + 0.72D2 * t846 * t306 + 0.108D3 * t859 * z - 0.270D3
     # * t859 * t85 + 0.360D3 * t859 * t89 - 0.270D3 * t859 * t86 + 0.10
     #8D3 * t859 * t265
      t873 = -t272 + t274 + t277 - t279 + t281 - t283 + t285 + t286 - t2
     #87 - t288 + t289 - t294 - t296 + t298 - t301 + t303 - t305 - t308
      t875 = t310 - t314 + t316 + t319 - t321 - t323 - t325 + t327 - t32
     #9 - t331 + t333 - t335 + t337 + t451 - t453 + t455 - t457 + t459
      t880 = t340 ** 2
      t898 = -0.18D2 * t859 * t306 - t340 * (t270 + t873 + t875 + t499 +
     # t618) + t880 * t447 / 0.2D1 - 0.18D2 * t802 - 0.18D2 * t859 + 0.3
     #6D2 * t846 - 0.18D2 * t790 * t306 + 0.36D2 * t790 * t265 - 0.54D2 
     #* t821 + 0.36D2 * t811 - 0.18D2 * t790 * t85 + 0.36D2 * t790 * t89
     # - 0.54D2 * t790 * t86
      t906 = -t196 * (t270 + t311 + t460 + t499 + t618) * t622 - t208 * 
     #t447 * t622 - 0.120D3 * t210 * (t636 + t647 + t658 + t672 + t687 +
     # t701 + t715 + t729 + t745 + t759 + t773 + t783 + t814 + t842 + t8
     #70 + t898) * t622
      t910 = FJET(XB1, XB2, s, -t2 * t228, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # -t906 * t221 * t10 / 0.480D3)
      rvgg2ght1s3e1 = t226 * t225 - t910 * t906 * t221 * t10 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = t3 * wd
      t5 = log(z)
      t6 = z ** 2
      t7 = t6 * z
      t8 = t5 * t7
      t9 = 0.168D3 * t8
      t10 = nf * t6
      t11 = -t1
      t12 = polylog(2, t11)
      t13 = 0.18D2 * t12
      t14 = 0.3D1 * t6
      t15 = 0.3D1 * t7
      t16 = t6 ** 2
      t17 = t12 * t16
      t18 = 0.18D2 * t17
      t19 = 0.84D2 * t5
      t20 = t12 * z
      t21 = 0.36D2 * t20
      t22 = nf * t7
      t23 = t5 * t6
      t24 = 0.252D3 * t23
      t25 = t5 * t16
      t26 = 0.84D2 * t25
      t27 = t5 * z
      t28 = 0.168D3 * t27
      t29 = t12 * t7
      t30 = 0.36D2 * t29
      t31 = t12 * t6
      t32 = 0.54D2 * t31
      t33 = x1 ** 2
      t34 = 0.1D1 / z
      t35 = t33 * t34
      t36 = t1 ** 2
      t37 = t36 * t1
      t39 = log(-t35 * t37)
      t45 = 0.18D2 * t5 + 0.18D2 * t25 - 0.36D2 * t8 - 0.36D2 * t27 + 0.
     #54D2 * t23
      t47 = t9 - t10 - t13 + t14 - t15 - t18 - t19 + t21 + t22 - t24 - t
     #26 + t28 + t30 - t32 - t39 * t45
      t50 = 0.440D3 * t3
      t53 = 0.720D3 - 0.720D3 * lh + 0.360D3 * t5
      t54 = t3 * t53
      t55 = t54 / 0.3D1
      t57 = (t50 + t55) * wd
      t60 = 0.1D1 / x1
      t65 = log(-t34 * t37)
      t66 = t65 * t3
      t69 = t9 - t10 - t13 + t14 - t15 - t18 - t19 + t21 + t22 - t24 - t
     #26 + t28 + t30 - t32
      t75 = t65 ** 2
      t78 = 0.3141592653589793D1 ** 2
      t82 = lh ** 2
      t86 = t5 ** 2
      t104 = polylog(3, t11)
      t108 = 0.6D1 - 0.12D2 * z - 0.2D1 * nf + 0.138D3 * t5 + t14 + t15 
     #- 0.2D1 * t10 + 0.84D2 * t12 - 0.18D2 * t104 - 0.276D3 * t27 + 0.4
     #14D3 * t23
      t125 = 0.138D3 * t25 - 0.276D3 * t8 - 0.168D3 * t29 + 0.36D2 * t10
     #4 * t7 + 0.84D2 * t17 - 0.18D2 * t104 * t16 + 0.252D3 * t31 - 0.16
     #8D3 * t20 + 0.36D2 * t104 * z - 0.54D2 * t104 * t6 + 0.4D1 * nf * 
     #z
      t133 = -(0.120D3 * t4 * t47 + t57 * t45) * t60 * t34 / 0.480D3 - (
     #(t50 + t55 - 0.120D3 * t66) * t69 + (0.960D3 * t3 + t54 - 0.360D3 
     #* t66 - t66 * t53 / 0.3D1 + 0.60D2 * t75 * t3 + t3 * (-0.60D2 * t7
     #8 + 0.720D3 * t5 + 0.1440D4 - 0.1440D4 * lh + 0.720D3 * t82 - 0.72
     #0D3 * t5 * lh + 0.180D3 * t86) / 0.3D1 + 0.80D2 / 0.3D1 * (0.8D1 -
     # 0.6D1 * lh + 0.3D1 * t5 - 0.3D1 * t65) * t3) * t45 + 0.120D3 * t3
     # * (t108 + t125)) * wd * t34 / 0.960D3
      t134 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t133)
      t136 = -0.1D1 + x1
      t139 = t33 ** 2
      t141 = t139 * x1
      t143 = t136 * t1
      t145 = log(0.1D1 - t143)
      t146 = t145 * t139
      t147 = t146 * t16
      t149 = t145 * x1
      t150 = t16 * z
      t151 = t149 * t150
      t153 = t145 * t33
      t154 = t153 * t150
      t156 = t33 * x1
      t157 = t145 * t156
      t158 = t157 * t150
      t160 = t146 * t150
      t162 = t16 * t6
      t163 = t153 * t162
      t165 = t157 * t162
      t167 = t146 * t162
      t171 = log(t35 * t37 * t136)
      t180 = t149 * z
      t182 = x1 * t7
      t184 = x1 * t16
      t186 = t33 * t6
      t188 = t33 * t7
      t190 = t33 * t16
      t192 = t156 * z
      t194 = 0.1638D4 * t147 + 0.252D3 * t151 - 0.648D3 * t154 + 0.900D3
     # * t158 - 0.756D3 * t160 + 0.144D3 * t163 - 0.180D3 * t165 + 0.144
     #D3 * t167 + 0.36D2 * t180 + 0.18D2 * t182 - 0.36D2 * t184 + 0.36D2
     # * t186 - 0.126D3 * t188 + 0.144D3 * t190 + 0.18D2 * t192
      t195 = t156 * t6
      t197 = t156 * t7
      t199 = t156 * t16
      t201 = t139 * z
      t203 = t139 * t6
      t205 = t139 * t7
      t207 = t139 * t16
      t209 = x1 * t150
      t211 = t33 * t150
      t213 = t156 * t150
      t215 = t139 * t150
      t220 = t139 * t33
      t221 = t145 * t220
      t223 = t145 * t141
      t225 = -0.108D3 * t195 + 0.216D3 * t197 - 0.180D3 * t199 - 0.18D2 
     #* t201 + 0.72D2 * t203 - 0.108D3 * t205 + 0.72D2 * t207 + 0.18D2 *
     # t209 - 0.54D2 * t211 + 0.54D2 * t213 - 0.18D2 * t215 + 0.54D2 * t
     #146 + 0.18D2 * t153 - 0.36D2 * t157 + 0.18D2 * t221 - 0.36D2 * t22
     #3
      t227 = t145 * t16
      t229 = t145 * t162
      t231 = t145 * t150
      t233 = t145 * t6
      t235 = t145 * t7
      t237 = t223 * z
      t239 = t223 * t6
      t241 = t223 * t7
      t243 = t223 * t16
      t245 = t223 * t150
      t247 = t223 * t162
      t249 = t221 * z
      t251 = t221 * t6
      t253 = t221 * t7
      t255 = t221 * t16
      t257 = t221 * t150
      t259 = 0.54D2 * t227 + 0.18D2 * t229 - 0.36D2 * t231 + 0.18D2 * t2
     #33 - 0.36D2 * t235 + 0.252D3 * t237 - 0.720D3 * t239 + 0.1080D4 * 
     #t241 - 0.900D3 * t243 + 0.396D3 * t245 - 0.72D2 * t247 - 0.108D3 *
     # t249 + 0.270D3 * t251 - 0.360D3 * t253 + 0.270D3 * t255 - 0.108D3
     # * t257
      t260 = t221 * t162
      t262 = t149 * t162
      t264 = t149 * t6
      t266 = t149 * t7
      t268 = t149 * t16
      t270 = t153 * z
      t272 = t153 * t6
      t274 = t153 * t7
      t276 = t153 * t16
      t278 = t157 * z
      t280 = t157 * t6
      t282 = t157 * t7
      t284 = t157 * t16
      t286 = t146 * z
      t288 = t146 * t6
      t290 = t146 * t7
      t292 = 0.18D2 * t260 - 0.72D2 * t262 - 0.144D3 * t264 + 0.324D3 * 
     #t266 - 0.396D3 * t268 - 0.144D3 * t270 + 0.558D3 * t272 - 0.1116D4
     # * t274 + 0.1188D4 * t276 + 0.324D3 * t278 - 0.1116D4 * t280 + 0.1
     #944D4 * t282 - 0.1836D4 * t284 - 0.396D3 * t286 + 0.1188D4 * t288 
     #- 0.1872D4 * t290
      t294 = t194 + t225 + t259 + t292
      t296 = nf * x1
      t299 = nf * t33
      t306 = nf * t141
      t313 = 0.3D1 * t139 - 0.3D1 * t141 - 0.7860D4 * t147 - 0.1230D4 * 
     #t151 + 0.3186D4 * t154 - 0.4362D4 * t158 + 0.3582D4 * t160 - 0.672
     #D3 * t163 + 0.840D3 * t165 - 0.672D3 * t167 - t171 * t294 - 0.2D1 
     #* t296 * t7 - 0.2D1 * t299 * t6 + 0.8D1 * t299 * t7 - 0.10D2 * t29
     #9 * t16 - 0.5D1 * t306 * z + 0.10D2 * t306 * t6 - 0.10D2 * t306 * 
     #t7
      t317 = nf * t156
      t325 = x1 * z
      t341 = 0.5D1 * t306 * t16 - t306 * t150 - 0.2D1 * t317 * z + 0.10D
     #2 * t317 * t6 - 0.18D2 * t317 * t7 - 0.168D3 * t180 + 0.36D2 * t32
     #5 - 0.108D3 * x1 * t6 + 0.84D2 * t182 + 0.9D1 * t184 - 0.72D2 * t3
     #3 * z + 0.162D3 * t186 - 0.30D2 * t188 - 0.138D3 * t190 + 0.12D2 *
     # t192 + 0.42D2 * t195 - 0.198D3 * t197 + 0.222D3 * t199 + 0.9D1 * 
     #t201
      t360 = nf * t139
      t365 = -0.66D2 * t203 + 0.114D3 * t205 - 0.81D2 * t207 - 0.21D2 * 
     #t209 + 0.78D2 * t211 - 0.78D2 * t213 + 0.21D2 * t215 + 0.15D2 * t1
     #41 * z - 0.30D2 * t141 * t6 + 0.30D2 * t141 * t7 - 0.15D2 * t141 *
     # t16 + 0.3D1 * t141 * t150 + t306 - t360 - nf * t16 + nf * t150 - 
     #0.252D3 * t146 - 0.84D2 * t153
      t374 = polylog(2, t143)
      t375 = t374 * t156
      t377 = t374 * t33
      t379 = t374 * t139
      t381 = t374 * t220
      t383 = t374 * t141
      t396 = 0.168D3 * t157 - 0.84D2 * t221 + 0.168D3 * t223 - 0.252D3 *
     # t227 - 0.84D2 * t229 + 0.168D3 * t231 - 0.84D2 * t233 + 0.168D3 *
     # t235 + 0.36D2 * t375 - 0.18D2 * t377 - 0.54D2 * t379 - 0.18D2 * t
     #381 + 0.36D2 * t383 - 0.54D2 * t374 * t16 - 0.18D2 * t374 * t162 +
     # 0.36D2 * t374 * t150 - 0.18D2 * t374 * t6 + 0.36D2 * t374 * t7 - 
     #0.1176D4 * t237
      t410 = t374 * x1
      t425 = 0.3360D4 * t239 - 0.5040D4 * t241 + 0.4200D4 * t243 - 0.184
     #8D4 * t245 + 0.336D3 * t247 + 0.504D3 * t249 - 0.1260D4 * t251 + 0
     #.1680D4 * t253 - 0.1260D4 * t255 + 0.504D3 * t257 - 0.84D2 * t260 
     #+ 0.72D2 * t410 * t162 - 0.36D2 * t410 * z + 0.144D3 * t410 * t6 -
     # 0.324D3 * t410 * t7 + 0.396D3 * t410 * t16 + 0.144D3 * t377 * z -
     # 0.558D3 * t377 * t6
      t464 = 0.1116D4 * t377 * t7 - 0.1188D4 * t377 * t16 - 0.324D3 * t3
     #75 * z + 0.1116D4 * t375 * t6 - 0.1944D4 * t375 * t7 + 0.1836D4 * 
     #t375 * t16 + 0.396D3 * t379 * z - 0.1188D4 * t379 * t6 + 0.1872D4 
     #* t379 * t7 - 0.1638D4 * t379 * t16 - 0.252D3 * t410 * t150 + 0.64
     #8D3 * t377 * t150 - 0.900D3 * t375 * t150 + 0.756D3 * t379 * t150 
     #- 0.144D3 * t377 * t162 + 0.180D3 * t375 * t162 - 0.144D3 * t379 *
     # t162 - 0.252D3 * t383 * z + 0.720D3 * t383 * t6
      t501 = -0.1080D4 * t383 * t7 + 0.900D3 * t383 * t16 - 0.396D3 * t3
     #83 * t150 + 0.72D2 * t383 * t162 + 0.108D3 * t381 * z - 0.270D3 * 
     #t381 * t6 + 0.360D3 * t381 * t7 - 0.270D3 * t381 * t16 + 0.108D3 *
     # t381 * t150 - 0.18D2 * t381 * t162 + 0.3D1 * t16 + 0.14D2 * t317 
     #* t16 + 0.7D1 * t360 * z - 0.18D2 * t360 * t6 + 0.22D2 * t360 * t7
     # - 0.13D2 * t360 * t16 - 0.4D1 * t317 * t150 + 0.3D1 * t360 * t150
      t524 = 0.5D1 * t296 * t16 - 0.3D1 * t296 * t150 + 0.4D1 * t299 * t
     #150 + 0.336D3 * t262 + 0.672D3 * t264 - 0.1566D4 * t266 + 0.1956D4
     # * t268 + 0.672D3 * t270 - 0.2712D4 * t272 + 0.5586D4 * t274 - 0.5
     #976D4 * t276 - 0.1566D4 * t278 + 0.5532D4 * t280 - 0.9720D4 * t282
     # + 0.9108D4 * t284 + 0.1902D4 * t286 - 0.5760D4 * t288 + 0.9060D4 
     #* t290 - 0.3D1 * t150
      t529 = (-z - x1 + t325) ** 2
      t530 = 0.1D1 / t529
      t536 = -0.120D3 * t4 * (t313 + t341 + t365 + t396 + t425 + t464 + 
     #t501 + t524) * t530 - t57 * t294 * t530
      t540 = FJET(XB1, XB2, s, -t2 * t136, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # -t536 * t60 * t34 / 0.480D3)
      rvgg2ght1s3e0 = t134 * t133 - t540 * t536 * t60 * t34 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = t3 * wd
      t5 = log(z)
      t7 = z ** 2
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = t7 * z
      t12 = t5 * t11
      t14 = t5 * z
      t16 = t5 * t7
      t18 = 0.18D2 * t5 + 0.18D2 * t9 - 0.36D2 * t12 - 0.36D2 * t14 + 0.
     #54D2 * t16
      t19 = 0.1D1 / x1
      t21 = 0.1D1 / z
      t28 = polylog(2, -t1)
      t45 = 0.168D3 * t12 - nf * t7 - 0.18D2 * t28 + 0.3D1 * t7 - 0.3D1 
     #* t11 - 0.18D2 * t28 * t8 - 0.84D2 * t5 + 0.36D2 * t28 * z + nf * 
     #t11 - 0.252D3 * t16 - 0.84D2 * t9 + 0.168D3 * t14 + 0.36D2 * t28 *
     # t11 - 0.54D2 * t28 * t7
      t54 = t1 ** 2
      t57 = log(-t21 * t54 * t1)
      t66 = -t4 * t18 * t19 * t21 / 0.4D1 - (0.120D3 * t3 * t45 + (0.440
     #D3 * t3 + t3 * (0.720D3 - 0.720D3 * lh + 0.360D3 * t5) / 0.3D1 - 0
     #.120D3 * t57 * t3) * t18) * wd * t21 / 0.960D3
      t67 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t66)
      t69 = -0.1D1 + x1
      t76 = x1 ** 2
      t83 = t76 * x1
      t92 = t76 ** 2
      t101 = t8 * z
      t106 = 0.18D2 * x1 * t11 - 0.36D2 * x1 * t8 + 0.36D2 * t76 * t7 - 
     #0.126D3 * t76 * t11 + 0.144D3 * t76 * t8 + 0.18D2 * t83 * z - 0.10
     #8D3 * t83 * t7 + 0.216D3 * t83 * t11 - 0.180D3 * t83 * t8 - 0.18D2
     # * t92 * z + 0.72D2 * t92 * t7 - 0.108D3 * t92 * t11 + 0.72D2 * t9
     #2 * t8 + 0.18D2 * x1 * t101 - 0.54D2 * t76 * t101
      t113 = log(0.1D1 - t69 * t1)
      t114 = t113 * t92
      t116 = t113 * t76
      t118 = t113 * t83
      t121 = t113 * t92 * t76
      t124 = t113 * t92 * x1
      t128 = t8 * t7
      t137 = t113 * x1
      t146 = 0.54D2 * t83 * t101 - 0.18D2 * t92 * t101 + 0.54D2 * t114 +
     # 0.18D2 * t116 - 0.36D2 * t118 + 0.18D2 * t121 - 0.36D2 * t124 + 0
     #.54D2 * t113 * t8 + 0.18D2 * t113 * t128 - 0.36D2 * t113 * t101 + 
     #0.18D2 * t113 * t7 - 0.36D2 * t113 * t11 - 0.72D2 * t137 * t128 - 
     #0.144D3 * t137 * t7 + 0.324D3 * t137 * t11 - 0.396D3 * t137 * t8
      t180 = -0.144D3 * t116 * z + 0.558D3 * t116 * t7 - 0.1116D4 * t116
     # * t11 + 0.1188D4 * t116 * t8 + 0.324D3 * t118 * z - 0.1116D4 * t1
     #18 * t7 + 0.1944D4 * t118 * t11 - 0.1836D4 * t118 * t8 - 0.396D3 *
     # t114 * z + 0.1188D4 * t114 * t7 - 0.1872D4 * t114 * t11 + 0.1638D
     #4 * t114 * t8 + 0.252D3 * t137 * t101 - 0.648D3 * t116 * t101 + 0.
     #900D3 * t118 * t101 - 0.756D3 * t114 * t101
      t213 = 0.144D3 * t116 * t128 - 0.180D3 * t118 * t128 + 0.144D3 * t
     #114 * t128 + 0.252D3 * t124 * z - 0.720D3 * t124 * t7 + 0.1080D4 *
     # t124 * t11 - 0.900D3 * t124 * t8 + 0.396D3 * t124 * t101 - 0.72D2
     # * t124 * t128 - 0.108D3 * t121 * z + 0.270D3 * t121 * t7 - 0.360D
     #3 * t121 * t11 + 0.270D3 * t121 * t8 - 0.108D3 * t121 * t101 + 0.1
     #8D2 * t121 * t128 + 0.36D2 * t137 * z
      t215 = t106 + t146 + t180 + t213
      t219 = (-z - x1 + x1 * z) ** 2
      t220 = 0.1D1 / t219
      t225 = FJET(XB1, XB2, s, -t2 * t69, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #t4 * t215 * t220 * t19 * t21 / 0.4D1)
      rvgg2ght1s3em1 = t67 * t66 + t225 * t3 * wd * t215 * t220 * t19 * 
     #t21 / 0.4D1

      end function



      doubleprecision function rvgg2ght1s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t4 = log(z)
      t6 = z ** 2
      t7 = t6 ** 2
      t17 = 0.18D2 * t4 + 0.18D2 * t4 * t7 - 0.36D2 * t4 * t6 * z - 0.36
     #D2 * t4 * z + 0.54D2 * t4 * t6
      t19 = 0.1D1 / z
      t23 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t3 * 
     #t17 * wd * t19 / 0.8D1)
      rvgg2ght1s3em2 = -t23 * t3 * t17 * wd * t19 / 0.8D1

      end function



      doubleprecision function rvgg2ght1s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s3em3 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s3em4 = 0.0D0

      end function


      doubleprecision function rvgg2ght1s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t6 = 0.1D1 / t1
      t7 = t6 * wd
      t9 = t1 ** 2
      t12 = 0.1D1 / z
      t13 = t3 ** 2
      t16 = log(-x1 * t9 * t1 * t12 * t13)
      t17 = t7 * t16
      t20 = log(z)
      t23 = t6 * (0.720D3 - 0.720D3 * lh + 0.360D3 * t20)
      t24 = t23 * wd
      t27 = t1 * x1
      t28 = polylog(2, -t27)
      t29 = x1 ** 2
      t30 = t29 ** 2
      t31 = t28 * t30
      t32 = z ** 2
      t33 = t32 ** 2
      t34 = t33 * z
      t35 = t31 * t34
      t37 = t28 * t29
      t38 = t33 * t32
      t39 = t37 * t38
      t41 = t29 * x1
      t42 = t28 * t41
      t43 = t42 * t38
      t45 = t31 * t38
      t47 = t30 * t29
      t48 = t28 * t47
      t49 = t48 * z
      t51 = t48 * t32
      t53 = t32 * z
      t54 = t48 * t53
      t56 = t48 * t33
      t58 = t48 * t34
      t60 = t48 * t38
      t62 = t30 * x1
      t63 = t28 * t62
      t64 = t63 * z
      t66 = t63 * t32
      t68 = t63 * t53
      t70 = t63 * t33
      t72 = t63 * t34
      t74 = t63 * t38
      t76 = x1 * z
      t78 = x1 * t32
      t80 = 0.396D3 * t35 - 0.18D2 * t39 + 0.36D2 * t43 - 0.54D2 * t45 +
     # 0.108D3 * t49 - 0.270D3 * t51 + 0.360D3 * t54 - 0.270D3 * t56 + 0
     #.108D3 * t58 - 0.18D2 * t60 - 0.396D3 * t64 + 0.900D3 * t66 - 0.10
     #80D4 * t68 + 0.720D3 * t70 - 0.252D3 * t72 + 0.36D2 * t74 - 0.39D2
     # * t76 + 0.72D2 * t78
      t81 = x1 * t53
      t83 = t29 * z
      t84 = 0.168D3 * t83
      t85 = t29 * t32
      t87 = t29 * t53
      t89 = t29 * t33
      t91 = t41 * z
      t93 = t41 * t32
      t95 = t41 * t53
      t97 = t41 * t33
      t99 = t30 * z
      t101 = t30 * t32
      t103 = t30 * t53
      t105 = t30 * t33
      t107 = t41 * t34
      t109 = t30 * t34
      t112 = 0.15D2 * t62 * z
      t114 = 0.30D2 * t62 * t32
      t116 = 0.30D2 * t62 * t53
      t117 = -0.36D2 * t81 + t84 - 0.408D3 * t85 + 0.360D3 * t87 - 0.108
     #D3 * t89 - 0.198D3 * t91 + 0.522D3 * t93 - 0.558D3 * t95 + 0.252D3
     # * t97 + 0.84D2 * t99 - 0.216D3 * t101 + 0.264D3 * t103 - 0.156D3 
     #* t105 - 0.36D2 * t107 + 0.36D2 * t109 - t112 + t114 - t116
      t120 = 0.15D2 * t62 * t33
      t122 = 0.3D1 * t62 * t34
      t123 = nf * x1
      t124 = nf * t29
      t126 = nf * t62
      t127 = nf * t41
      t129 = nf * t30
      t132 = log(0.1D1 + t27)
      t133 = t132 * t32
      t135 = t132 * t41
      t137 = t132 * t30
      t139 = t132 * t62
      t141 = t132 * t47
      t143 = t132 * t33
      t145 = t132 * z
      t147 = t132 * x1
      t149 = t132 * t29
      t151 = t132 * t53
      t153 = t28 * t32
      t155 = t120 - t122 - t123 + 0.4D1 * t124 - t126 - 0.6D1 * t127 + 0
     #.4D1 * t129 - 0.252D3 * t133 + 0.840D3 * t135 - 0.672D3 * t137 + 0
     #.336D3 * t139 - 0.84D2 * t141 - 0.84D2 * t143 + 0.168D3 * t145 + 0
     #.336D3 * t147 - 0.672D3 * t149 + 0.168D3 * t151 - 0.54D2 * t153
      t156 = t28 * t53
      t162 = t28 * t33
      t164 = t28 * z
      t166 = t28 * x1
      t172 = 0.3D1 * t62
      t173 = t123 * z
      t176 = 0.4D1 * t123 * t32
      t178 = 0.2D1 * t123 * t53
      t179 = t124 * z
      t181 = t124 * t32
      t184 = 0.14D2 * t124 * t53
      t185 = 0.36D2 * t156 + 0.180D3 * t42 - 0.144D3 * t31 + 0.72D2 * t6
     #3 - 0.18D2 * t48 - 0.18D2 * t162 + 0.36D2 * t164 + 0.72D2 * t166 -
     # 0.144D3 * t37 - 0.12D2 * t29 + 0.18D2 * t41 - 0.12D2 * t30 + t172
     # + 0.3D1 * t173 - t176 + t178 - 0.14D2 * t179 + 0.20D2 * t181 - t1
     #84
      t189 = 0.4D1 * t124 * t33
      t199 = t127 * z
      t201 = t127 * t32
      t203 = t127 * t53
      t206 = 0.12D2 * t127 * t33
      t207 = t129 * z
      t209 = t129 * t32
      t211 = t129 * t53
      t213 = t129 * t33
      t216 = 0.2D1 * t127 * t34
      t218 = 0.2D1 * t129 * t34
      t219 = t149 * t32
      t221 = t139 * t53
      t223 = t189 + 0.5D1 * t126 * z - 0.10D2 * t126 * t32 + 0.10D2 * t1
     #26 * t53 - 0.5D1 * t126 * t33 + t126 * t34 + 0.24D2 * t199 - 0.38D
     #2 * t201 + 0.30D2 * t203 - t206 - 0.18D2 * t207 + 0.32D2 * t209 - 
     #0.28D2 * t211 + 0.12D2 * t213 + t216 - t218 - 0.5976D4 * t219 - 0.
     #5040D4 * t221
      t224 = t139 * t34
      t226 = t141 * t32
      t228 = t147 * z
      t230 = t147 * t32
      t232 = t149 * z
      t234 = t147 * t53
      t236 = t149 * t53
      t238 = t135 * z
      t240 = t135 * t32
      t242 = t135 * t53
      t244 = t147 * t33
      t246 = t149 * t33
      t248 = t135 * t33
      t250 = t137 * z
      t252 = t137 * t32
      t254 = t137 * t53
      t256 = t137 * t33
      t258 = t147 * t34
      t260 = -0.1176D4 * t224 - 0.1260D4 * t226 - 0.1230D4 * t228 + 0.19
     #56D4 * t230 + 0.3186D4 * t232 - 0.1566D4 * t234 + 0.5586D4 * t236 
     #- 0.4362D4 * t238 + 0.9108D4 * t240 - 0.9720D4 * t242 + 0.672D3 * 
     #t244 - 0.2712D4 * t246 + 0.5532D4 * t248 + 0.3582D4 * t250 - 0.786
     #0D4 * t252 + 0.9060D4 * t254 - 0.5760D4 * t256 - 0.168D3 * t258
      t262 = t149 * t34
      t264 = t135 * t34
      t266 = t137 * t34
      t268 = t149 * t38
      t270 = t135 * t38
      t272 = t137 * t38
      t274 = t141 * z
      t276 = t141 * t53
      t278 = t141 * t33
      t280 = t141 * t34
      t282 = t141 * t38
      t284 = t139 * z
      t286 = t139 * t32
      t288 = t139 * t33
      t290 = t139 * t38
      t292 = t166 * z
      t294 = t166 * t32
      t296 = t37 * z
      t298 = 0.672D3 * t262 - 0.1566D4 * t264 + 0.1902D4 * t266 - 0.84D2
     # * t268 + 0.168D3 * t270 - 0.252D3 * t272 + 0.504D3 * t274 + 0.168
     #0D4 * t276 - 0.1260D4 * t278 + 0.504D3 * t280 - 0.84D2 * t282 - 0.
     #1848D4 * t284 + 0.4200D4 * t286 + 0.3360D4 * t288 + 0.168D3 * t290
     # - 0.252D3 * t292 + 0.396D3 * t294 + 0.648D3 * t296
      t299 = t37 * t32
      t301 = t166 * t53
      t303 = t37 * t53
      t305 = t42 * z
      t307 = t42 * t32
      t309 = t42 * t53
      t311 = t166 * t33
      t313 = t37 * t33
      t315 = t42 * t33
      t317 = t31 * z
      t319 = t31 * t32
      t321 = t31 * t53
      t323 = t31 * t33
      t325 = t166 * t34
      t327 = t34 * t37
      t329 = t42 * t34
      t333 = 0.3D1 * x1
      t334 = -0.1188D4 * t299 - 0.324D3 * t301 + 0.1116D4 * t303 - 0.900
     #D3 * t305 + 0.1836D4 * t307 - 0.1944D4 * t309 + 0.144D3 * t311 - 0
     #.558D3 * t313 + 0.1116D4 * t315 + 0.756D3 * t317 - 0.1638D4 * t319
     # + 0.1872D4 * t321 - 0.1188D4 * t323 - 0.36D2 * t325 + 0.144D3 * t
     #327 - 0.324D3 * t329 - 0.84D2 * t132 - 0.18D2 * t28 + t333
      t341 = t16 ** 2
      t347 = 0.3141592653589793D1 ** 2
      t351 = lh ** 2
      t355 = t20 ** 2
      t384 = 0.18D2 * t76 - 0.36D2 * t78 + 0.18D2 * t81 - 0.54D2 * t83 +
     # 0.144D3 * t85 - 0.126D3 * t87 + 0.36D2 * t89 + 0.54D2 * t91 - 0.1
     #80D3 * t93 + 0.216D3 * t95 - 0.108D3 * t97 - 0.18D2 * t99 + 0.72D2
     # * t101 - 0.108D3 * t103 + 0.72D2 * t105
      t401 = 0.18D2 * t107 - 0.18D2 * t109 + 0.54D2 * t133 - 0.180D3 * t
     #135 + 0.144D3 * t137 - 0.72D2 * t139 + 0.18D2 * t141 + 0.18D2 * t1
     #43 - 0.36D2 * t145 - 0.72D2 * t147 + 0.144D3 * t149 - 0.36D2 * t15
     #1 + 0.1188D4 * t219 + 0.1080D4 * t221 + 0.252D3 * t224 + 0.270D3 *
     # t226
      t419 = 0.252D3 * t228 - 0.396D3 * t230 - 0.648D3 * t232 + 0.324D3 
     #* t234 - 0.1116D4 * t236 + 0.900D3 * t238 - 0.1836D4 * t240 + 0.19
     #44D4 * t242 - 0.144D3 * t244 + 0.558D3 * t246 - 0.1116D4 * t248 - 
     #0.756D3 * t250 + 0.1638D4 * t252 - 0.1872D4 * t254 + 0.1188D4 * t2
     #56 + 0.36D2 * t258
      t436 = -0.144D3 * t262 + 0.324D3 * t264 - 0.396D3 * t266 + 0.18D2 
     #* t268 - 0.36D2 * t270 + 0.54D2 * t272 - 0.108D3 * t274 - 0.360D3 
     #* t276 + 0.270D3 * t278 - 0.108D3 * t280 + 0.18D2 * t282 + 0.396D3
     # * t284 - 0.900D3 * t286 - 0.720D3 * t288 - 0.36D2 * t290 + 0.18D2
     # * t132
      t451 = -0.1902D4 * t35 + 0.84D2 * t39 - 0.168D3 * t43 + 0.252D3 * 
     #t45 - 0.504D3 * t49 + 0.1260D4 * t51 - 0.1680D4 * t54 + 0.1260D4 *
     # t56 - 0.504D3 * t58 + 0.84D2 * t60 + 0.1848D4 * t64
      t463 = -0.4200D4 * t66 + 0.5040D4 * t68 - 0.3360D4 * t70 + 0.1176D
     #4 * t72 - 0.168D3 * t74 + 0.27D2 * t76 - 0.84D2 * t78 + 0.132D3 * 
     #t81 - t84 + 0.462D3 * t85 - 0.564D3 * t87 + 0.324D3 * t89
      t475 = 0.222D3 * t91 - 0.546D3 * t93 + 0.606D3 * t95 - 0.312D3 * t
     #97 - 0.96D2 * t99 + 0.204D3 * t101 - 0.216D3 * t103 + 0.114D3 * t1
     #05 + 0.60D2 * t107 - 0.24D2 * t109 + t112 - t114
      t485 = t116 - t120 + t122 - 0.2D1 * t124 + 0.4D1 * t127 - 0.2D1 * 
     #t129 + 0.414D3 * t133 - 0.1380D4 * t135 + 0.1104D4 * t137 - 0.552D
     #3 * t139 + 0.138D3 * t141 + 0.138D3 * t143
      t499 = -0.276D3 * t145 - 0.552D3 * t147 + 0.1104D4 * t149 - 0.276D
     #3 * t151 + 0.252D3 * t153 - 0.168D3 * t156 - 0.840D3 * t42 + 0.672
     #D3 * t31 - 0.336D3 * t63 + 0.84D2 * t48 + 0.84D2 * t162
      t509 = polylog(3, -t27)
      t514 = t509 * t41
      t516 = t509 * t30
      t518 = t509 * t62
      t520 = t509 * t47
      t522 = -0.168D3 * t164 - 0.336D3 * t166 + 0.672D3 * t37 - 0.108D3 
     #* t33 * x1 + 0.36D2 * x1 * t34 - 0.72D2 * t29 * t34 - 0.54D2 * t50
     #9 * t32 + 0.36D2 * t509 * t53 + 0.180D3 * t514 - 0.144D3 * t516 + 
     #0.72D2 * t518 - 0.18D2 * t520
      t528 = t509 * x1
      t530 = t509 * t29
      t537 = -0.18D2 * t509 * t33 + 0.36D2 * t509 * z + 0.72D2 * t528 - 
     #0.144D3 * t530 + 0.18D2 * t29 - 0.30D2 * t41 + 0.18D2 * t30 - t172
     # - 0.18D2 * t509 - 0.2D1 * t173 + t176 - t178
      t547 = 0.10D2 * t179 - 0.18D2 * t181 + t184 - t189 - 0.18D2 * t199
     # + 0.32D2 * t201 - 0.28D2 * t203 + t206 + 0.10D2 * t207 - 0.20D2 *
     # t209 + 0.20D2 * t211 - 0.10D2 * t213
      t560 = -t216 + t218 + 0.10692D5 * t219 + 0.8280D4 * t221 + 0.1932D
     #4 * t224 + 0.2070D4 * t226 + 0.2130D4 * t228 - 0.3432D4 * t230 - 0
     #.5562D4 * t232 + 0.2682D4 * t234 - 0.9942D4 * t236
      t573 = 0.7494D4 * t238 - 0.16056D5 * t240 + 0.17280D5 * t242 - 0.1
     #104D4 * t244 + 0.4674D4 * t246 - 0.9744D4 * t248 - 0.5994D4 * t250
     # + 0.13350D5 * t252 - 0.15540D5 * t254 + 0.9900D4 * t256 + 0.276D3
     # * t258 - 0.1104D4 * t262
      t587 = 0.2682D4 * t264 - 0.3234D4 * t266 + 0.138D3 * t268 - 0.276D
     #3 * t270 + 0.414D3 * t272 - 0.828D3 * t274 - 0.2760D4 * t276 + 0.2
     #070D4 * t278 - 0.828D3 * t280 + 0.138D3 * t282 + 0.3036D4 * t284 -
     # 0.6900D4 * t286
      t600 = -0.5520D4 * t288 - 0.276D3 * t290 + 0.1230D4 * t292 - 0.195
     #6D4 * t294 - 0.3186D4 * t296 + 0.5976D4 * t299 + 0.1566D4 * t301 -
     # 0.5586D4 * t303 + 0.4362D4 * t305 - 0.9108D4 * t307 + 0.9720D4 * 
     #t309 - 0.672D3 * t311
      t616 = 0.2712D4 * t313 - 0.5532D4 * t315 - 0.3582D4 * t317 + 0.786
     #0D4 * t319 - 0.9060D4 * t321 + 0.5760D4 * t323 + 0.168D3 * t325 - 
     #0.672D3 * t327 + 0.1566D4 * t329 - 0.252D3 * t528 * z + 0.396D3 * 
     #t528 * t32
      t641 = 0.648D3 * t530 * z - 0.1188D4 * t530 * t32 - 0.324D3 * t528
     # * t53 + 0.1116D4 * t530 * t53 - 0.900D3 * t514 * z + 0.1836D4 * t
     #514 * t32 - 0.1944D4 * t514 * t53 + 0.144D3 * t528 * t33 - 0.558D3
     # * t530 * t33 + 0.1116D4 * t514 * t33 + 0.756D3 * t516 * z - 0.163
     #8D4 * t516 * t32
      t667 = 0.1872D4 * t516 * t53 - 0.1188D4 * t516 * t33 - 0.36D2 * t5
     #28 * t34 + 0.144D3 * t530 * t34 - 0.324D3 * t514 * t34 + 0.396D3 *
     # t516 * t34 - 0.18D2 * t530 * t38 + 0.36D2 * t514 * t38 - 0.54D2 *
     # t516 * t38 + 0.108D3 * t520 * z - 0.270D3 * t520 * t32 + 0.360D3 
     #* t520 * t53
      t688 = -0.270D3 * t520 * t33 + 0.108D3 * t520 * t34 - 0.18D2 * t52
     #0 * t38 - 0.396D3 * t518 * z + 0.900D3 * t518 * t32 - 0.1080D4 * t
     #518 * t53 + 0.720D3 * t518 * t33 - 0.252D3 * t518 * t34 + 0.36D2 *
     # t518 * t38 + 0.138D3 * t132 + 0.84D2 * t28 - t333
      t695 = (0.440D3 * t7 - 0.120D3 * t17 + t24 / 0.3D1) * (t80 + t117 
     #+ t155 + t185 + t223 + t260 + t298 + t334) + (0.960D3 * t7 - 0.360
     #D3 * t17 + t24 + 0.60D2 * t7 * t341 - t23 * wd * t16 / 0.3D1 + t6 
     #* (-0.60D2 * t347 + 0.720D3 * t20 + 0.1440D4 - 0.1440D4 * lh + 0.7
     #20D3 * t351 - 0.720D3 * t20 * lh + 0.180D3 * t355) * wd / 0.3D1 + 
     #0.80D2 / 0.3D1 * wd * (-0.3D1 * t16 + 0.8D1 - 0.6D1 * lh + 0.3D1 *
     # t20) * t6) * (t384 + t401 + t419 + t436) + 0.120D3 * t7 * (t451 +
     # t463 + t475 + t485 + t499 + t522 + t537 + t547 + t560 + t573 + t5
     #87 + t600 + t616 + t641 + t667 + t688)
      t697 = (0.1D1 - x1 + t76) ** 2
      t698 = 0.1D1 / t697
      t700 = 0.1D1 / x1
      t704 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, t
     #695 * t698 * t700 * t12 / 0.480D3)
      rvgg2ght1s4e1 = t704 * t695 * t698 * t700 * t12 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t6 = 0.1D1 / t1
      t7 = t6 * wd
      t8 = x1 * z
      t10 = z ** 2
      t11 = x1 * t10
      t13 = t10 * z
      t14 = x1 * t13
      t16 = x1 ** 2
      t17 = t16 * z
      t19 = t16 * t10
      t21 = t16 * t13
      t23 = t10 ** 2
      t24 = t16 * t23
      t26 = x1 * t16
      t27 = t26 * z
      t29 = t26 * t10
      t31 = t26 * t13
      t33 = t26 * t23
      t35 = t16 ** 2
      t36 = t35 * z
      t38 = t35 * t10
      t40 = t35 * t13
      t42 = t35 * t23
      t44 = t23 * z
      t45 = t26 * t44
      t47 = t35 * t44
      t49 = t35 * x1
      t52 = -0.39D2 * t8 + 0.72D2 * t11 - 0.36D2 * t14 + 0.168D3 * t17 -
     # 0.408D3 * t19 + 0.360D3 * t21 - 0.108D3 * t24 - 0.198D3 * t27 + 0
     #.522D3 * t29 - 0.558D3 * t31 + 0.252D3 * t33 + 0.84D2 * t36 - 0.21
     #6D3 * t38 + 0.264D3 * t40 - 0.156D3 * t42 - 0.36D2 * t45 + 0.36D2 
     #* t47 - 0.15D2 * t49 * z
      t61 = nf * x1
      t62 = nf * t16
      t64 = nf * t49
      t65 = nf * t26
      t67 = nf * t35
      t69 = t1 * x1
      t71 = log(0.1D1 + t69)
      t72 = t71 * t10
      t74 = t71 * t26
      t76 = t71 * t35
      t78 = t71 * t49
      t80 = t35 * t16
      t81 = t71 * t80
      t83 = t71 * t23
      t85 = t71 * z
      t87 = t71 * x1
      t89 = t71 * t16
      t91 = 0.30D2 * t49 * t10 - 0.30D2 * t49 * t13 + 0.15D2 * t49 * t23
     # - 0.3D1 * t49 * t44 - t61 + 0.4D1 * t62 - t64 - 0.6D1 * t65 + 0.4
     #D1 * t67 - 0.252D3 * t72 + 0.840D3 * t74 - 0.672D3 * t76 + 0.336D3
     # * t78 - 0.84D2 * t81 - 0.84D2 * t83 + 0.168D3 * t85 + 0.336D3 * t
     #87 - 0.672D3 * t89
      t93 = t71 * t13
      t95 = polylog(2, -t69)
      t100 = t95 * t26
      t102 = t95 * t35
      t104 = t95 * t49
      t106 = t95 * t80
      t112 = t95 * x1
      t114 = t95 * t16
      t126 = 0.168D3 * t93 - 0.54D2 * t95 * t10 + 0.36D2 * t95 * t13 + 0
     #.180D3 * t100 - 0.144D3 * t102 + 0.72D2 * t104 - 0.18D2 * t106 - 0
     #.18D2 * t95 * t23 + 0.36D2 * t95 * z + 0.72D2 * t112 - 0.144D3 * t
     #114 - 0.12D2 * t16 + 0.18D2 * t26 - 0.12D2 * t35 + 0.3D1 * t49 + 0
     #.3D1 * t61 * z - 0.4D1 * t61 * t10 + 0.2D1 * t61 * t13
      t164 = -0.14D2 * t62 * z + 0.20D2 * t62 * t10 - 0.14D2 * t62 * t13
     # + 0.4D1 * t62 * t23 + 0.5D1 * t64 * z - 0.10D2 * t64 * t10 + 0.10
     #D2 * t64 * t13 - 0.5D1 * t64 * t23 + t64 * t44 + 0.24D2 * t65 * z 
     #- 0.38D2 * t65 * t10 + 0.30D2 * t65 * t13 - 0.12D2 * t65 * t23 - 0
     #.18D2 * t67 * z + 0.32D2 * t67 * t10 - 0.28D2 * t67 * t13 + 0.12D2
     # * t67 * t23 + 0.2D1 * t65 * t44 - 0.2D1 * t67 * t44
      t167 = t89 * t10
      t169 = t78 * t13
      t171 = t78 * t44
      t173 = t81 * t10
      t175 = t87 * z
      t177 = t87 * t10
      t179 = t89 * z
      t181 = t87 * t13
      t183 = t89 * t13
      t185 = t74 * z
      t187 = t74 * t10
      t189 = t74 * t13
      t191 = t87 * t23
      t193 = t89 * t23
      t195 = t74 * t23
      t197 = t76 * z
      t199 = t76 * t10
      t201 = t76 * t13
      t203 = -0.5976D4 * t167 - 0.5040D4 * t169 - 0.1176D4 * t171 - 0.12
     #60D4 * t173 - 0.1230D4 * t175 + 0.1956D4 * t177 + 0.3186D4 * t179 
     #- 0.1566D4 * t181 + 0.5586D4 * t183 - 0.4362D4 * t185 + 0.9108D4 *
     # t187 - 0.9720D4 * t189 + 0.672D3 * t191 - 0.2712D4 * t193 + 0.553
     #2D4 * t195 + 0.3582D4 * t197 - 0.7860D4 * t199 + 0.9060D4 * t201
      t204 = t76 * t23
      t206 = t87 * t44
      t208 = t89 * t44
      t210 = t74 * t44
      t212 = t76 * t44
      t214 = t23 * t10
      t215 = t89 * t214
      t217 = t74 * t214
      t219 = t76 * t214
      t221 = t81 * z
      t223 = t81 * t13
      t225 = t81 * t23
      t227 = t81 * t44
      t229 = t81 * t214
      t231 = t78 * z
      t233 = t78 * t10
      t235 = t78 * t23
      t237 = t78 * t214
      t241 = -0.5760D4 * t204 - 0.168D3 * t206 + 0.672D3 * t208 - 0.1566
     #D4 * t210 + 0.1902D4 * t212 - 0.84D2 * t215 + 0.168D3 * t217 - 0.2
     #52D3 * t219 + 0.504D3 * t221 + 0.1680D4 * t223 - 0.1260D4 * t225 +
     # 0.504D3 * t227 - 0.84D2 * t229 - 0.1848D4 * t231 + 0.4200D4 * t23
     #3 + 0.3360D4 * t235 + 0.168D3 * t237 - 0.252D3 * t112 * z
      t279 = 0.396D3 * t112 * t10 + 0.648D3 * t114 * z - 0.1188D4 * t114
     # * t10 - 0.324D3 * t112 * t13 + 0.1116D4 * t114 * t13 - 0.900D3 * 
     #t100 * z + 0.1836D4 * t100 * t10 - 0.1944D4 * t100 * t13 + 0.144D3
     # * t112 * t23 - 0.558D3 * t114 * t23 + 0.1116D4 * t100 * t23 + 0.7
     #56D3 * t102 * z - 0.1638D4 * t102 * t10 + 0.1872D4 * t102 * t13 - 
     #0.1188D4 * t102 * t23 - 0.36D2 * t112 * t44 + 0.144D3 * t114 * t44
     # - 0.324D3 * t100 * t44
      t315 = 0.396D3 * t102 * t44 - 0.18D2 * t114 * t214 + 0.36D2 * t100
     # * t214 - 0.54D2 * t102 * t214 + 0.108D3 * t106 * z - 0.270D3 * t1
     #06 * t10 + 0.360D3 * t106 * t13 - 0.270D3 * t106 * t23 + 0.108D3 *
     # t106 * t44 - 0.18D2 * t106 * t214 - 0.396D3 * t104 * z + 0.900D3 
     #* t104 * t10 - 0.1080D4 * t104 * t13 + 0.720D3 * t104 * t23 - 0.25
     #2D3 * t104 * t44 + 0.36D2 * t104 * t214 + 0.3D1 * x1 - 0.84D2 * t7
     #1 - 0.18D2 * t95
      t322 = t1 ** 2
      t325 = 0.1D1 / z
      t326 = t3 ** 2
      t329 = log(-x1 * t322 * t1 * t325 * t326)
      t333 = log(z)
      t355 = 0.18D2 * t8 - 0.36D2 * t11 + 0.18D2 * t14 - 0.54D2 * t17 + 
     #0.144D3 * t19 - 0.126D3 * t21 + 0.36D2 * t24 + 0.54D2 * t27 - 0.18
     #0D3 * t29 + 0.216D3 * t31 - 0.108D3 * t33 - 0.18D2 * t36 + 0.72D2 
     #* t38 - 0.108D3 * t40 + 0.72D2 * t42
      t372 = 0.18D2 * t45 - 0.18D2 * t47 + 0.54D2 * t72 - 0.180D3 * t74 
     #+ 0.144D3 * t76 - 0.72D2 * t78 + 0.18D2 * t81 + 0.18D2 * t83 - 0.3
     #6D2 * t85 - 0.72D2 * t87 + 0.144D3 * t89 - 0.36D2 * t93 + 0.1188D4
     # * t167 + 0.1080D4 * t169 + 0.252D3 * t171 + 0.270D3 * t173
      t390 = 0.252D3 * t175 - 0.396D3 * t177 - 0.648D3 * t179 + 0.324D3 
     #* t181 - 0.1116D4 * t183 + 0.900D3 * t185 - 0.1836D4 * t187 + 0.19
     #44D4 * t189 - 0.144D3 * t191 + 0.558D3 * t193 - 0.1116D4 * t195 - 
     #0.756D3 * t197 + 0.1638D4 * t199 - 0.1872D4 * t201 + 0.1188D4 * t2
     #04 + 0.36D2 * t206
      t407 = -0.144D3 * t208 + 0.324D3 * t210 - 0.396D3 * t212 + 0.18D2 
     #* t215 - 0.36D2 * t217 + 0.54D2 * t219 - 0.108D3 * t221 - 0.360D3 
     #* t223 + 0.270D3 * t225 - 0.108D3 * t227 + 0.18D2 * t229 + 0.396D3
     # * t231 - 0.900D3 * t233 - 0.720D3 * t235 - 0.36D2 * t237 + 0.18D2
     # * t71
      t411 = 0.120D3 * t7 * (t52 + t91 + t126 + t164 + t203 + t241 + t27
     #9 + t315) + (0.440D3 * t7 - 0.120D3 * t7 * t329 + t6 * (0.720D3 - 
     #0.720D3 * lh + 0.360D3 * t333) * wd / 0.3D1) * (t355 + t372 + t390
     # + t407)
      t413 = (0.1D1 - x1 + t8) ** 2
      t414 = 0.1D1 / t413
      t416 = 0.1D1 / x1
      t420 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, t
     #411 * t414 * t416 * t325 / 0.480D3)
      rvgg2ght1s4e0 = t420 * t411 * t414 * t416 * t325 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = 0.1D1 / t1
      t10 = log(0.1D1 + t1 * x1)
      t11 = x1 ** 2
      t12 = t10 * t11
      t13 = z ** 2
      t16 = t11 ** 2
      t18 = t10 * t16 * x1
      t19 = t13 * z
      t22 = t13 ** 2
      t23 = t22 * z
      t27 = t10 * t16 * t11
      t30 = x1 * t10
      t41 = x1 * t11
      t42 = t10 * t41
      t55 = 0.1188D4 * t12 * t13 + 0.1080D4 * t18 * t19 + 0.252D3 * t18 
     #* t23 + 0.270D3 * t27 * t13 + 0.252D3 * t30 * z - 0.396D3 * t30 * 
     #t13 - 0.648D3 * t12 * z + 0.324D3 * t30 * t19 - 0.1116D4 * t12 * t
     #19 + 0.900D3 * t42 * z - 0.1836D4 * t42 * t13 + 0.1944D4 * t42 * t
     #19 - 0.144D3 * t30 * t22 + 0.558D3 * t12 * t22 - 0.1116D4 * t42 * 
     #t22
      t56 = t16 * t10
      t73 = t22 * t13
      t90 = -0.756D3 * t56 * z + 0.1638D4 * t56 * t13 - 0.1872D4 * t56 *
     # t19 + 0.1188D4 * t56 * t22 + 0.36D2 * t30 * t23 - 0.144D3 * t12 *
     # t23 + 0.324D3 * t42 * t23 - 0.396D3 * t56 * t23 + 0.18D2 * t12 * 
     #t73 - 0.36D2 * t42 * t73 + 0.54D2 * t56 * t73 - 0.108D3 * t27 * z 
     #- 0.360D3 * t27 * t19 + 0.270D3 * t27 * t22 - 0.108D3 * t27 * t23 
     #+ 0.18D2 * t27 * t73
      t100 = x1 * z
      t124 = 0.396D3 * t18 * z - 0.900D3 * t18 * t13 - 0.720D3 * t18 * t
     #22 - 0.36D2 * t18 * t73 + 0.18D2 * t100 - 0.36D2 * x1 * t13 + 0.18
     #D2 * x1 * t19 - 0.54D2 * t11 * z + 0.144D3 * t11 * t13 - 0.126D3 *
     # t11 * t19 + 0.36D2 * t11 * t22 + 0.54D2 * t41 * z - 0.180D3 * t41
     # * t13 + 0.216D3 * t41 * t19 - 0.108D3 * t41 * t22 - 0.18D2 * t16 
     #* z
      t150 = 0.72D2 * t16 * t13 - 0.108D3 * t16 * t19 + 0.72D2 * t16 * t
     #22 + 0.18D2 * t41 * t23 - 0.18D2 * t16 * t23 + 0.54D2 * t10 * t13 
     #- 0.180D3 * t42 + 0.144D3 * t56 - 0.72D2 * t18 + 0.18D2 * t27 + 0.
     #18D2 * t10 * t22 - 0.36D2 * t10 * z - 0.72D2 * t30 + 0.144D3 * t12
     # - 0.36D2 * t10 * t19 + 0.18D2 * t10
      t152 = t55 + t90 + t124 + t150
      t155 = (0.1D1 - x1 + t100) ** 2
      t156 = 0.1D1 / t155
      t157 = 0.1D1 / x1
      t159 = 0.1D1 / z
      t163 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D
     #0, 0.0D0, t6 * wd * t152 * t156 * t157 * t159 / 0.4D1)
      rvgg2ght1s4em1 = t163 * t6 * wd * t152 * t156 * t157 * t159 / 0.4D
     #1

      end function



      doubleprecision function rvgg2ght1s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s4em2 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s4em3 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s4em4 = 0.0D0

      end function


      doubleprecision function rvgg2ght1s5e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x1
      t6 = 0.1D1 / t1
      t7 = t6 * wd
      t9 = t1 ** 2
      t12 = 0.1D1 / z
      t13 = t4 ** 2
      t16 = log(-x1 * t9 * t1 * t12 * t13)
      t17 = t7 * t16
      t20 = log(z)
      t23 = t6 * (0.720D3 - 0.720D3 * lh + 0.360D3 * t20)
      t24 = t23 * wd
      t27 = nf * x1
      t28 = t27 * z
      t30 = 0.3D1 * x1
      t31 = z ** 2
      t33 = 0.4D1 * t27 * t31
      t34 = x1 ** 2
      t35 = nf * t34
      t36 = t31 ** 2
      t38 = 0.4D1 * t35 * t36
      t39 = t31 * z
      t41 = 0.14D2 * t35 * t39
      t42 = t35 * t31
      t44 = t35 * z
      t47 = 0.2D1 * t27 * t39
      t48 = t34 ** 2
      t49 = t48 * x1
      t50 = nf * t49
      t59 = t36 * z
      t61 = t34 * x1
      t62 = nf * t61
      t63 = t62 * z
      t65 = nf * t48
      t67 = 0.2D1 * t65 * t59
      t69 = 0.2D1 * t62 * t59
      t70 = t65 * t36
      t72 = t65 * t39
      t74 = 0.3D1 * t28 + t30 - t33 + t38 - t41 + 0.20D2 * t42 - 0.14D2 
     #* t44 + t47 - 0.10D2 * t50 * t31 + 0.5D1 * t50 * z + 0.10D2 * t50 
     #* t39 - 0.5D1 * t50 * t36 + t50 * t59 + 0.24D2 * t63 - t67 + t69 +
     # 0.12D2 * t70 - 0.28D2 * t72
      t75 = t65 * t31
      t77 = t65 * z
      t80 = 0.12D2 * t62 * t36
      t81 = t62 * t39
      t83 = t62 * t31
      t85 = t1 * x1
      t87 = log(0.1D1 + t85)
      t88 = t87 * t34
      t89 = t88 * t31
      t91 = t87 * t49
      t92 = t91 * t39
      t94 = t91 * t59
      t96 = t88 * z
      t98 = t87 * x1
      t99 = t98 * t31
      t101 = t98 * z
      t103 = t48 * t34
      t104 = t87 * t103
      t105 = t104 * t31
      t107 = t87 * t48
      t108 = t107 * t31
      t110 = t107 * z
      t112 = t87 * t61
      t113 = t112 * t36
      t115 = t88 * t36
      t117 = t98 * t36
      t119 = t112 * t39
      t121 = 0.32D2 * t75 - 0.18D2 * t77 - t80 + 0.30D2 * t81 - 0.38D2 *
     # t83 - 0.5976D4 * t89 - 0.5040D4 * t92 - 0.1176D4 * t94 + 0.3186D4
     # * t96 + 0.1956D4 * t99 - 0.1230D4 * t101 - 0.1260D4 * t105 - 0.78
     #60D4 * t108 + 0.3582D4 * t110 + 0.5532D4 * t113 - 0.2712D4 * t115 
     #+ 0.672D3 * t117 - 0.9720D4 * t119
      t123 = t112 * t31
      t125 = t112 * z
      t127 = t88 * t39
      t129 = t98 * t39
      t131 = t36 * t31
      t132 = t91 * t131
      t134 = t91 * t36
      t136 = t91 * t31
      t138 = t91 * z
      t140 = t104 * t131
      t142 = t104 * t59
      t144 = t104 * t36
      t146 = t104 * t39
      t148 = t104 * z
      t150 = t107 * t131
      t152 = t112 * t131
      t154 = t88 * t131
      t156 = t107 * t59
      t158 = t112 * t59
      t160 = 0.9108D4 * t123 - 0.4362D4 * t125 + 0.5586D4 * t127 - 0.156
     #6D4 * t129 + 0.168D3 * t132 + 0.3360D4 * t134 + 0.4200D4 * t136 - 
     #0.1848D4 * t138 - 0.84D2 * t140 + 0.504D3 * t142 - 0.1260D4 * t144
     # + 0.1680D4 * t146 + 0.504D3 * t148 - 0.252D3 * t150 + 0.168D3 * t
     #152 - 0.84D2 * t154 + 0.1902D4 * t156 - 0.1566D4 * t158
      t161 = t88 * t59
      t163 = t98 * t59
      t165 = t107 * t36
      t167 = t107 * t39
      t169 = polylog(2, -t85)
      t170 = t169 * t61
      t171 = t170 * t31
      t173 = t170 * z
      t175 = t169 * t34
      t176 = t175 * t39
      t178 = t169 * x1
      t179 = t178 * t39
      t181 = t175 * t31
      t183 = t175 * z
      t185 = t178 * t31
      t187 = t178 * z
      t189 = t170 * t36
      t191 = t175 * t36
      t193 = t178 * t36
      t195 = t170 * t39
      t197 = t175 * t59
      t199 = t178 * t59
      t201 = t169 * t48
      t202 = t201 * t36
      t204 = 0.672D3 * t161 - 0.168D3 * t163 - 0.5760D4 * t165 + 0.9060D
     #4 * t167 + 0.1836D4 * t171 - 0.900D3 * t173 + 0.1116D4 * t176 - 0.
     #324D3 * t179 - 0.1188D4 * t181 + 0.648D3 * t183 + 0.396D3 * t185 -
     # 0.252D3 * t187 + 0.1116D4 * t189 - 0.558D3 * t191 + 0.144D3 * t19
     #3 - 0.1944D4 * t195 + 0.144D3 * t197 - 0.36D2 * t199 - 0.1188D4 * 
     #t202
      t207 = t201 * t39
      t209 = t201 * t31
      t211 = t201 * z
      t213 = t169 * t49
      t214 = t213 * t131
      t216 = t213 * t59
      t218 = t213 * t36
      t220 = t213 * t39
      t222 = t213 * t31
      t224 = t213 * z
      t226 = t169 * t103
      t227 = t226 * t131
      t229 = t226 * t59
      t231 = t226 * t36
      t233 = t226 * t39
      t235 = t226 * t31
      t237 = t226 * z
      t239 = t201 * t131
      t241 = t170 * t131
      t243 = t175 * t131
      t245 = 0.1872D4 * t207 - 0.1638D4 * t209 + 0.756D3 * t211 + 0.36D2
     # * t214 - 0.252D3 * t216 + 0.720D3 * t218 - 0.1080D4 * t220 + 0.90
     #0D3 * t222 - 0.396D3 * t224 - 0.18D2 * t227 + 0.108D3 * t229 - 0.2
     #70D3 * t231 + 0.360D3 * t233 - 0.270D3 * t235 + 0.108D3 * t237 - 0
     #.54D2 * t239 + 0.36D2 * t241 - 0.18D2 * t243
      t246 = t201 * t59
      t248 = t170 * t59
      t252 = t87 * t39
      t254 = t169 * t31
      t256 = t169 * t39
      t261 = t87 * t36
      t263 = t87 * z
      t265 = t87 * t31
      t270 = 0.3D1 * t49
      t272 = 0.396D3 * t246 - 0.324D3 * t248 + 0.336D3 * t98 - 0.672D3 *
     # t88 + 0.168D3 * t252 - 0.54D2 * t254 + 0.36D2 * t256 - 0.672D3 * 
     #t107 + 0.336D3 * t91 - 0.84D2 * t104 - 0.84D2 * t261 + 0.168D3 * t
     #263 - 0.252D3 * t265 + 0.840D3 * t112 + 0.18D2 * t61 - 0.12D2 * t4
     #8 + t270 - 0.12D2 * t34
      t277 = t48 * t59
      t280 = 0.15D2 * t49 * z
      t282 = 0.30D2 * t49 * t31
      t284 = 0.30D2 * t49 * t39
      t286 = 0.15D2 * t49 * t36
      t288 = 0.3D1 * t49 * t59
      t291 = t48 * z
      t293 = t48 * t31
      t295 = t48 * t39
      t297 = t48 * t36
      t299 = t61 * t59
      t301 = -0.18D2 * t169 - 0.84D2 * t87 + 0.4D1 * t65 + 0.36D2 * t277
     # - t280 + t282 - t284 + t286 - t288 - t27 + 0.4D1 * t35 - t50 - 0.
     #6D1 * t62 + 0.84D2 * t291 - 0.216D3 * t293 + 0.264D3 * t295 - 0.15
     #6D3 * t297 - 0.36D2 * t299
      t302 = t34 * t36
      t304 = t61 * z
      t306 = t61 * t31
      t308 = t61 * t39
      t310 = t61 * t36
      t312 = x1 * t31
      t314 = x1 * t39
      t316 = t34 * z
      t317 = 0.168D3 * t316
      t318 = t34 * t31
      t320 = t34 * t39
      t322 = x1 * z
      t328 = t169 * t36
      t330 = t169 * z
      t334 = -0.108D3 * t302 - 0.198D3 * t304 + 0.522D3 * t306 - 0.558D3
     # * t308 + 0.252D3 * t310 + 0.72D2 * t312 - 0.36D2 * t314 + t317 - 
     #0.408D3 * t318 + 0.360D3 * t320 - 0.39D2 * t322 + 0.180D3 * t170 -
     # 0.144D3 * t201 + 0.72D2 * t213 - 0.18D2 * t226 - 0.18D2 * t328 + 
     #0.36D2 * t330 + 0.72D2 * t178 - 0.144D3 * t175
      t341 = t16 ** 2
      t347 = 0.3141592653589793D1 ** 2
      t351 = lh ** 2
      t355 = t20 ** 2
      t384 = 0.1188D4 * t89 + 0.1080D4 * t92 + 0.252D3 * t94 - 0.648D3 *
     # t96 - 0.396D3 * t99 + 0.252D3 * t101 + 0.270D3 * t105 + 0.1638D4 
     #* t108 - 0.756D3 * t110 - 0.1116D4 * t113 + 0.558D3 * t115 - 0.144
     #D3 * t117 + 0.1944D4 * t119 - 0.1836D4 * t123 + 0.900D3 * t125
      t401 = -0.1116D4 * t127 + 0.324D3 * t129 - 0.36D2 * t132 - 0.720D3
     # * t134 - 0.900D3 * t136 + 0.396D3 * t138 + 0.18D2 * t140 - 0.108D
     #3 * t142 + 0.270D3 * t144 - 0.360D3 * t146 - 0.108D3 * t148 + 0.54
     #D2 * t150 - 0.36D2 * t152 + 0.18D2 * t154 - 0.396D3 * t156 + 0.324
     #D3 * t158
      t419 = -0.144D3 * t161 + 0.36D2 * t163 + 0.1188D4 * t165 - 0.1872D
     #4 * t167 - 0.72D2 * t98 + 0.144D3 * t88 - 0.36D2 * t252 + 0.144D3 
     #* t107 - 0.72D2 * t91 + 0.18D2 * t104 + 0.18D2 * t261 - 0.36D2 * t
     #263 + 0.54D2 * t265 - 0.180D3 * t112 + 0.18D2 * t87 - 0.18D2 * t27
     #7
      t436 = -0.18D2 * t291 + 0.72D2 * t293 - 0.108D3 * t295 + 0.72D2 * 
     #t297 + 0.18D2 * t299 + 0.36D2 * t302 + 0.54D2 * t304 - 0.180D3 * t
     #306 + 0.216D3 * t308 - 0.108D3 * t310 - 0.36D2 * t312 + 0.18D2 * t
     #314 - 0.54D2 * t316 + 0.144D3 * t318 - 0.126D3 * t320 + 0.18D2 * t
     #322
      t443 = polylog(3, -t85)
      t444 = t443 * t34
      t447 = -0.2D1 * t28 - t30 + t33 - t38 + t41 - 0.18D2 * t42 + 0.10D
     #2 * t44 - t47 - 0.144D3 * t444 - 0.18D2 * t63 + t67
      t456 = t443 * t49
      t461 = -t69 - 0.10D2 * t70 + 0.20D2 * t72 - 0.20D2 * t75 + 0.10D2 
     #* t77 + t80 - 0.28D2 * t81 + 0.32D2 * t83 + 0.10692D5 * t89 + 0.82
     #80D4 * t92 + 0.720D3 * t456 * t36 - 0.252D3 * t456 * t59
      t468 = t443 * t61
      t471 = t443 * t48
      t474 = t443 * t103
      t489 = 0.36D2 * t456 * t131 + 0.1932D4 * t94 - 0.18D2 * t444 * t13
     #1 + 0.36D2 * t468 * t131 - 0.54D2 * t471 * t131 + 0.108D3 * t474 *
     # z - 0.270D3 * t474 * t31 + 0.360D3 * t474 * t39 - 0.270D3 * t474 
     #* t36 + 0.108D3 * t474 * t59 - 0.18D2 * t474 * t131 - 0.396D3 * t4
     #56 * z
      t498 = t443 * x1
      t513 = 0.900D3 * t456 * t31 - 0.1080D4 * t456 * t39 + 0.1872D4 * t
     #471 * t39 - 0.1188D4 * t471 * t36 - 0.36D2 * t498 * t59 + 0.144D3 
     #* t444 * t59 - 0.324D3 * t468 * t59 + 0.396D3 * t471 * t59 + 0.72D
     #2 * t456 - 0.18D2 * t474 - 0.18D2 * t443 * t36 + 0.36D2 * t443 * z
      t535 = 0.72D2 * t498 - 0.54D2 * t443 * t31 + 0.36D2 * t443 * t39 +
     # 0.180D3 * t468 - 0.144D3 * t471 - 0.72D2 * t34 * t59 + 0.36D2 * x
     #1 * t59 - 0.1944D4 * t468 * t39 + 0.144D3 * t498 * t36 - 0.558D3 *
     # t444 * t36 + 0.1116D4 * t468 * t36
      t550 = 0.756D3 * t471 * z - 0.1638D4 * t471 * t31 - 0.5562D4 * t96
     # - 0.3432D4 * t99 + 0.2130D4 * t101 + 0.2070D4 * t105 + 0.13350D5 
     #* t108 - 0.5994D4 * t110 - 0.9744D4 * t113 + 0.4674D4 * t115 - 0.1
     #104D4 * t117 + 0.17280D5 * t119
      t572 = -0.16056D5 * t123 + 0.7494D4 * t125 - 0.9942D4 * t127 + 0.2
     #682D4 * t129 + 0.648D3 * t444 * z - 0.1188D4 * t444 * t31 - 0.324D
     #3 * t498 * t39 + 0.1116D4 * t444 * t39 - 0.900D3 * t468 * z + 0.18
     #36D4 * t468 * t31 - 0.252D3 * t498 * z + 0.396D3 * t498 * t31
      t586 = -0.108D3 * x1 * t36 - 0.276D3 * t132 - 0.5520D4 * t134 - 0.
     #6900D4 * t136 + 0.3036D4 * t138 + 0.138D3 * t140 - 0.828D3 * t142 
     #+ 0.2070D4 * t144 - 0.2760D4 * t146 - 0.828D3 * t148 + 0.414D3 * t
     #150 - 0.276D3 * t152
      t601 = 0.138D3 * t154 - 0.3234D4 * t156 + 0.2682D4 * t158 - 0.1104
     #D4 * t161 + 0.276D3 * t163 + 0.9900D4 * t165 - 0.15540D5 * t167 - 
     #0.9108D4 * t171 + 0.4362D4 * t173 - 0.5586D4 * t176 + 0.1566D4 * t
     #179
      t614 = 0.5976D4 * t181 - 0.3186D4 * t183 - 0.1956D4 * t185 + 0.123
     #0D4 * t187 - 0.5532D4 * t189 + 0.2712D4 * t191 - 0.672D3 * t193 + 
     #0.9720D4 * t195 - 0.672D3 * t197 + 0.168D3 * t199 + 0.5760D4 * t20
     #2 - 0.9060D4 * t207
      t628 = 0.7860D4 * t209 - 0.3582D4 * t211 - 0.168D3 * t214 + 0.1176
     #D4 * t216 - 0.3360D4 * t218 + 0.5040D4 * t220 - 0.4200D4 * t222 + 
     #0.1848D4 * t224 + 0.84D2 * t227 - 0.504D3 * t229 + 0.1260D4 * t231
     # - 0.1680D4 * t233
      t641 = 0.1260D4 * t235 - 0.504D3 * t237 + 0.252D3 * t239 - 0.168D3
     # * t241 + 0.84D2 * t243 - 0.1902D4 * t246 + 0.1566D4 * t248 - 0.55
     #2D3 * t98 + 0.1104D4 * t88 - 0.276D3 * t252 + 0.252D3 * t254 - 0.1
     #68D3 * t256
      t654 = 0.1104D4 * t107 - 0.552D3 * t91 + 0.138D3 * t104 + 0.138D3 
     #* t261 - 0.276D3 * t263 - 0.18D2 * t443 + 0.414D3 * t265 - 0.1380D
     #4 * t112 - 0.30D2 * t61 + 0.18D2 * t48 - t270
      t662 = 0.18D2 * t34 + 0.84D2 * t169 + 0.138D3 * t87 - 0.2D1 * t65 
     #- 0.24D2 * t277 + t280 - t282 + t284 - t286 + t288 - 0.2D1 * t35 +
     # 0.4D1 * t62
      t676 = -0.96D2 * t291 + 0.204D3 * t293 - 0.216D3 * t295 + 0.114D3 
     #* t297 + 0.60D2 * t299 + 0.324D3 * t302 + 0.222D3 * t304 - 0.546D3
     # * t306 + 0.606D3 * t308 - 0.312D3 * t310 - 0.84D2 * t312 + 0.132D
     #3 * t314
      t688 = -t317 + 0.462D3 * t318 - 0.564D3 * t320 + 0.27D2 * t322 - 0
     #.840D3 * t170 + 0.672D3 * t201 - 0.336D3 * t213 + 0.84D2 * t226 + 
     #0.84D2 * t328 - 0.168D3 * t330 - 0.336D3 * t178 + 0.672D3 * t175
      t695 = (0.440D3 * t7 - 0.120D3 * t17 + t24 / 0.3D1) * (t74 + t121 
     #+ t160 + t204 + t245 + t272 + t301 + t334) + (0.960D3 * t7 - 0.360
     #D3 * t17 + t24 + 0.60D2 * t7 * t341 - t23 * wd * t16 / 0.3D1 + t6 
     #* (-0.60D2 * t347 + 0.720D3 * t20 + 0.1440D4 - 0.1440D4 * lh + 0.7
     #20D3 * t351 - 0.720D3 * t20 * lh + 0.180D3 * t355) * wd / 0.3D1 + 
     #0.80D2 / 0.3D1 * wd * (-0.3D1 * t16 + 0.8D1 - 0.6D1 * lh + 0.3D1 *
     # t20) * t6) * (t384 + t401 + t419 + t436) + 0.120D3 * t7 * (t447 +
     # t461 + t489 + t513 + t535 + t550 + t572 + t586 + t601 + t614 + t6
     #28 + t641 + t654 + t662 + t676 + t688)
      t697 = (0.1D1 - x1 + t322) ** 2
      t698 = 0.1D1 / t697
      t700 = 0.1D1 / x1
      t704 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, t
     #695 * t698 * t700 * t12 / 0.480D3)
      rvgg2ght1s5e1 = t704 * t695 * t698 * t700 * t12 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s5e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x1
      t6 = 0.1D1 / t1
      t7 = t6 * wd
      t8 = t1 * x1
      t10 = log(0.1D1 + t8)
      t11 = z ** 2
      t12 = t10 * t11
      t14 = x1 ** 2
      t15 = t14 * x1
      t16 = t10 * t15
      t18 = t14 ** 2
      t19 = t10 * t18
      t21 = t18 * x1
      t22 = t10 * t21
      t24 = t18 * t14
      t25 = t10 * t24
      t27 = t11 ** 2
      t28 = t10 * t27
      t30 = t10 * z
      t32 = t10 * x1
      t34 = t10 * t14
      t36 = t11 * z
      t37 = t10 * t36
      t39 = polylog(2, -t8)
      t42 = t39 * t15
      t44 = t39 * t18
      t46 = t39 * t21
      t48 = t39 * t24
      t54 = x1 * t39
      t56 = -0.252D3 * t12 + 0.840D3 * t16 - 0.672D3 * t19 + 0.336D3 * t
     #22 - 0.84D2 * t25 - 0.84D2 * t28 + 0.168D3 * t30 + 0.336D3 * t32 -
     # 0.672D3 * t34 + 0.168D3 * t37 - 0.54D2 * t39 * t11 + 0.180D3 * t4
     #2 - 0.144D3 * t44 + 0.72D2 * t46 - 0.18D2 * t48 - 0.18D2 * t27 * t
     #39 + 0.36D2 * t39 * z + 0.72D2 * t54
      t57 = t39 * t14
      t59 = t34 * t11
      t61 = t22 * t36
      t63 = t27 * z
      t64 = t22 * t63
      t66 = t25 * t11
      t68 = t32 * z
      t70 = t32 * t11
      t72 = t34 * z
      t74 = t32 * t36
      t76 = t34 * t36
      t78 = t16 * z
      t80 = t16 * t11
      t82 = t16 * t36
      t84 = t32 * t27
      t86 = t34 * t27
      t88 = t16 * t27
      t90 = t19 * z
      t92 = t19 * t11
      t94 = -0.144D3 * t57 - 0.5976D4 * t59 - 0.5040D4 * t61 - 0.1176D4 
     #* t64 - 0.1260D4 * t66 - 0.1230D4 * t68 + 0.1956D4 * t70 + 0.3186D
     #4 * t72 - 0.1566D4 * t74 + 0.5586D4 * t76 - 0.4362D4 * t78 + 0.910
     #8D4 * t80 - 0.9720D4 * t82 + 0.672D3 * t84 - 0.2712D4 * t86 + 0.55
     #32D4 * t88 + 0.3582D4 * t90 - 0.7860D4 * t92
      t96 = t19 * t36
      t98 = t19 * t27
      t100 = t32 * t63
      t102 = t34 * t63
      t104 = t16 * t63
      t106 = t19 * t63
      t108 = t27 * t11
      t109 = t34 * t108
      t111 = t16 * t108
      t113 = t19 * t108
      t115 = t25 * z
      t117 = t25 * t36
      t119 = t25 * t27
      t127 = 0.9060D4 * t96 - 0.5760D4 * t98 - 0.168D3 * t100 + 0.672D3 
     #* t102 - 0.1566D4 * t104 + 0.1902D4 * t106 - 0.84D2 * t109 + 0.168
     #D3 * t111 - 0.252D3 * t113 + 0.504D3 * t115 + 0.1680D4 * t117 - 0.
     #1260D4 * t119 + 0.3D1 * x1 - 0.84D2 * t10 - 0.18D2 * t39 - 0.12D2 
     #* t14 + 0.18D2 * t15 - 0.12D2 * t18
      t129 = x1 * z
      t131 = x1 * t11
      t133 = x1 * t36
      t135 = t14 * z
      t137 = t14 * t11
      t139 = t14 * t36
      t141 = t14 * t27
      t143 = t15 * z
      t145 = t15 * t11
      t147 = t15 * t36
      t149 = t15 * t27
      t151 = t18 * z
      t153 = t18 * t11
      t155 = t18 * t36
      t157 = t18 * t27
      t159 = t15 * t63
      t161 = t18 * t63
      t165 = 0.3D1 * t21 - 0.39D2 * t129 + 0.72D2 * t131 - 0.36D2 * t133
     # + 0.168D3 * t135 - 0.408D3 * t137 + 0.360D3 * t139 - 0.108D3 * t1
     #41 - 0.198D3 * t143 + 0.522D3 * t145 - 0.558D3 * t147 + 0.252D3 * 
     #t149 + 0.84D2 * t151 - 0.216D3 * t153 + 0.264D3 * t155 - 0.156D3 *
     # t157 - 0.36D2 * t159 + 0.36D2 * t161 - 0.15D2 * t21 * z
      t176 = nf * x1
      t177 = nf * t14
      t179 = nf * t21
      t180 = nf * t15
      t182 = nf * t18
      t184 = t25 * t63
      t186 = t25 * t108
      t188 = t22 * z
      t190 = t22 * t11
      t192 = t22 * t27
      t194 = t22 * t108
      t202 = 0.30D2 * t21 * t11 - 0.30D2 * t21 * t36 + 0.15D2 * t21 * t2
     #7 - 0.3D1 * t21 * t63 - t176 + 0.4D1 * t177 - t179 - 0.6D1 * t180 
     #+ 0.4D1 * t182 + 0.504D3 * t184 - 0.84D2 * t186 - 0.1848D4 * t188 
     #+ 0.4200D4 * t190 + 0.3360D4 * t192 + 0.168D3 * t194 - 0.252D3 * t
     #54 * z + 0.396D3 * t54 * t11 + 0.648D3 * t57 * z
      t239 = -0.1188D4 * t57 * t11 - 0.324D3 * t54 * t36 + 0.1116D4 * t5
     #7 * t36 - 0.900D3 * t42 * z + 0.1836D4 * t42 * t11 - 0.1944D4 * t4
     #2 * t36 + 0.144D3 * t54 * t27 - 0.558D3 * t57 * t27 + 0.1116D4 * t
     #42 * t27 + 0.756D3 * t44 * z - 0.1638D4 * t44 * t11 + 0.1872D4 * t
     #44 * t36 - 0.1188D4 * t44 * t27 - 0.36D2 * t54 * t63 + 0.144D3 * t
     #57 * t63 - 0.324D3 * t42 * t63 + 0.396D3 * t44 * t63 - 0.18D2 * t5
     #7 * t108
      t277 = 0.36D2 * t42 * t108 - 0.54D2 * t44 * t108 + 0.108D3 * t48 *
     # z - 0.270D3 * t48 * t11 + 0.360D3 * t48 * t36 - 0.270D3 * t48 * t
     #27 + 0.108D3 * t48 * t63 - 0.18D2 * t48 * t108 - 0.396D3 * t46 * z
     # + 0.900D3 * t46 * t11 - 0.1080D4 * t46 * t36 + 0.720D3 * t46 * t2
     #7 - 0.252D3 * t46 * t63 + 0.36D2 * t46 * t108 + 0.36D2 * t39 * t36
     # + 0.3D1 * t176 * z - 0.4D1 * t176 * t11 + 0.2D1 * t176 * t36
      t315 = -0.14D2 * t177 * z + 0.20D2 * t177 * t11 - 0.14D2 * t177 * 
     #t36 + 0.4D1 * t177 * t27 + 0.5D1 * t179 * z - 0.10D2 * t179 * t11 
     #+ 0.10D2 * t179 * t36 - 0.5D1 * t179 * t27 + t179 * t63 + 0.24D2 *
     # t180 * z - 0.38D2 * t180 * t11 + 0.30D2 * t180 * t36 - 0.12D2 * t
     #180 * t27 - 0.18D2 * t182 * z + 0.32D2 * t182 * t11 - 0.28D2 * t18
     #2 * t36 + 0.12D2 * t182 * t27 + 0.2D1 * t180 * t63 - 0.2D1 * t182 
     #* t63
      t322 = t1 ** 2
      t325 = 0.1D1 / z
      t326 = t4 ** 2
      t329 = log(-x1 * t322 * t1 * t325 * t326)
      t333 = log(z)
      t355 = 0.54D2 * t12 - 0.180D3 * t16 + 0.144D3 * t19 - 0.72D2 * t22
     # + 0.18D2 * t25 + 0.18D2 * t28 - 0.36D2 * t30 - 0.72D2 * t32 + 0.1
     #44D3 * t34 - 0.36D2 * t37 + 0.1188D4 * t59 + 0.1080D4 * t61 + 0.25
     #2D3 * t64 + 0.270D3 * t66 + 0.252D3 * t68
      t372 = -0.396D3 * t70 - 0.648D3 * t72 + 0.324D3 * t74 - 0.1116D4 *
     # t76 + 0.900D3 * t78 - 0.1836D4 * t80 + 0.1944D4 * t82 - 0.144D3 *
     # t84 + 0.558D3 * t86 - 0.1116D4 * t88 - 0.756D3 * t90 + 0.1638D4 *
     # t92 - 0.1872D4 * t96 + 0.1188D4 * t98 + 0.36D2 * t100 - 0.144D3 *
     # t102
      t390 = 0.324D3 * t104 - 0.396D3 * t106 + 0.18D2 * t109 - 0.36D2 * 
     #t111 + 0.54D2 * t113 - 0.108D3 * t115 - 0.360D3 * t117 + 0.270D3 *
     # t119 + 0.18D2 * t10 + 0.18D2 * t129 - 0.36D2 * t131 + 0.18D2 * t1
     #33 - 0.54D2 * t135 + 0.144D3 * t137 - 0.126D3 * t139 + 0.36D2 * t1
     #41
      t407 = 0.54D2 * t143 - 0.180D3 * t145 + 0.216D3 * t147 - 0.108D3 *
     # t149 - 0.18D2 * t151 + 0.72D2 * t153 - 0.108D3 * t155 + 0.72D2 * 
     #t157 + 0.18D2 * t159 - 0.18D2 * t161 - 0.108D3 * t184 + 0.18D2 * t
     #186 + 0.396D3 * t188 - 0.900D3 * t190 - 0.720D3 * t192 - 0.36D2 * 
     #t194
      t411 = 0.120D3 * t7 * (t56 + t94 + t127 + t165 + t202 + t239 + t27
     #7 + t315) + (0.440D3 * t7 - 0.120D3 * t7 * t329 + t6 * (0.720D3 - 
     #0.720D3 * lh + 0.360D3 * t333) * wd / 0.3D1) * (t355 + t372 + t390
     # + t407)
      t413 = (0.1D1 - x1 + t129) ** 2
      t414 = 0.1D1 / t413
      t416 = 0.1D1 / x1
      t420 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, t
     #411 * t414 * t416 * t325 / 0.480D3)
      rvgg2ght1s5e0 = t420 * t411 * t414 * t416 * t325 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s5em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = 0.1D1 / t1
      t10 = log(0.1D1 + t1 * x1)
      t11 = x1 ** 2
      t12 = t10 * t11
      t13 = z ** 2
      t16 = t11 ** 2
      t18 = t10 * t16 * x1
      t19 = t13 * z
      t22 = t13 ** 2
      t23 = t22 * z
      t27 = t10 * t16 * t11
      t30 = t10 * x1
      t41 = x1 * t11
      t42 = t10 * t41
      t55 = 0.1188D4 * t12 * t13 + 0.1080D4 * t18 * t19 + 0.252D3 * t18 
     #* t23 + 0.270D3 * t27 * t13 + 0.252D3 * t30 * z - 0.396D3 * t30 * 
     #t13 - 0.648D3 * t12 * z + 0.324D3 * t30 * t19 - 0.1116D4 * t12 * t
     #19 + 0.900D3 * t42 * z - 0.1836D4 * t42 * t13 + 0.1944D4 * t42 * t
     #19 - 0.144D3 * t30 * t22 + 0.558D3 * t12 * t22 - 0.1116D4 * t42 * 
     #t22
      t56 = t10 * t16
      t73 = t22 * t13
      t90 = -0.756D3 * t56 * z + 0.1638D4 * t56 * t13 - 0.1872D4 * t56 *
     # t19 + 0.1188D4 * t56 * t22 + 0.36D2 * t30 * t23 - 0.144D3 * t12 *
     # t23 + 0.324D3 * t42 * t23 - 0.396D3 * t56 * t23 + 0.18D2 * t12 * 
     #t73 - 0.36D2 * t42 * t73 + 0.54D2 * t56 * t73 - 0.108D3 * t27 * z 
     #- 0.360D3 * t19 * t27 + 0.270D3 * t22 * t27 - 0.108D3 * t27 * t23 
     #+ 0.18D2 * t27 * t73
      t100 = x1 * z
      t124 = 0.396D3 * t18 * z - 0.900D3 * t18 * t13 - 0.720D3 * t18 * t
     #22 - 0.36D2 * t18 * t73 + 0.18D2 * t100 - 0.36D2 * x1 * t13 + 0.18
     #D2 * x1 * t19 - 0.54D2 * t11 * z + 0.144D3 * t11 * t13 - 0.126D3 *
     # t19 * t11 + 0.36D2 * t22 * t11 + 0.54D2 * t41 * z - 0.180D3 * t41
     # * t13 + 0.216D3 * t41 * t19 - 0.108D3 * t41 * t22 - 0.18D2 * t16 
     #* z
      t150 = 0.72D2 * t16 * t13 - 0.108D3 * t16 * t19 + 0.72D2 * t16 * t
     #22 + 0.18D2 * t41 * t23 - 0.18D2 * t16 * t23 + 0.54D2 * t10 * t13 
     #- 0.180D3 * t42 + 0.144D3 * t56 - 0.72D2 * t18 + 0.18D2 * t27 + 0.
     #18D2 * t10 * t22 - 0.36D2 * t10 * z - 0.72D2 * t30 + 0.144D3 * t12
     # - 0.36D2 * t10 * t19 + 0.18D2 * t10
      t152 = t55 + t90 + t124 + t150
      t155 = (0.1D1 - x1 + t100) ** 2
      t156 = 0.1D1 / t155
      t157 = 0.1D1 / x1
      t159 = 0.1D1 / z
      t163 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D
     #0, 0.0D0, t6 * wd * t152 * t156 * t157 * t159 / 0.4D1)
      rvgg2ght1s5em1 = t163 * t6 * wd * t152 * t156 * t157 * t159 / 0.4D
     #1

      end function



      doubleprecision function rvgg2ght1s5em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s5em2 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s5em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s5em3 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s5em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s5em4 = 0.0D0

      end function


      doubleprecision function rvgg2ght1s6e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = log(z)
      t7 = 0.720D3 - 0.720D3 * lh + 0.360D3 * t5
      t8 = t3 * t7
      t10 = 0.1D1 / z
      t11 = t1 ** 2
      t12 = t11 * t1
      t14 = log(-t10 * t12)
      t15 = t14 * t3
      t18 = t14 ** 2
      t19 = t18 * t3
      t22 = t5 ** 2
      t25 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t42 = t5 * lh
      t44 = 0.60D2 * t22 * t5 + 0.1726025372966790D4 + 0.720D3 * t5 * t2
     #5 - 0.120D3 * t28 - 0.2880D4 * lh - 0.360D3 * t22 * lh - 0.480D3 *
     # t25 * lh + 0.1440D4 * t5 + 0.360D3 * t22 + 0.120D3 * t28 * lh + 0
     #.1440D4 * t25 - 0.60D2 * t28 * t5 - 0.1440D4 * t42
      t56 = -0.60D2 * t28 + 0.720D3 * t5 + 0.1440D4 - 0.1440D4 * lh + 0.
     #720D3 * t25 - 0.720D3 * t42 + 0.180D3 * t22
      t71 = 0.48D2 * t14 - 0.36D2 * t14 * lh + 0.18D2 * t14 * t5 - 0.9D1
     # * t18 - 0.48D2 * t5 - 0.36D2 * t25 + 0.3D1 * t28 + 0.96D2 * lh - 
     #0.104D3 - 0.9D1 * t22 + 0.36D2 * t42
      t74 = t15 * t7
      t76 = t3 * t56
      t77 = 0.6D1 * lh
      t78 = 0.3D1 * t5
      t81 = (0.8D1 - t77 + t78 - 0.3D1 * t14) * t3
      t83 = 0.2D1 * t8 - 0.720D3 * t15 + 0.1680D4 * t3 + t19 * t7 / 0.6D
     #1 + t3 * t44 / 0.3D1 - 0.20D2 * t18 * t14 * t3 - t15 * t56 / 0.3D1
     # - 0.40D2 / 0.9D1 * t71 * t3 - t74 + 0.180D3 * t19 + t76 + 0.80D2 
     #* t81
      t85 = z ** 2
      t86 = t85 ** 2
      t87 = t5 * t86
      t89 = t85 * z
      t90 = t5 * t89
      t92 = t5 * z
      t94 = t5 * t85
      t96 = 0.18D2 * t5 + 0.18D2 * t87 - 0.36D2 * t90 - 0.36D2 * t92 + 0
     #.54D2 * t94
      t98 = 0.12D2 * z
      t100 = 0.6D1 * t85
      t101 = -t1
      t102 = polylog(2, t101)
      t104 = polylog(3, t101)
      t106 = polylog(4, t101)
      t112 = -0.6D1 + t98 - 0.96D2 * t5 - t100 - 0.138D3 * t102 + 0.84D2
     # * t104 - 0.18D2 * t106 + 0.192D3 * t92 - 0.288D3 * t94 - 0.96D2 *
     # t87 + 0.192D3 * t90
      t113 = t102 * t89
      t115 = t104 * t89
      t119 = t102 * t86
      t121 = t104 * t86
      t125 = t102 * t85
      t127 = t102 * z
      t129 = t104 * z
      t133 = t104 * t85
      t137 = 0.276D3 * t113 - 0.168D3 * t115 + 0.36D2 * t106 * t89 - 0.1
     #38D3 * t119 + 0.84D2 * t121 - 0.18D2 * t106 * t86 - 0.414D3 * t125
     # + 0.276D3 * t127 - 0.168D3 * t129 + 0.36D2 * t106 * z + 0.252D3 *
     # t133 - 0.54D2 * t106 * t85
      t141 = 0.440D3 * t3
      t142 = t8 / 0.3D1
      t145 = 0.2D1 * nf
      t146 = 0.138D3 * t5
      t147 = 0.3D1 * t85
      t148 = 0.3D1 * t89
      t149 = nf * t85
      t150 = 0.2D1 * t149
      t151 = 0.84D2 * t102
      t152 = 0.18D2 * t104
      t153 = 0.276D3 * t92
      t154 = 0.414D3 * t94
      t155 = 0.6D1 - t98 - t145 + t146 + t147 + t148 - t150 + t151 - t15
     #2 - t153 + t154
      t156 = 0.138D3 * t87
      t157 = 0.276D3 * t90
      t158 = 0.168D3 * t113
      t159 = 0.36D2 * t115
      t160 = 0.84D2 * t119
      t161 = 0.18D2 * t121
      t162 = 0.252D3 * t125
      t163 = 0.168D3 * t127
      t164 = 0.36D2 * t129
      t165 = 0.54D2 * t133
      t167 = 0.4D1 * nf * z
      t168 = t156 - t157 - t158 + t159 + t160 - t161 + t162 - t163 + t16
     #4 - t165 + t167
      t171 = 0.960D3 * t3
      t175 = t76 / 0.3D1
      t178 = 0.168D3 * t90
      t179 = 0.18D2 * t102
      t180 = 0.18D2 * t119
      t181 = 0.84D2 * t5
      t182 = 0.36D2 * t127
      t183 = nf * t89
      t184 = 0.252D3 * t94
      t185 = 0.84D2 * t87
      t186 = 0.168D3 * t92
      t187 = 0.36D2 * t113
      t188 = 0.54D2 * t125
      t189 = t178 - t149 - t179 + t147 - t148 - t180 - t181 + t182 + t18
     #3 - t184 - t185 + t186 + t187 - t188
      t196 = (t141 + t142) * wd
      t197 = x1 ** 2
      t198 = t197 * t10
      t200 = log(-t198 * t12)
      t202 = t178 - t149 - t179 + t147 - t148 - t180 - t181 + t182 + t18
     #3 - t184 - t185 + t186 + t187 - t188 - t200 * t96
      t208 = (t171 + t8 + t175 + 0.80D2 / 0.3D1 * (0.8D1 - t77 + t78) * 
     #t3) * wd
      t210 = t3 * wd
      t212 = 0.6D1 - t200 * t189 - t98 - t145 + t146 + t147 + t148 - t15
     #0 + t151 - t152 - t153 + t154
      t213 = t200 ** 2
      t216 = t156 - t157 - t158 + t159 + t160 - t161 + t162 - t163 + t16
     #4 - t165 + t167 + t213 * t96 / 0.2D1
      t221 = 0.1D1 / x1
      t225 = -(t83 * t96 + 0.120D3 * t3 * (t112 + t137) + (t141 + t142 -
     # 0.120D3 * t15) * (t155 + t168) + (t171 + t8 - 0.360D3 * t15 - t74
     # / 0.3D1 + 0.60D2 * t19 + t175 + 0.80D2 / 0.3D1 * t81) * t189) * w
     #d * t10 / 0.960D3 - (t196 * t202 + t208 * t96 + 0.120D3 * t210 * (
     #t212 + t216)) * t221 * t10 / 0.480D3
      t226 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t225)
      t229 = -0.1D1 + x1
      t231 = t197 ** 2
      t232 = 0.3D1 * t231
      t233 = t231 * x1
      t234 = 0.3D1 * t233
      t235 = nf * x1
      t236 = t235 * t89
      t238 = nf * t197
      t239 = t238 * t85
      t241 = t238 * t89
      t243 = x1 * z
      t245 = x1 * t85
      t247 = x1 * t89
      t249 = x1 * t86
      t252 = 0.72D2 * t197 * z
      t253 = t197 * t85
      t255 = t197 * t89
      t257 = t197 * t86
      t259 = t197 * x1
      t260 = t259 * z
      t261 = 0.12D2 * t260
      t262 = t259 * t85
      t264 = t259 * t89
      t266 = t259 * t86
      t268 = t231 * z
      t270 = t232 - t234 - 0.2D1 * t236 - 0.2D1 * t239 + 0.8D1 * t241 + 
     #0.36D2 * t243 - 0.108D3 * t245 + 0.84D2 * t247 + 0.9D1 * t249 - t2
     #52 + 0.162D3 * t253 - 0.30D2 * t255 - 0.138D3 * t257 + t261 + 0.42
     #D2 * t262 - 0.198D3 * t264 + 0.222D3 * t266 + 0.9D1 * t268
      t271 = t231 * t85
      t272 = 0.66D2 * t271
      t273 = t231 * t89
      t274 = 0.114D3 * t273
      t275 = t231 * t86
      t276 = 0.81D2 * t275
      t277 = t86 * z
      t278 = x1 * t277
      t279 = 0.21D2 * t278
      t280 = t197 * t277
      t281 = 0.78D2 * t280
      t282 = t259 * t277
      t283 = 0.78D2 * t282
      t284 = t231 * t277
      t285 = 0.21D2 * t284
      t287 = 0.15D2 * t233 * z
      t289 = 0.30D2 * t233 * t85
      t291 = 0.30D2 * t233 * t89
      t293 = 0.15D2 * t233 * t86
      t295 = 0.3D1 * t233 * t277
      t296 = nf * t231
      t297 = nf * t86
      t298 = nf * t277
      t299 = t229 * t1
      t301 = log(0.1D1 - t299)
      t302 = t301 * t231
      t303 = 0.252D3 * t302
      t304 = t301 * t197
      t305 = 0.84D2 * t304
      t306 = t301 * t259
      t307 = 0.168D3 * t306
      t308 = t231 * t197
      t309 = t301 * t308
      t310 = 0.84D2 * t309
      t311 = -t272 + t274 - t276 - t279 + t281 - t283 + t285 + t287 - t2
     #89 + t291 - t293 + t295 - t296 - t297 + t298 - t303 - t305 + t307 
     #- t310
      t313 = t301 * t233
      t314 = 0.168D3 * t313
      t315 = t301 * t86
      t316 = 0.252D3 * t315
      t317 = t86 * t85
      t318 = t301 * t317
      t319 = 0.84D2 * t318
      t320 = t301 * t277
      t321 = 0.168D3 * t320
      t322 = t301 * t85
      t323 = 0.84D2 * t322
      t324 = t301 * t89
      t325 = 0.168D3 * t324
      t326 = polylog(2, t299)
      t327 = t326 * t259
      t328 = 0.36D2 * t327
      t329 = t326 * t197
      t330 = 0.18D2 * t329
      t331 = t326 * t231
      t332 = 0.54D2 * t331
      t333 = t326 * t308
      t334 = 0.18D2 * t333
      t335 = t326 * t233
      t336 = 0.36D2 * t335
      t337 = t326 * t86
      t338 = 0.54D2 * t337
      t339 = t326 * t317
      t340 = 0.18D2 * t339
      t341 = t326 * t277
      t342 = 0.36D2 * t341
      t343 = t326 * t85
      t344 = 0.18D2 * t343
      t345 = t326 * t89
      t346 = 0.36D2 * t345
      t349 = log(t198 * t12 * t229)
      t365 = 0.18D2 * t247 - 0.36D2 * t249 + 0.36D2 * t253 - 0.126D3 * t
     #255 + 0.144D3 * t257 + 0.18D2 * t260 - 0.108D3 * t262 + 0.216D3 * 
     #t264 - 0.180D3 * t266 - 0.18D2 * t268 + 0.72D2 * t271 - 0.108D3 * 
     #t273 + 0.72D2 * t275 + 0.18D2 * t278 - 0.54D2 * t280
      t378 = t301 * x1
      t379 = t378 * t317
      t381 = t378 * t85
      t383 = t378 * t89
      t385 = t378 * t86
      t387 = 0.54D2 * t282 - 0.18D2 * t284 + 0.54D2 * t302 + 0.18D2 * t3
     #04 - 0.36D2 * t306 + 0.18D2 * t309 - 0.36D2 * t313 + 0.54D2 * t315
     # + 0.18D2 * t318 - 0.36D2 * t320 + 0.18D2 * t322 - 0.36D2 * t324 -
     # 0.72D2 * t379 - 0.144D3 * t381 + 0.324D3 * t383 - 0.396D3 * t385
      t389 = t304 * z
      t391 = t304 * t85
      t393 = t304 * t89
      t395 = t304 * t86
      t397 = t306 * z
      t399 = t306 * t85
      t401 = t306 * t89
      t403 = t306 * t86
      t405 = t302 * z
      t407 = t302 * t85
      t409 = t302 * t89
      t411 = t302 * t86
      t413 = t378 * t277
      t415 = t304 * t277
      t417 = t306 * t277
      t419 = t302 * t277
      t421 = -0.144D3 * t389 + 0.558D3 * t391 - 0.1116D4 * t393 + 0.1188
     #D4 * t395 + 0.324D3 * t397 - 0.1116D4 * t399 + 0.1944D4 * t401 - 0
     #.1836D4 * t403 - 0.396D3 * t405 + 0.1188D4 * t407 - 0.1872D4 * t40
     #9 + 0.1638D4 * t411 + 0.252D3 * t413 - 0.648D3 * t415 + 0.900D3 * 
     #t417 - 0.756D3 * t419
      t422 = t304 * t317
      t424 = t306 * t317
      t426 = t302 * t317
      t428 = t313 * z
      t430 = t313 * t85
      t432 = t313 * t89
      t434 = t313 * t86
      t436 = t313 * t277
      t438 = t313 * t317
      t440 = t309 * z
      t442 = t309 * t85
      t444 = t309 * t89
      t446 = t309 * t86
      t448 = t309 * t277
      t450 = t309 * t317
      t452 = t378 * z
      t454 = 0.144D3 * t422 - 0.180D3 * t424 + 0.144D3 * t426 + 0.252D3 
     #* t428 - 0.720D3 * t430 + 0.1080D4 * t432 - 0.900D3 * t434 + 0.396
     #D3 * t436 - 0.72D2 * t438 - 0.108D3 * t440 + 0.270D3 * t442 - 0.36
     #0D3 * t444 + 0.270D3 * t446 - 0.108D3 * t448 + 0.18D2 * t450 + 0.3
     #6D2 * t452
      t456 = t365 + t387 + t421 + t454
      t458 = 0.3D1 * t86
      t459 = t314 - t316 - t319 + t321 - t323 + t325 + t328 - t330 - t33
     #2 - t334 + t336 - t338 - t340 + t342 - t344 + t346 - t349 * t456 +
     # t458
      t460 = t238 * t86
      t462 = nf * t233
      t472 = nf * t259
      t473 = t472 * z
      t475 = t472 * t85
      t477 = t472 * t89
      t479 = t472 * t86
      t481 = t296 * z
      t483 = t296 * t85
      t485 = t296 * t89
      t487 = t296 * t86
      t489 = t472 * t277
      t491 = t296 * t277
      t493 = t235 * t86
      t495 = t235 * t277
      t497 = t238 * t277
      t499 = -0.10D2 * t460 - 0.5D1 * t462 * z + 0.10D2 * t462 * t85 - 0
     #.10D2 * t462 * t89 + 0.5D1 * t462 * t86 - t462 * t277 - 0.2D1 * t4
     #73 + 0.10D2 * t475 - 0.18D2 * t477 + 0.14D2 * t479 + 0.7D1 * t481 
     #- 0.18D2 * t483 + 0.22D2 * t485 - 0.13D2 * t487 - 0.4D1 * t489 + 0
     #.3D1 * t491 + 0.5D1 * t493 - 0.3D1 * t495 + 0.4D1 * t497
      t520 = 0.336D3 * t379 + 0.672D3 * t381 - 0.1566D4 * t383 + 0.1956D
     #4 * t385 + 0.672D3 * t389 - 0.2712D4 * t391 + 0.5586D4 * t393 - 0.
     #5976D4 * t395 - 0.1566D4 * t397 + 0.5532D4 * t399 - 0.9720D4 * t40
     #1 + 0.9108D4 * t403 + 0.1902D4 * t405 - 0.5760D4 * t407 + 0.9060D4
     # * t409 - 0.7860D4 * t411 - 0.1230D4 * t413 + 0.3186D4 * t415
      t538 = t326 * x1
      t539 = t538 * t317
      t541 = t538 * z
      t543 = -0.4362D4 * t417 + 0.3582D4 * t419 - 0.672D3 * t422 + 0.840
     #D3 * t424 - 0.672D3 * t426 - 0.1176D4 * t428 + 0.3360D4 * t430 - 0
     #.5040D4 * t432 + 0.4200D4 * t434 - 0.1848D4 * t436 + 0.336D3 * t43
     #8 + 0.504D3 * t440 - 0.1260D4 * t442 + 0.1680D4 * t444 - 0.1260D4 
     #* t446 + 0.504D3 * t448 - 0.84D2 * t450 + 0.72D2 * t539 - 0.36D2 *
     # t541
      t545 = t538 * t85
      t547 = t538 * t89
      t549 = t538 * t86
      t551 = t329 * z
      t553 = t329 * t85
      t555 = t329 * t89
      t557 = t329 * t86
      t559 = t327 * z
      t561 = t327 * t85
      t563 = t327 * t89
      t565 = t327 * t86
      t567 = t331 * z
      t569 = t331 * t85
      t571 = t331 * t89
      t573 = t331 * t86
      t575 = t538 * t277
      t577 = t329 * t277
      t579 = t327 * t277
      t581 = 0.144D3 * t545 - 0.324D3 * t547 + 0.396D3 * t549 + 0.144D3 
     #* t551 - 0.558D3 * t553 + 0.1116D4 * t555 - 0.1188D4 * t557 - 0.32
     #4D3 * t559 + 0.1116D4 * t561 - 0.1944D4 * t563 + 0.1836D4 * t565 +
     # 0.396D3 * t567 - 0.1188D4 * t569 + 0.1872D4 * t571 - 0.1638D4 * t
     #573 - 0.252D3 * t575 + 0.648D3 * t577 - 0.900D3 * t579
      t582 = t331 * t277
      t584 = t329 * t317
      t586 = t327 * t317
      t588 = t331 * t317
      t590 = t335 * z
      t592 = t335 * t85
      t594 = t335 * t89
      t596 = t335 * t86
      t598 = t335 * t277
      t600 = t335 * t317
      t602 = t333 * z
      t604 = t333 * t85
      t606 = t333 * t89
      t608 = t333 * t86
      t610 = t333 * t277
      t612 = t333 * t317
      t615 = 0.3D1 * t277
      t616 = 0.756D3 * t582 - 0.144D3 * t584 + 0.180D3 * t586 - 0.144D3 
     #* t588 - 0.252D3 * t590 + 0.720D3 * t592 - 0.1080D4 * t594 + 0.900
     #D3 * t596 - 0.396D3 * t598 + 0.72D2 * t600 + 0.108D3 * t602 - 0.27
     #0D3 * t604 + 0.360D3 * t606 - 0.270D3 * t608 + 0.108D3 * t610 - 0.
     #18D2 * t612 - 0.168D3 * t452 - t615 + t462
      t618 = t520 + t543 + t581 + t616
      t621 = (-z - x1 + t243) ** 2
      t622 = 0.1D1 / t621
      t627 = -t272 + t274 - t276 - t279 + t281 - t283 + t285 + t287 - t2
     #89 + t291 - t293 + t295 - t296 - t297 + t298 - t303 - t305 + t307
      t629 = -t310 + t314 - t316 - t319 + t321 - t323 + t325 + t328 - t3
     #30 - t332 - t334 + t336 - t338 - t340 + t342 - t344 + t346 + t458
      t634 = t349 ** 2
      t644 = -t349 * (t270 + t627 + t629 + t499 + t618) + t634 * t456 / 
     #0.2D1 + t232 + t234 - 0.22D2 * t236 - 0.42D2 * t239 + 0.50D2 * t24
     #1 - 0.48D2 * t243 + 0.132D3 * t245 - 0.108D3 * t247 + 0.15D2 * t24
     #9 + t252
      t656 = -0.252D3 * t253 + 0.258D3 * t255 - 0.78D2 * t257 + t261 + 0
     #.30D2 * t262 - 0.42D2 * t264 + 0.6D1 * t266 - 0.21D2 * t268 + 0.54
     #D2 * t271 - 0.66D2 * t273 + 0.39D2 * t275 + 0.9D1 * t278
      t665 = -0.6D1 * t280 + 0.6D1 * t282 - 0.9D1 * t284 - t287 + t289 -
     # t291 + t293 - t295 - 0.2D1 * t296 - 0.2D1 * t297 + 0.414D3 * t302
     # + 0.138D3 * t304
      t679 = -0.276D3 * t306 + 0.138D3 * t309 - 0.276D3 * t313 + 0.414D3
     # * t315 + 0.138D3 * t318 - 0.276D3 * t320 + 0.138D3 * t322 - 0.276
     #D3 * t324 - 0.168D3 * t327 + 0.84D2 * t329 + 0.252D3 * t331 + 0.84
     #D2 * t333 - 0.168D3 * t335
      t689 = polylog(3, t299)
      t690 = t689 * t197
      t692 = t689 * t308
      t694 = t689 * t233
      t700 = 0.252D3 * t337 + 0.84D2 * t339 - 0.168D3 * t341 + 0.84D2 * 
     #t343 - 0.168D3 * t345 - 0.2D1 * t238 + 0.4D1 * t472 - 0.18D2 * t69
     #0 - 0.18D2 * t692 + 0.36D2 * t694 - 0.18D2 * t689 * t317 + 0.36D2 
     #* t689 * t277
      t701 = t689 * t231
      t703 = t689 * t259
      t716 = -0.54D2 * t701 + 0.36D2 * t703 - 0.18D2 * t689 * t85 + 0.36
     #D2 * t689 * t89 - 0.54D2 * t689 * t86 - t150 + 0.4D1 * t183 + t458
     # - 0.12D2 * t89 + t100 - 0.28D2 * t460 - 0.22D2 * t473 + 0.48D2 * 
     #t475
      t730 = -0.52D2 * t477 + 0.28D2 * t479 + 0.10D2 * t481 - 0.20D2 * t
     #483 + 0.20D2 * t485 - 0.10D2 * t487 - 0.6D1 * t489 + 0.2D1 * t491 
     #+ 0.12D2 * t493 - 0.2D1 * t495 + 0.6D1 * t497 - 0.552D3 * t379
      t744 = -0.1104D4 * t381 + 0.2682D4 * t383 - 0.3432D4 * t385 - 0.11
     #04D4 * t389 + 0.4674D4 * t391 - 0.9942D4 * t393 + 0.10692D5 * t395
     # + 0.2682D4 * t397 - 0.9744D4 * t399 + 0.17280D5 * t401 - 0.16056D
     #5 * t403 - 0.3234D4 * t405 + 0.9900D4 * t407
      t760 = -0.15540D5 * t409 + 0.13350D5 * t411 + 0.2130D4 * t413 - 0.
     #5562D4 * t415 + 0.7494D4 * t417 - 0.5994D4 * t419 + 0.1104D4 * t42
     #2 - 0.1380D4 * t424 + 0.1104D4 * t426 + 0.1932D4 * t428 - 0.5520D4
     # * t430 + 0.8280D4 * t432
      t774 = -0.6900D4 * t434 + 0.3036D4 * t436 - 0.552D3 * t438 - 0.828
     #D3 * t440 + 0.2070D4 * t442 - 0.2760D4 * t444 + 0.2070D4 * t446 - 
     #0.828D3 * t448 + 0.138D3 * t450 - 0.336D3 * t539 + 0.168D3 * t541 
     #- 0.672D3 * t545 + 0.1566D4 * t547
      t788 = -0.1956D4 * t549 - 0.672D3 * t551 + 0.2712D4 * t553 - 0.558
     #6D4 * t555 + 0.5976D4 * t557 + 0.1566D4 * t559 - 0.5532D4 * t561 +
     # 0.9720D4 * t563 - 0.9108D4 * t565 - 0.1902D4 * t567 + 0.5760D4 * 
     #t569 - 0.9060D4 * t571
      t802 = 0.7860D4 * t573 + 0.1230D4 * t575 - 0.3186D4 * t577 + 0.436
     #2D4 * t579 - 0.3582D4 * t582 + 0.672D3 * t584 - 0.840D3 * t586 + 0
     #.672D3 * t588 + 0.1176D4 * t590 - 0.3360D4 * t592 + 0.5040D4 * t59
     #4 - 0.4200D4 * t596 + 0.1848D4 * t598
      t817 = -0.336D3 * t600 - 0.504D3 * t602 + 0.1260D4 * t604 - 0.1680
     #D4 * t606 + 0.1260D4 * t608 - 0.504D3 * t610 + 0.84D2 * t612 + 0.2
     #76D3 * t452 + 0.6D1 * t197 - 0.12D2 * t259 + t615 - 0.4D1 * t235 *
     # z
      t822 = t689 * x1
      t845 = 0.16D2 * t235 * t85 + 0.16D2 * t238 * z + 0.72D2 * t822 * t
     #317 - 0.36D2 * t822 * z + 0.144D3 * t822 * t85 - 0.324D3 * t822 * 
     #t89 + 0.396D3 * t822 * t86 + 0.144D3 * t690 * z - 0.558D3 * t690 *
     # t85 + 0.1116D4 * t690 * t89 - 0.1188D4 * t690 * t86 - 0.324D3 * t
     #703 * z + 0.1116D4 * t703 * t85
      t871 = -0.1944D4 * t703 * t89 + 0.1836D4 * t703 * t86 + 0.396D3 * 
     #t701 * z - 0.1188D4 * t701 * t85 + 0.1872D4 * t701 * t89 - 0.1638D
     #4 * t701 * t86 - 0.252D3 * t822 * t277 + 0.648D3 * t690 * t277 - 0
     #.900D3 * t703 * t277 + 0.756D3 * t701 * t277 - 0.144D3 * t690 * t3
     #17 + 0.180D3 * t703 * t317
      t898 = -0.144D3 * t701 * t317 - 0.252D3 * t694 * z + 0.720D3 * t69
     #4 * t85 - 0.1080D4 * t694 * t89 + 0.900D3 * t694 * t86 - 0.396D3 *
     # t694 * t277 + 0.72D2 * t694 * t317 + 0.108D3 * t692 * z - 0.270D3
     # * t692 * t85 + 0.360D3 * t692 * t89 - 0.270D3 * t692 * t86 + 0.10
     #8D3 * t692 * t277 - 0.18D2 * t692 * t317
      t906 = -t196 * (t270 + t311 + t459 + t499 + t618) * t622 - t208 * 
     #t456 * t622 - 0.120D3 * t210 * (t644 + t656 + t665 + t679 + t700 +
     # t716 + t730 + t744 + t760 + t774 + t788 + t802 + t817 + t845 + t8
     #71 + t898) * t622
      t910 = FJET(XB1, XB2, s, t2 * x1, -t2 * t229, 0.0D0, 0.0D0, 0.0D0,
     # -t906 * t221 * t10 / 0.480D3)
      rvgg2ght1s6e1 = t226 * t225 - t910 * t906 * t221 * t10 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s6e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = t3 * wd
      t5 = log(z)
      t6 = z ** 2
      t7 = t6 * z
      t8 = t5 * t7
      t9 = 0.168D3 * t8
      t10 = nf * t6
      t11 = -t1
      t12 = polylog(2, t11)
      t13 = 0.18D2 * t12
      t14 = 0.3D1 * t6
      t15 = 0.3D1 * t7
      t16 = t6 ** 2
      t17 = t12 * t16
      t18 = 0.18D2 * t17
      t19 = 0.84D2 * t5
      t20 = t12 * z
      t21 = 0.36D2 * t20
      t22 = nf * t7
      t23 = t5 * t6
      t24 = 0.252D3 * t23
      t25 = t5 * t16
      t26 = 0.84D2 * t25
      t27 = t5 * z
      t28 = 0.168D3 * t27
      t29 = t12 * t7
      t30 = 0.36D2 * t29
      t31 = t12 * t6
      t32 = 0.54D2 * t31
      t33 = x1 ** 2
      t34 = 0.1D1 / z
      t35 = t33 * t34
      t36 = t1 ** 2
      t37 = t36 * t1
      t39 = log(-t35 * t37)
      t45 = 0.18D2 * t5 + 0.18D2 * t25 - 0.36D2 * t8 - 0.36D2 * t27 + 0.
     #54D2 * t23
      t47 = t9 - t10 - t13 + t14 - t15 - t18 - t19 + t21 + t22 - t24 - t
     #26 + t28 + t30 - t32 - t39 * t45
      t50 = 0.440D3 * t3
      t53 = 0.720D3 - 0.720D3 * lh + 0.360D3 * t5
      t54 = t3 * t53
      t55 = t54 / 0.3D1
      t57 = (t50 + t55) * wd
      t60 = 0.1D1 / x1
      t65 = log(-t34 * t37)
      t66 = t65 * t3
      t69 = t9 - t10 - t13 + t14 - t15 - t18 - t19 + t21 + t22 - t24 - t
     #26 + t28 + t30 - t32
      t75 = t65 ** 2
      t78 = 0.3141592653589793D1 ** 2
      t82 = lh ** 2
      t86 = t5 ** 2
      t104 = polylog(3, t11)
      t108 = 0.6D1 - 0.12D2 * z - 0.2D1 * nf + 0.138D3 * t5 + t14 + t15 
     #- 0.2D1 * t10 + 0.84D2 * t12 - 0.18D2 * t104 - 0.276D3 * t27 + 0.4
     #14D3 * t23
      t125 = 0.138D3 * t25 - 0.276D3 * t8 - 0.168D3 * t29 + 0.36D2 * t10
     #4 * t7 + 0.84D2 * t17 - 0.18D2 * t104 * t16 + 0.252D3 * t31 - 0.16
     #8D3 * t20 + 0.36D2 * t104 * z - 0.54D2 * t104 * t6 + 0.4D1 * nf * 
     #z
      t133 = -(0.120D3 * t4 * t47 + t57 * t45) * t60 * t34 / 0.480D3 - (
     #(t50 + t55 - 0.120D3 * t66) * t69 + (0.960D3 * t3 + t54 - 0.360D3 
     #* t66 - t66 * t53 / 0.3D1 + 0.60D2 * t75 * t3 + t3 * (-0.60D2 * t7
     #8 + 0.720D3 * t5 + 0.1440D4 - 0.1440D4 * lh + 0.720D3 * t82 - 0.72
     #0D3 * t5 * lh + 0.180D3 * t86) / 0.3D1 + 0.80D2 / 0.3D1 * (0.8D1 -
     # 0.6D1 * lh + 0.3D1 * t5 - 0.3D1 * t65) * t3) * t45 + 0.120D3 * t3
     # * (t108 + t125)) * wd * t34 / 0.960D3
      t134 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t133)
      t137 = -0.1D1 + x1
      t139 = nf * x1
      t142 = nf * t33
      t149 = t33 ** 2
      t150 = t149 * x1
      t151 = nf * t150
      t156 = t16 * z
      t158 = t137 * t1
      t159 = polylog(2, t158)
      t160 = t33 * x1
      t161 = t159 * t160
      t166 = t159 * t149
      t175 = t159 * x1
      t178 = t159 * t33
      t185 = t16 * t6
      t188 = -0.2D1 * t139 * t7 - 0.2D1 * t142 * t6 + 0.8D1 * t142 * t7 
     #- 0.10D2 * t142 * t16 - 0.5D1 * t151 * z + 0.10D2 * t151 * t6 - 0.
     #3D1 * t156 - 0.1944D4 * t161 * t7 + 0.1836D4 * t161 * t16 + 0.396D
     #3 * t166 * z - 0.1188D4 * t166 * t6 + 0.1872D4 * t166 * t7 - 0.163
     #8D4 * t166 * t16 - 0.252D3 * t175 * t156 + 0.648D3 * t178 * t156 -
     # 0.900D3 * t161 * t156 + 0.756D3 * t166 * t156 - 0.144D3 * t178 * 
     #t185
      t193 = t159 * t150
      t206 = t149 * t33
      t207 = t159 * t206
      t221 = log(0.1D1 - t158)
      t222 = t221 * t160
      t223 = t222 * t156
      t225 = t221 * t149
      t226 = t225 * t156
      t228 = t221 * t33
      t229 = t228 * t185
      t231 = t222 * t185
      t233 = t225 * t185
      t235 = 0.180D3 * t161 * t185 - 0.144D3 * t166 * t185 - 0.252D3 * t
     #193 * z + 0.720D3 * t193 * t6 - 0.1080D4 * t193 * t7 + 0.900D3 * t
     #193 * t16 - 0.396D3 * t193 * t156 + 0.72D2 * t193 * t185 + 0.108D3
     # * t207 * z - 0.270D3 * t207 * t6 + 0.360D3 * t207 * t7 - 0.270D3 
     #* t207 * t16 + 0.108D3 * t207 * t156 - 0.18D2 * t207 * t185 - 0.43
     #62D4 * t223 + 0.3582D4 * t226 - 0.672D3 * t229 + 0.840D3 * t231 - 
     #0.672D3 * t233
      t237 = t221 * t150
      t238 = t237 * z
      t240 = t237 * t6
      t242 = t237 * t7
      t244 = t237 * t16
      t246 = t237 * t156
      t248 = t237 * t185
      t250 = t221 * t206
      t251 = t250 * z
      t253 = t250 * t6
      t255 = t250 * t7
      t257 = t250 * t16
      t259 = t250 * t156
      t261 = t250 * t185
      t275 = -0.1176D4 * t238 + 0.3360D4 * t240 - 0.5040D4 * t242 + 0.42
     #00D4 * t244 - 0.1848D4 * t246 + 0.336D3 * t248 + 0.504D3 * t251 - 
     #0.1260D4 * t253 + 0.1680D4 * t255 - 0.1260D4 * t257 + 0.504D3 * t2
     #59 - 0.84D2 * t261 + 0.72D2 * t175 * t185 - 0.36D2 * t175 * z + 0.
     #144D3 * t175 * t6 - 0.324D3 * t175 * t7 + 0.396D3 * t175 * t16 + 0
     #.144D3 * t178 * z
      t286 = nf * t149
      t289 = nf * t160
      t300 = t221 * x1
      t301 = t300 * t185
      t303 = t300 * t6
      t305 = t300 * t7
      t307 = t300 * t16
      t309 = t228 * z
      t311 = t228 * t6
      t313 = t228 * t7
      t315 = t228 * t16
      t317 = -0.558D3 * t178 * t6 + 0.1116D4 * t178 * t7 - 0.1188D4 * t1
     #78 * t16 - 0.324D3 * t161 * z + 0.1116D4 * t161 * t6 - 0.13D2 * t2
     #86 * t16 - 0.4D1 * t289 * t156 + 0.3D1 * t286 * t156 + 0.5D1 * t13
     #9 * t16 - 0.3D1 * t139 * t156 + 0.4D1 * t142 * t156 + 0.336D3 * t3
     #01 + 0.672D3 * t303 - 0.1566D4 * t305 + 0.1956D4 * t307 + 0.672D3 
     #* t309 - 0.2712D4 * t311 + 0.5586D4 * t313 - 0.5976D4 * t315
      t320 = t222 * z
      t322 = t222 * t6
      t324 = t222 * t7
      t326 = t222 * t16
      t328 = t225 * z
      t330 = t225 * t6
      t332 = t225 * t7
      t334 = t225 * t16
      t336 = t300 * t156
      t338 = t228 * t156
      t355 = -0.1566D4 * t320 + 0.5532D4 * t322 - 0.9720D4 * t324 + 0.91
     #08D4 * t326 + 0.1902D4 * t328 - 0.5760D4 * t330 + 0.9060D4 * t332 
     #- 0.7860D4 * t334 - 0.1230D4 * t336 + 0.3186D4 * t338 - 0.10D2 * t
     #151 * t7 + 0.5D1 * t151 * t16 - t151 * t156 - 0.2D1 * t289 * z + 0
     #.10D2 * t289 * t6 - 0.18D2 * t289 * t7 + 0.14D2 * t289 * t16 + 0.7
     #D1 * t286 * z
      t362 = t300 * z
      t368 = log(t35 * t37 * t137)
      t384 = 0.900D3 * t223 - 0.756D3 * t226 + 0.144D3 * t229 - 0.180D3 
     #* t231 + 0.144D3 * t233 + 0.252D3 * t238 - 0.720D3 * t240 + 0.1080
     #D4 * t242 - 0.900D3 * t244 + 0.396D3 * t246 - 0.72D2 * t248 - 0.10
     #8D3 * t251 + 0.270D3 * t253 - 0.360D3 * t255 + 0.270D3 * t257
      t401 = -0.108D3 * t259 + 0.18D2 * t261 - 0.72D2 * t301 - 0.144D3 *
     # t303 + 0.324D3 * t305 - 0.396D3 * t307 - 0.144D3 * t309 + 0.558D3
     # * t311 - 0.1116D4 * t313 + 0.1188D4 * t315 + 0.324D3 * t320 - 0.1
     #116D4 * t322 + 0.1944D4 * t324 - 0.1836D4 * t326 - 0.396D3 * t328 
     #+ 0.1188D4 * t330
      t409 = t221 * t6
      t411 = t221 * t7
      t413 = t160 * z
      t415 = t160 * t6
      t417 = t160 * t7
      t419 = t160 * t16
      t421 = t149 * z
      t423 = t149 * t6
      t425 = t149 * t7
      t427 = t149 * t16
      t429 = -0.1872D4 * t332 + 0.1638D4 * t334 + 0.252D3 * t336 - 0.648
     #D3 * t338 + 0.36D2 * t362 + 0.54D2 * t225 + 0.18D2 * t409 - 0.36D2
     # * t411 + 0.18D2 * t413 - 0.108D3 * t415 + 0.216D3 * t417 - 0.180D
     #3 * t419 - 0.18D2 * t421 + 0.72D2 * t423 - 0.108D3 * t425 + 0.72D2
     # * t427
      t430 = x1 * t156
      t432 = t33 * t156
      t434 = t160 * t156
      t436 = t149 * t156
      t442 = t221 * t16
      t444 = t221 * t185
      t446 = t221 * t156
      t448 = x1 * t7
      t450 = x1 * t16
      t452 = t33 * t6
      t454 = t33 * t7
      t456 = t33 * t16
      t458 = 0.18D2 * t430 - 0.54D2 * t432 + 0.54D2 * t434 - 0.18D2 * t4
     #36 + 0.18D2 * t228 - 0.36D2 * t222 + 0.18D2 * t250 - 0.36D2 * t237
     # + 0.54D2 * t442 + 0.18D2 * t444 - 0.36D2 * t446 + 0.18D2 * t448 -
     # 0.36D2 * t450 + 0.36D2 * t452 - 0.126D3 * t454 + 0.144D3 * t456
      t460 = t384 + t401 + t429 + t458
      t477 = -0.18D2 * t286 * t6 + 0.22D2 * t286 * t7 + 0.3D1 * t149 - 0
     #.3D1 * t150 - 0.168D3 * t362 - 0.252D3 * t225 + 0.3D1 * t16 - t368
     # * t460 - 0.84D2 * t409 + 0.168D3 * t411 + 0.36D2 * t161 - 0.18D2 
     #* t178 - 0.54D2 * t166 - 0.18D2 * t207 + 0.36D2 * t193 - 0.54D2 * 
     #t159 * t16 - 0.18D2 * t159 * t185 + 0.36D2 * t159 * t156 - 0.18D2 
     #* t159 * t6
      t503 = 0.36D2 * t159 * t7 + 0.12D2 * t413 + 0.42D2 * t415 - 0.198D
     #3 * t417 + 0.222D3 * t419 + 0.9D1 * t421 - 0.66D2 * t423 + 0.114D3
     # * t425 - 0.81D2 * t427 - 0.21D2 * t430 + 0.78D2 * t432 - 0.78D2 *
     # t434 + 0.21D2 * t436 + 0.15D2 * t150 * z - 0.30D2 * t150 * t6 + 0
     #.30D2 * t150 * t7 - 0.15D2 * t150 * t16 + 0.3D1 * t150 * t156
      t513 = x1 * z
      t524 = t151 - t286 - nf * t16 + nf * t156 - 0.84D2 * t228 + 0.168D
     #3 * t222 - 0.84D2 * t250 + 0.168D3 * t237 - 0.252D3 * t442 - 0.84D
     #2 * t444 + 0.168D3 * t446 + 0.36D2 * t513 - 0.108D3 * x1 * t6 + 0.
     #84D2 * t448 + 0.9D1 * t450 - 0.72D2 * t33 * z + 0.162D3 * t452 - 0
     #.30D2 * t454 - 0.138D3 * t456
      t529 = (-z - x1 + t513) ** 2
      t530 = 0.1D1 / t529
      t536 = -0.120D3 * t4 * (t188 + t235 + t275 + t317 + t355 + t477 + 
     #t503 + t524) * t530 - t57 * t460 * t530
      t540 = FJET(XB1, XB2, s, t2 * x1, -t2 * t137, 0.0D0, 0.0D0, 0.0D0,
     # -t536 * t60 * t34 / 0.480D3)
      rvgg2ght1s6e0 = t134 * t133 - t540 * t536 * t60 * t34 / 0.480D3

      end function



      doubleprecision function rvgg2ght1s6em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = t3 * wd
      t5 = log(z)
      t7 = z ** 2
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = t7 * z
      t12 = t5 * t11
      t14 = t5 * z
      t16 = t5 * t7
      t18 = 0.18D2 * t5 + 0.18D2 * t9 - 0.36D2 * t12 - 0.36D2 * t14 + 0.
     #54D2 * t16
      t19 = 0.1D1 / x1
      t21 = 0.1D1 / z
      t28 = polylog(2, -t1)
      t45 = 0.168D3 * t12 - nf * t7 - 0.18D2 * t28 + 0.3D1 * t7 - 0.3D1 
     #* t11 - 0.18D2 * t28 * t8 - 0.84D2 * t5 + 0.36D2 * t28 * z + nf * 
     #t11 - 0.252D3 * t16 - 0.84D2 * t9 + 0.168D3 * t14 + 0.36D2 * t28 *
     # t11 - 0.54D2 * t28 * t7
      t54 = t1 ** 2
      t57 = log(-t21 * t54 * t1)
      t66 = -t4 * t18 * t19 * t21 / 0.4D1 - (0.120D3 * t3 * t45 + (0.440
     #D3 * t3 + t3 * (0.720D3 - 0.720D3 * lh + 0.360D3 * t5) / 0.3D1 - 0
     #.120D3 * t57 * t3) * t18) * wd * t21 / 0.960D3
      t67 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t66)
      t70 = -0.1D1 + x1
      t74 = log(0.1D1 - t70 * t1)
      t75 = t74 * x1
      t76 = t8 * t7
      t85 = x1 ** 2
      t86 = t74 * t85
      t101 = t85 * x1
      t110 = -0.72D2 * t75 * t76 - 0.144D3 * t75 * t7 + 0.324D3 * t75 * 
     #t11 - 0.396D3 * t75 * t8 - 0.144D3 * t86 * z + 0.558D3 * t86 * t7 
     #+ 0.18D2 * x1 * t11 - 0.36D2 * x1 * t8 + 0.36D2 * t85 * t7 - 0.126
     #D3 * t85 * t11 + 0.144D3 * t85 * t8 + 0.18D2 * t101 * z - 0.108D3 
     #* t101 * t7 + 0.216D3 * t101 * t11 - 0.180D3 * t101 * t8
      t111 = t85 ** 2
      t120 = t8 * z
      t129 = t74 * t111
      t132 = t74 * t101
      t135 = t74 * t111 * t85
      t138 = t74 * t111 * x1
      t146 = -0.18D2 * t111 * z + 0.72D2 * t111 * t7 - 0.108D3 * t111 * 
     #t11 + 0.72D2 * t111 * t8 + 0.18D2 * x1 * t120 - 0.54D2 * t85 * t12
     #0 + 0.54D2 * t101 * t120 - 0.18D2 * t111 * t120 + 0.54D2 * t129 + 
     #0.18D2 * t86 - 0.36D2 * t132 + 0.18D2 * t135 - 0.36D2 * t138 + 0.5
     #4D2 * t74 * t8 + 0.18D2 * t74 * t76 - 0.36D2 * t74 * t120
      t180 = 0.18D2 * t74 * t7 - 0.36D2 * t74 * t11 - 0.1116D4 * t86 * t
     #11 + 0.1188D4 * t86 * t8 + 0.324D3 * t132 * z - 0.1116D4 * t132 * 
     #t7 + 0.1944D4 * t132 * t11 - 0.1836D4 * t132 * t8 - 0.396D3 * t129
     # * z + 0.1188D4 * t129 * t7 - 0.1872D4 * t129 * t11 + 0.1638D4 * t
     #129 * t8 + 0.252D3 * t75 * t120 - 0.648D3 * t86 * t120 + 0.900D3 *
     # t132 * t120 - 0.756D3 * t129 * t120
      t213 = 0.144D3 * t86 * t76 - 0.180D3 * t132 * t76 + 0.144D3 * t129
     # * t76 + 0.252D3 * t138 * z - 0.720D3 * t138 * t7 + 0.1080D4 * t13
     #8 * t11 - 0.900D3 * t138 * t8 + 0.396D3 * t138 * t120 - 0.72D2 * t
     #138 * t76 - 0.108D3 * t135 * z + 0.270D3 * t135 * t7 - 0.360D3 * t
     #135 * t11 + 0.270D3 * t135 * t8 - 0.108D3 * t135 * t120 + 0.18D2 *
     # t135 * t76 + 0.36D2 * t75 * z
      t215 = t110 + t146 + t180 + t213
      t219 = (-z - x1 + x1 * z) ** 2
      t220 = 0.1D1 / t219
      t225 = FJET(XB1, XB2, s, t2 * x1, -t2 * t70, 0.0D0, 0.0D0, 0.0D0, 
     #t4 * t215 * t220 * t19 * t21 / 0.4D1)
      rvgg2ght1s6em1 = t67 * t66 + t225 * t3 * wd * t215 * t220 * t19 * 
     #t21 / 0.4D1

      end function



      doubleprecision function rvgg2ght1s6em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t3 = 0.1D1 / t1
      t4 = log(z)
      t6 = z ** 2
      t7 = t6 ** 2
      t17 = 0.18D2 * t4 + 0.18D2 * t4 * t7 - 0.36D2 * t4 * t6 * z - 0.36
     #D2 * t4 * z + 0.54D2 * t4 * t6
      t19 = 0.1D1 / z
      t23 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, -t3 * 
     #t17 * wd * t19 / 0.8D1)
      rvgg2ght1s6em2 = -t23 * t3 * t17 * wd * t19 / 0.8D1

      end function



      doubleprecision function rvgg2ght1s6em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s6em3 = 0.0D0

      end function



      doubleprecision function rvgg2ght1s6em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      rvgg2ght1s6em4 = 0.0D0

      end function
