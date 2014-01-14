      subroutine rrgg2qqbarht11
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt11
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt11
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhardt11s1e1  
      doubleprecision rrgg2qqbarhhardt11s1e0  
      doubleprecision rrgg2qqbarhhardt11s1em1  
      doubleprecision rrgg2qqbarhhardt11s1em2  
      doubleprecision rrgg2qqbarhhardt11s1em3  
      doubleprecision rrgg2qqbarhhardt11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt11s1e1
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = x2 * z
      t8 = (t6 - x2 + 0.1D1) ** 2
      t9 = 0.1D1 / t8
      t10 = z * t9
      t11 = t1 ** 2
      t12 = t10 * t11
      t13 = wd * nf
      t14 = x2 * x3
      t15 = x4 * pi
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t21 = t20 * t4
      t24 = log(-0.4D1 * t14 * t21)
      t25 = t24 ** 2
      t26 = -x2 + t6 - 0.1D1
      t32 = t10 * t11 * lh
      t37 = lh ** 2
      t39 = pi ** 2
      t41 = -0.180D3 * t37 + 0.30D2 * t39
      t42 = t41 * wd
      t45 = t12 * t42 * nf * t26
      t47 = 0.1D1 / x3
      t50 = t26 * z
      t51 = t9 * t11
      t52 = t50 * t51
      t60 = log(-0.4D1 * x2 * t17 * t19 * t4)
      t61 = t60 * t26
      t64 = (0.2D1 * x2 - 0.2D1 * t6 + 0.2D1 - t61) * z * t51
      t67 = t60 ** 2
      t68 = t67 * t26
      t72 = (-x2 + t6 - 0.1D1 + 0.2D1 * t61 + t68 / 0.2D1) * z * t51
      t101 = z * t11
      t102 = t101 * wd
      t103 = x1 ** 2
      t104 = t14 * t103
      t107 = log(-0.4D1 * t104 * t21)
      t109 = t26 * t9
      t113 = t101 * lh
      t119 = 0.1D1 / x1
      t122 = x2 * t103
      t125 = log(-0.4D1 * t122 * t21)
      t126 = t125 ** 2
      t132 = t101 * lh * wd
      t140 = (-0.45D2 * t12 * t13 * t25 * t26 - 0.180D3 * t32 * t13 * t2
     #4 * t26 + t45) * t47 / 0.2160D4 + (0.180D3 * (0.3D1 * t52 + 0.2D1 
     #* t64 + t72) * lh + t50 * t51 * (-0.60D2 * lh * t39 + 0.240D3 * ze
     #ta3 + 0.120D3 * t37 * lh) - 0.360D3 * t52 - 0.270D3 * t64 - 0.180D
     #3 * t72 - 0.90D2 * (-t61 - t68 - t67 * t60 * t26 / 0.6D1) * z * t5
     #1 + (0.2D1 * t52 + t64) * t41) * wd * nf / 0.2160D4 - (-0.90D2 * t
     #102 * nf * t107 * t109 - 0.180D3 * t113 * t13 * t109) * t47 * t119
     # / 0.1080D4 - (0.45D2 * t102 * nf * t126 * t109 + 0.180D3 * t132 *
     # nf * t125 * t109 - t45) * t119 / 0.1080D4
      t141 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t140)
      t143 = 0.2D1 * t14
      t144 = cos(t15)
      t146 = -0.1D1 + x3
      t149 = Sqrt(x2 * t4 * x3 * t146)
      t151 = 0.2D1 * t144 * t149
      t153 = t2 * (0.1D1 - x2 - x3 + t143 + t151)
      t155 = t2 * (-x3 + t143 - x2 + t151)
      t161 = log(0.4D1 * t14 * t17 * t19 * t146 * t4)
      t162 = t161 ** 2
      t163 = -x2 + t6 - 0.1D1 + x3
      t167 = 0.45D2 * t12 * t13 * t162 * t163
      t171 = 0.180D3 * t32 * t13 * t161 * t163
      t183 = log(0.4D1 * t104 * t20 * t4 * t146)
      t185 = t163 * t9
      t195 = (0.90D2 * t102 * nf * t183 * t185 + 0.180D3 * t113 * t13 * 
     #t185) * t47 * t119 / 0.1080D4
      t196 = (t167 + t171 - t12 * t42 * nf * t163) * t47 / 0.2160D4 - t1
     #95
      t197 = FJET(XB1, XB2, s, 0.0D0, t153, 0.0D0, -t155, 0.0D0, t196)
      t199 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t140)
      t201 = FJET(XB1, XB2, s, 0.0D0, -t155, 0.0D0, t153, 0.0D0, t196)
      t203 = -0.1D1 + x1
      t205 = x1 * z
      t206 = 0.1D1 - x1 + t205
      t207 = 0.1D1 / t206
      t209 = t2 * t203 * x2 * t207
      t210 = t2 * x1
      t213 = t4 * s * t1 * t203
      t218 = s * t11 * x2 * x1 * t203 * t207
      t219 = t101 * t13
      t221 = t14 * t103 * t17
      t222 = t19 * t207
      t223 = t203 ** 2
      t224 = t223 * t4
      t225 = t222 * t224
      t228 = log(-0.4D1 * t221 * t225)
      t229 = x2 * x1
      t230 = t229 * z
      t232 = (-t6 + t230 + x2 - t229 - 0.1D1 + x1 - t205) ** 2
      t233 = 0.1D1 / t232
      t235 = x2 - t229 - t6 + t230 + 0.1D1 - x1 + t205
      t236 = t235 * t203
      t241 = nf * t233 * t236
      t250 = log(-0.4D1 * t122 * t17 * t225)
      t251 = t250 ** 2
      t267 = -(0.90D2 * t219 * t228 * t233 * t236 + 0.180D3 * t132 * t24
     #1) * t47 * t119 / 0.1080D4 - (-0.45D2 * t219 * t251 * t233 * t236 
     #- 0.180D3 * t132 * nf * t250 * t233 * t235 * t203 + t101 * t42 * t
     #241) * t119 / 0.1080D4
      t268 = FJET(XB1, XB2, s, 0.0D0, -t209, t210, t213, -t218, t267)
      t270 = FJET(XB1, XB2, s, t210, t213, 0.0D0, -t209, -t218, t267)
      t272 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t140)
      t274 = FJET(XB1, XB2, s, t153, 0.0D0, -t155, 0.0D0, 0.0D0, t196)
      t276 = x3 * x1
      t277 = t2 * t276
      t278 = t276 * z
      t279 = t14 * x1
      t280 = t14 * t205
      t285 = Sqrt(x3 * t4 * t206 * x2 * t146)
      t287 = 0.2D1 * t144 * t285
      t291 = t2 * t203 * (-x3 + t276 - t278 + t143 - t279 + t280 - x2 + 
     #t287) * t207
      t294 = t146 * s * t1 * x1
      t295 = 0.1D1 - x1 + t205 - x2 + t229 - t230 - x3 + t276 - t278 + t
     #143 - t279 + t280 + t287
      t298 = t2 * t203 * t295 * t207
      t303 = log(0.4D1 * t221 * t222 * t224 * t146)
      t304 = x2 - t229 - t6 + t230 + 0.1D1 - x1 + t205 - x3 + t276 - t27
     #8
      t306 = t233 * t203
      t314 = -0.90D2 * t219 * t303 * t304 * t306 - 0.180D3 * t132 * nf *
     # t304 * t306
      t317 = t314 * t47 * t119 / 0.1080D4
      t318 = FJET(XB1, XB2, s, t277, t291, -t294, -t298, -t218, -t317)
      t320 = t47 * t119
      t323 = FJET(XB1, XB2, s, t213, t210, -t209, 0.0D0, -t218, t267)
      t325 = FJET(XB1, XB2, s, t291, t277, -t298, -t294, -t218, -t317)
      t329 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t140)
      t337 = (t167 + t171 - t12 * t42 * nf * t163) * t47 / 0.2160D4 - t1
     #95
      t338 = FJET(XB1, XB2, s, -t155, 0.0D0, t153, 0.0D0, 0.0D0, t337)
      t340 = FJET(XB1, XB2, s, -t294, -t298, t277, t291, -t218, -t317)
      t344 = FJET(XB1, XB2, s, -t209, 0.0D0, t213, t210, -t218, t267)
      t346 = FJET(XB1, XB2, s, -t298, -t294, t291, t277, -t218, -t317)
      rrgg2qqbarhhardt11s1e1 = t141 * t140 + t197 * t196 + t199 * t140 +
     # t201 * t196 + t268 * t267 + t270 * t267 + t272 * t140 + t274 * t1
     #96 - t318 * t314 * t320 / 0.1080D4 + t323 * t267 - t325 * t314 * t
     #320 / 0.1080D4 + t329 * t140 + t338 * t337 - t340 * t314 * t320 / 
     #0.1080D4 + t344 * t267 - t346 * t314 * t320 / 0.1080D4

      end function



      doubleprecision function rrgg2qqbarhhardt11s1e0
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = x2 * z
      t8 = (t6 - x2 + 0.1D1) ** 2
      t9 = 0.1D1 / t8
      t11 = t1 ** 2
      t12 = z * t9 * t11
      t13 = wd * nf
      t14 = x2 * x3
      t15 = x4 * pi
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t21 = t17 * t19 * t4
      t24 = log(-0.4D1 * t14 * t21)
      t25 = -x2 + t6 - 0.1D1
      t30 = z * t11
      t31 = t30 * lh
      t32 = t25 * t9
      t35 = 0.180D3 * t31 * t13 * t32
      t37 = 0.1D1 / x3
      t40 = t30 * t13
      t41 = 0.1D1 / x1
      t42 = t37 * t41
      t47 = x1 ** 2
      t48 = x2 * t47
      t51 = log(-0.4D1 * t48 * t21)
      t59 = t25 * z
      t60 = t9 * t11
      t61 = t59 * t60
      t66 = t19 * t4
      t69 = log(-0.4D1 * x2 * t17 * t66)
      t70 = t69 * t25
      t73 = (0.2D1 * x2 - 0.2D1 * t6 + 0.2D1 - t70) * z * t60
      t80 = t69 ** 2
      t87 = lh ** 2
      t89 = pi ** 2
      t98 = (0.90D2 * t12 * t13 * t24 * t25 + t35) * t37 / 0.2160D4 - t4
     #0 * t32 * t42 / 0.12D2 - (-0.90D2 * t30 * wd * nf * t51 * t32 - t3
     #5) * t41 / 0.1080D4 + (0.180D3 * (0.2D1 * t61 + t73) * lh - 0.270D
     #3 * t61 - 0.180D3 * t73 - 0.90D2 * (-x2 + t6 - 0.1D1 + 0.2D1 * t70
     # + t80 * t25 / 0.2D1) * z * t60 + t59 * t60 * (-0.180D3 * t87 + 0.
     #30D2 * t89)) * wd * nf / 0.2160D4
      t99 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t98)
      t101 = 0.2D1 * t14
      t102 = cos(t15)
      t104 = -0.1D1 + x3
      t107 = Sqrt(x2 * t4 * x3 * t104)
      t109 = 0.2D1 * t102 * t107
      t111 = t2 * (0.1D1 - x2 - x3 + t101 + t109)
      t113 = t2 * (-x3 + t101 - x2 + t109)
      t119 = log(0.4D1 * t14 * t17 * t19 * t104 * t4)
      t120 = -x2 + t6 - 0.1D1 + x3
      t124 = 0.90D2 * t12 * t13 * t119 * t120
      t125 = lh * wd
      t134 = t120 * t9
      t137 = t40 * t134 * t42 / 0.12D2
      t138 = (-t124 - 0.180D3 * t12 * t125 * nf * t120) * t37 / 0.2160D4
     # + t137
      t139 = FJET(XB1, XB2, s, 0.0D0, t111, 0.0D0, -t113, 0.0D0, t138)
      t141 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t98)
      t143 = FJET(XB1, XB2, s, 0.0D0, -t113, 0.0D0, t111, 0.0D0, t138)
      t145 = -0.1D1 + x1
      t147 = x1 * z
      t148 = 0.1D1 - x1 + t147
      t149 = 0.1D1 / t148
      t151 = t2 * t145 * x2 * t149
      t152 = t2 * x1
      t155 = t4 * s * t1 * t145
      t160 = s * t11 * x2 * x1 * t145 * t149
      t161 = x2 * x1
      t162 = t161 * z
      t164 = (-t6 + t162 + x2 - t161 - 0.1D1 + x1 - t147) ** 2
      t165 = 0.1D1 / t164
      t166 = x2 - t161 - t6 + t162 + 0.1D1 - x1 + t147
      t169 = t145 * t37 * t41
      t174 = t145 ** 2
      t179 = log(-0.4D1 * t48 * t17 * t66 * t174 * t149)
      t181 = t166 * t145
      t193 = t40 * t165 * t166 * t169 / 0.12D2 - (0.90D2 * t40 * t179 * 
     #t165 * t181 + 0.180D3 * t30 * t125 * nf * t165 * t181) * t41 / 0.1
     #080D4
      t194 = FJET(XB1, XB2, s, 0.0D0, -t151, t152, t155, -t160, t193)
      t196 = FJET(XB1, XB2, s, t152, t155, 0.0D0, -t151, -t160, t193)
      t198 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t98)
      t200 = FJET(XB1, XB2, s, t111, 0.0D0, -t113, 0.0D0, 0.0D0, t138)
      t202 = x3 * x1
      t203 = t2 * t202
      t204 = t202 * z
      t205 = t14 * x1
      t206 = t14 * t147
      t211 = Sqrt(x3 * t4 * t148 * x2 * t104)
      t213 = 0.2D1 * t102 * t211
      t217 = t2 * t145 * (-x3 + t202 - t204 + t101 - t205 + t206 - x2 + 
     #t213) * t149
      t220 = t104 * s * t1 * x1
      t221 = 0.1D1 - x1 + t147 - x2 + t161 - t162 - x3 + t202 - t204 + t
     #101 - t205 + t206 + t213
      t224 = t2 * t145 * t221 * t149
      t227 = (x2 - t161 - t6 + t162 + 0.1D1 - x1 + t147 - x3 + t202 - t2
     #04) * t165 * t169
      t229 = t40 * t227 / 0.12D2
      t230 = FJET(XB1, XB2, s, t203, t217, -t220, -t224, -t160, -t229)
      t233 = t11 * wd * nf
      t237 = FJET(XB1, XB2, s, t155, t152, -t151, 0.0D0, -t160, t193)
      t239 = FJET(XB1, XB2, s, t217, t203, -t224, -t220, -t160, -t229)
      t244 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t98)
      t252 = (-t124 - 0.180D3 * t31 * t13 * t134) * t37 / 0.2160D4 + t13
     #7
      t253 = FJET(XB1, XB2, s, -t113, 0.0D0, t111, 0.0D0, 0.0D0, t252)
      t255 = FJET(XB1, XB2, s, -t220, -t224, t203, t217, -t160, -t229)
      t260 = FJET(XB1, XB2, s, -t151, 0.0D0, t155, t152, -t160, t193)
      t262 = FJET(XB1, XB2, s, -t224, -t220, t217, t203, -t160, -t229)
      rrgg2qqbarhhardt11s1e0 = t99 * t98 + t139 * t138 + t141 * t98 + t1
     #43 * t138 + t194 * t193 + t196 * t193 + t198 * t98 + t200 * t138 -
     # t230 * z * t233 * t227 / 0.12D2 + t237 * t193 - t239 * z * t233 *
     # t227 / 0.12D2 + t244 * t98 + t253 * t252 - t255 * z * t233 * t227
     # / 0.12D2 + t260 * t193 - t262 * z * t233 * t227 / 0.12D2

      end function



      doubleprecision function rrgg2qqbarhhardt11s1em1
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = t7 * wd
      t9 = x2 * z
      t10 = -x2 + t9 - 0.1D1
      t11 = nf * t10
      t13 = (t9 - x2 + 0.1D1) ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 / x1
      t20 = t10 * z
      t21 = t14 * t6
      t29 = x4 * pi
      t30 = Sin(t29)
      t31 = t30 ** 2
      t33 = z ** 2
      t38 = log(-0.4D1 * x2 * t31 / t33 * t4)
      t48 = 0.1D1 / x3
      t49 = t14 * t48
      t53 = -t8 * t11 * t14 * t15 / 0.12D2 + (0.180D3 * t20 * t21 * lh -
     # 0.180D3 * t20 * t21 - 0.90D2 * (0.2D1 * x2 - 0.2D1 * t9 + 0.2D1 -
     # t38 * t10) * z * t21) * wd * nf / 0.2160D4 - t8 * t11 * t49 / 0.2
     #4D2
      t54 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t53)
      t57 = 0.2D1 * x2 * x3
      t58 = cos(t29)
      t63 = Sqrt(x2 * t4 * x3 * (-0.1D1 + x3))
      t65 = 0.2D1 * t58 * t63
      t67 = t2 * (0.1D1 - x2 - x3 + t57 + t65)
      t69 = t2 * (-x3 + t57 - x2 + t65)
      t72 = wd * nf
      t73 = x2 - t9 - x3 + 0.1D1
      t75 = t72 * t73 * t48
      t77 = z * t14 * t6 * t75 / 0.24D2
      t78 = FJET(XB1, XB2, s, 0.0D0, t67, 0.0D0, -t69, 0.0D0, -t77)
      t83 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t53)
      t85 = FJET(XB1, XB2, s, 0.0D0, -t69, 0.0D0, t67, 0.0D0, -t77)
      t90 = -0.1D1 + x1
      t92 = x1 * z
      t94 = 0.1D1 / (0.1D1 - x1 + t92)
      t96 = t2 * t90 * x2 * t94
      t97 = t2 * x1
      t100 = t4 * s * t1 * t90
      t105 = s * t6 * x2 * x1 * t90 * t94
      t107 = x2 * x1
      t108 = t107 * z
      t110 = (-t9 + t108 + x2 - t107 - 0.1D1 + x1 - t92) ** 2
      t111 = 0.1D1 / t110
      t112 = x2 - t107 - t9 + t108 + 0.1D1 - x1 + t92
      t117 = t7 * t72 * t111 * t112 * t90 * t15 / 0.12D2
      t118 = FJET(XB1, XB2, s, 0.0D0, -t96, t97, t100, -t105, t117)
      t120 = t6 * wd
      t125 = nf * t111 * t112 * t90 * t15
      t128 = FJET(XB1, XB2, s, t97, t100, 0.0D0, -t96, -t105, t117)
      t133 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t53)
      t135 = FJET(XB1, XB2, s, t67, 0.0D0, -t69, 0.0D0, 0.0D0, -t77)
      t140 = FJET(XB1, XB2, s, t100, t97, -t96, 0.0D0, -t105, t117)
      t145 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t53)
      t147 = FJET(XB1, XB2, s, -t96, 0.0D0, t100, t97, -t105, t117)
      t152 = -t73
      t157 = FJET(XB1, XB2, s, -t69, 0.0D0, t67, 0.0D0, 0.0D0, t8 * nf *
     # t152 * t49 / 0.24D2)
      rrgg2qqbarhhardt11s1em1 = t54 * t53 - t78 * z * t21 * t75 / 0.24D2
     # + t83 * t53 - t85 * z * t21 * t75 / 0.24D2 + t118 * z * t120 * t1
     #25 / 0.12D2 + t128 * z * t120 * t125 / 0.12D2 + t133 * t53 - t135 
     #* z * t21 * t75 / 0.24D2 + t140 * z * t120 * t125 / 0.12D2 + t145 
     #* t53 + t147 * z * t120 * t125 / 0.12D2 + t157 * z * t21 * t72 * t
     #152 * t48 / 0.24D2

      end function



      doubleprecision function rrgg2qqbarhhardt11s1em2
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
      t3 = t2 * x2
      t5 = t2 * (-0.1D1 + x2)
      t6 = t1 ** 2
      t9 = x2 * z
      t10 = -x2 + t9 - 0.1D1
      t13 = (t9 - x2 + 0.1D1) ** 2
      t14 = 0.1D1 / t13
      t17 = z * t6 * wd * nf * t10 * t14 / 0.24D2
      t18 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t17)
      t23 = wd * nf * t10 * t14
      t25 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, -t17)
      t29 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t17)
      t33 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, -t17)
      rrgg2qqbarhhardt11s1em2 = -t18 * z * t6 * t23 / 0.24D2 - t25 * z *
     # t6 * t23 / 0.24D2 - t29 * z * t6 * t23 / 0.24D2 - t33 * z * t6 * 
     #t23 / 0.24D2

      end function



      doubleprecision function rrgg2qqbarhhardt11s1em3
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
      rrgg2qqbarhhardt11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt11s1em4
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
      rrgg2qqbarhhardt11s1em4 = 0.0D0

      end function
  
      subroutine rrgg2qqbarhsoftt11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt11s1e1  
      doubleprecision rrgg2qqbarhsoftt11s1e0  
      doubleprecision rrgg2qqbarhsoftt11s1em1  
      doubleprecision rrgg2qqbarhsoftt11s1em2  
      doubleprecision rrgg2qqbarhsoftt11s1em3  
      doubleprecision rrgg2qqbarhsoftt11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt11s1e1
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
      rrgg2qqbarhsoftt11s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt11s1e0
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
      rrgg2qqbarhsoftt11s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt11s1em1
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
      rrgg2qqbarhsoftt11s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt11s1em2
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
      rrgg2qqbarhsoftt11s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt11s1em3
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
      rrgg2qqbarhsoftt11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt11s1em4
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
      rrgg2qqbarhsoftt11s1em4 = 0.0D0

      end function
