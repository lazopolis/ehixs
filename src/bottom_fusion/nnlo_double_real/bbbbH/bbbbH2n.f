  
      subroutine bbbbH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH21J1  
      doubleprecision bbbbH21J2  
      doubleprecision bbbbH21J3  
      doubleprecision bbbbH22J1  
      doubleprecision bbbbH22J2  
      doubleprecision bbbbH22J3  
      doubleprecision bbbbH2n1e1  
      doubleprecision bbbbH2n1e0  
      doubleprecision bbbbH2n1em1  
      doubleprecision bbbbH2n1em2  
      doubleprecision bbbbH2n1em3  
      doubleprecision bbbbH2n1em4  
      doubleprecision bbbbH2n2e1  
      doubleprecision bbbbH2n2e0  
      doubleprecision bbbbH2n2em1  
      doubleprecision bbbbH2n2em2  
      doubleprecision bbbbH2n2em3  
      doubleprecision bbbbH2n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH2n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH2n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH2n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH2n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH2n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbbbH2n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = x4 * t4
      t16 = log(-0.4D1 * t12 * t13)
      t19 = t1 ** 2
      t20 = (-0.180D3 * lh - 0.90D2 * t16) * t19
      t21 = 0.1D1 / s
      t22 = bbbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t26 = lh ** 2
      t27 = 0.180D3 * t26
      t28 = 0.3141592653589793D1 ** 2
      t29 = 0.30D2 * t28
      t32 = t16 ** 2
      t35 = (t27 - t29 + 0.180D3 * t16 * lh + 0.45D2 * t32) * t19
      t36 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t44 = t27 - t29
      t51 = (0.60D2 * lh * t28 - 0.2884936567583026D3 - 0.120D3 * t26 * 
     #lh - t16 * t44 - 0.90D2 * t32 * lh - 0.15D2 * t32 * t16) * t19
      t52 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t53 = t21 * t52
      t56 = lh * t19
      t57 = 0.1D1 - x3
      t58 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t59 = x3 * t9
      t61 = -t57
      t65 = log(0.4D1 * t59 * t11 * t13 * t61)
      t66 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t68 = t11 * x4
      t69 = t68 * t4
      t72 = log(-0.4D1 * t59 * t69)
      t78 = t19 * t21
      t79 = t72 ** 2
      t84 = bbbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t85 = t65 ** 2
      t91 = t44 * t19
      t93 = t21 * (t66 - t52)
      t96 = 0.1D1 / x3
      t99 = x1 ** 2
      t100 = x3 * t99
      t101 = t100 * t9
      t104 = log(-0.4D1 * t101 * t69)
      t110 = log(0.4D1 * t101 * t68 * t4 * t61)
      t119 = 0.1D1 / x1
      t122 = t99 * t9
      t125 = log(-0.4D1 * t122 * t69)
      t131 = t125 ** 2
      t142 = t20 * t21 * t22 / 0.5760D4 + t35 * t21 * t36 / 0.5760D4 + t
     #51 * t53 / 0.5760D4 - (-0.180D3 * t56 * t21 * (t58 - t65 * t66 - t
     #36 + t72 * t52) + 0.90D2 * t78 * (-t79 * t52 / 0.2D1 + t72 * t36 -
     # t22 - t65 * t58 + t84 + t85 * t66 / 0.2D1) + t91 * t93) * t96 / 0
     #.5760D4 - (0.90D2 * t78 * (t104 * t52 - t36 - t110 * t66 + t58) - 
     #0.180D3 * t56 * t93) * t96 * t119 / 0.2880D4 - (-0.180D3 * t56 * t
     #21 * (-t36 + t125 * t52) + 0.90D2 * t78 * (-t131 * t52 / 0.2D1 + t
     #125 * t36 - t22) - t91 * t53) * t119 / 0.2880D4
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t142)
      t145 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t149 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t150 = t21 * t149
      t153 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t154 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t163 = bbbbH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t165 = bbbbH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t173 = t21 * (t154 - t149)
      t207 = t35 * t21 * t145 / 0.5760D4 + t51 * t150 / 0.5760D4 - (-0.1
     #80D3 * t56 * t21 * (t153 - t65 * t154 - t145 + t72 * t149) + 0.90D
     #2 * t78 * (-t79 * t149 / 0.2D1 - t163 + t72 * t145 + t165 - t65 * 
     #t153 + t85 * t154 / 0.2D1) + t91 * t173) * t96 / 0.5760D4 + t20 * 
     #t21 * t163 / 0.5760D4 - (0.90D2 * t78 * (t104 * t149 + t153 - t110
     # * t154 - t145) - 0.180D3 * t56 * t173) * t96 * t119 / 0.2880D4 - 
     #(-0.180D3 * t56 * t21 * (-t145 + t125 * t149) + 0.90D2 * t78 * (t1
     #25 * t145 - t131 * t149 / 0.2D1 - t163) - t91 * t150) * t119 / 0.2
     #880D4
      t208 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t211 = s * t210
      t212 = t1 * x1
      t213 = t211 * t212
      t214 = -0.1D1 + x1
      t215 = t1 * t214
      t216 = t215 * x4
      t217 = t211 * t216
      t218 = t215 * t4
      t219 = t211 * t218
      t220 = t210 ** 2
      t223 = x1 * t214
      t225 = s * t220 * t19 * t223 * t4
      t226 = t214 * t220
      t228 = 0.1D1 / (-0.2D1 + t210)
      t229 = bbbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t231 = t226 * t228 * t229
      t232 = t100 * t12
      t233 = t214 ** 2
      t234 = t220 ** 2
      t236 = t13 * t233 * t234
      t239 = log(-0.4D1 * t232 * t236)
      t240 = t239 * t214
      t241 = t220 * t228
      t242 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t243 = t241 * t242
      t248 = t56 * t21
      t250 = t226 * t228 * t242
      t259 = log(-0.4D1 * t122 * t11 * t236)
      t260 = t259 * t214
      t266 = t259 ** 2
      t267 = t266 * t214
      t270 = bbbbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t278 = t91 * t21
      t283 = -(0.90D2 * t78 * (t231 - t240 * t243) - 0.180D3 * t248 * t2
     #50) * t96 * t119 / 0.2880D4 - (-0.180D3 * t56 * t21 * (t231 - t260
     # * t243) + 0.90D2 * t78 * (t267 * t243 / 0.2D1 + t226 * t228 * t27
     #0 - t260 * t241 * t229) + t278 * t250) * t119 / 0.2880D4
      t284 = FJET(XB1, XB2, s, t213, 0.0D0, -t217, t219, t225, t283)
      t286 = bbbbH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t288 = t226 * t228 * t286
      t289 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t290 = t241 * t289
      t296 = t226 * t228 * t289
      t309 = bbbbH22J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t321 = -(0.90D2 * t78 * (t288 - t240 * t290) - 0.180D3 * t248 * t2
     #96) * t96 * t119 / 0.2880D4 - (-0.180D3 * t56 * t21 * (t288 - t260
     # * t290) + 0.90D2 * t78 * (t267 * t290 / 0.2D1 + t226 * t228 * t30
     #9 - t260 * t241 * t286) + t278 * t296) * t119 / 0.2880D4
      t322 = FJET(XB1, XB2, s, -t217, t219, t213, 0.0D0, t225, t321)
      t324 = KAPPA2(x1, x2, t57, x4, z)
      t325 = s * t324
      t327 = t325 * t212 * t61
      t329 = t325 * t212 * x3
      t330 = t325 * t216
      t331 = t325 * t218
      t332 = t324 ** 2
      t337 = cos(t7)
      t340 = sqrt(x3 * t61 * t13)
      t345 = s * t332 * t19 * t223 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t337 * t340)
      t346 = t214 * t332
      t348 = 0.1D1 / (-0.2D1 + t324)
      t349 = bbbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t353 = t332 ** 2
      t358 = log(0.4D1 * t232 * t13 * t233 * t61 * t353)
      t359 = t358 * t214
      t360 = t332 * t348
      t361 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t371 = 0.90D2 * t78 * (-t346 * t348 * t349 + t359 * t360 * t361) +
     # 0.180D3 * t248 * t346 * t348 * t361
      t375 = FJET(XB1, XB2, s, -t327, t329, -t330, t331, t345, -t371 * t
     #96 * t119 / 0.2880D4)
      t377 = t96 * t119
      t380 = bbbbH22J2(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t383 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t393 = 0.90D2 * t78 * (-t346 * t348 * t380 + t359 * t360 * t383) +
     # 0.180D3 * t248 * t346 * t348 * t383
      t397 = FJET(XB1, XB2, s, -t330, t331, -t327, t329, t345, -t393 * t
     #96 * t119 / 0.2880D4)
      bbbbH2n1e1 = t143 * t142 + t208 * t207 + t284 * t283 + t322 * t321
     # - t375 * t371 * t377 / 0.2880D4 - t397 * t393 * t377 / 0.2880D4

      end function



      doubleprecision function bbbbH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = 0.1D1 - x3
      t10 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t26 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t28 = t16 * x4 * t4
      t31 = log(-0.4D1 * t14 * t28)
      t32 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t37 = lh * t6
      t38 = t24 - t32
      t43 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t13
      t55 = log(-0.4D1 * t52 * t28)
      t60 = t7 * t32
      t66 = bbbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t73 = log(-0.4D1 * t13 * t16 * t18)
      t76 = (-0.180D3 * lh - 0.90D2 * t73) * t6
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t73 ** 2
      t89 = (0.180D3 * t80 - 0.30D2 * t82 + 0.180D3 * t73 * lh + 0.45D2 
     #* t86) * t6
      t92 = -(0.90D2 * t8 * (t10 - t23 * t24 - t26 + t31 * t32) - 0.180D
     #3 * t37 * t7 * t38) * t43 / 0.5760D4 - t8 * t38 * t43 * t47 / 0.32
     #D2 - (0.90D2 * t8 * (-t26 + t55 * t32) + 0.180D3 * t37 * t60) * t4
     #7 / 0.2880D4 + t8 * t66 / 0.64D2 + t76 * t7 * t26 / 0.5760D4 + t89
     # * t60 / 0.5760D4
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t92)
      t95 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t96 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t98 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t99 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t104 = t96 - t99
      t119 = t7 * t99
      t125 = bbbbH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t133 = -(0.90D2 * t8 * (t95 - t23 * t96 - t98 + t31 * t99) - 0.180
     #D3 * t37 * t7 * t104) * t43 / 0.5760D4 - t8 * t104 * t43 * t47 / 0
     #.32D2 - (0.90D2 * t8 * (-t98 + t55 * t99) + 0.180D3 * t37 * t119) 
     #* t47 / 0.2880D4 + t8 * t125 / 0.64D2 + t76 * t7 * t98 / 0.5760D4 
     #+ t89 * t119 / 0.5760D4
      t134 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t133)
      t136 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t137 = s * t136
      t138 = t1 * x1
      t139 = t137 * t138
      t140 = -0.1D1 + x1
      t141 = t1 * t140
      t142 = t141 * x4
      t143 = t137 * t142
      t144 = t141 * t4
      t145 = t137 * t144
      t146 = t136 ** 2
      t149 = x1 * t140
      t151 = s * t146 * t6 * t149 * t4
      t152 = t140 * t146
      t153 = t8 * t152
      t155 = 0.1D1 / (-0.2D1 + t136)
      t156 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t157 = t155 * t156
      t158 = t43 * t47
      t162 = bbbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t166 = t146 ** 2
      t167 = t140 ** 2
      t172 = log(-0.4D1 * t52 * t16 * t18 * t166 * t167)
      t173 = t172 * t140
      t174 = t146 * t155
      t180 = t37 * t7
      t187 = -t153 * t157 * t158 / 0.32D2 - (0.90D2 * t8 * (t152 * t155 
     #* t162 - t173 * t174 * t156) - 0.180D3 * t180 * t152 * t157) * t47
     # / 0.2880D4
      t188 = FJET(XB1, XB2, s, t139, 0.0D0, -t143, t145, t151, t187)
      t190 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t191 = t155 * t190
      t195 = bbbbH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t209 = -t153 * t191 * t158 / 0.32D2 - (0.90D2 * t8 * (t152 * t155 
     #* t195 - t173 * t174 * t190) - 0.180D3 * t180 * t152 * t191) * t47
     # / 0.2880D4
      t210 = FJET(XB1, XB2, s, -t143, t145, t139, 0.0D0, t151, t209)
      t212 = KAPPA2(x1, x2, t9, x4, z)
      t213 = s * t212
      t215 = t213 * t138 * t19
      t217 = t213 * t138 * x3
      t218 = t213 * t142
      t219 = t213 * t144
      t220 = t212 ** 2
      t225 = cos(t11)
      t228 = sqrt(x3 * t19 * t18)
      t233 = s * t220 * t6 * t149 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t225 * t228)
      t235 = t8 * t140 * t220
      t237 = 0.1D1 / (-0.2D1 + t212)
      t238 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t9, x4)
      t243 = FJET(XB1, XB2, s, -t215, t217, -t218, t219, t233, t235 * t2
     #37 * t238 * t158 / 0.32D2)
      t245 = t7 * t140
      t247 = t220 * t237
      t253 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, t9, x4)
      t258 = FJET(XB1, XB2, s, -t218, t219, -t215, t217, t233, t235 * t2
     #37 * t253 * t158 / 0.32D2)
      bbbbH2n1e0 = t93 * t92 + t134 * t133 + t188 * t187 + t210 * t209 +
     # t243 * t6 * t245 * t247 * t238 * t43 * t47 / 0.32D2 + t258 * t6 *
     # t245 * t247 * t253 * t43 * t47 / 0.32D2

      end function



      doubleprecision function bbbbH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t22 = log(-0.4D1 * t15 / t16 * x4 * t4)
      t25 = (-0.180D3 * lh - 0.90D2 * t22) * t6
      t26 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t30 = 0.1D1 - x3
      t31 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t30, x4)
      t33 = 0.1D1 / x3
      t37 = 0.1D1 / x1
      t41 = t8 * t9 / 0.64D2 + t25 * t7 * t26 / 0.5760D4 - t8 * (t31 - t
     #26) * t33 / 0.64D2 + t8 * t26 * t37 / 0.32D2
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t41)
      t44 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t47 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t51 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t30, x4)
      t59 = t8 * t44 / 0.64D2 + t25 * t7 * t47 / 0.5760D4 - t8 * (t51 - 
     #t47) * t33 / 0.64D2 + t8 * t47 * t37 / 0.32D2
      t60 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t59)
      t62 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t63 = s * t62
      t65 = t63 * t1 * x1
      t66 = -0.1D1 + x1
      t67 = t1 * t66
      t69 = t63 * t67 * x4
      t71 = t63 * t67 * t4
      t72 = t62 ** 2
      t77 = s * t72 * t6 * x1 * t66 * t4
      t78 = t8 * t66
      t81 = t72 / (-0.2D1 + t62)
      t82 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t84 = t81 * t82 * t37
      t87 = FJET(XB1, XB2, s, t65, 0.0D0, -t69, t71, t77, -t78 * t84 / 0
     #.32D2)
      t89 = t7 * t66
      t93 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t95 = t81 * t93 * t37
      t98 = FJET(XB1, XB2, s, -t69, t71, t65, 0.0D0, t77, -t78 * t95 / 0
     #.32D2)
      bbbbH2n1em1 = t42 * t41 + t60 * t59 - t87 * t6 * t89 * t84 / 0.32D
     #2 - t98 * t6 * t89 * t95 / 0.32D2

      end function



      doubleprecision function bbbbH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t8 * t9 / 0.
     #64D2)
      t16 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t19 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t8 * t16 / 0
     #.64D2)
      bbbbH2n1em2 = t12 * t6 * t7 * t9 / 0.64D2 + t19 * t6 * t7 * t16 / 
     #0.64D2

      end function



      doubleprecision function bbbbH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      bbbbH2n1em3 = 0.0D0

      end function



      doubleprecision function bbbbH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      bbbbH2n1em4 = 0.0D0

      end function


      doubleprecision function bbbbH2n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = x4 * t4
      t16 = log(-0.4D1 * t12 * t13)
      t19 = t1 ** 2
      t20 = (-0.180D3 * lh - 0.90D2 * t16) * t19
      t21 = 0.1D1 / s
      t22 = bbbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t26 = lh ** 2
      t27 = 0.180D3 * t26
      t28 = 0.3141592653589793D1 ** 2
      t29 = 0.30D2 * t28
      t32 = t16 ** 2
      t35 = (t27 - t29 + 0.180D3 * t16 * lh + 0.45D2 * t32) * t19
      t36 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t44 = t27 - t29
      t51 = (0.60D2 * lh * t28 - 0.2884936567583026D3 - 0.120D3 * t26 * 
     #lh - t16 * t44 - 0.90D2 * t32 * lh - 0.15D2 * t32 * t16) * t19
      t52 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t53 = t21 * t52
      t56 = lh * t19
      t57 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t58 = x3 * t9
      t60 = -0.1D1 + x3
      t64 = log(0.4D1 * t58 * t11 * t13 * t60)
      t65 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t67 = t11 * x4
      t68 = t67 * t4
      t71 = log(-0.4D1 * t58 * t68)
      t77 = t19 * t21
      t78 = t71 ** 2
      t83 = bbbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t84 = t64 ** 2
      t90 = t44 * t19
      t91 = t65 - t52
      t95 = 0.1D1 / x3
      t98 = x1 ** 2
      t99 = x3 * t98
      t100 = t99 * t9
      t105 = log(0.4D1 * t100 * t67 * t4 * t60)
      t109 = log(-0.4D1 * t100 * t68)
      t120 = 0.1D1 / x1
      t123 = t98 * t9
      t126 = log(-0.4D1 * t123 * t68)
      t132 = t126 ** 2
      t143 = t20 * t21 * t22 / 0.5760D4 + t35 * t21 * t36 / 0.5760D4 + t
     #51 * t53 / 0.5760D4 - (-0.180D3 * t56 * t21 * (t57 - t64 * t65 - t
     #36 + t71 * t52) + 0.90D2 * t77 * (-t78 * t52 / 0.2D1 + t71 * t36 -
     # t22 - t64 * t57 + t83 + t84 * t65 / 0.2D1) + t90 * t21 * t91) * t
     #95 / 0.5760D4 + (0.90D2 * t77 * (-t57 + t105 * t65 + t36 - t109 * 
     #t52) + 0.180D3 * t56 * t21 * t91) * t95 * t120 / 0.2880D4 + (-0.18
     #0D3 * t56 * t21 * (t36 - t126 * t52) + 0.90D2 * t77 * (t132 * t52 
     #/ 0.2D1 - t126 * t36 + t22) + t90 * t53) * t120 / 0.2880D4
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t143)
      t146 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t147 = s * t146
      t148 = t1 * x1
      t149 = t147 * t148
      t150 = -0.1D1 + x1
      t151 = t1 * t150
      t152 = t151 * x4
      t153 = t147 * t152
      t154 = t151 * t4
      t155 = t147 * t154
      t156 = t146 ** 2
      t159 = x1 * t150
      t161 = s * t156 * t19 * t159 * x4
      t162 = t99 * t12
      t163 = t150 ** 2
      t164 = t156 ** 2
      t166 = t13 * t163 * t164
      t169 = log(-0.4D1 * t162 * t166)
      t170 = t169 * t150
      t172 = 0.1D1 / (-0.2D1 + t146)
      t173 = t156 * t172
      t174 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t175 = t173 * t174
      t177 = t150 * t156
      t178 = bbbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t180 = t177 * t172 * t178
      t184 = t56 * t21
      t186 = t177 * t172 * t174
      t195 = log(-0.4D1 * t123 * t11 * t166)
      t196 = t195 * t150
      t204 = t195 ** 2
      t205 = t204 * t150
      t208 = bbbbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t214 = t90 * t21
      t219 = (0.90D2 * t77 * (t170 * t175 - t180) + 0.180D3 * t184 * t18
     #6) * t95 * t120 / 0.2880D4 + (-0.180D3 * t56 * t21 * (t196 * t175 
     #- t180) + 0.90D2 * t77 * (t196 * t173 * t178 - t205 * t175 / 0.2D1
     # - t177 * t172 * t208) - t214 * t186) * t120 / 0.2880D4
      t220 = FJET(XB1, XB2, s, 0.0D0, t149, -t153, t155, -t161, t219)
      t222 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t226 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t227 = t21 * t226
      t230 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t231 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t240 = bbbbH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t242 = bbbbH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t249 = t231 - t226
      t286 = t35 * t21 * t222 / 0.5760D4 + t51 * t227 / 0.5760D4 - (-0.1
     #80D3 * t56 * t21 * (t230 - t64 * t231 - t222 + t71 * t226) + 0.90D
     #2 * t77 * (-t78 * t226 / 0.2D1 - t240 + t71 * t222 + t242 - t64 * 
     #t230 + t84 * t231 / 0.2D1) + t90 * t21 * t249) * t95 / 0.5760D4 + 
     #t20 * t21 * t240 / 0.5760D4 + (0.90D2 * t77 * (t105 * t231 - t109 
     #* t226 - t230 + t222) + 0.180D3 * t56 * t21 * t249) * t95 * t120 /
     # 0.2880D4 + (-0.180D3 * t56 * t21 * (-t126 * t226 + t222) + 0.90D2
     # * t77 * (-t126 * t222 + t132 * t226 / 0.2D1 + t240) + t90 * t227)
     # * t120 / 0.2880D4
      t287 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t286)
      t289 = KAPPA2(x1, x2, x3, x4, z)
      t290 = s * t289
      t292 = t290 * t148 * x3
      t294 = t290 * t148 * t60
      t295 = t290 * t152
      t296 = t290 * t154
      t297 = t289 ** 2
      t302 = cos(t7)
      t305 = sqrt(x3 * t60 * t13)
      t310 = s * t297 * t19 * t159 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t302 * t305)
      t311 = t150 * t297
      t313 = 0.1D1 / (-0.2D1 + t289)
      t314 = bbbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t318 = t297 ** 2
      t323 = log(0.4D1 * t162 * t13 * t163 * t60 * t318)
      t324 = t323 * t150
      t325 = t297 * t313
      t326 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t336 = 0.90D2 * t77 * (t311 * t313 * t314 - t324 * t325 * t326) - 
     #0.180D3 * t184 * t311 * t313 * t326
      t340 = FJET(XB1, XB2, s, t292, -t294, -t295, t296, t310, t336 * t9
     #5 * t120 / 0.2880D4)
      t342 = t95 * t120
      t345 = bbbbH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t347 = t177 * t172 * t345
      t348 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t349 = t173 * t348
      t355 = t177 * t172 * t348
      t368 = bbbbH22J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t380 = (0.90D2 * t77 * (-t347 + t170 * t349) + 0.180D3 * t184 * t3
     #55) * t95 * t120 / 0.2880D4 + (-0.180D3 * t56 * t21 * (-t347 + t19
     #6 * t349) + 0.90D2 * t77 * (-t205 * t349 / 0.2D1 - t177 * t172 * t
     #368 + t196 * t173 * t345) - t214 * t355) * t120 / 0.2880D4
      t381 = FJET(XB1, XB2, s, -t153, t155, 0.0D0, t149, -t161, t380)
      t383 = bbbbH22J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t386 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t396 = 0.90D2 * t77 * (t311 * t313 * t383 - t324 * t325 * t386) - 
     #0.180D3 * t184 * t311 * t313 * t386
      t400 = FJET(XB1, XB2, s, -t295, t296, t292, -t294, t310, t396 * t9
     #5 * t120 / 0.2880D4)
      bbbbH2n2e1 = t144 * t143 + t220 * t219 + t287 * t286 + t340 * t336
     # * t342 / 0.2880D4 + t381 * t380 + t400 * t396 * t342 / 0.2880D4

      end function



      doubleprecision function bbbbH2n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t25 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t27 = t15 * x4 * t4
      t30 = log(-0.4D1 * t13 * t27)
      t31 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t36 = lh * t6
      t37 = t23 - t31
      t42 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t12
      t55 = log(-0.4D1 * t52 * t27)
      t60 = t7 * t31
      t66 = bbbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t73 = log(-0.4D1 * t12 * t15 * t17)
      t76 = (-0.180D3 * lh - 0.90D2 * t73) * t6
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t73 ** 2
      t89 = (0.180D3 * t80 - 0.30D2 * t82 + 0.180D3 * t73 * lh + 0.45D2 
     #* t86) * t6
      t92 = -(0.90D2 * t8 * (t9 - t22 * t23 - t25 + t30 * t31) - 0.180D3
     # * t36 * t7 * t37) * t42 / 0.5760D4 - t8 * t37 * t42 * t47 / 0.32D
     #2 + (0.90D2 * t8 * (t25 - t55 * t31) - 0.180D3 * t36 * t60) * t47 
     #/ 0.2880D4 + t8 * t66 / 0.64D2 + t76 * t7 * t25 / 0.5760D4 + t89 *
     # t60 / 0.5760D4
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t92)
      t95 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t96 = s * t95
      t97 = t1 * x1
      t98 = t96 * t97
      t99 = -0.1D1 + x1
      t100 = t1 * t99
      t101 = t100 * x4
      t102 = t96 * t101
      t103 = t100 * t4
      t104 = t96 * t103
      t105 = t95 ** 2
      t108 = x1 * t99
      t110 = s * t105 * t6 * t108 * x4
      t111 = t99 * t105
      t112 = t8 * t111
      t114 = 0.1D1 / (-0.2D1 + t95)
      t115 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t116 = t114 * t115
      t117 = t42 * t47
      t122 = t105 ** 2
      t123 = t99 ** 2
      t128 = log(-0.4D1 * t52 * t15 * t17 * t122 * t123)
      t129 = t128 * t99
      t130 = t105 * t114
      t133 = bbbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t139 = t36 * t7
      t146 = -t112 * t116 * t117 / 0.32D2 + (0.90D2 * t8 * (t129 * t130 
     #* t115 - t111 * t114 * t133) + 0.180D3 * t139 * t111 * t116) * t47
     # / 0.2880D4
      t147 = FJET(XB1, XB2, s, 0.0D0, t98, -t102, t104, -t110, t146)
      t149 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t150 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t152 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t153 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t158 = t150 - t153
      t174 = t7 * t153
      t180 = bbbbH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t188 = -(0.90D2 * t8 * (t149 - t22 * t150 - t152 + t30 * t153) - 0
     #.180D3 * t36 * t7 * t158) * t42 / 0.5760D4 - t8 * t158 * t42 * t47
     # / 0.32D2 + (0.90D2 * t8 * (-t55 * t153 + t152) - 0.180D3 * t36 * 
     #t174) * t47 / 0.2880D4 + t8 * t180 / 0.64D2 + t76 * t7 * t152 / 0.
     #5760D4 + t89 * t174 / 0.5760D4
      t189 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t188)
      t191 = KAPPA2(x1, x2, x3, x4, z)
      t192 = s * t191
      t194 = t192 * t97 * x3
      t196 = t192 * t97 * t18
      t197 = t192 * t101
      t198 = t192 * t103
      t199 = t191 ** 2
      t204 = cos(t10)
      t207 = sqrt(x3 * t18 * t17)
      t212 = s * t199 * t6 * t108 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t204 * t207)
      t214 = t8 * t99 * t199
      t216 = 0.1D1 / (-0.2D1 + t191)
      t217 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t222 = FJET(XB1, XB2, s, t194, -t196, -t197, t198, t212, t214 * t2
     #16 * t217 * t117 / 0.32D2)
      t224 = t7 * t99
      t226 = t199 * t216
      t232 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t233 = t114 * t232
      t237 = bbbbH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t251 = -t112 * t233 * t117 / 0.32D2 + (0.90D2 * t8 * (-t111 * t114
     # * t237 + t129 * t130 * t232) + 0.180D3 * t139 * t111 * t233) * t4
     #7 / 0.2880D4
      t252 = FJET(XB1, XB2, s, -t102, t104, 0.0D0, t98, -t110, t251)
      t254 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t259 = FJET(XB1, XB2, s, -t197, t198, t194, -t196, t212, t214 * t2
     #16 * t254 * t117 / 0.32D2)
      bbbbH2n2e0 = t93 * t92 + t147 * t146 + t189 * t188 + t222 * t6 * t
     #224 * t226 * t217 * t42 * t47 / 0.32D2 + t252 * t251 + t259 * t6 *
     # t224 * t226 * t254 * t42 * t47 / 0.32D2

      end function



      doubleprecision function bbbbH2n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t22 = log(-0.4D1 * t15 / t16 * x4 * t4)
      t25 = (-0.180D3 * lh - 0.90D2 * t22) * t6
      t26 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t30 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t32 = 0.1D1 / x3
      t36 = 0.1D1 / x1
      t40 = t8 * t9 / 0.64D2 + t25 * t7 * t26 / 0.5760D4 - t8 * (t30 - t
     #26) * t32 / 0.64D2 + t8 * t26 * t36 / 0.32D2
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t40)
      t43 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t44 = s * t43
      t46 = t44 * t1 * x1
      t47 = -0.1D1 + x1
      t48 = t1 * t47
      t50 = t44 * t48 * x4
      t52 = t44 * t48 * t4
      t53 = t43 ** 2
      t58 = s * t53 * t6 * x1 * t47 * x4
      t59 = t8 * t47
      t62 = t53 / (-0.2D1 + t43)
      t63 = bbbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t65 = t62 * t63 * t36
      t68 = FJET(XB1, XB2, s, 0.0D0, t46, -t50, t52, -t58, -t59 * t65 / 
     #0.32D2)
      t70 = t7 * t47
      t74 = bbbbH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t77 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t81 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t89 = t8 * t74 / 0.64D2 + t25 * t7 * t77 / 0.5760D4 - t8 * (t81 - 
     #t77) * t32 / 0.64D2 + t8 * t77 * t36 / 0.32D2
      t90 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = bbbbH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t94 = t62 * t92 * t36
      t97 = FJET(XB1, XB2, s, -t50, t52, 0.0D0, t46, -t58, -t59 * t94 / 
     #0.32D2)
      bbbbH2n2em1 = t41 * t40 - t68 * t6 * t70 * t65 / 0.32D2 + t90 * t8
     #9 - t97 * t6 * t70 * t94 / 0.32D2

      end function



      doubleprecision function bbbbH2n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t8 * t9 / 0.
     #64D2)
      t16 = bbbbH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t19 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t8 * t16 / 0
     #.64D2)
      bbbbH2n2em2 = t12 * t6 * t7 * t9 / 0.64D2 + t19 * t6 * t7 * t16 / 
     #0.64D2

      end function



      doubleprecision function bbbbH2n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      bbbbH2n2em3 = 0.0D0

      end function



      doubleprecision function bbbbH2n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH21J1
      doubleprecision bbbbH21J2
      doubleprecision bbbbH21J3
      doubleprecision bbbbH22J1
      doubleprecision bbbbH22J2
      doubleprecision bbbbH22J3
      bbbbH2n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbbbH21J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t11 = x1 ** 2
      t12 = t11 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t17 = s * t3
      t18 = t7 * x1
      t21 = t18 * t13
      t23 = s - x3 * t18 * t17 - t17 * t21
      t27 = t23 ** 2
      t28 = t27 * t1
      t29 = t28 * t4
      t30 = t8 * t11
      t31 = x3 ** 2
      t40 = x1 * t13
      t46 = 0.1D1 - x1
      t47 = t7 * t46
      t50 = 0.1D1 - x4
      t53 = s - t17 * t47 * x4 - t17 * t47 * t50
      t55 = t27 * t53 * s
      t58 = t28 * t4 * t8
      t59 = x1 * x3
      t60 = t46 * x4
      t65 = t6 * t9 * x1
      t66 = x3 * t23
      t67 = t46 ** 2
      t68 = x4 ** 2
      t73 = t2 * t4
      t74 = t8 * x1
      t75 = t73 * t74
      t77 = t46 * t50
      t80 = x3 * t53
      t87 = t1 * t4
      t88 = t87 * t74
      t89 = t53 * t46
      t98 = t67 * t50
      t103 = t13 * t53
      t110 = t23 * t46
      t118 = t50 ** 2
      t125 = -0.2D1 * t6 * t9 * t12 * t14 * t13 * t23 - 0.3D1 * t29 * t3
     #0 * t31 - 0.5D1 * t29 * t30 * t14 + t2 * t3 * t7 * t40 * t23 - 0.2
     #D1 * t28 * t3 * t21 + 0.4D1 * t55 - 0.2D1 * t58 * t59 * t60 + 0.4D
     #1 * t65 * t66 * t67 * t68 - t75 * t13 * t23 * t77 - 0.2D1 * t75 * 
     #t80 * t77 - 0.2D1 * t75 * t80 * t60 + 0.4D1 * t88 * t66 * t89 * t5
     #0 + 0.2D1 * t88 * t66 * t89 * x4 + 0.3D1 * t65 * t66 * t98 * x4 - 
     #0.2D1 * t75 * t103 * t77 - 0.2D1 * t75 * t103 * t60 + 0.5D1 * t88 
     #* t103 * t110 * x4 + t88 * t103 * t110 * t50 + t65 * t66 * t67 * t
     #118 - 0.2D1 * t58 * t40 * t77
      t126 = t4 ** 2
      t128 = t8 ** 2
      t130 = t2 * t126 * t128 * t11
      t135 = cos(x2 * 0.3141592653589793D1)
      t139 = sqrt(x3 * t13 * x4 * t50)
      t142 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t135 * t139
      t151 = t9 * t11
      t152 = t6 * t151
      t153 = t46 * t142
      t158 = t1 * t5 * t151
      t171 = t28 * t5 * t9
      t181 = t142 ** 2
      t189 = t1 * t3
      t190 = t189 * t18
      t191 = t53 * z
      t200 = t3 * t7
      t207 = t189 * t7
      t208 = t53 * t23
      t214 = t73 * t8
      t215 = t11 * t31
      t219 = t11 * t14
      t223 = t87 * t8
      t231 = t27 * s * t3
      t238 = 0.6D1 * t190 * t103 * t23 * z - 0.2D1 * t55 * t200 * t60 - 
     #0.2D1 * t55 * t200 * t77 + t207 * t40 * t208 + 0.3D1 * t207 * t59 
     #* t208 - 0.6D1 * t214 * t215 * t191 - 0.6D1 * t214 * t219 * t191 -
     # 0.6D1 * t223 * t219 * t208 - 0.6D1 * t223 * t215 * t208 - 0.8D1 *
     # t231 * t18 * t80 - 0.6D1 * t231 * t18 * t103
      bbbbH21J1 = 0.16D2 / 0.3D1 * wd * (t125 - 0.4D1 * t130 * t66 * t67
     # * x4 * t142 - 0.2D1 * t130 * t66 * t98 * t142 + 0.2D1 * t152 * t8
     #0 * t153 + 0.3D1 * t158 * t66 * t89 * t142 + 0.2D1 * t152 * t103 *
     # t153 + 0.4D1 * t158 * t103 * t110 * t142 + 0.3D1 * t171 * t11 * x
     #3 * t153 + t2 * t126 * t3 * t128 * t7 * t12 * t66 * t67 * t181 + 0
     #.3D1 * t171 * t11 * t13 * t153 + 0.6D1 * t190 * t66 * t191 + t238)
     # / t27 / t53

      end function
  
   
 

      doubleprecision function bbbbH21J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t4 = 0.1D1 - z
      t5 = t1 * t2 * t4
      t6 = 0.1D1 - x3
      t7 = x1 * t6
      t8 = s * t2
      t9 = 0.1D1 - x1
      t10 = t4 * t9
      t13 = 0.1D1 - x4
      t16 = s - t8 * t10 * x4 - t8 * t10 * t13
      t17 = t4 * x1
      t20 = t17 * t6
      t22 = s - t8 * t17 * x3 - t8 * t20
      t23 = t16 * t22
      t27 = x1 * x3
      t30 = t2 ** 2
      t31 = t1 * t30
      t32 = t4 ** 2
      t33 = t31 * t32
      t34 = x1 ** 2
      t35 = t6 ** 2
      t40 = x3 ** 2
      t45 = t22 ** 2
      t48 = t6 * t16
      t52 = t45 * t1
      t54 = t52 * t30 * t32
      t55 = t9 * t13
      t59 = t45 * t16
      t62 = t32 * x1
      t66 = cos(x2 * 0.3141592653589793D1)
      t70 = sqrt(x3 * t6 * x4 * t13)
      t73 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t66 * t70
      t74 = t9 * t73
      t78 = t9 * x4
      t82 = s * t1
      t83 = t30 * t2
      t84 = t82 * t83
      t85 = t32 * t4
      t87 = t84 * t85 * x1
      t88 = x3 * t22
      t89 = t9 ** 2
      t90 = x4 ** 2
      t96 = t82 * t30 * t62
      t100 = x3 * t16
      t109 = t31 * t62
      t110 = t16 * t9
      t123 = -0.3D1 * t5 * t7 * t23 - t5 * t27 * t23 + 0.2D1 * t33 * t34
     # * t35 * t23 - 0.2D1 * t33 * t34 * t40 * t23 + 0.10D2 * t45 * s * 
     #t2 * t17 * t48 - 0.4D1 * t54 * t27 * t55 - 0.3D1 * t59 * s * t30 *
     # t62 * t74 - 0.4D1 * t54 * t27 * t78 + 0.2D1 * t87 * t88 * t89 * t
     #90 + t96 * t6 * t22 * t55 + 0.2D1 * t96 * t100 * t55 - 0.4D1 * t59
     # * s + 0.2D1 * t96 * t100 * t78 - t109 * t88 * t110 * t13 + 0.2D1 
     #* t109 * t88 * t110 * x4 + 0.3D1 * t87 * t88 * t89 * t13 * x4
      t130 = t22 * t9
      t138 = t13 ** 2
      t142 = t85 * t34
      t143 = t84 * t142
      t157 = t52 * t83 * t85
      t162 = t30 ** 2
      t165 = t32 ** 2
      t167 = t34 * x1
      t170 = t73 ** 2
      t184 = t52 * t30
      t185 = t32 * t34
      t199 = 0.2D1 * t96 * t48 * t55 + 0.2D1 * t96 * t48 * t78 - 0.6D1 *
     # t109 * t48 * t130 * x4 - t109 * t48 * t130 * t13 + t87 * t88 * t8
     #9 * t138 - 0.2D1 * t143 * t100 * t74 - 0.2D1 * t143 * t48 * t74 + 
     #0.3D1 * t1 * t83 * t142 * t48 * t130 * t73 - 0.3D1 * t157 * t34 * 
     #x3 * t74 - t82 * t162 * t2 * t165 * t4 * t167 * t88 * t89 * t170 -
     # 0.3D1 * t157 * t34 * t6 * t74 + 0.2D1 * t84 * t85 * t167 * t35 * 
     #t6 * t22 + 0.3D1 * t184 * t185 * t40 + 0.5D1 * t184 * t185 * t35 -
     # t82 * t2 * t4 * t7 * t22 - 0.2D1 * t52 * t2 * t20
      bbbbH21J2 = 0.16D2 / 0.3D1 * wd * (t123 + t199) / t45 / t16

      end function
  
   
 

      doubleprecision function bbbbH21J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = t10 ** 2
      t12 = 0.1D1 - x1
      t13 = t3 * t12
      t16 = 0.1D1 - x4
      t19 = s - t2 * t13 * x4 - t2 * t13 * t16
      t20 = t11 * t19
      t21 = t1 ** 2
      t24 = t3 ** 2
      t25 = t24 * x1
      t29 = cos(x2 * 0.3141592653589793D1)
      t33 = sqrt(x3 * t7 * x4 * t16)
      t36 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t29 * t33
      t37 = t12 * t36
      t42 = t7 * t19
      t48 = s ** 2
      t49 = t11 * t48
      t50 = t49 * t21
      t51 = x1 ** 2
      t52 = t24 * t51
      t53 = x3 ** 2
      t56 = t7 ** 2
      t59 = t21 * t1
      t61 = t24 * t3
      t63 = t48 * t59 * t61 * t51
      t64 = x3 * t10
      t69 = t48 * s
      t70 = t21 ** 2
      t71 = t69 * t70
      t72 = t24 ** 2
      t74 = t71 * t72 * t51
      t75 = t12 ** 2
      t76 = t75 * t16
      t86 = t51 * x1
      t89 = t36 ** 2
      t94 = t49 * t21 * t24
      t95 = x1 * t7
      t96 = t12 * t16
      t102 = -t20 * s * t21 * t25 * t37 - 0.2D1 * t11 * s * t1 * t4 * t4
     #2 - 0.2D1 * t20 * s + t50 * t52 * t53 + t50 * t52 * t56 + t63 * t6
     #4 * t19 * t12 * t36 + 0.2D1 * t74 * t64 * t76 * t36 + t49 * t1 * t
     #8 - t69 * t70 * t1 * t72 * t3 * t86 * t64 * t75 * t89 - t94 * t95 
     #* t96 + t94 * x1 * x3 * t96
      t113 = t48 * t21
      t115 = t10 * t12
      t135 = t69 * t59 * t61 * x1
      t139 = t113 * t24
      t141 = t19 * t10
      t144 = t16 ** 2
      t160 = -0.2D1 * t49 * t59 * t61 * t51 * x3 * t37 - t69 * t1 * t3 *
     # t95 * t10 + t113 * t25 * t42 * t115 * x4 + 0.2D1 * t71 * t72 * t8
     #6 * t56 * t10 * t37 + 0.2D1 * t74 * t64 * t75 * x4 * t36 - t63 * t
     #42 * t115 * t36 - t135 * t64 * t76 * x4 + t139 * t51 * t56 * t141 
     #- t135 * t64 * t75 * t144 + t69 * t21 * t25 * t7 * t10 * t96 + t13
     #9 * t51 * t53 * t141 + t48 * t1 * t3 * t95 * t141
      bbbbH21J3 = 0.16D2 / 0.3D1 * wd * (t102 + t160) / t11 / t19

      end function
  
   
 

      doubleprecision function bbbbH22J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t11 = t6 * t9 * x1
      t12 = 0.1D1 - x3
      t13 = s * t3
      t14 = x1 * t7
      t15 = t14 * x3
      t19 = s - t13 * t15 - t13 * t14 * t12
      t20 = t12 * t19
      t21 = 0.1D1 - x1
      t22 = t21 ** 2
      t23 = 0.1D1 - x4
      t24 = t22 * t23
      t29 = t23 ** 2
      t34 = x4 ** 2
      t38 = t1 * t3
      t39 = t38 * t14
      t40 = x3 * t19
      t41 = t7 * t21
      t46 = s - t13 * t41 * x4 - t13 * t41 * t23
      t47 = t46 * z
      t51 = t12 * t46
      t56 = t19 ** 2
      t57 = t56 * t1
      t58 = t57 * t4
      t59 = x1 ** 2
      t60 = t8 * t59
      t61 = x3 ** 2
      t65 = t12 ** 2
      t69 = t2 * t4
      t70 = t8 * x1
      t71 = t69 * t70
      t72 = t21 * x4
      t77 = t56 * t46 * s
      t79 = t1 * t4
      t80 = t79 * t70
      t81 = t19 * t21
      t91 = t57 * t4 * t8
      t92 = x1 * t12
      t93 = t21 * t23
      t97 = t9 * t59
      t98 = t6 * t97
      t99 = x3 * t46
      t103 = cos(x2 * 0.3141592653589793D1)
      t107 = sqrt(x3 * t12 * x4 * t23)
      t110 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t103 * t107
      t111 = t21 * t110
      t115 = t3 * t7
      t122 = t38 * t7
      t123 = x1 * x3
      t124 = t19 * t46
      t130 = t69 * t8
      t131 = t59 * t61
      t135 = t59 * t65
      t140 = t56 * s * t3
      t144 = 0.3D1 * t11 * t20 * t24 * x4 + 0.4D1 * t11 * t20 * t22 * t2
     #9 + t11 * t20 * t22 * t34 + 0.6D1 * t39 * t40 * t47 + 0.6D1 * t39 
     #* t51 * t19 * z - 0.5D1 * t58 * t60 * t61 - 0.3D1 * t58 * t60 * t6
     #5 - 0.2D1 * t71 * t72 * t51 + 0.4D1 * t77 + 0.4D1 * t80 * t51 * t8
     #1 * x4 + 0.2D1 * t80 * t51 * t81 * t23 - 0.2D1 * t91 * t92 * t93 +
     # 0.2D1 * t98 * t99 * t111 - 0.2D1 * t77 * t115 * t72 - 0.2D1 * t77
     # * t115 * t93 + t122 * t123 * t124 + 0.3D1 * t122 * t92 * t124 - 0
     #.6D1 * t130 * t131 * t47 - 0.6D1 * t130 * t135 * t47 - 0.8D1 * t14
     #0 * t14 * t51
      t148 = t79 * t8
      t155 = t4 ** 2
      t157 = t8 ** 2
      t159 = t2 * t155 * t157 * t59
      t172 = t59 * x1
      t175 = t110 ** 2
      t182 = t46 * t21
      t195 = t1 * t5 * t97
      t201 = t57 * t5 * t9
      t238 = 0.3D1 * t195 * t51 * t81 * t110 + 0.3D1 * t201 * t59 * x3 *
     # t111 + 0.3D1 * t201 * t59 * t12 * t111 - 0.2D1 * t57 * t3 * t15 +
     # t2 * t3 * t7 * t123 * t19 - 0.2D1 * t6 * t9 * t172 * t61 * x3 * t
     #19 - t71 * t40 * t72 + 0.4D1 * t195 * t40 * t182 * t110 + 0.2D1 * 
     #t98 * t51 * t111 - 0.2D1 * t71 * t99 * t93 - 0.2D1 * t71 * t99 * t
     #72
      bbbbH22J1 = 0.16D2 / 0.3D1 * wd * (t144 - 0.6D1 * t140 * t14 * t99
     # - 0.6D1 * t148 * t131 * t124 - 0.6D1 * t148 * t135 * t124 - 0.2D1
     # * t159 * t20 * t22 * x4 * t110 - 0.4D1 * t159 * t20 * t24 * t110 
     #+ t2 * t155 * t3 * t157 * t7 * t172 * t20 * t22 * t175 - 0.2D1 * t
     #91 * t123 * t72 + 0.5D1 * t80 * t40 * t182 * t23 + t80 * t40 * t18
     #2 * x4 - 0.2D1 * t71 * t51 * t93 + t238) / t56 / t46

      end function
  
   
 

      doubleprecision function bbbbH22J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t12 = x1 ** 2
      t13 = x1 * t12
      t16 = 0.1D1 - x3
      t17 = s * t3
      t18 = t8 * x1
      t19 = t18 * x3
      t23 = s - t17 * t19 - t17 * t18 * t16
      t24 = t16 * t23
      t25 = 0.1D1 - x1
      t26 = t25 ** 2
      t30 = cos(x2 * 0.3141592653589793D1)
      t32 = 0.1D1 - x4
      t35 = sqrt(x3 * t16 * x4 * t32)
      t38 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t30 * t35
      t39 = t38 ** 2
      t43 = t23 ** 2
      t44 = t43 * t1
      t46 = t44 * t4 * t9
      t47 = x1 * t16
      t48 = t25 * x4
      t52 = t4 * t3
      t53 = t9 * t8
      t55 = t44 * t52 * t53
      t57 = t25 * t38
      t61 = t8 * t25
      t66 = s - t17 * t61 * x4 - t17 * t61 * t32
      t67 = t43 * t66
      t72 = t9 * x1
      t77 = t2 * t4 * t72
      t78 = x3 * t66
      t79 = t25 * t32
      t86 = t1 * t4
      t87 = t86 * t72
      t88 = x3 * t23
      t89 = t66 * t25
      t94 = t2 * t52
      t96 = x3 ** 2
      t107 = x1 * x3
      t113 = t16 * t66
      t120 = t23 * t25
      t124 = t44 * t4
      t125 = t9 * t12
      t129 = -t2 * t5 * t3 * t10 * t8 * t13 * t24 * t26 * t39 - 0.4D1 * 
     #t46 * t47 * t48 - 0.3D1 * t55 * t12 * t16 * t57 - 0.4D1 * t67 * s 
     #- 0.3D1 * t67 * s * t4 * t72 * t57 + 0.2D1 * t77 * t78 * t79 + 0.2
     #D1 * t77 * t78 * t48 - 0.6D1 * t87 * t88 * t89 * t32 + 0.2D1 * t94
     # * t53 * t13 * t96 * x3 * t23 - 0.2D1 * t44 * t3 * t19 - t2 * t3 *
     # t8 * t107 * t23 - t87 * t88 * t89 * x4 + 0.2D1 * t77 * t113 * t79
     # + 0.2D1 * t77 * t113 * t48 - t87 * t113 * t120 * x4 + 0.5D1 * t12
     #4 * t125 * t96
      t130 = t16 ** 2
      t137 = t53 * t12
      t138 = t94 * t137
      t156 = t1 * t3 * t8
      t157 = t66 * t23
      t163 = t86 * t9
      t178 = t94 * t53 * x1
      t179 = t32 ** 2
      t184 = x4 ** 2
      t199 = 0.3D1 * t124 * t125 * t130 - 0.4D1 * t46 * t47 * t79 - 0.2D
     #1 * t138 * t78 * t57 + 0.3D1 * t1 * t52 * t137 * t88 * t89 * t38 -
     # 0.2D1 * t138 * t113 * t57 - 0.3D1 * t55 * t12 * x3 * t57 - t156 *
     # t47 * t157 - 0.3D1 * t156 * t107 * t157 - 0.2D1 * t163 * t12 * t1
     #30 * t157 + 0.10D2 * t43 * s * t3 * t18 * t78 + 0.2D1 * t163 * t12
     # * t96 * t157 + 0.2D1 * t178 * t24 * t26 * t179 + t178 * t24 * t26
     # * t184 + t77 * t88 * t48 + 0.3D1 * t178 * t24 * t26 * t32 * x4 + 
     #0.2D1 * t87 * t113 * t120 * t32
      bbbbH22J2 = 0.16D2 / 0.3D1 * wd * (t129 + t199) / t43 / t66

      end function
  
   
 

      doubleprecision function bbbbH22J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t5 = t4 * x3
      t7 = 0.1D1 - x3
      t10 = s - t2 * t5 - t2 * t4 * t7
      t11 = t10 ** 2
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t1 ** 2
      t15 = t3 ** 2
      t17 = t13 * t14 * t15
      t18 = x1 * x3
      t19 = 0.1D1 - x1
      t20 = t19 * x4
      t23 = t3 * t19
      t26 = 0.1D1 - x4
      t29 = s - t2 * t23 * x4 - t2 * t23 * t26
      t30 = t11 * t29
      t35 = t15 * x1
      t39 = cos(x2 * 0.3141592653589793D1)
      t43 = sqrt(x3 * t7 * x4 * t26)
      t46 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t39 * t43
      t47 = t19 * t46
      t50 = t14 * t1
      t52 = t15 * t3
      t53 = x1 ** 2
      t55 = t12 * t50 * t52 * t53
      t67 = t13 * t14
      t68 = t15 * t53
      t69 = x3 ** 2
      t72 = t12 * s
      t75 = x3 * t10
      t78 = t7 ** 2
      t84 = t12 * t14
      t85 = t84 * t15
      t87 = t29 * t10
      t92 = -t17 * t18 * t20 - 0.2D1 * t30 * s - t30 * s * t14 * t35 * t
     #47 + t55 * t7 * t29 * t10 * t19 * t46 - 0.2D1 * t11 * s * t1 * t4 
     #* x3 * t29 + t67 * t68 * t69 + t72 * t14 * t35 * t75 * t20 + t67 *
     # t68 * t78 + t17 * x1 * t7 * t20 + t85 * t53 * t78 * t87 + t13 * t
     #1 * t5
      t95 = t72 * t50 * t52 * x1
      t96 = t7 * t10
      t97 = t19 ** 2
      t98 = x4 ** 2
      t103 = t29 * t19
      t113 = t14 ** 2
      t114 = t72 * t113
      t115 = t15 ** 2
      t117 = t114 * t115 * t53
      t123 = t97 * t26
      t145 = t53 * x1
      t148 = t46 ** 2
      t161 = -t95 * t96 * t97 * t98 + t84 * t35 * t75 * t103 * t26 + t85
     # * t53 * t69 * t87 - t55 * t75 * t103 * t46 + 0.2D1 * t117 * t96 *
     # t97 * x4 * t46 + 0.2D1 * t117 * t96 * t123 * t46 - 0.2D1 * t13 * 
     #t50 * t52 * t53 * t7 * t47 - t72 * t1 * t3 * t18 * t10 + t12 * t1 
     #* t3 * t18 * t87 - t72 * t113 * t1 * t115 * t3 * t145 * t96 * t97 
     #* t148 - t95 * t96 * t123 * x4 + 0.2D1 * t114 * t115 * t145 * t69 
     #* t10 * t47
      bbbbH22J3 = 0.16D2 / 0.3D1 * wd * (t92 + t161) / t11 / t29

      end function
  
 