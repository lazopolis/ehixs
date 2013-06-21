  
      subroutine bbarbbarH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH21J1  
      doubleprecision bbarbbarH21J2  
      doubleprecision bbarbbarH21J3  
      doubleprecision bbarbbarH22J1  
      doubleprecision bbarbbarH22J2  
      doubleprecision bbarbbarH22J3  
      doubleprecision bbarbbarH2n1e1  
      doubleprecision bbarbbarH2n1e0  
      doubleprecision bbarbbarH2n1em1  
      doubleprecision bbarbbarH2n1em2  
      doubleprecision bbarbbarH2n1em3  
      doubleprecision bbarbbarH2n1em4  
      doubleprecision bbarbbarH2n2e1  
      doubleprecision bbarbbarH2n2e0  
      doubleprecision bbarbbarH2n2em1  
      doubleprecision bbarbbarH2n2em2  
      doubleprecision bbarbbarH2n2em3  
      doubleprecision bbarbbarH2n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH2n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH2n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH2n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH2n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH2n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH2n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
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
      t22 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t26 = lh ** 2
      t27 = 0.180D3 * t26
      t28 = 0.3141592653589793D1 ** 2
      t29 = 0.30D2 * t28
      t32 = t16 ** 2
      t35 = (t27 - t29 + 0.180D3 * t16 * lh + 0.45D2 * t32) * t19
      t36 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t44 = t27 - t29
      t51 = (-0.2884936567583026D3 - 0.120D3 * t26 * lh + 0.60D2 * lh * 
     #t28 - t16 * t44 - 0.90D2 * t32 * lh - 0.15D2 * t32 * t16) * t19
      t52 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t53 = t21 * t52
      t56 = lh * t19
      t57 = 0.1D1 - x3
      t58 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t59 = x3 * t9
      t61 = -t57
      t65 = log(0.4D1 * t59 * t11 * t13 * t61)
      t66 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t68 = t11 * x4
      t69 = t68 * t4
      t72 = log(-0.4D1 * t59 * t69)
      t78 = t19 * t21
      t79 = t72 ** 2
      t84 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t85 = t65 ** 2
      t91 = t44 * t19
      t93 = t21 * (t66 - t52)
      t96 = 0.1D1 / x3
      t99 = x1 ** 2
      t100 = x3 * t99
      t101 = t100 * t9
      t104 = log(-0.4D1 * t101 * t69)
      t110 = log(0.4D1 * t101 * t68 * t4 * t61)
      t118 = 0.1D1 / x1
      t122 = t99 * t9
      t125 = log(-0.4D1 * t122 * t69)
      t131 = t125 ** 2
      t142 = t20 * t21 * t22 / 0.5760D4 + t35 * t21 * t36 / 0.5760D4 + t
     #51 * t53 / 0.5760D4 - (-0.180D3 * t56 * t21 * (t58 - t65 * t66 - t
     #36 + t72 * t52) + 0.90D2 * t78 * (-t79 * t52 / 0.2D1 + t72 * t36 -
     # t22 - t65 * t58 + t84 + t85 * t66 / 0.2D1) + t91 * t93) * t96 / 0
     #.5760D4 - (0.90D2 * t78 * (t104 * t52 - t36 - t110 * t66 + t58) - 
     #0.180D3 * t56 * t93) * t118 * t96 / 0.2880D4 - (-0.180D3 * t56 * t
     #21 * (-t36 + t125 * t52) + 0.90D2 * t78 * (-t131 * t52 / 0.2D1 + t
     #125 * t36 - t22) - t91 * t53) * t118 / 0.2880D4
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t142)
      t145 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4
     #)
      t149 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4
     #)
      t150 = t21 * t149
      t153 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t154 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t163 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4
     #)
      t165 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t173 = t21 * (t154 - t149)
      t207 = t35 * t21 * t145 / 0.5760D4 + t51 * t150 / 0.5760D4 - (-0.1
     #80D3 * t56 * t21 * (t153 - t65 * t154 - t145 + t72 * t149) + 0.90D
     #2 * t78 * (-t79 * t149 / 0.2D1 - t163 + t72 * t145 + t165 - t65 * 
     #t153 + t85 * t154 / 0.2D1) + t91 * t173) * t96 / 0.5760D4 + t20 * 
     #t21 * t163 / 0.5760D4 - (0.90D2 * t78 * (t104 * t149 + t153 - t110
     # * t154 - t145) - 0.180D3 * t56 * t173) * t118 * t96 / 0.2880D4 - 
     #(-0.180D3 * t56 * t21 * (-t145 + t125 * t149) + 0.90D2 * t78 * (t1
     #25 * t145 - t131 * t149 / 0.2D1 - t163) - t91 * t150) * t118 / 0.2
     #880D4
      t208 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t211 = s * t210
      t212 = x1 * t1
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
      t229 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t231 = t226 * t228 * t229
      t232 = t100 * t12
      t233 = t214 ** 2
      t234 = t220 ** 2
      t236 = t13 * t233 * t234
      t239 = log(-0.4D1 * t232 * t236)
      t240 = t239 * t214
      t241 = t220 * t228
      t242 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t243 = t241 * t242
      t248 = t56 * t21
      t250 = t226 * t228 * t242
      t259 = log(-0.4D1 * t122 * t11 * t236)
      t260 = t259 * t214
      t266 = t259 ** 2
      t267 = t266 * t214
      t270 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t278 = t91 * t21
      t283 = -(0.90D2 * t78 * (t231 - t240 * t243) - 0.180D3 * t248 * t2
     #50) * t118 * t96 / 0.2880D4 - (-0.180D3 * t56 * t21 * (t231 - t260
     # * t243) + 0.90D2 * t78 * (t267 * t243 / 0.2D1 + t226 * t228 * t27
     #0 - t260 * t241 * t229) + t278 * t250) * t118 / 0.2880D4
      t284 = FJET(XB1, XB2, s, t213, 0.0D0, -t217, t219, t225, t283)
      t286 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t288 = t226 * t228 * t286
      t289 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t290 = t241 * t289
      t296 = t226 * t228 * t289
      t309 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t321 = -(0.90D2 * t78 * (t288 - t240 * t290) - 0.180D3 * t248 * t2
     #96) * t118 * t96 / 0.2880D4 - (-0.180D3 * t56 * t21 * (t288 - t260
     # * t290) + 0.90D2 * t78 * (t267 * t290 / 0.2D1 + t226 * t228 * t30
     #9 - t260 * t241 * t286) + t278 * t296) * t118 / 0.2880D4
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
      t349 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t353 = t332 ** 2
      t358 = log(0.4D1 * t232 * t13 * t233 * t61 * t353)
      t359 = t358 * t214
      t360 = t332 * t348
      t361 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t371 = 0.90D2 * t78 * (-t346 * t348 * t349 + t359 * t360 * t361) +
     # 0.180D3 * t248 * t346 * t348 * t361
      t375 = FJET(XB1, XB2, s, -t327, t329, -t330, t331, t345, -t371 * t
     #118 * t96 / 0.2880D4)
      t377 = t118 * t96
      t380 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t383 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t393 = 0.90D2 * t78 * (-t346 * t348 * t380 + t359 * t360 * t383) +
     # 0.180D3 * t248 * t346 * t348 * t383
      t397 = FJET(XB1, XB2, s, -t330, t331, -t327, t329, t345, -t393 * t
     #118 * t96 / 0.2880D4)
      bbarbbarH2n1e1 = t143 * t142 + t208 * t207 + t284 * t283 + t322 * 
     #t321 - t375 * t371 * t377 / 0.2880D4 - t397 * t393 * t377 / 0.2880
     #D4

      end function



      doubleprecision function bbarbbarH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = 0.1D1 - x3
      t10 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t26 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t28 = t16 * x4 * t4
      t31 = log(-0.4D1 * t14 * t28)
      t32 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t37 = lh * t6
      t38 = t24 - t32
      t43 = 0.1D1 / x3
      t46 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t13
      t55 = log(-0.4D1 * t52 * t28)
      t60 = t7 * t32
      t66 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t73 = log(-0.4D1 * t13 * t16 * t18)
      t76 = (-0.180D3 * lh - 0.90D2 * t73) * t6
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t73 ** 2
      t89 = (0.180D3 * t80 - 0.30D2 * t82 + 0.180D3 * t73 * lh + 0.45D2 
     #* t86) * t6
      t92 = -(0.90D2 * t8 * (t10 - t23 * t24 - t26 + t31 * t32) - 0.180D
     #3 * t37 * t7 * t38) * t43 / 0.5760D4 - t8 * t38 * t46 * t43 / 0.32
     #D2 - (0.90D2 * t8 * (-t26 + t55 * t32) + 0.180D3 * t37 * t60) * t4
     #6 / 0.2880D4 + t8 * t66 / 0.64D2 + t76 * t7 * t26 / 0.5760D4 + t89
     # * t60 / 0.5760D4
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t92)
      t95 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t96 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t98 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t99 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t104 = t96 - t99
      t119 = t7 * t99
      t125 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4
     #)
      t133 = -(0.90D2 * t8 * (t95 - t23 * t96 - t98 + t31 * t99) - 0.180
     #D3 * t37 * t7 * t104) * t43 / 0.5760D4 - t8 * t104 * t46 * t43 / 0
     #.32D2 - (0.90D2 * t8 * (-t98 + t55 * t99) + 0.180D3 * t37 * t119) 
     #* t46 / 0.2880D4 + t8 * t125 / 0.64D2 + t76 * t7 * t98 / 0.5760D4 
     #+ t89 * t119 / 0.5760D4
      t134 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t133)
      t136 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t137 = s * t136
      t138 = x1 * t1
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
      t156 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t157 = t155 * t156
      t158 = t46 * t43
      t162 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t166 = t146 ** 2
      t167 = t140 ** 2
      t172 = log(-0.4D1 * t52 * t16 * t18 * t166 * t167)
      t173 = t172 * t140
      t174 = t146 * t155
      t180 = t37 * t7
      t187 = -t153 * t157 * t158 / 0.32D2 - (0.90D2 * t8 * (t152 * t155 
     #* t162 - t173 * t174 * t156) - 0.180D3 * t180 * t152 * t157) * t46
     # / 0.2880D4
      t188 = FJET(XB1, XB2, s, t139, 0.0D0, -t143, t145, t151, t187)
      t190 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t191 = t155 * t190
      t195 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t209 = -t153 * t191 * t158 / 0.32D2 - (0.90D2 * t8 * (t152 * t155 
     #* t195 - t173 * t174 * t190) - 0.180D3 * t180 * t152 * t191) * t46
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
      t238 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t9, x4)
      t243 = FJET(XB1, XB2, s, -t215, t217, -t218, t219, t233, t235 * t2
     #37 * t238 * t158 / 0.32D2)
      t245 = t7 * t140
      t247 = t220 * t237
      t253 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, t9, x4)
      t258 = FJET(XB1, XB2, s, -t218, t219, -t215, t217, t233, t235 * t2
     #37 * t253 * t158 / 0.32D2)
      bbarbbarH2n1e0 = t93 * t92 + t134 * t133 + t188 * t187 + t210 * t2
     #09 + t243 * t6 * t245 * t247 * t238 * t46 * t43 / 0.32D2 + t258 * 
     #t6 * t245 * t247 * t253 * t46 * t43 / 0.32D2

      end function



      doubleprecision function bbarbbarH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t22 = log(-0.4D1 * t15 / t16 * x4 * t4)
      t25 = (-0.180D3 * lh - 0.90D2 * t22) * t6
      t26 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t30 = 0.1D1 - x3
      t31 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t30, x4)
      t33 = 0.1D1 / x3
      t37 = 0.1D1 / x1
      t41 = t8 * t9 / 0.64D2 + t25 * t7 * t26 / 0.5760D4 - t8 * (t31 - t
     #26) * t33 / 0.64D2 + t8 * t26 * t37 / 0.32D2
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t41)
      t44 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t47 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t51 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t30, x4)
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
      t82 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t84 = t81 * t82 * t37
      t87 = FJET(XB1, XB2, s, t65, 0.0D0, -t69, t71, t77, -t78 * t84 / 0
     #.32D2)
      t89 = t7 * t66
      t93 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t95 = t81 * t93 * t37
      t98 = FJET(XB1, XB2, s, -t69, t71, t65, 0.0D0, t77, -t78 * t95 / 0
     #.32D2)
      bbarbbarH2n1em1 = t42 * t41 + t60 * t59 - t87 * t6 * t89 * t84 / 0
     #.32D2 - t98 * t6 * t89 * t95 / 0.32D2

      end function



      doubleprecision function bbarbbarH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t8 * t9 / 0.
     #64D2)
      t16 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t19 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t8 * t16 / 0
     #.64D2)
      bbarbbarH2n1em2 = t12 * t6 * t7 * t9 / 0.64D2 + t19 * t6 * t7 * t1
     #6 / 0.64D2
      end function



      doubleprecision function bbarbbarH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      bbarbbarH2n1em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      bbarbbarH2n1em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH2n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
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
      t22 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t26 = lh ** 2
      t27 = 0.180D3 * t26
      t28 = 0.3141592653589793D1 ** 2
      t29 = 0.30D2 * t28
      t32 = t16 ** 2
      t35 = (t27 - t29 + 0.180D3 * t16 * lh + 0.45D2 * t32) * t19
      t36 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t44 = t27 - t29
      t51 = (-0.2884936567583026D3 - 0.120D3 * t26 * lh + 0.60D2 * lh * 
     #t28 - t16 * t44 - 0.90D2 * t32 * lh - 0.15D2 * t32 * t16) * t19
      t52 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t53 = t21 * t52
      t56 = lh * t19
      t57 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t58 = x3 * t9
      t60 = -0.1D1 + x3
      t64 = log(0.4D1 * t58 * t11 * t13 * t60)
      t65 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t67 = t11 * x4
      t68 = t67 * t4
      t71 = log(-0.4D1 * t58 * t68)
      t77 = t19 * t21
      t78 = t71 ** 2
      t83 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t84 = t64 ** 2
      t90 = t44 * t19
      t91 = t65 - t52
      t95 = 0.1D1 / x3
      t98 = x1 ** 2
      t99 = x3 * t98
      t100 = t99 * t9
      t105 = log(0.4D1 * t100 * t67 * t4 * t60)
      t109 = log(-0.4D1 * t100 * t68)
      t119 = 0.1D1 / x1
      t123 = t98 * t9
      t126 = log(-0.4D1 * t123 * t68)
      t132 = t126 ** 2
      t143 = t20 * t21 * t22 / 0.5760D4 + t35 * t21 * t36 / 0.5760D4 + t
     #51 * t53 / 0.5760D4 - (-0.180D3 * t56 * t21 * (t57 - t64 * t65 - t
     #36 + t71 * t52) + 0.90D2 * t77 * (-t78 * t52 / 0.2D1 + t71 * t36 -
     # t22 - t64 * t57 + t83 + t84 * t65 / 0.2D1) + t90 * t21 * t91) * t
     #95 / 0.5760D4 + (0.90D2 * t77 * (-t57 + t105 * t65 + t36 - t109 * 
     #t52) + 0.180D3 * t56 * t21 * t91) * t119 * t95 / 0.2880D4 + (-0.18
     #0D3 * t56 * t21 * (t36 - t126 * t52) + 0.90D2 * t77 * (t132 * t52 
     #/ 0.2D1 - t126 * t36 + t22) + t90 * t53) * t119 / 0.2880D4
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
      t174 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t175 = t173 * t174
      t177 = t150 * t156
      t178 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t180 = t177 * t172 * t178
      t184 = t56 * t21
      t186 = t177 * t172 * t174
      t195 = log(-0.4D1 * t123 * t11 * t166)
      t196 = t195 * t150
      t204 = t195 ** 2
      t205 = t204 * t150
      t208 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t214 = t90 * t21
      t219 = (0.90D2 * t77 * (t170 * t175 - t180) + 0.180D3 * t184 * t18
     #6) * t119 * t95 / 0.2880D4 + (-0.180D3 * t56 * t21 * (t196 * t175 
     #- t180) + 0.90D2 * t77 * (t196 * t173 * t178 - t205 * t175 / 0.2D1
     # - t177 * t172 * t208) - t214 * t186) * t119 / 0.2880D4
      t220 = FJET(XB1, XB2, s, 0.0D0, t149, -t153, t155, -t161, t219)
      t222 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t226 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t227 = t21 * t226
      t230 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t231 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t240 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t242 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t249 = t231 - t226
      t286 = t35 * t21 * t222 / 0.5760D4 + t51 * t227 / 0.5760D4 - (-0.1
     #80D3 * t56 * t21 * (t230 - t64 * t231 - t222 + t71 * t226) + 0.90D
     #2 * t77 * (-t78 * t226 / 0.2D1 - t240 + t71 * t222 + t242 - t64 * 
     #t230 + t84 * t231 / 0.2D1) + t90 * t21 * t249) * t95 / 0.5760D4 + 
     #t20 * t21 * t240 / 0.5760D4 + (0.90D2 * t77 * (t105 * t231 - t109 
     #* t226 - t230 + t222) + 0.180D3 * t56 * t21 * t249) * t119 * t95 /
     # 0.2880D4 + (-0.180D3 * t56 * t21 * (-t126 * t226 + t222) + 0.90D2
     # * t77 * (-t126 * t222 + t132 * t226 / 0.2D1 + t240) + t90 * t227)
     # * t119 / 0.2880D4
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
      t314 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t318 = t297 ** 2
      t323 = log(0.4D1 * t162 * t13 * t163 * t60 * t318)
      t324 = t323 * t150
      t325 = t297 * t313
      t326 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t336 = 0.90D2 * t77 * (t311 * t313 * t314 - t324 * t325 * t326) - 
     #0.180D3 * t184 * t311 * t313 * t326
      t340 = FJET(XB1, XB2, s, t292, -t294, -t295, t296, t310, t336 * t1
     #19 * t95 / 0.2880D4)
      t342 = t119 * t95
      t345 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t347 = t177 * t172 * t345
      t348 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t349 = t173 * t348
      t355 = t177 * t172 * t348
      t368 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t380 = (0.90D2 * t77 * (-t347 + t170 * t349) + 0.180D3 * t184 * t3
     #55) * t119 * t95 / 0.2880D4 + (-0.180D3 * t56 * t21 * (-t347 + t19
     #6 * t349) + 0.90D2 * t77 * (-t205 * t349 / 0.2D1 - t177 * t172 * t
     #368 + t196 * t173 * t345) - t214 * t355) * t119 / 0.2880D4
      t381 = FJET(XB1, XB2, s, -t153, t155, 0.0D0, t149, -t161, t380)
      t383 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t386 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t396 = 0.90D2 * t77 * (t311 * t313 * t383 - t324 * t325 * t386) - 
     #0.180D3 * t184 * t311 * t313 * t386
      t400 = FJET(XB1, XB2, s, -t295, t296, t292, -t294, t310, t396 * t1
     #19 * t95 / 0.2880D4)
      bbarbbarH2n2e1 = t144 * t143 + t220 * t219 + t287 * t286 + t340 * 
     #t336 * t342 / 0.2880D4 + t381 * t380 + t400 * t396 * t342 / 0.2880
     #D4

      end function



      doubleprecision function bbarbbarH2n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t25 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t27 = t15 * x4 * t4
      t30 = log(-0.4D1 * t13 * t27)
      t31 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t36 = lh * t6
      t37 = t23 - t31
      t42 = 0.1D1 / x3
      t46 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t12
      t55 = log(-0.4D1 * t52 * t27)
      t60 = t7 * t31
      t66 = bbarbbarH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t73 = log(-0.4D1 * t12 * t15 * t17)
      t76 = (-0.180D3 * lh - 0.90D2 * t73) * t6
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t73 ** 2
      t89 = (0.180D3 * t80 - 0.30D2 * t82 + 0.180D3 * t73 * lh + 0.45D2 
     #* t86) * t6
      t92 = -(0.90D2 * t8 * (t9 - t22 * t23 - t25 + t30 * t31) - 0.180D3
     # * t36 * t7 * t37) * t42 / 0.5760D4 - t8 * t37 * t46 * t42 / 0.32D
     #2 + (0.90D2 * t8 * (t25 - t55 * t31) - 0.180D3 * t36 * t60) * t46 
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
      t115 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t116 = t114 * t115
      t117 = t46 * t42
      t122 = t105 ** 2
      t123 = t99 ** 2
      t128 = log(-0.4D1 * t52 * t15 * t17 * t122 * t123)
      t129 = t128 * t99
      t130 = t105 * t114
      t133 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t139 = t36 * t7
      t146 = -t112 * t116 * t117 / 0.32D2 + (0.90D2 * t8 * (t129 * t130 
     #* t115 - t111 * t114 * t133) + 0.180D3 * t139 * t111 * t116) * t46
     # / 0.2880D4
      t147 = FJET(XB1, XB2, s, 0.0D0, t98, -t102, t104, -t110, t146)
      t149 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t150 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t152 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t153 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t158 = t150 - t153
      t174 = t7 * t153
      t180 = bbarbbarH22J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t188 = -(0.90D2 * t8 * (t149 - t22 * t150 - t152 + t30 * t153) - 0
     #.180D3 * t36 * t7 * t158) * t42 / 0.5760D4 - t8 * t158 * t46 * t42
     # / 0.32D2 + (0.90D2 * t8 * (-t55 * t153 + t152) - 0.180D3 * t36 * 
     #t174) * t46 / 0.2880D4 + t8 * t180 / 0.64D2 + t76 * t7 * t152 / 0.
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
      t217 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t222 = FJET(XB1, XB2, s, t194, -t196, -t197, t198, t212, t214 * t2
     #16 * t217 * t117 / 0.32D2)
      t224 = t7 * t99
      t226 = t199 * t216
      t232 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t233 = t114 * t232
      t237 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t251 = -t112 * t233 * t117 / 0.32D2 + (0.90D2 * t8 * (-t111 * t114
     # * t237 + t129 * t130 * t232) + 0.180D3 * t139 * t111 * t233) * t4
     #6 / 0.2880D4
      t252 = FJET(XB1, XB2, s, -t102, t104, 0.0D0, t98, -t110, t251)
      t254 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t259 = FJET(XB1, XB2, s, -t197, t198, t194, -t196, t212, t214 * t2
     #16 * t254 * t117 / 0.32D2)
      bbarbbarH2n2e0 = t93 * t92 + t147 * t146 + t189 * t188 + t222 * t6
     # * t224 * t226 * t217 * t46 * t42 / 0.32D2 + t252 * t251 + t259 * 
     #t6 * t224 * t226 * t254 * t46 * t42 / 0.32D2

      end function



      doubleprecision function bbarbbarH2n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbarbbarH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t22 = log(-0.4D1 * t15 / t16 * x4 * t4)
      t25 = (-0.180D3 * lh - 0.90D2 * t22) * t6
      t26 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t30 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
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
      t63 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t65 = t62 * t63 * t36
      t68 = FJET(XB1, XB2, s, 0.0D0, t46, -t50, t52, -t58, -t59 * t65 / 
     #0.32D2)
      t70 = t7 * t47
      t74 = bbarbbarH22J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t77 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t81 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t89 = t8 * t74 / 0.64D2 + t25 * t7 * t77 / 0.5760D4 - t8 * (t81 - 
     #t77) * t32 / 0.64D2 + t8 * t77 * t36 / 0.32D2
      t90 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t94 = t62 * t92 * t36
      t97 = FJET(XB1, XB2, s, -t50, t52, 0.0D0, t46, -t58, -t59 * t94 / 
     #0.32D2)
      bbarbbarH2n2em1 = t41 * t40 - t68 * t6 * t70 * t65 / 0.32D2 + t90 
     #* t89 - t97 * t6 * t70 * t94 / 0.32D2

      end function



      doubleprecision function bbarbbarH2n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bbarbbarH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t8 * t9 / 0.
     #64D2)
      t16 = bbarbbarH22J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t19 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t8 * t16 / 0
     #.64D2)
      bbarbbarH2n2em2 = t12 * t6 * t7 * t9 / 0.64D2 + t19 * t6 * t7 * t1
     #6 / 0.64D2

      end function



      doubleprecision function bbarbbarH2n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      bbarbbarH2n2em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH2n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH21J1
      doubleprecision bbarbbarH21J2
      doubleprecision bbarbbarH21J3
      doubleprecision bbarbbarH22J1
      doubleprecision bbarbbarH22J2
      doubleprecision bbarbbarH22J3
      bbarbbarH2n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbarbbarH21J1
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
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = t10 ** 2
      t12 = s ** 2
      t13 = t12 * s
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 * t1
      t17 = t3 ** 2
      t18 = t17 * t3
      t19 = t16 * t18
      t20 = t14 * t19
      t21 = x1 * t7
      t22 = 0.1D1 - x1
      t23 = t22 ** 2
      t24 = x4 ** 2
      t26 = t21 * t23 * t24
      t29 = x1 ** 2
      t30 = t7 ** 2
      t31 = t29 * t30
      t32 = t22 * x4
      t33 = t31 * t32
      t36 = 0.1D1 - x4
      t37 = t36 ** 2
      t39 = t21 * t23 * t37
      t41 = t12 ** 2
      t42 = t3 * t22
      t47 = s - t2 * t42 * x4 - t2 * t42 * t36
      t48 = t41 * t47
      t49 = t48 * t19
      t52 = t10 * t13
      t54 = t52 * t47 * t15
      t55 = t17 * x1
      t56 = t7 * t22
      t58 = t55 * t56 * t36
      t60 = t15 ** 2
      t61 = t17 ** 2
      t62 = t60 * t61
      t63 = t14 * t62
      t64 = t29 * t7
      t69 = cos(x2 * 0.3141592653589793D1)
      t73 = sqrt(x3 * t7 * x4 * t36)
      t76 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t69 * t73
      t78 = t64 * t23 * x4 * t76
      t81 = t10 * t41
      t82 = t81 * t62
      t83 = t23 * t36
      t85 = t64 * t83 * t76
      t88 = t81 * t19
      t91 = t11 * t47
      t92 = t91 * t12
      t93 = t15 * t17
      t94 = t93 * t31
      t97 = t52 * t47
      t100 = t1 * t3 * t21
      t105 = t4 * t7 * z
      t112 = t18 * t29 * t56 * t76
      t115 = t22 * t76
      t125 = t60 * t1 * t61 * t3
      t127 = t29 * x1
      t129 = t76 ** 2
      t131 = t127 * t7 * t23 * t129
      t133 = -0.2D1 * t20 * t26 + 0.2D1 * t20 * t33 + t20 * t39 + 0.2D1 
     #* t49 * t33 + t54 * t58 - 0.2D1 * t63 * t78 - 0.2D1 * t82 * t85 + 
     #0.2D1 * t88 * t39 + 0.4D1 * t92 * t94 - t97 * t94 - 0.6D1 * t92 * 
     #t100 + 0.6D1 * t48 * t1 * t105 - 0.2D1 * t91 * t12 * t16 * t112 + 
     #0.5D1 * t20 * t64 * t115 + 0.2D1 * t88 * t21 * t83 * x4 + t81 * t1
     #25 * t131
      t139 = t127 * t30 * t115
      t142 = t14 * t93
      t147 = t22 * t36
      t161 = t31 * t147
      t189 = -0.2D1 * t14 * t125 * t131 - 0.2D1 * t48 * t62 * t139 + 0.2
     #D1 * t142 * t21 * t32 + t88 * t26 - 0.2D1 * t142 * t21 * t147 - 0.
     #4D1 * t54 * t55 * t56 * x4 + 0.2D1 * t91 * t12 * t15 * t58 + 0.6D1
     # * t97 * t100 + 0.2D1 * t49 * t161 + 0.2D1 * t20 * t161 - 0.3D1 * 
     #t14 * t15 * t17 * t29 * t30 - t14 * t16 * t18 * t127 * t30 * t7 - 
     #t63 * t85 - 0.5D1 * t52 * t47 * t16 * t112 + 0.2D1 * t63 * t139 - 
     #0.2D1 * t82 * t78 + 0.6D1 * t52 * t47 * t1 * t105
      bbarbbarH21J1 = 0.16D2 / 0.3D1 * wd * (t133 + t189) / t11 / s / t4
     #7

      end function
  
   
 

      doubleprecision function bbarbbarH21J2
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
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t11 ** 2
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 * t1
      t16 = t3 ** 2
      t17 = t16 * t3
      t18 = t15 * t17
      t19 = t13 * t18
      t20 = x1 * t7
      t21 = 0.1D1 - x1
      t22 = t21 ** 2
      t23 = x4 ** 2
      t28 = t14 ** 2
      t30 = t16 ** 2
      t34 = x1 ** 2
      t35 = t34 * x1
      t40 = cos(x2 * 0.3141592653589793D1)
      t42 = 0.1D1 - x4
      t45 = sqrt(x3 * t7 * x4 * t42)
      t48 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t40 * t45
      t49 = t48 ** 2
      t53 = t10 ** 2
      t54 = t3 * t21
      t59 = s - t2 * t54 * x4 - t2 * t54 * t42
      t60 = t53 * t59
      t63 = t16 * x1
      t64 = t7 * t21
      t66 = t63 * t64 * x4
      t69 = t11 * s
      t70 = t53 * t69
      t71 = t28 * t30
      t73 = t34 * t7
      t74 = t22 * t42
      t76 = t73 * t74 * t48
      t78 = t12 * t59
      t80 = t7 ** 2
      t82 = t21 * t48
      t91 = t10 * t69
      t93 = t91 * t59 * t14
      t96 = t13 * t71
      t107 = t70 * t18
      t108 = t42 ** 2
      t110 = t20 * t22 * t108
      t115 = t91 * t59
      t116 = t14 * t16
      t117 = t34 * t80
      t118 = t116 * t117
      t121 = -0.3D1 * t19 * t20 * t22 * t23 - t13 * t28 * t1 * t30 * t3 
     #* t35 * t7 * t22 * t49 - 0.3D1 * t60 * t11 * t14 * t66 + t70 * t71
     # * t76 + 0.2D1 * t78 * t71 * t35 * t80 * t82 + t70 * t15 * t17 * t
     #35 * t80 * t7 - 0.3D1 * t93 * t66 + 0.4D1 * t96 * t73 * t22 * x4 *
     # t48 + 0.3D1 * t70 * t14 * t16 * t34 * t80 + t107 * t110 - t93 * t
     #63 * t64 * t42 + 0.3D1 * t115 * t118
      t123 = t21 * x4
      t129 = t78 * t18
      t131 = t117 * t21 * t42
      t151 = t60 * t11
      t159 = t1 * t3 * t20
      t164 = 0.2D1 * t70 * t116 * t20 * t123 - 0.2D1 * t19 * t110 - 0.2D
     #1 * t129 * t131 - 0.2D1 * t107 * t131 + 0.2D1 * t96 * t76 - 0.5D1 
     #* t107 * t73 * t82 + 0.6D1 * t91 * t59 * t15 * t17 * t34 * t64 * t
     #48 - 0.2D1 * t129 * t117 * t123 - 0.4D1 * t151 * t118 - 0.4D1 * t1
     #9 * t20 * t74 * x4 + 0.10D2 * t151 * t159 - 0.2D1 * t115 * t159
      bbarbbarH21J2 = 0.16D2 / 0.3D1 * wd * (t121 + t164) / t53 / s / t5
     #9

      end function
  
   
 

      doubleprecision function bbarbbarH21J3
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
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = t10 ** 2
      t12 = 0.1D1 - x1
      t13 = t3 * t12
      t16 = 0.1D1 - x4
      t19 = s - t2 * t13 * x4 - t2 * t13 * t16
      t20 = t11 * t19
      t21 = s ** 2
      t22 = t1 ** 2
      t25 = t3 ** 2
      t27 = t7 * t12
      t29 = t25 * x1 * t27 * x4
      t31 = t21 * s
      t32 = t10 * t31
      t33 = t32 * t19
      t34 = t22 * t25
      t35 = x1 ** 2
      t36 = t7 ** 2
      t37 = t35 * t36
      t38 = t34 * t37
      t40 = t11 * t31
      t41 = t22 * t1
      t42 = t25 * t3
      t44 = t40 * t41 * t42
      t49 = cos(x2 * 0.3141592653589793D1)
      t53 = sqrt(x3 * t7 * x4 * t16)
      t56 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t49 * t53
      t60 = t20 * t21
      t72 = t40 * t34
      t73 = x1 * t7
      t74 = t12 * t16
      t84 = t1 * t3 * t73
      t93 = -t20 * t21 * t22 * t29 - t33 * t38 - t44 * t35 * t7 * t12 * 
     #t56 - 0.2D1 * t60 * t38 - t32 * t19 * t41 * t42 * t35 * t27 * t56 
     #+ t32 * t19 * t22 * t29 - t72 * t73 * t74 + t40 * t41 * t42 * t35 
     #* x1 * t36 * t7 - t33 * t84 - t72 * t73 * t12 * x4 - 0.2D1 * t60 *
     # t84 - t44 * t37 * t74
      bbarbbarH21J3 = 0.16D2 / 0.3D1 * wd * t93 / t11 / s / t19

      end function
  
   
 

      doubleprecision function bbarbbarH22J1
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
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t5 * t7
      t9 = x1 ** 2
      t10 = x3 ** 2
      t11 = t9 * t10
      t12 = s * t3
      t13 = t6 * x1
      t16 = 0.1D1 - x3
      t19 = s - t12 * t13 * x3 - t12 * t13 * t16
      t20 = t19 ** 2
      t24 = t4 * t3
      t25 = t2 * t24
      t26 = t6 * t7
      t28 = t9 * x1
      t33 = t1 ** 2
      t34 = t33 * t24
      t35 = t26 * x1
      t36 = t34 * t35
      t37 = x3 * t19
      t38 = 0.1D1 - x1
      t39 = t38 ** 2
      t40 = x4 ** 2
      t41 = t39 * t40
      t45 = x1 * t7
      t46 = t5 * t45
      t47 = x3 * t20
      t48 = t38 * x4
      t52 = t25 * t35
      t57 = t6 * t38
      t60 = 0.1D1 - x4
      t63 = s - t12 * t57 * x4 - t12 * t57 * t60
      t64 = t63 * z
      t69 = t26 * t9
      t71 = t63 * t38
      t75 = cos(x2 * 0.3141592653589793D1)
      t79 = sqrt(x3 * t16 * x4 * t60)
      t82 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t75 * t79
      t83 = t71 * t82
      t87 = t4 ** 2
      t88 = t87 * t3
      t90 = t7 ** 2
      t92 = t90 * t6 * t28
      t94 = t82 ** 2
      t95 = t39 * t94
      t99 = t2 * t87
      t100 = t90 * t28
      t102 = t10 * t20
      t103 = t38 * t82
      t111 = t1 * t4
      t113 = t20 * t63
      t123 = t34 * t69
      t124 = t10 * t63
      t128 = t60 * t38
      t132 = t71 * x4
      t139 = -0.3D1 * t8 * t11 * t20 - t25 * t26 * t28 * t10 * x3 * t20 
     #+ 0.2D1 * t36 * t37 * t41 - 0.2D1 * t46 * t47 * t48 + t52 * t47 * 
     #t41 - 0.6D1 * t33 * t4 * t7 * t11 * t64 - 0.2D1 * t1 * t24 * t69 *
     # t47 * t83 - 0.2D1 * t2 * t88 * t92 * t47 * t95 + 0.2D1 * t99 * t1
     #00 * t102 * t103 - 0.7D1 * t8 * t11 * t19 * t63 + 0.4D1 * t111 * t
     #7 * t11 * t113 - 0.6D1 * t1 * t3 * t6 * x1 * x3 * t113 + 0.2D1 * t
     #123 * t124 * t48 + 0.2D1 * t123 * t124 * t128 + t46 * t37 * t132 +
     # 0.2D1 * t46 * t37 * t71 * t60
      t140 = t90 * t9
      t141 = t99 * t140
      t143 = t39 * x4 * t82
      t146 = t33 * t87
      t147 = t146 * t140
      t151 = t39 * t60
      t152 = t151 * t82
      t156 = t60 ** 2
      t157 = t39 * t156
      t161 = t25 * t69
      t202 = -t141 * t47 * t143 - 0.2D1 * t147 * t37 * t143 - 0.2D1 * t1
     #41 * t47 * t152 - 0.2D1 * t52 * t47 * t157 + 0.2D1 * t161 * t102 *
     # t128 + 0.5D1 * t161 * t47 * t103 + t33 * t88 * t92 * t37 * t95 + 
     #0.2D1 * t161 * t102 * t48 + t36 * t37 * t157 + 0.6D1 * t2 * t3 * t
     #13 * t37 * t64 + 0.2D1 * t111 * t45 * t47 * t132 + 0.2D1 * t36 * t
     #37 * t151 * x4 - 0.2D1 * t147 * t37 * t152 - 0.2D1 * t146 * t100 *
     # t124 * t103 + t161 * t37 * t83 + 0.2D1 * t46 * t47 * t128
      bbarbbarH22J1 = 0.16D2 / 0.3D1 * wd * (t139 + t202) / t20 / s / t6
     #3

      end function
  
   
 

      doubleprecision function bbarbbarH22J2
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
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = x1 * t7
      t9 = t5 * t8
      t10 = s * t3
      t11 = t6 * x1
      t14 = 0.1D1 - x3
      t17 = s - t10 * t11 * x3 - t10 * t11 * t14
      t18 = x3 * t17
      t19 = 0.1D1 - x1
      t20 = t6 * t19
      t23 = 0.1D1 - x4
      t26 = s - t10 * t20 * x4 - t10 * t20 * t23
      t27 = t26 * t19
      t28 = t27 * t23
      t32 = t1 ** 2
      t33 = t4 * t3
      t34 = t32 * t33
      t35 = t6 * t7
      t36 = x1 ** 2
      t37 = t35 * t36
      t38 = t34 * t37
      t39 = x3 ** 2
      t40 = t39 * t26
      t41 = t19 * t23
      t45 = t19 * x4
      t49 = t5 * t7
      t50 = t36 * t39
      t51 = t17 * t26
      t57 = x1 * x3
      t58 = t17 ** 2
      t59 = t58 * t26
      t69 = t4 ** 2
      t70 = t32 * t69
      t71 = t7 ** 2
      t72 = t71 * t36
      t73 = t70 * t72
      t74 = t19 ** 2
      t79 = cos(x2 * 0.3141592653589793D1)
      t83 = sqrt(x3 * t14 * x4 * t23)
      t86 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t79 * t83
      t87 = t74 * x4 * t86
      t91 = t2 * t33
      t92 = t91 * t37
      t98 = t36 * x1
      t103 = x3 * t58
      t112 = -0.3D1 * t9 * t18 * t28 - 0.2D1 * t38 * t40 * t41 - 0.2D1 *
     # t38 * t40 * t45 + 0.3D1 * t49 * t50 * t51 + 0.10D2 * t1 * t3 * t6
     # * t57 * t59 - t9 * t18 * t27 * x4 + 0.3D1 * t49 * t50 * t58 + 0.2
     #D1 * t73 * t18 * t87 + 0.6D1 * t92 * t18 * t27 * t86 + t91 * t35 *
     # t98 * t39 * x3 * t58 + 0.2D1 * t9 * t103 * t41 - 0.2D1 * t2 * t3 
     #* t6 * t57 * t51
      t113 = t74 * t23
      t123 = t86 ** 2
      t131 = t35 * x1
      t133 = x4 ** 2
      t134 = t74 * t133
      t139 = t19 * t86
      t143 = t34 * t131
      t144 = t23 ** 2
      t149 = t1 * t4
      t172 = 0.4D1 * t73 * t18 * t113 * t86 - t32 * t69 * t3 * t71 * t6 
     #* t98 * t18 * t74 * t123 - 0.2D1 * t92 * t39 * t58 * t45 + t91 * t
     #131 * t103 * t134 + 0.2D1 * t70 * t71 * t98 * t40 * t139 - 0.3D1 *
     # t143 * t18 * t74 * t144 - 0.4D1 * t149 * t7 * t50 * t59 + t2 * t6
     #9 * t72 * t103 * t87 - 0.5D1 * t92 * t103 * t139 - 0.3D1 * t149 * 
     #t8 * t103 * t28 - 0.4D1 * t143 * t18 * t113 * x4 - 0.2D1 * t143 * 
     #t18 * t134
      bbarbbarH22J2 = 0.16D2 / 0.3D1 * wd * (t112 + t172) / t58 / s / t2
     #6

      end function
  
   
 

      doubleprecision function bbarbbarH22J3
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
      t6 = t2 * t4 * t3
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = x1 ** 2
      t12 = t6 * t9 * t10
      t13 = s * t3
      t14 = x1 * t7
      t17 = 0.1D1 - x3
      t20 = s - t13 * t14 * x3 - t13 * t14 * t17
      t21 = t20 ** 2
      t22 = x3 * t21
      t23 = 0.1D1 - x1
      t27 = cos(x2 * 0.3141592653589793D1)
      t29 = 0.1D1 - x4
      t32 = sqrt(x3 * t17 * x4 * t29)
      t35 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t27 * t32
      t41 = x3 ** 2
      t46 = t2 * t4
      t47 = t8 * x1
      t48 = t46 * t47
      t49 = x3 * t20
      t50 = t7 * t23
      t55 = s - t13 * t50 * x4 - t13 * t50 * t29
      t56 = t55 * t23
      t57 = t56 * t29
      t60 = t23 * x4
      t64 = t10 * t41
      t65 = t20 * t55
      t71 = t1 * t4
      t73 = t21 * t55
      t79 = x1 * x3
      t96 = -t12 * t22 * t23 * t35 + t6 * t9 * t10 * x1 * t41 * x3 * t21
     # + t48 * t49 * t57 - t48 * t22 * t60 - t46 * t8 * t64 * t65 - t12 
     #* t41 * t21 * t60 - 0.2D1 * t71 * t8 * t64 * t73 - 0.2D1 * t1 * t3
     # * t7 * t79 * t73 - t12 * t49 * t56 * t35 - t2 * t3 * t7 * t79 * t
     #65 - t48 * t22 * t23 * t29 - t71 * t47 * t22 * t57
      bbarbbarH22J3 = 0.16D2 / 0.3D1 * wd * t96 / t21 / s / t55

      end function
  
 