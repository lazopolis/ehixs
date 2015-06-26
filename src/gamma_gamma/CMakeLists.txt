file( GLOB QQ2YYG_STU_COEFFS qq2yyg/dirstucol6D/*.cpp )
file( GLOB QQ2YYG_STU_NOBAR_COEFFS qq2yyg/dirstucol6Dnobar/*.cpp )

add_library(gammagamma
            qq2gammagammaX
            gammagamma
            gammagamma_me
            qq2yyg/coeffscol6D
            qq2yyg/coeffsstucol6Ddouble
            qq2yyg/coeffsstucol6Dquad
            qq2yyg/coeffsstucol6Drat
            qq2yyg/coeffsstucol6Dnobar
            ${QQ2YYG_STU_COEFFS}
            ${QQ2YYG_STU_NOBAR_COEFFS}
            )
