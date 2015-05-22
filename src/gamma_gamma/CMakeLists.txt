file( GLOB EXACT_COEFFS qq2yyg/dircol6DE/*.cpp )

add_library(gammagamma
            qq2gammagammaX
            gammagamma
            gammagamma_me
            qq2yyg/coeffs4D
            qq2yyg/coeffs6D
            qq2yyg/coeffscol4D
            qq2yyg/coeffscol6D
            qq2yyg/coeffscol6DE
            qq2yyg/coeffsz6D
            qq2yyg/coeffszcol6D
            ${EXACT_COEFFS}
            )
