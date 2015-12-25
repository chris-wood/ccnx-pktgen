#!/bin/bash

SEED=42
NUMBERS=( 100 1000 10000 100000 1000000 )
MINSIZE=( 256 256  256  256  256  256   512  512  512  512  512 )
MAXSIZE=( 512 1024 2048 4096 8192 16384 1024 2048 4096 8192 16384 )
MINNL=( 2  4  6  8 )
MAXNL=( 10 10 10 10 )
MINNC=( 2 )
MAXNC=( 10 )

for N in "${NUMBERS[@]}"
do
    SI=0
    while [ $SI -lt ${#MINSIZE[@]} ]
    do
        NLI=0
        while [ $NLI -lt ${#MINNL[@]} ]
        do
            NCI=0
            while [ $NCI -lt ${#MINNC[@]} ]
            do
                # echo $N ${MINSIZE[$SI]} $SI ${MINNL[$NLI]} $NLI ${MINNC[$NCI]} $NCI
                # ${MINSIZE[$SI]}
                # ${MAXSIZE[$SI]}
                # ${MINNL[$NLI]}
                # ${MAXNL[$NLI]}
                # ${MINNC[$NCI]}
                # ${MAXNC[$NCI]}

                PREFIX=data_${N}_${MINSIZE[$SI]}_${MAXSIZE[$SI]}_${MINNL[$NLI]}_${MAXNL[$NLI]}_${MINNC[$NCI]}_${MAXNC[$NCI]}

                echo runhaskell generator.hs $PREFIX $SEED $N ${MINNL[$NLI]} ${MAXNL[$NLI]} ${MINNC[$NCI]} ${MAXNC[$NCI]} ${MINSIZE[$SI]} ${MAXSIZE[$SI]}
                runhaskell generator.hs $PREFIX $SEED $N ${MINNL[$NLI]} ${MAXNL[$NLI]} ${MINNC[$NCI]} ${MAXNC[$NCI]} ${MINSIZE[$SI]} ${MAXSIZE[$SI]}

                NCI=$[$NCI+1]
            done
            NLI=$[$NLI+1]
        done
        SI=$[$SI+1]
    done
done



# runhaskell generator.hs data 42 100 2 10 1 10 256 4096
