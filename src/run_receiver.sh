#!/bin/bash
IP=$1
PUSHER_PORT=$2
RECEIVER_PORT=$3

# RUN:
# ./run_receiver.sh 127.0.0.1 9698 9697

## make this better
FWD=/Users/cwood/PARC/Chunker/build/bin/metis_daemon

SEED=42
NUMBERS=( 100 1000 10000 ) # 100000 1000000 )
MINSIZE=( 256 256  256  256  256  256   512  512  512  512  512 )
MAXSIZE=( 512 1024 2048 4096 8192 16384 1024 2048 4096 8192 16384 )
MINNL=( 2  4  6  8 )
MAXNL=( 10 10 10 10 )
MINNC=( 2 )
MAXNC=( 10 )

WINDOWS=( 1 5 10 50 100 1000 )

FILES="files"

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
                # data_100_256_8192_2_10_2_10_int
                PREFIX=data_${N}_${MINSIZE[$SI]}_${MAXSIZE[$SI]}_${MINNL[$NLI]}_${MAXNL[$NLI]}_${MINNC[$NCI]}_${MAXNC[$NCI]}
                INTFILE=${PREFIX}_int
                DATAFILE=${PREFIX}_data

                for W in "${WINDOWS[@]}"
                do
                    # 1. Start the receiver
                    echo ./receiver ${RECEIVER_PORT} ${DATAFILE}
                    ./receiver ${RECEIVER_PORT} ${DATAFILE} &
                    RPID=$!

                    sleep 2

                    # 2. Start the forwarder
                    echo $FWD --config ~/Projects/ccnx-pktgen/src/metis.cfg --capacity 0
                    $FWD --config ~/Projects/ccnx-pktgen/src/metis.cfg --capacity 0 &
                    FPID=$!

                    # 3. Wait for a second...
                    sleep 2

                    # 4. Now run the pusher to completion
                    echo "./pusher -w ${W} ${IP} ${PUSHER_PORT} ${INTFILE} > ${INTFILE}_${W}.out"
                    ./pusher -w ${W} ${IP} ${PUSHER_PORT} ${INTFILE} > ${INTFILE}_${W}.out

                    # 5. When done, kill the receiver and forwarder
                    kill -SIGTERM $RPID
                    kill -SIGTERM $FPID

                    while ps -p $RPID > /dev/null; do sleep 1; echo "Waiting for receiver to close..."; done;
                    while ps -p $FPID > /dev/null; do sleep 1; echo "Waiting for forwarder to close..."; done;

                    # Take a breath....
                    sleep 3

                    # (save the output file name)
                    echo ${INTFILE}_${W}.out >> $FILES
                done

                NCI=$[$NCI+1]
            done
            NLI=$[$NLI+1]
        done
        SI=$[$SI+1]
    done
done
