#!/bin/bash

#defaults
NUM_CORES=1
debug=0
help=0
path="/home/lewis/programs/OpenFl/"


while [[ $# -gt 0 ]];
do
key="$1"

case $key in
    -n)
    NUM_CORES="$2"
    shift # past argument
    ;;
    -m)
    NUM_CORES="0"
    shift # past argument
    ;;
    -d)
    NUM_CORES="1"
    debug='1'
    shift #past argument
    ;;
    -h)
    Help='1'
    shift #past argument
    ;;
    -p)
    path="$path$2"
    shift
    ;;
    *)
        # unknown option
    ;;
esac
shift # past argument or value
done

if [ "$Help" = 1 ];then
  echo 'Usage: ./install.sh [option]'
  echo 'Compile and run mcgrid'
  echo 
  echo '   -n, -n #of cores           compiles and run code on # cores. Defualt is 1 core'
  echo '   -d                         compiles and runs code with debug flags on 1 core'
  echo '   -m                         compiles the code'
  exit 0
fi
cd src
set -e

if [ "$debug" = 1 ];then
    make clean && make debug
elif [ "$NUM_CORES" = 0 ];then
    make clean && make build
else
    make clean && make 
fi

cd ..
if [ ! -d "data" ]; then
    mkdir "data"
    mkdir "data/jmean"
    mkdir "data/im"
    mkdir "data/deposit"
fi

if [ ! -d "build" ]; then
   mkdir "build"
fi
cd build
ndirec="$(pwd)"
cd ..
if [ ! -d "bin" ]; then
   mkdir "bin"
fi
cd bin
bdirc="$(pwd)"
cd ..
cd src


for i in *; do
   if [ "${i}" != "${i%.mod}" ];then
      cp "${i}" "$ndirec"
   fi
   if [ "${i}" != "${i%.o}" ];then
      mv "${i}" "$ndirec"
   fi
done


if [ "$NUM_CORES" = "0" ]; then #just make code
    exit 0
fi

mv fortgl "$bdirc" && echo " "&& echo "*****Install complete*****" && echo " "

clear
cd ../bin

if [ "$NUM_CORES" = "1" ]; then
    ./fortgl "$path"
else
  echo
fi