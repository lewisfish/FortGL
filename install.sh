#!/bin/bash

function showhelp
{

  echo 'Usage: ./install.sh [option] [option]...'
  echo 'Compile and run mcgrid.'
  echo 
  echo '   -h, --help            Shows this dialog.'
  echo '   -n, --cores           Compiles and run code on n cores. Default is 1 core.'
  echo '   -d, --debug           Compiles and runs code with debug flags on n core.'
  echo '   -m, --make            Compiles the code with warning enabled.'
  echo '   -p, --path            Path to mesh that is to be rendered.'

}

function makebuild
{
  if [ "$comp" = 'gnu' ];then
      string="FCOMP=mpifort"
    elif [ "$comp" = 'intel' ];then
      string="FCOMP=mpiifort"
  fi

  if [ "$debug" = 1 ];then
    make clean && make debug $string
elif [ "$NUM_CORES" = 0 ];then
    make clean && make build $string
else
    if [ "$comp" = 'gnu' ];then
      make clean && make $string
    elif [ "$comp" = 'intel' ];then
      make clean && make $string
    fi
fi
}

function createdirs
{
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
}

function run
{
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
  echo $(pwd)
  mv fortgl "$bdirc" && echo " "&& echo "*****Install complete*****" && echo " "

  # clear
  cd ../bin

  if [ "$NUM_CORES" = "1" ]; then
      ./fortgl $path
  else
    exit 0
    # if [ $comp = 'gnu' ];then
    #   /usr/local/bin/mpirun -n $NUM_CORES ./fortgl
    # elif [ $comp = 'intel' ];then
    #   mpirun -n $NUM_CORES ./fortgl
    # fi
  fi
}

#defaults
NUM_CORES=1
debug=0
help=0
comp="gnu"
path="$(pwd)"
path="$path/"

set -e

createdirs

while [ "$1" != "" ]; do
    case $1 in
        -n | --cores )          NUM_CORES=$2
                                ;;
        -p | --path )           path="$path$2"
                                ;;
        -c | --comp )           comp=$2
                                ;;
        -h | --help )           showhelp
                                exit
                                ;;
        -m | --make )           NUM_CORES=0
                                makebuild
                                exit
                                ;;
        -d | --debug )          debug=1
                                ;;
    esac
    shift
done

makebuild
run