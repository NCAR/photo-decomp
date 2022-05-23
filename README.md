PhotoDecomp
===========

PhotoDecomp: A photolysis rate constant calculator.

[![License](https://img.shields.io/github/license/NCAR/photo-decomp.svg)](https://github.com/NCAR/photo-decomp/blob/main/LICENSE)
[![CI Status](https://github.com/NCAR/photo-decomp/actions/workflows/test.yml/badge.svg)](https://github.com/NCAR/photo-decomp/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/NCAR/photo-decomp/branch/main/graph/badge.svg?token=H46AAEAQF9)](https://codecov.io/gh/NCAR/photo-decomp)

Copyright (C) 2020 National Center for Atmospheric Research

# Build and run (Docker version)

To build and run the stand-alone version of PhotoDecomp, you must have [Docker Desktop](https://www.docker.com/get-started) installed and running. With Docker Desktop running, open a terminal window, navigate to a folder where you would like the PhotoDecomp files to exist, and run the following commands:

```
git clone --recurse-submodules https://github.com/NCAR/photo-decomp.git
cd photo-decomp
docker build -t photo-test .
docker run -it photo-test bash
```
Then, from inside the Docker container run PhotoDecomp with:

```
cd build
./photo
```

# Build and run (local build version)

To build and run PhotoDecomp locally, you must have the following libraries available:

- [json-fortran](https://github.com/jacobwilliams/json-fortran)
- [NetCDF](https://www.unidata.ucar.edu/software/netcdf/) (both C and Fortran libraries)
- [nc4fortran](https://github.com/geospace-code/nc4fortran)

You must also have CMake installed on your machine. To build and run PhotoDecomp locally,open a terminal window, navigate to a folder where you would like the PhotoDecomp files to exist, and run the following commands:

```
git clone --recurse-submodules https://github.com/NCAR/photo-decomp.git
cd photo-decomp
mkdir build
cd build
ccmake ..
make
./photo
```
