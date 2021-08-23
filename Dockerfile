FROM fedora:33

RUN dnf -y update \
    && dnf -y install \
        gcc-fortran \
        gcc-c++ \
        gcc \
        netcdf-fortran-devel \
        cmake \
        make \
    && dnf clean all

COPY . /photo-decomp/

# install json-fortran
RUN curl -LO https://github.com/jacobwilliams/json-fortran/archive/8.2.0.tar.gz \
    && tar -zxvf 8.2.0.tar.gz \
    && cd json-fortran-8.2.0 \
    && export FC=gfortran \
    && mkdir build \
    && cd build \
    && cmake -D SKIP_DOC_GEN:BOOL=TRUE .. \
    && sudo make install

# install nc4fortran
RUN curl -LO https://github.com/geospace-code/nc4fortran/archive/refs/tags/v1.4.2.tar.gz \
      && tar -zxvf v1.4.2.tar.gz \
      && cd /nc4fortran-1.4.2 \
      && mkdir build \
      && cd build \
      && cmake .. \
      && make install

# build the photo-decomp tool
RUN mkdir /build \
      && cd /build \
      && export JSON_FORTRAN_HOME="/usr/local/jsonfortran-gnu-8.2.0" \
      && cmake /photo-decomp \
      && make