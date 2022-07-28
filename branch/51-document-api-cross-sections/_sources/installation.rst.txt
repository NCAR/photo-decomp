.. Installation instructions for TUV-x

Installation
============

Docker
------

The quickest way to get started with TUV-x is with Docker.
The only requirement is that you have `Docker Desktop <https://www.docker.com/get-started>`_
installed and running.

With Docker Desktop running, open a terminal window and run the following command to start the TUV-x container:

.. code-block:: bash

   docker run -it ghcr.io/NCAR/photo-decomp:main bash


Inside the container, you can run the TUV-x tests from the ``/build/`` folder:

.. code-block:: bash

   cd build/
   make test

Local Installation
------------------

To build TUV-x locally, you will need to have:

- NetCDF-Fortran
- `JSON-Fortran <https://github.com/jacobwilliams/json-fortran/archive/8.2.0.tar.gz>`_
- `nc4fortran <https://github.com/geospace-code/nc4fortran/archive/refs/tags/v1.4.2.tar.gz>`_

If you plan to run the TUV-x tests, you will also need Python 3.x with numpy and scipy.

To build TUV-x, from the root TUV-x folder run:

.. code-block:: bash

   mkdir build
   cd build/
   export JSON_FORTRAN_HOME=/path/to/json-fortran/installation/
   cmake ..
   make

The ``JSON_FORTRAN_HOME`` environment variable should point to the root JSON-Fortran
installation folder, such that the jsonfortran library is located at
``JSON_FORTRAN_HOME/lib/libjsonfortran.so``.

Depending on where you have the NetCDF and nc4fortran libraries installed, you may also
need to set the CMake variables for paths to their include folders and to the library files.
