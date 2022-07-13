.. Instructions for TUV-x developers

Science Contributions
=====================

To submit a new feature or bug-fix, first create an issue on the
`TUV-x GitHub repository <https://github.com/NCAR/photo-decomp>`_
that describes the new feature or bug. The software contribution can
then be submitted as a pull request. Please reference the issue in
the pull request description.

Software contributions must apply the
`MUSICA FORTRAN Style Guide <https://ncar.github.io/musica-core/html/coding_style.html>`_.
See also the `MUSICA Recommendations for Contributors <https://ncar.github.io/musica-core/html/contributors.html>`_.

Developers must ensure that new features have complete unit test
coverage. Pull requests that decrease the test coverage of the
TUV-x code base will not be accepted. Developers must also ensure that
new features are fully run-time configurable.

Cross Sections
--------------

<describe adding cross sections>

Dose Rates
----------

<describe adding dose rates>

Quantum Yields
--------------

The base functionality for quantum yield calculations is described in
``src/quantum_yield.F90``. The base quantum yield type ``quantum_yield_t``
defined in this module includes a constructor that reads a set of
NetCDF files specified in the configuration JSON object as an
array of file paths with the key ``netcdf files`` if present, or
can set the value of the quantum yield to a constant when the
``constant value`` key is present and set to a real number.

Here is an example configuration for a quantum yield:

.. code-block:: JSON

   {
     "type": "base",
     "constant value": 1.0
   }


Data from each NetCDF file will be loaded into an element of the
``quantum_yield_parms`` data member. If a NetCDF variable named
``quantum_yield_parameters`` is present, it will be used to populate
the ``array(:,:)`` data member of the ``quantum_yield_parms_t`` object.
This first dimension of the array is wavelength, and will be interpolated
to the native TUV-x wavelength grid if this differs from the wavelength
grid in the netcdf file (named ``wavelength``). The second dimension
is used to accomodate multiple wavelength-resolved parameters for
quantum yield calculations.

If a NetCDF variable named ``temperature`` is present, it will be
used to populate the ``temperature(:)`` data member of the
``quantum_yield_parms_t`` object.

The calculation of quantum yields is done by calling the ``calculate()``
type-bound procedure on a ``quantum_yield_t`` object.

The base-class calculation of quantum yields returns the
wavelength-interpolated first parameter (``array(:,1)``) from the first
NetCDF file (``quantum_yield_parms(1)``) specified in the configuration
data.

Quantum yield subclasses are located in the ``src/quantum_yields/`` folder.
These each provide unique algorithms for calculating quantum yields.

Before adding a new quantum yield class, first check to make sure there
is not an existing class that can be configured to accomodate your
needs. If you find that an existing quantum yield subclass could be used
by moving one or more hard-coded parameters to the configuration data, this
is preferable to adding a new subclass.

If you determine that a new quantum yield subclass is needed, this can be
done in three steps:

- :ref:`qy-create-subclass`
- :ref:`qy-add-to-build-scripts`
- :ref:`qy-add-to-factory`
- :ref:`qy-create-unit-test`

.. _qy-create-subclass:

Create subclass module
^^^^^^^^^^^^^^^^^^^^^^

First, choose a unique name for your quantum yield calculation. Ideally,
this name will describe the algorithm, rather than the specific photolysis
reaction you are applying it to. However, many subclasses currently in TUV-x
are named for specific photolysis reactions. For this example, we will use
the name ``foo`` for our quantum yield algorithm.

**Pay special attention to naming of files, modules, types, and functions
in these instructions.**

Create a file to hold your new subclass module in ``src/quantum_yields/`` named
``foo.F90``. The general layout of the module will be (comments have been omitted
for this example, but should be included in an actual module):

.. code-block:: fortran

   ! Copyright (C) 2020 National Center for Atmospheric Research
   ! SPDX-License-Identifier: Apache-2.0
   !
   module tuvx_quantum_yield_foo

     use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor

     implicit none
     private

     public :: quantum_yield_foo_t

     type, extends(quantum_yield_t) :: quantum_yield_foo_t
     contains
       procedure :: calculate => run
     end type quantum_yield_foo_t

     interface quantum_yield_foo_t
       module procedure constructor
     end interface

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     function constructor( config, grid_warehouse, profile_warehouse )           &
         result( this )

       use musica_config,                 only : config_t
       use tuvx_grid_warehouse,           only : grid_warehouse_t
       use tuvx_profile_warehouse,        only : profile_warehouse_t

       class(quantum_yield_t),    pointer       :: this
       type(config_t),            intent(inout) :: config
       type(grid_warehouse_t),    intent(inout) :: grid_warehouse
       type(profile_warehouse_t), intent(inout) :: profile_warehouse

       allocate( quantum_yield_foo_t :: this )

       ! You can call the base_constructor function to load data from NetCDF
       ! files into the `quantum_yield_parms(:)` data member according to the
       ! standard base class logic. Alternatively, you can perform custom
       ! initialization of the subclass object here.
       call base_constructor( this, config, grid_warehouse, profile_warehouse )

     end function constructor

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     function run( this, grid_warehouse, profile_warehouse )                     &
         result( quantum_yield )

       use musica_constants,              only : dk => musica_dk
       use tuvx_grid_warehouse,           only : grid_warehouse_t
       use tuvx_profile_warehouse,        only : profile_warehouse_t

       class(quantum_yield_foo_t), intent(in)    :: this
       type(grid_warehouse_t),     intent(inout) :: grid_warehouse
       type(profile_warehouse_t),  intent(inout) :: profile_warehouse
       real(kind=dk), allocatable                :: quantum_yield(:,:)

       ! Do your calculations here

     end function run

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   end module tuvx_quantum_yield_foo


The constructor function is reponsible for initializing new instances of your
quantum yield subclass. First, you allocate the pointer to be returned as
your new type (``quantum_yield_foo_t`` in this example). Then you initialize
its data members. If you just want to use the default initialization of the
base class, you can call the ``base_constructor()`` function as shown above.
You can alternatively initialize data members of the base class
(``quantum_yield_parms(:)``) directly in this function or add data members
to your subclass and initialize them here (see
``src/quantum_yields/tint.F90`` for an example).

The run function overrides the base-class run function and will be called when
a user calls the ``calculate()`` type-bound procedure on an instance of your
new subclass. You can access grid and profile data from the "warehouse" objects
passed in as function arguments, and any data in the base-class data members
or in data members you've added to your subclass to perform your calculations.
See the files in ``src/quantum_yields/`` for examples of how to access this
data in the ``run()`` function.

.. _qy-add-to-build-scripts:

Add subclass module to build scripts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To include your new class in the build, edit the ``src/quantum_yields/CMakeLists.txt``
file and add your file name to the list saved to ``SRC``. Files are listed in
alphabetical order.

.. code-block:: cmake
   :emphasize-lines: 12

   set(SRC acetone-ch3co_ch3.F90
        c2h5cho.F90
        ch2chcho.F90
        ch2o.F90
        ch3cho-ch3_hco.F90
        ch3coch2ch3-ch3co_ch2ch3.F90
        ch3cocho.F90
        clo-cl_o1d.F90
        clo-cl_o3p.F90
        clono2-clo_no2.F90
        clono2-cl_no3.F90
        foo.F90
        ho2-oh_o.F90
        mvk.F90
        no2_tint.F90
        no3_aq.F90
        o3-o2_o1d.F90
        o3-o2_o3p.F90
        tint.F90
        )

.. _qy-add-to-factory:

Add subclass to factory function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to use your new subclass, you will need to add it to the
``tuvx_quantum_yield_factory`` module in ``src/quantum_yield_factory.F90``.
First use-associate your new class at the module level:

.. code-block:: fortran

   use tuvx_quantum_yield_foo,            only : quantum_yield_foo_t

Then, inside the ``quantum_yield_builder()`` function, add these lines to the
``select case`` block:

.. code-block:: fortran

   case( 'foo' )
     quantum_yield => quantum_yield_foo_t( config, grid_warehouse,              &
                                           profile_warehouse )

Now, when you add a quantum yield of type ``foo`` to the configuration data,
an instance of your new subclass will be created.

.. _qy-create-unit-test:

Create unit test
^^^^^^^^^^^^^^^^

The last step to adding a quantum yield is to create a unit test. This will ensure
that your calculations are doing what you intended. It will also serve as an example
for how users can configure and use your new subclass.

.. note::

   Add instructions for creating quantum yield tests when tests for existing classes
   have been added.


Radiators
---------

<describe adding radiators>
