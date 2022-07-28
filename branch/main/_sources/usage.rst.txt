.. Usage information for TUV-x

Usage
=====

General
-------

Stand-alone TUV-x can be run from the command-line as:

.. code-block:: bash

   ./tuv-x configuration_file.json

The ``configuration_file.json`` contains the TUV-x configuration information described in
:doc:`configuration`.

TUV-x will output a file named ``photolysis_rate_constants.nc`` in the working directory. This
file will contain the photolysis rate constants for each reaction at each vertical level
for every time specified in the configuration file.
