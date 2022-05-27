#!/bin/bash

# exit on error
set -e
# turn on command echoing
set -v

exec_oldtuv() {
  ./oldtuv DO_RAYLEIGH DO_O2 DO_O3 DO_AEROSOLS DO_CLOUDS < test/regression/tuv_scenario_2.in
}
exec_newtuv() {
  ./tuv-x test/data/photorates.test.config.json
}
exec_analysis() {
  python3 test/regression/photolysis_rate/xsqy.compare.py test/regression/photolysis_rate OUTPUTS
}

if ! exec_oldtuv; then
  echo FAIL - old TUV
  exit 1
fi

if ! exec_newtuv; then
  echo FAIL - new TUV
  exit 1
fi

if ! exec_analysis; then
  echo FAIL - analysis
  exit 1
fi

echo PASS
exit 0
