#!/usr/bin/env sh

./test.sh > /dev/null 2>&1
test=$?
if [ $test -ne 0 ]; then
    echo "TESTS FAILED"
fi

./lint.sh > /dev/null 2>&1
lint=$?
if [ $lint -ne 0 ]; then
    echo "LINT FAILED"
fi

./fmt.sh > /dev/null 2>&1
fmt=$?
if [ $fmt -ne 0 ]; then
    echo "FMT FAILED"
fi

code=$(( $test | $lint | $fmt ))

if [ $code -eq 0 ]; then 
    echo "ALL CHECKS PASSED"
else
    echo "SOME CHECKS FAILED"
fi

exit $code
