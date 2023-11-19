#!/bin/bash
emacs --quick --batch --load=ert \
      --load=test/mood-line-test.el \
      --load=test/mood-line-segment-vc-test.el \
      --funcall=ert-run-tests-batch-and-exit
