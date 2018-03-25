(ns org.panchromatic.mokuhan.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            org.panchromatic.mokuhan-test
            org.panchromatic.mokuhan.renderer-test
            org.panchromatic.mokuhan.walker-test))

(enable-console-print!)

(doo-tests 'org.panchromatic.mokuhan.walker-test
           'org.panchromatic.mokuhan.renderer-test
           'org.panchromatic.mokuhan-test)
