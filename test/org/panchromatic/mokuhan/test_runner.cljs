(ns org.panchromatic.mokuhan.test-runner
  (:require [cljs.test :as t :include-macros true]
            org.panchromatic.mokuhan-test
            org.panchromatic.mokuhan.walker-test
            org.panchromatic.mokuhan.renderer-test))

(enable-console-print!)

(t/run-tests 'org.panchromatic.mokuhan.walker-test
             'org.panchromatic.mokuhan.renderer-test
             'org.panchromatic.mokuhan-test)
