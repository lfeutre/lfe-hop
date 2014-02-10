(defmodule lfe-hop_tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")


(deftest first
  (is-equal 1 (: lfe-hop first '(1 2 3)))
  (is-equal 1 (: lfe-hop first '(1)))
  (is-equal 'false (: lfe-hop first '())))
