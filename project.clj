(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.finger-tree "0.0.3"]
                 [digest "1.4.10"]]
  :main ^:skip-aot aoc.core
  :target-path "target/%s"
  :jvm-opts ["-Xmx1g"]
  :profiles {:uberjar {:aot :all}})
