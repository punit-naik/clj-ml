(defproject org.clojars.punit-naik/clj-ml "1.0.0"
  :description "Collection of ML and it's corresponsing utilities in Clojure"
  :url "https://github.com/punit-naik/clj-ml"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 ;; Hashing Library
                 [aesahaettr "0.1.2"]]
  :profiles {:uberjar {:aot :all}})
