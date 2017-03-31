(defproject project3 "0.1.0-SNAPSHOT"
  :description "PPL class project 3: Trying out clojure."
  :url "https://gitlab.com/CupricWolf/PPL-Project3-Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot project3.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  )
