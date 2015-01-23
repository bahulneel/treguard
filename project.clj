(defproject treguard "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  :plugins [[lein-marginalia "0.8.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}}
  :marginalia {:javascript ["js/underscore-min.js"
                            "js/raphael-min.js"
                            "js/flowchart-latest.js"
                            "js/sequence-diagram-min.js"
                            "js/viz.js"
                            "js/jquery-2.1.1.min.js"
                            "js/main.js"]})
