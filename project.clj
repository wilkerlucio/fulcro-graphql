(defproject com.wsscode/fulcro-graphql "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.908"]
                 [org.clojure/core.async "0.3.443"]
                 [fulcrologic/fulcro "1.0.0-beta9-SNAPSHOT"]
                 [fulcrologic/fulcro-css "1.0.0"]
                 [org.omcljs/om "1.0.0-beta2-SNAPSHOT"]
                 [figwheel-sidecar "0.5.13"]
                 [binaryage/devtools "0.9.4"]
                 [spec-coerce "1.0.0-alpha5"]
                 [com.wsscode/pathom "1.0.0-beta7-SNAPSHOT"]]

  :cljsbuild {:builds [{:id           "site"
                        :figwheel     true
                        :source-paths ["src"]
                        :compiler     {:main                 fulcro-graphql.main
                                       :source-map-timestamp true
                                       :asset-path           "/site"
                                       :output-to            "resources/public/site/site.js"
                                       :output-dir           "resources/public/site"
                                       :preloads             [devtools.preload]
                                       :parallel-build       true
                                       :verbose              false}}]})
