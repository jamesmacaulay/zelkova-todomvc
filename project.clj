(defproject zelkova-todomvc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :source-paths ["src/clj" "src/cljs"]

  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/clojurescript "0.0-2740"]
                 [jamesmacaulay/zelkova "0.2.1-SNAPSHOT"]
                 [com.facebook/react "0.11.2"]
                 [reagent "0.4.3"]
                 [reagent-forms "0.4.3"]
                 [reagent-utils "0.1.2"]
                 [secretary "1.2.1"]
                 [com.cemerick/piggieback "0.1.5"]
                 [weasel "0.5.0"]
                 [ring "1.3.2"]
                 [ring/ring-defaults "0.1.3"]
                 [prone "0.8.0"]
                 [compojure "1.3.1"]
                 [selmer "0.8.0"]
                 [environ "1.0.0"]
                 [leiningen "2.5.1"]
                 [figwheel "0.1.6-SNAPSHOT"]]

  :plugins [
            [lein-cljsbuild "1.0.4"]
            [lein-environ "1.0.0"]
            [lein-ring "0.9.0"]
            [lein-asset-minifier "0.2.2"]]

  :ring {:handler zelkova-todomvc.handler/app
         :uberwar-name "zelkova-todomvc.war"}

  :min-lein-version "2.5.0"

  :uberjar-name "zelkova-todomvc.jar"

  :main zelkova-todomvc.server

  :clean-targets ^{:protect false} ["resources/public/js"]

  :minify-assets
  {:assets
    {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild {:builds {:app {:source-paths ["src/cljs"]
                             :compiler {:output-to     "resources/public/js/app.js"
                                        :output-dir    "resources/public/js/out"
                                        :externs       ["react/externs/react.js"]
                                        :optimizations :none
                                        :pretty-print  true}}
                       :production {:source-paths ["env/prod/cljs"]
                                    :compiler {:output-to     "js/app.js"
                                               :output-dir    "js/out"
                                               :externs       ["react/externs/react.js"]
                                               :optimizations :advanced
                                               :pretty-print  false}}}}

  :profiles {:dev {:repl-options {:init-ns zelkova-todomvc.handler
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

                   :dependencies [[ring-mock "0.1.5"]
                                  [ring/ring-devel "1.3.2"]
                                  [pjstadig/humane-test-output "0.6.0"]]

                   :plugins [[lein-figwheel "0.2.0-SNAPSHOT"]]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :figwheel {:http-server-root "public"
                              :server-port 3449
                              :css-dirs ["resources/public/css"]
                              :ring-handler zelkova-todomvc.handler/app}

                   :env {:dev? true}

                   :cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                              :compiler {:source-map true}}
}
}}

             :uberjar {:hooks [leiningen.cljsbuild minify-assets.plugin/hooks]
                       :env {:production true}
                       :aot :all
                       :omit-source true
                       :cljsbuild {:jar true
                                   :builds {:app
                                             {:source-paths ["env/prod/cljs"]
                                              :compiler
                                              {:optimizations :advanced
                                               :pretty-print false}}}}}

             :production {:ring {:open-browser? false
                                 :stacktraces?  false
                                 :auto-reload?  false}}})
