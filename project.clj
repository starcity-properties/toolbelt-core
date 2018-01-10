(defproject starcity/toolbelt-core "0.4.0-SNAPSHOT"
  :description "Generic utility functions for Clojure/Script."
  :url "https://github.com/starcity-properties/toolbelt-core"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]
  :deploy-repositories [["releases" {:url   "https://clojars.org/repo"
                                     :creds :gpg}]])
