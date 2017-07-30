(ns fulcro-graphql.main
  (:require [fulcro-graphql.demos.github :as demos.github]
            [fulcro-graphql.demos.hackernews :as demos.hackernews]))

(demos.hackernews/init)
#_ (demos.github/init)
