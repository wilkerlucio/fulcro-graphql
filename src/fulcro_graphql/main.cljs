(ns fulcro-graphql.main
  (:require [fulcro-graphql.demos.github :as demos.github]
            [fulcro-graphql.demos.hackernews :as demos.hackernews]))

#_ (demos.hackernews/init)
(demos.github/init)
