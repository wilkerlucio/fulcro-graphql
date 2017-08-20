(ns fulcro-graphql.main
  (:require [fulcro-graphql.demos.github :as demos.github]
            [fulcro-graphql.demos.hackernews :as demos.hackernews]
            [fulcro-graphql.demos.contacts :as demos.contacts]))

#_ (demos.hackernews/init)
#_ (demos.github/init)
(demos.contacts/init)
