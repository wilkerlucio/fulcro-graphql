(ns fulcro-graphql.models.contacts.repository
  (:require [cljs.spec.alpha :as s]))

(s/def ::id string?)
(s/def ::name string?)

#_ (s/def ::github :fulcro-graphql.models.github.repository/graph-node)
#_ (s/def ::groups (s/coll-of :fulcro-graphql.models.contacts.group/graph-node))

(s/def ::graph-node (s/keys :opt [::id ::name ::groups]))
