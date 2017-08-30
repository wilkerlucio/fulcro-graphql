(ns fulcro-graphql.models.contacts.group
  (:require [cljs.spec.alpha :as s]))

(s/def ::id string?)
(s/def ::name string?)

#_ (s/def ::contacts (s/coll-of :fulcro-graphql.models.contacts.contact/graph-node))
#_ (s/def ::repositories (s/coll-of :fulcro-graphql.models.contacts.repository/graph-node))

(s/def ::graph-node (s/keys :opt [::id ::name ::contacts ::repositories]))
