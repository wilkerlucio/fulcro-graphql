(ns fulcro-graphql.models.contacts.group
  (:require [cljs.spec.alpha :as s]))

(s/def ::graph-node (s/keys :opt [::id ::name ::contacts ::repositories]))

(s/def ::id string?)
(s/def ::name string?)

(s/def ::contacts (s/coll-of :fulcro-graphql.models.contacts.contact/graph-node))
(s/def ::repositories (s/coll-of :fulcro-graphql.models.contacts.repository/graph-node))
