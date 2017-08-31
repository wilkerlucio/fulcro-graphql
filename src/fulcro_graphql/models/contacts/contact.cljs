(ns fulcro-graphql.models.contacts.contact
  (:require [cljs.spec.alpha :as s]))

(s/def ::graph-node (s/keys :opt [::id ::name ::groups ::github-user]))

(s/def ::id string?)
(s/def ::github string?)

(s/def ::groups (s/coll-of :fulcro-graphql.models.contacts.group/graph-node))
(s/def ::github-user (s/and :fulcro-graphql.models.github.user/graph-node))
