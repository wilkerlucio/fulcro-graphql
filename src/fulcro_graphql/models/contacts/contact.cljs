(ns fulcro-graphql.models.contacts.contact
  (:require [cljs.spec.alpha :as s]))

(s/def ::id string?)
(s/def ::github string?)

#_ (s/def ::groups (s/coll-of :fulcro-graphql.models.contacts.group/graph-node))
#_ (s/def ::github-user :fulcro-graphql.models.github.user/graph-node)

(s/def ::graph-node (s/keys :opt [::id ::name ::groups ::github-user]))
