(ns fulcro-graphql.models.github.repository
  (:require [cljs.spec.alpha :as s]))

(s/def ::id string?)
(s/def ::name string?)

#_ (s/def ::owner :fulcro-graphql.models.github.user/graph-node)
#_ (s/def ::contacts-repo :fulcro-graphql.models.contacts.repository/graph-node)

(s/def ::graph-node (s/keys :opt [::id ::name ::owner]))
