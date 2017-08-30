(ns fulcro-graphql.models.github.user
  (:require [cljs.spec.alpha :as s]))

(s/def ::id string?)
(s/def ::login string?)
(s/def ::avatar-url string?)
(s/def ::company string?)
(s/def ::viewer-is-following boolean?)

#_ (s/def ::contact :fulcro-graphql.models.contacts.contact/graph-node)

(s/def ::graph-node (s/keys :opt [::id ::login ::contact]))
