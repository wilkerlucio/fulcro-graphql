(ns fulcro-graphql.models.github.user
  (:require [cljs.spec.alpha :as s]))

(s/def ::graph-node
  (s/keys :opt [::id ::login ::avatar-url ::company ::viewer-is-following ::contact ::starred-repositories]))

(s/def ::id string?)
(s/def ::login string?)
(s/def ::avatar-url string?)
(s/def ::company string?)
(s/def ::viewer-is-following boolean?)

(s/def ::contact (s/and :fulcro-graphql.models.contacts.contact/graph-node))
(s/def ::starred-repositories (s/coll-of :fulcro-graphql.models.github.repository/graph-node))
