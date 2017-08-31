(ns fulcro-graphql.models.github.repository
  (:require [cljs.spec.alpha :as s]))

(s/def ::graph-node (s/keys :opt [::id ::name ::url ::owner ::viewer-has-starred]))

(s/def ::id string?)
(s/def ::name string?)
(s/def ::url string?)
(s/def ::viewer-has-starred boolean?)

(s/def ::owner (s/and :fulcro-graphql.models.github.user/graph-node))
(s/def ::contacts-repo (s/and :fulcro-graphql.models.contacts.repository/graph-node))
