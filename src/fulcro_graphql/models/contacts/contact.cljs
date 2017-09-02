(ns fulcro-graphql.models.contacts.contact
  (:require [cljs.spec.alpha :as s]
            [om.next :as om]))

(s/def ::graph-node (s/keys :opt [::id ::name ::groups ::github-user]))

(s/def ::id (s/or :persistent string? :temp om/tempid?))
(s/def ::github (s/and string? #(-> % count (> 0))))

(s/def ::groups (s/coll-of :fulcro-graphql.models.contacts.group/graph-node))
(s/def ::github-user (s/and :fulcro-graphql.models.github.user/graph-node))

(s/def ::new-contact (s/keys :req [::github]))
