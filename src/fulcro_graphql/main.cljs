(ns fulcro-graphql.main
  (:require [cljs.spec.alpha :as s]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [com.wsscode.fulcro-graphql.network :refer [graphql-network]]
            [om.dom :as dom]
            [om.next :as om]))

(defn get-load-query [comp]
  (conj (om/get-query comp) :ui/fetch-state))

(defonce app
  (atom (fulcro/new-fulcro-client :networking (graphql-network "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"))))

(s/def :lifecycle/created-at inst?)

(om/defui ^:once UiLink
  static om/IQuery
  (query [_] [:link/id :link/description :lifecycle/created-at :link/url])

  static om/Ident
  (ident [_ props] [:link/by-id (:link/id props)])

  Object
  (render [this]
    (let [{:link/keys [description]
           :keys      [lifecycle/created-at] :as props} (om/props this)]
      (dom/div nil
        (str created-at) " - " description))))

(def ui-link (om/factory UiLink {:keyfn :link/id}))

(om/defui ^:once Root
  static om/IQuery
  (query [_] [{:link/all-links (get-load-query UiLink)}])

  Object
  (render [this]
    (let [{:keys [link/all-links]} (om/props this)]
      (dom/div nil
        (dom/button #js {:onClick #(fetch/load this :link/all-links UiLink)}
          "Load Links")
        (if (fetch/loading? (:ui/fetch-state all-links))
          (dom/div nil "Loading..."))
        (if (sequential? all-links)
          (map ui-link all-links))))))

(defn init []
  (swap! app fulcro/mount Root "app-container"))

(init)

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(s/def :link/updated-at inst?)


